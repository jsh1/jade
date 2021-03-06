/* redisplay.c -- Redisplay algorithms
   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "jade.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/* The upper bound on edit operations per window. Zero denotes unbounded. */
static int redisplay_max_d = 0;

/* When COMPARE_FAST_AND_LOOSE is defined as one just compare hash
   codes of lines to see if they match. Obviously, if the hash codes
   of two lines match, they may not actually be the same, but this
   is very unlikely. (only 80 columns (160 bytes with attrs) are
   hashed usually, and the hash function rotates to avoid overflow..) */
#ifndef COMPARE_FAST_AND_LOOSE
# define COMPARE_FAST_AND_LOOSE 1
#endif

/* Define DEBUG to get lists of every single screen update (i.e. which
   lines are drawn, which are copied, etc) */
#undef DEBUG

/* When greater than zero, we're in redisplay, and hence no Lisp code
   should be run, and no asynchronous input should be taken. */
int redisplay_lock;

/* When true the next redisplay operation won't try to copy damaged lines.
   Cleared after each redisplay pass. */
static bool redisplay_no_copy;


/* Glyph buffer basics */

/* Allocate a new glyph buffer and initialise it. */
glyph_buf *
alloc_glyph_buf(intptr_t cols, intptr_t rows)
{
    size_t size = (sizeof(glyph_buf)
		   + sizeof(uint8_t *) * rows
		   + sizeof(uint8_t *) * rows
		   + sizeof(uintptr_t) * rows
		   + sizeof(uint8_t) * rows * cols * 2);
    glyph_buf *g = rep_alloc(size);
    if(g == 0)
	abort();
    else
    {
	/* Initialise pointers */
	uint8_t *p = ((uint8_t *)g) + sizeof(glyph_buf);
	intptr_t i;
	g->cols = cols;
	g->rows = rows;
	g->codes = (void *)p;
	p += sizeof(uint8_t *) * rows;
	g->attrs = (void *)p;
	p += sizeof(uint8_t *) * rows;
	g->hashes = (void *)p;
	p += sizeof(uintptr_t) * rows;
	for(i = 0; i < rows; i++)
	{
	    memset (p, 0, cols * sizeof(uint8_t) * 2);
	    g->codes[i] = p;
	    p += sizeof(uint8_t) * cols;
	    g->attrs[i] = p;
	    p += sizeof(uint8_t) * cols;
	}
    }
    return g;
}

/* Return a glyph buffer to the heap. */
void
free_glyph_buf(glyph_buf *gb)
{
    rep_free(gb);
}

void
copy_glyph_buf(glyph_buf *dst, glyph_buf *src)
{
    memcpy(dst->codes[0], src->codes[0],
	   sizeof(uint8_t) * dst->rows * dst->cols * 2);
    memcpy(dst->hashes, src->hashes, sizeof(dst->hashes[0]) * dst->rows);
}

/* Compute and return the hash code of line ROW in buffer G. */
static inline uintptr_t
hash_glyph_row(glyph_buf *g, intptr_t row)
{
    uintptr_t value = 0;
    intptr_t togo = g->cols;
    uint8_t *codes = g->codes[row];
    uint8_t *attrs = g->attrs[row];

    while(togo-- > 0)
    {
	uintptr_t tem = *attrs++;
	tem = (tem << 7) | *codes++;
	value = (value * 33) + tem;
    }

    return value;
}

/* Hash every row in glyph buffer G. */
static inline void
hash_glyph_buf(glyph_buf *g)
{
    intptr_t i;
    for(i = 0; i < g->rows; i++)
	g->hashes[i] = hash_glyph_row(g, i);
}

/* Record that the WIDTH,HEIGHT rectangle of glyphs at X,Y in window W's
   glyph buffer has been mangled, and therefore will need to be redrawn. */
void
garbage_glyphs(Lisp_Window *w, intptr_t x, intptr_t y,
	       intptr_t width, intptr_t height)
{
    glyph_buf *g = w->content;
    if(x + width > g->cols)
	width = g->cols - x;
    if(y + height > g->rows)
	height = g->rows - y;
    while(height > 0)
    {
	memset(g->attrs[y] + x * sizeof(uint8_t), GA_Garbage, width);
	g->hashes[y] = hash_glyph_row(g, y);
	y++; height--;
    }
}

/* Returns true iff line L1 of glyph buffer G1 exactly matches line L2 of
   buffer G2 (matching both codes and attributes). The hash arrays should
   have been filled in. In this function L1 and L2 count from zero.. */
static inline bool
compare_lines(glyph_buf *g1, glyph_buf *g2, intptr_t line1, intptr_t line2)
{
#if COMPARE_FAST_AND_LOOSE
    return g1->hashes[line1] == g2->hashes[line2];
#else
    return (g1->hashes[line1] == g2->hashes[line2]
	    && !memcmp(g1->codes[line1], g2->codes[line1],
		       g1->cols * sizeof(uint8_t) * 2));
#endif
}


/* Screen primitives */

/* Draw a single line of glyphs: LINE in NEW-G, given that the
   current contents of this line are contained in OLD-G at LINE.
   In this function LINE counts from one.. */
static void
redisplay_do_draw(Lisp_Window *w, glyph_buf *old_g,
		  glyph_buf *new_g, intptr_t line)
{
    /* Draw LINE from NEW-G. OLD-G[LINE] _will_ reflect the currently
       displayed contents of LINE. */
    uint8_t *old_codes = old_g->codes[line-1];
    uint8_t *new_codes = new_g->codes[line-1];
    uint8_t *old_attrs = old_g->attrs[line-1];
    uint8_t *new_attrs = new_g->attrs[line-1];

    intptr_t prefix, suffix;

    assert(line > 0);

    /* First try to find a common prefix to the two lines */
    for(prefix = 0; prefix < old_g->cols; prefix++)
	if(old_codes[prefix] != new_codes[prefix]
	   || old_attrs[prefix] != new_attrs[prefix])
	    break;

    /* Then the suffix, unless all of the lines matched already */
    if(prefix == old_g->cols)
	return;
    suffix = old_g->cols - 1;
    while(1)			/* it's impossible to pass column zero */
    {
	if(old_codes[suffix] != new_codes[suffix]
	   || old_attrs[suffix] != new_attrs[suffix])
	{
	    suffix++;
	    break;
	}
	suffix--;
    }

    /* So we have PREFIX glyphs in common at the start of the line, and
       SUFFIX in common at the end. Just draw the bit inbetween,
       taking care to detect attribute changes. Also track if the chunk
       to be drawn consists solely of SPC characters (if so we can
       just fill or clear a rectangle, instead of drawing the text) */
    while(prefix < suffix)
    {
	uint8_t attr = new_attrs[prefix];
	intptr_t end, len;
	bool all_spaces = true;

	for(end = prefix; end < suffix; end++)
	{
	    if(new_attrs[end] != attr)
		break;
	    if(all_spaces && new_codes[end] != ' ')
		all_spaces = false;
	}
	len = end - prefix;

	SYS_DRAW_GLYPHS(w, prefix, line-1, attr,
			(char *) new_codes + prefix, len, all_spaces);

	prefix = end;
    }
}

/* Copy N-LINES from SRC-LINE to DST-LINE (note that SRC-LINE and DST-LINE
   both count from one). Updates OLD-G to reflect this. In this function
   SRC-LINE and DST-LINE count from one.. */
static void
redisplay_do_copy(Lisp_Window *w, glyph_buf *old_g, glyph_buf *new_g,
		  intptr_t src_line, intptr_t dst_line,
		  intptr_t n_lines)
{
    intptr_t i;

    assert(src_line > 0 && dst_line > 0);

    if (!redisplay_no_copy)
	COPY_GLYPHS(w, 0, src_line - 1, w->column_count, n_lines, 0, dst_line - 1);
    else
    {
	for (i = 0; i < n_lines; i++)
	    redisplay_do_draw (w, old_g, new_g, dst_line + i);
    }
    
    memmove(old_g->codes[dst_line-1], old_g->codes[src_line-1],
	    sizeof(uint8_t) * n_lines * old_g->cols * 2);
    memmove(old_g->hashes + (dst_line-1), old_g->hashes + (src_line-1),
	    sizeof(old_g->hashes[0]) * n_lines);
}


/* Glyph buffer diffing

   This uses the algorithm described in Chapter 3 of:

	@Book{Miller:1987:STS,
	  author =       "Webb Miller",
	  title =        "A Software Tools Sampler",
	  year =         "1987",
	  publisher =    "Prentice-Hall, Inc.",
	  address =      "Englewood Cliffs, New Jersey",
	}

   and the paper:

	@Article{Miller:1985:FCP,
	  author =       "Webb Miller and Eugene W. Myers",
	  title =        "A File Comparison Program",
	  journal =      "Software---Practice and Experience",
	  volume =       "15",
	  number =       "11",
	  year =         "1985",
	  month =        nov,
	  pages =        "1025--1040",
	}

   Apparently GNU diff(1) also uses it. Anyway, the procedure for
   displaying a glyph buffer given the current contents of the display
   (in a second glyph buffer) is as follows:

	1. Use the fcomp algorithm from the references above to
	   compute a minimal edit script to change the old buffer
	   into the new buffer. The script consists of a sequence
	   of INSERT line or DELETE line operations.

	2. Use the edit script to produce an array of ``links'' from
	   the new buffer to the old buffer. Each line has a single
	   link, either the row in the old buffer that should be
	   displayed at this line, or -1 meaning it has to be
	   drawn from scratch.

	3. Use the link array to update the display, this is done
	   in two passes, copies (i.e. scrolling) followed by
	   drawing. The copy pass updates the link array when
	   it munges displayed lines.

   I can only see one obvious shortcoming (apart from the overall
   complexity), step (3) may not find the optimal COPY sequence. It
   works strictly from the top of the link array to the bottom,
   possibly preventing copies lower down.. */

/* The two types of edit operation. */
enum Edit_Op {
    Edit_Insert = 0,
    Edit_Delete
};

/* One operation in the edit script. */
struct edit_script {
    struct edit_script *link;		/* previous edit instruction */
    enum Edit_Op op;			/* insertion or deletion? */
    intptr_t line1;			/* line number in buffer 1 */
    intptr_t line2;			/* line number in buffer 2 */
};

#ifdef DEBUG
/* Print the unprocessed edit script. */
static void
dump_script(struct edit_script *start)
{
    fprintf(stderr, "script (reversed) = {\n");
    while(start != 0)
    {
	fprintf(stderr, "    { %s, L1=%ld L2=%ld }\n",
		start->op == Edit_Insert ? "Insert" : "Delete",
		start->line1, start->line2);
	start = start->link;
    }
    fprintf(stderr, "}\n");
}

static void
dump_glyph_line (uint8_t *data, intptr_t length)
{
    fputs ("{ ", stderr);
    fwrite (data, length, 1, stderr);
    fputs (" }", stderr);
}

static void
dump_glyph_buf (glyph_buf *g)
{
    intptr_t row;
    fprintf (stderr, "\nGlyph buffer %p (%dx%d):", g, g->cols, g->rows);
    for (row = 0; row < g->rows; row++)
    {
	fprintf (stderr, "\ncodes[%03d] = ", row);
	dump_glyph_line (g->codes[row], g->cols);
	fprintf (stderr, "\nattrs[%03d] = ", row);
	dump_glyph_line (g->attrs[row], g->cols);
    }
    fputs ("\n", stderr);
}
#endif

/* Use the constructed minimal edit script to patch W from the current
   contents as depicted by OLD-G to the desired contents as shown by NEW-G.
   POINT should be the (currently reversed) edit script. */
static void
execute_script(Lisp_Window *w, glyph_buf *old_g, glyph_buf *new_g,
	       struct edit_script *point)
{
    struct edit_script *lookahead, *behind;
    intptr_t *links;			/* zeroth element unused */
    intptr_t current1 = 1, last2 = 1, i;

    /* First of all, reverse the script */
    lookahead = point;
    point = 0;
    while(lookahead != 0)
    {
	behind = point;
	point = lookahead;
	lookahead = lookahead->link;
	point->link = behind;		/* flip the pointer */
    }

    links = alloca(sizeof(intptr_t) * (old_g->rows + 1));

    /* Make the links. LINKS[K] is the position of the line currently
       displayed (i.e. in A) that should be displayed at line K after
       refresh is complete. */
    while(point != 0)
    {
	assert (current1 > 0 && last2 > 0);
	if(point->op == Edit_Insert)
	{
	    while(last2 < point->line2)
		links[last2++] = current1++;
	    links[last2++] = -1;
	}
	else if(point->op == Edit_Delete)
	{
	    while(current1 < point->line1)
		links[last2++] = current1++;
	    current1++;
	}
	point = point->link;
    }
    while(last2 <= new_g->rows)
	links[last2++] = current1++;

#ifdef DEBUG
    fprintf(stderr, "Drawing: ");
#endif

    /* Use the links
       Pass1: do all copying */
    i = 1;
    while(i <= new_g->rows)
    {
	intptr_t begin, delta, src, dst, n;

	/* Skip lines that must be drawn in pass 2 */
	while(i <= new_g->rows && links[i] == -1)
	    i++;

	begin = i;
	delta = links[i] - i;

	/* Find the length of the current section of lines that
	   can be copied in one chunk (i.e. because the distance
           between the line and the line to be copied to it is the
	   same across all the lines in the section.) */
	while(i <= new_g->rows && (links[i] - i) == delta && links[i] != -1)
	    i++;

	/* Start of a new section. We need to display lines
	   LINKS[BEGIN]->LINKS[i-1] of the old display, at line
	   BEGIN of the new. */

	src = links[begin];
	dst = begin;
	n = i - begin;

	/* Skip a null copy */
	if(src == dst || n == 0)
	    continue;

	redisplay_do_copy(w, old_g, new_g, src, dst, n);
#ifdef DEBUG
	fprintf(stderr, " COPY(%d: %d->%d) ", n, src, dst);
#endif

	/* In all lines yet to be updated, make sure that the
	   link to the line to be drawn there still points to
	   the same line. And if that line has been overwritten
	   flag that it must be redrawn from scratch. */
	if(src < dst)
	{
	    /* We're scrolling down the page */
	    intptr_t j;
	    for(j = i; j <= new_g->rows; j++)
	    {
		intptr_t link_j = links[j];
		if(link_j >= dst && link_j < src + n)
		{
		    /* The line to be drawn at J was one of the
		       lines we just moved. Update the link */
		    links[j] += (dst - src);
#ifdef DEBUG
		    fprintf(stderr, " RELOC(%d: %d->%d) ",
			    j, link_j, links[j]);
#endif
		}
		else if(link_j >= src + n && link_j < dst + n)
		{
		    /* We just overwrote the line to be displayed
		    at line J :-( */
		    links[j] = -1;
#ifdef DEBUG
		    fprintf(stderr, " TRASH(%d) ", j);
#endif
		}
	    }
	}
	else
	{
	    /* We're scrolling up the page */
	    intptr_t j;
	    for(j = i; j <= new_g->rows; j++)
	    {
		intptr_t link_j = links[j];
		if(link_j >= dst && link_j < src)
		{
		    /* Overwritten */
		    links[j] = -1;
#ifdef DEBUG
		    fprintf(stderr, " TRASH(%d) ", j);
#endif
		}
		else if(link_j >= src && link_j <= dst + n)
		{
		    /* Moved */
		    links[j] -= (src - dst);
#ifdef DEBUG
		    fprintf(stderr, " RELOC(%d: %d->%d) ",
			    j, link_j, links[j]);
#endif
		}
	    }
	}
    }

    /* Pass2: do all drawing */
    for(i = 1; i <= new_g->rows; i++)
    {
	if(links[i] == -1)
	{
	    redisplay_do_draw(w, old_g, new_g, i);
#ifdef DEBUG
	    fprintf(stderr, " DRAW(%d) ", i);
#endif
	}
    }
#ifdef DEBUG
    fprintf(stderr, "\n");
#endif
}


/* MAX-D is bound on size of edit script (zero means unbounded). Returns
   true if the comparison went ok and everything's been redisplayed.
   Returns false when more than MAX-D edit instructions were needed. */
static bool
patch_display(Lisp_Window *w, glyph_buf *old_g, glyph_buf *new_g)
{
    intptr_t col;			/* column number */
    intptr_t d;			/* current edit distance */
    intptr_t lower;			/* left-most diag under consid. */
    intptr_t k;			/* current diagonal */
    intptr_t row;			/* row number */
    intptr_t upper;			/* right-most diagonal under
					   consideration */
    int max_d;

#define ORIGIN (old_g->rows)		/* for accessing the arrays */
    
    /* for each diagonal, two items are saved: */
    intptr_t *last_d;		/* row containing the last d */
    struct edit_script **script;	/* corresponding edit script */

    struct edit_script *new;

    assert(old_g->rows == new_g->rows && old_g->cols == new_g->cols);

#ifdef DEBUG
    /* Print the contents of the buffers */
    fprintf (stderr, "\npatch_display; from\n");
    dump_glyph_buf (old_g);
    fprintf (stderr, "\nto:\n");
    dump_glyph_buf (new_g);
#endif

    last_d = alloca(sizeof(intptr_t) * (2 * old_g->rows + 1));
    script = alloca(sizeof(struct edit_script *) * (2 * old_g->rows + 1));

    if(redisplay_max_d == 0)
	max_d = 2 * old_g->rows;	/* no limit */
    else
	max_d = redisplay_max_d;

    /* initialise: 0 entries in D indicate identical prefixes. */
    for(row = 0;
	row < old_g->rows && row < new_g->rows
	&& compare_lines(old_g, new_g, row, row);
	++row) ;

    last_d[ORIGIN] = row;
    script[ORIGIN] = 0;
    lower = (row == old_g->rows) ? ORIGIN + 1 : ORIGIN - 1;
    upper = (row == new_g->rows) ? ORIGIN - 1 : ORIGIN + 1;
    if(lower > upper)
	return true;			/* buffers are identical */

    /* for each value of the edit distance... */
    for(d = 1; d <= max_d; ++d)
    {
	/* for each relevant diagonal... */
	for(k = lower; k <= upper; k += 2)
	{
	    /* get space for the next edit instruction */
	    new = (struct edit_script *) alloca(sizeof(struct edit_script));

	    /* find a d on diagonal k */
	    if(k == ORIGIN - d
	       || (k != ORIGIN + d && last_d[k+1] >= last_d[k-1]))
	    {
		/* Moving down from the last d-1 on diagonal k+1
		   puts you farther along diagonal k than does
		   moving right from the last d-1 on diagonal k-1. */
		row = last_d[k+1] + 1;
		new->link = script[k+1];
		new->op = Edit_Delete;
	    }
	    else
	    {
		/* Move right from the last d-1 on diagonal k-1 */
		row = last_d[k-1];
		new->link = script[k-1];
		new->op = Edit_Insert;
	    }
	    /* Code common to the two cases */
	    new->line1 = row;
	    new->line2 = col = row + k - ORIGIN;
	    script[k] = new;

	    /* Slide down the diagonal */
	    while(row < old_g->rows
		  && col < new_g->rows
		  && compare_lines(old_g, new_g, row, col))
	    {
		++row;
		++col;
	    }
	    last_d[k] = row;

	    if(row == old_g->rows && col == new_g->rows)
	    {
		/* Hit southeast corner; have the answer */
#ifdef DEBUG
		dump_script(script[k]);
#endif
		execute_script(w, old_g, new_g, script[k]);
		return true;
	    }

	    if(row == old_g->rows)
		/* Hit last row; don't look to the left */
		lower = k+2;

	    if(col == new_g->rows)
		/* Hit last column; don't look to the right */
		upper = k-2;
	}
	--lower;
	++upper;
    }

    /* Too many edit operations, give up. */
    return false;

#undef ORIGIN
}


/* Putting it all together */

DEFUN_INT ("redisplay-window", Fredisplay_window, Sredisplay_window,
	   (repv win, repv arg), rep_Subr2, "\nP") /*
::doc:redisplay-window::
redisplay-window [WINDOW] [FORCE]

Redisplay everything that needs to be in WINDOW (or the current window).

When FORCE (the raw prefix arg) is non-nil, absolutely everything is
refreshed, not just what changed.
::end:: */
{
    Lisp_Window *w;
    rep_DECLARE1 (win, WINDOWP);
    w = VWINDOW (win);

#ifdef DEBUG
    fprintf(stderr, "Entering redisplay..\n");
#endif

    redisplay_lock++;

    if (w->w_Window != WINDOW_NIL)
    {
	glyph_buf *tem;

	if(arg != Qnil || (w->car & WINFF_FORCE_REFRESH))
	{
	    /* Must redraw this window. The easiest way to do this
	       is to just garbage the entire contents */
	    garbage_glyphs(w, 0, 0, w->column_count, w->row_count);
	    w->car &= ~(WINFF_FORCE_REFRESH | WINFF_PRESERVING);
	}

	if((w->car & WINFF_PRESERVING) == 0)
	{
	    make_window_glyphs(w->new_content, w);
	    hash_glyph_buf(w->new_content);
	}

#ifdef SYS_BEGIN_REDISPLAY
	SYS_BEGIN_REDISPLAY (w);
#endif

	if(!patch_display(w, w->content, w->new_content))
	{
	    /* MAX-D was exceeded. Draw all lines manually. */
	    intptr_t row;
	    for(row = 1; row <= w->row_count; row++)
		redisplay_do_draw(w, w->content, w->new_content, row);
	}

#ifdef SYS_END_REDISPLAY
	SYS_END_REDISPLAY (w);
#endif

	/* Flip the old and new glyph buffers. */
	tem = w->new_content;
	w->new_content = w->content;
	w->content = tem;
	w->car &= ~WINFF_PRESERVING;

	/* See if we should update the window name */
	if(w->current_view->tx->status_string != 0
	   && w->displayed_name != w->current_view->tx->status_string)
	{
	    w->displayed_name = w->current_view->tx->status_string;
	    sys_set_win_name(w, rep_STR(w->displayed_name));
	}
    }
    redisplay_lock--;

#ifdef DEBUG
    fprintf(stderr, "Leaving redisplay.\n");
#endif

    return Qt;
}

DEFUN_INT("redisplay", Fredisplay, Sredisplay, (repv arg), rep_Subr1, "P") /*
::doc:redisplay::
redisplay [FORCE]

Redisplay everything that needs to be. When FORCE (the raw prefix arg) is
non-nil, absolutely everything is refreshed, not just what changed.
::end:: */
{
    Lisp_Window *w;

    for(w = win_chain; w != 0; w = w->next)
    {
	if (w->w_Window != WINDOW_NIL)
	{
	    Fredisplay_window (rep_VAL (w), arg);
	}
    }

    Fflush_output();
    return Qt;
}

static void
redisplay (void)
{
    Fredisplay (Qnil);
}

DEFUN("redisplay-max-d", var_redisplay_max_d, Sredisplay_max_d, (repv val), rep_Subr1) /*
::doc:redisplay-max-d::
redisplay-max-d [NEW-VALUE]

The upper bound on the number of edit operations (insert line, or delete
line) per window redisplay. Zero means unbounded (i.e. 2*ROWS).

When the limit is exceeded the search for an optimal edit script is
aborted and each row of the window is redisplayed manually.
::end:: */
{
    return rep_handle_var_int(val, &redisplay_max_d);
}

/* Just refresh the contents of the message displayed at the bottom
   of window W. */
void
redisplay_message(Lisp_Window *w)
{
    if(w->w_Window == 0 || !(w->car & WINFF_MESSAGE))
	return;

    redisplay_lock++;

#ifdef SYS_BEGIN_REDISPLAY
    SYS_BEGIN_REDISPLAY (w);
#endif

    /* Copy existing contents of the message to new_content */
    memcpy(w->new_content->codes[w->row_count - 1],
	   w->content->codes[w->row_count - 1],
	   w->column_count * sizeof(uint8_t) * 2);
    w->new_content->hashes[w->row_count-1] = w->content->hashes[w->row_count-1];

    make_message_glyphs(w->content, w);
    w->content->hashes[w->row_count-1]
        = hash_glyph_row(w->content, w->row_count-1);
    redisplay_do_draw(w, w->new_content, w->content, w->row_count);

#ifdef SYS_END_REDISPLAY
    SYS_END_REDISPLAY (w);
#endif

    Fflush_output();

    redisplay_lock--;
}

void
redisplay_set_no_copy (void)
{
    redisplay_no_copy = true;
}

void
redisplay_init(void)
{
    rep_ADD_SUBR_INT(Sredisplay);
    rep_ADD_SUBR_INT(Sredisplay_window);
    rep_ADD_SUBR(Sredisplay_max_d);
    rep_redisplay_fun = redisplay;
}
