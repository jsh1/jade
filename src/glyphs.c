/* glyphs.c -- Construction of character images
   Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <string.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

static long line_glyph_length(Lisp_Buffer *tx, long line);

DEFSYM(glyph_table, "glyph-table");

int glyph_table_type;


/* Glyph table data structures */

/* This table stores the number of glyphs which have to be printed to
   make up each character. 0 is a special case -- TABSIZE spaces.  */
typedef char glyph_widths_t[256];

/* This stores the glyphs which have to be printed to create each
   single character, each character (except tabs) can be made of
   no more than 4 glyphs. The above table is used to decide how
   many to use out of the possible 4.  */
typedef u_char glyph_glyphs_t[256][4];

typedef struct glyph_table {
    repv		gt_Car;
    struct glyph_table *gt_Next;
    glyph_widths_t	gt_Widths;
    glyph_glyphs_t	gt_Glyphs;
} glyph_table_t;
#define GTF_STATIC (1 << rep_CELL16_TYPE_BITS)	/* Don't free() this table */

static glyph_table_t default_glyph_table = {
    GTF_STATIC,
    NULL,
    {
	2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
    },
    {
	"^@  ", "^A  ", "^B  ", "^C  ", "^D  ", "^E  ", "^F  ", "^G  ",
	"^H  ", " TAB", "^J  ", "^K  ", "^L  ", "^M  ", "^N  ", "^O  ",
	"^P  ", "^Q  ", "^R  ", "^S  ", "^T  ", "^U  ", "^V  ", "^W  ",
	"^X  ", "^Y  ", "^Z  ", "^[  ", "^\\  ", "^]  ", "^^  ", "^_  ",
	"    ", "!   ", "\"   ", "#   ", "$   ", "%   ", "&   ", "'   ",
	"(   ", ")   ", "*   ", "+   ", ",   ", "-   ", ".   ", "/   ",
	"0   ", "1   ", "2   ", "3   ", "4   ", "5   ", "6   ", "7   ",
	"8   ", "9   ", ":   ", ";   ", "<   ", "=   ", ">   ", "?   ",
	"@   ", "A   ", "B   ", "C   ", "D   ", "E   ", "F   ", "G   ",
	"H   ", "I   ", "J   ", "K   ", "L   ", "M   ", "N   ", "O   ",
	"P   ", "Q   ", "R   ", "S   ", "T   ", "U   ", "V   ", "W   ",
	"X   ", "Y   ", "Z   ", "[   ", "\\   ", "]   ", "^   ", "_   ",
	"`   ", "a   ", "b   ", "c   ", "d   ", "e   ", "f   ", "g   ",
	"h   ", "i   ", "j   ", "k   ", "l   ", "m   ", "n   ", "o   ",
	"p   ", "q   ", "r   ", "s   ", "t   ", "u   ", "v   ", "w   ",
	"x   ", "y   ", "z   ", "{   ", "|   ", "}   ", "~   ", "^?  ",
	"\\200", "\\201", "\\202", "\\203", "\\204", "\\205", "\\206", "\\207",
	"\\210", "\\211", "\\212", "\\213", "\\214", "\\215", "\\216", "\\217",
	"\\220", "\\221", "\\222", "\\223", "\\224", "\\225", "\\226", "\\227",
	"\\230", "\\231", "\\232", "\\233", "\\234", "\\235", "\\236", "\\237",
	"\\240", "\\241", "\\242", "\\243", "\\244", "\\245", "\\246", "\\247",
	"\\250", "\\251", "\\252", "\\253", "\\254", "\\255", "\\256", "\\257",
	"\\260", "\\261", "\\262", "\\263", "\\264", "\\265", "\\266", "\\267",
	"\\270", "\\271", "\\272", "\\273", "\\274", "\\275", "\\276", "\\277",
	"\\300", "\\301", "\\302", "\\303", "\\304", "\\305", "\\306", "\\307",
	"\\310", "\\311", "\\312", "\\313", "\\314", "\\315", "\\316", "\\317",
	"\\320", "\\321", "\\322", "\\323", "\\324", "\\325", "\\326", "\\327",
	"\\330", "\\331", "\\332", "\\333", "\\334", "\\335", "\\336", "\\337",
	"\\340", "\\341", "\\342", "\\343", "\\344", "\\345", "\\346", "\\347",
	"\\350", "\\351", "\\252", "\\353", "\\354", "\\355", "\\356", "\\357",
	"\\360", "\\361", "\\362", "\\363", "\\364", "\\365", "\\366", "\\367",
	"\\370", "\\371", "\\372", "\\373", "\\374", "\\375", "\\376", "\\377"
    }
};

static glyph_table_t *gt_chain = &default_glyph_table;


/* Data structures for the cache of "number of glyphs in a line" */

/* These next two parameters can obviously take some tuning. Intuitively,
   I think that there should always be more sets than there are lines in
   the display. Associativity is harder to pin down, the higher the
   better maybe, but then more searching needs to be done. It's also the
   case that a lot of misses are due to the blanket invalidation, so
   making the cache too big is pointless anyway.. */
#define GL_CACHE_SETS	256		/* number of cache sets */
#define GL_CACHE_ASSOC	1		/* entries in each set */

/* Map a line number to a gl-cache set */
#define GL_MAP_LINE(l) ((l) % GL_CACHE_SETS)

typedef struct {
    u_long line;			/* line number */
    Lisp_Buffer *tx;				/* buffer */
    u_long glyphs;			/* number of glyphs in line */
    u_long changes;			/* change-count at calc. time */
#if GL_CACHE_ASSOC > 1
    u_long lru_clock;			/* last access time */
#endif
} gl_cache_entry_t;

typedef struct {
    gl_cache_entry_t data[GL_CACHE_SETS * GL_CACHE_ASSOC];
#if GL_CACHE_ASSOC > 1
    u_long lru_clock;
#endif
    u_long misses, valid_hits, invalid_hits;
} gl_cache_t;

/* Get a pointer to the array of entries forming set S */
#define GL_GET_SET(gl, s) ((gl)->data + ((s) * GL_CACHE_ASSOC))

static gl_cache_t gl_cache;


/* Filling glyph buffers */

/* Fill glyph buffer G with whatever should be displayed in window W. */
void
make_window_glyphs(glyph_buf *g, Lisp_Window *w)
{
    static u_char spaces[] = "                                                 "
"                                                                            ";

    Lisp_View *vw;

    free_visible_extents (w);
    for(vw = w->view_list; vw != 0; vw = vw->next_view)
    {
	glyph_widths_t *width_table;
	glyph_glyphs_t *glyph_table;
	repv glyph_tab = Fbuffer_symbol_value(Qglyph_table,
					      vw->display_origin,
					      rep_VAL(vw->tx), Qt);
	glyph_attr attr = 0;
	int tab_size = vw->tx->tab_size;
	long first_col, first_row, first_char_col;
	int glyph_row, last_row, char_row;
	long cursor_col;

	bool in_block, rect_block = FALSE, block_active = FALSE;
	long block_start = 0, block_end = 0;

	Lisp_Extent *extent;
	long extent_delta;
	Pos next_extent;

	if(!GLYPHTABP(glyph_tab))
	    glyph_tab = Fdefault_glyph_table();
	width_table = &VGLYPHTAB(glyph_tab)->gt_Widths;
	glyph_table = &VGLYPHTAB(glyph_tab)->gt_Glyphs;

	recenter_cursor(vw);

	/* First and last glyph columns of the viewable part of the buffer. */
	first_col = VCOL(vw->display_origin);
	first_char_col = char_col(vw->tx, VCOL(vw->display_origin),
				  VROW(vw->display_origin));

	/* current glyph row, first char row, and last glyph row */
	glyph_row = vw->min_y;
	first_row = VROW(vw->display_origin);
	last_row = glyph_row + vw->height;

	/* Current row in the buffer. */
	char_row = first_row;
	cursor_col = VCOL(vw->cursor_pos);

	/* Current extent, and actual position of its first row */
	{
	    Pos tem;
	    Lisp_Extent *x, *xc;
	    tem.col = 0;
	    tem.row = char_row;
	    extent = find_extent(vw->tx->global_extent, &tem);
	    start_visible_extent (vw, extent, 0, glyph_row);

	    extent_delta = 0;
	    for(xc = extent, x = xc->parent; x != 0; xc = x, x = x->parent)
	    {
		x->tem = xc->right_sibling;
		extent_delta += x->start.row;
		start_visible_extent (vw, x, 0, glyph_row);
	    }
	    extent->tem = extent->first_child;

	    if(extent->tem != 0)
	    {
		next_extent = extent->tem->start;
		next_extent.row += extent->start.row;
	    }
	    else
		next_extent = extent->end;
	    next_extent.row += extent_delta;
	}

	/* Memorise some facts about when the block starts and stops. */
	if(vw->block_state == 0)
	{
	    if((VROW(vw->block_end) < VROW(vw->display_origin)
	        || (VROW(vw->block_end) == VROW(vw->display_origin)
	            && VCOL(vw->block_end) <= first_char_col))
	       || VROW(vw->block_start) > first_row + vw->height)
		in_block = FALSE;
	    else
	    {
		in_block = TRUE;
		if(vw->car & VWFF_RECTBLOCKS)
		{
		    if(VCOL(vw->block_start) == VCOL(vw->block_end))
			in_block = FALSE;
		    else
		    {
			rect_block = TRUE;
			block_start = glyph_col(vw->tx, VCOL(vw->block_start),
						VROW(vw->block_start));
			block_end = glyph_col(vw->tx, VCOL(vw->block_end),
					      VROW(vw->block_end));
			if(block_start > block_end)
			{
			    long tem = block_start;
			    block_start = block_end;
			    block_end = tem;
			}
		    }
		}
		else
		    rect_block = FALSE;
	    }
	}
	else
	    in_block = FALSE;

	while(glyph_row < last_row && char_row < vw->tx->logical_end)
	{
	    /* Fill in the glyphs for CHAR_ROW */

	    glyph_code *codes = w->new_content->codes[glyph_row];
	    glyph_attr *attrs = w->new_content->attrs[glyph_row];

	    u_char *src = (u_char *)vw->tx->lines[char_row].ln_Line;
	    long src_len = vw->tx->lines[char_row].ln_Strlen - 1;

	    /* Position in current screen row, logical glyph position in
	       current buffer line, actual character in buffer line. */
	    long real_glyph_col = 0, glyph_col = 0, char_col = 0;

	    /* Is the cursor in this row? */
	    bool cursor_row = (vw == w->current_view
			       && VROW(vw->cursor_pos) == char_row);
	    bool block_row = FALSE;

	    /* Assuming a block is active, check if it starts or ends at
	       the current position, if so update the attribute value.
	       CC is the positition in the line of the current character,
	       GC is the position in the screen row of the next glyph to
	       be output. */
#define CHECK_BLOCK_ATTR(cc_, gc_)					\
            do {							\
		long cc = (cc_), gc = (gc_);				\
		if(!rect_block)						\
		{							\
		    /* check for a normal block */			\
		    if(!block_active && block_start			\
		       && (cc) == VCOL(vw->block_start))			\
		    {							\
			block_active = TRUE;				\
			attr = merge_faces(vw, extent,			\
					   block_active, FALSE);	\
		    }							\
		    if(block_active && block_end			\
		       && (cc) == VCOL(vw->block_end))			\
		    {							\
			block_active = FALSE;				\
			attr = merge_faces(vw, extent,			\
					   block_active, FALSE);	\
		    }							\
		}							\
		else if(block_row)					\
		{							\
		    /* check for a rectangular block */			\
		    if((gc) == block_start)				\
		    {							\
			block_active = TRUE;				\
			attr = merge_faces(vw, extent,			\
					   block_active, FALSE);	\
		    }							\
		    else if((gc) == block_end)				\
		    {							\
			block_active = FALSE;				\
			attr = merge_faces(vw, extent,			\
					   block_active, FALSE);	\
		    }							\
		}							\
	    } while (0)

	    /* Output a glyph CH with attribute defined by the
	       variable "attr". */
#define OUTPUT(ch)							\
	    do {							\
		*codes++ = (ch);					\
		*attrs++ = attr;					\
		if(cursor_row && char_col == cursor_col)		\
		{							\
		    attrs[-1] = merge_faces(vw, extent,			\
					    block_active, TRUE);	\
		    cursor_row = FALSE;					\
		}							\
	    } while (0)

	    /* Use ``tem'' field of an extent to record its child that
	       should be entered next, or null if no more children. */
#define CHECK_EXTENT()								\
	    do {								\
		Lisp_Extent *orig = extent, *old;				\
		do {								\
		    old = extent;						\
		    if(char_row > next_extent.row				\
		       || (char_row == next_extent.row				\
			   && char_col >= next_extent.col))			\
		    {								\
			if(extent->tem != 0					\
			   && (char_row > (extent->tem->start.row		\
					  + extent->start.row			\
					  + extent_delta)			\
			       || (char_row == (extent->tem->start.row		\
						+ extent->start.row		\
						+ extent_delta)			\
				   && char_col >= extent->tem->start.col)))	\
			{							\
			    /* Entering a new extent */				\
			    extent_delta += extent->start.row;			\
			    extent = extent->tem;				\
			    extent->tem = extent->first_child;			\
			    start_visible_extent (vw, extent,			\
						  real_glyph_col,		\
						  glyph_row);			\
			}							\
			else if(extent->parent != 0)				\
			{							\
			    /* Move back up one level. */			\
			    end_visible_extent (vw, extent,			\
						real_glyph_col,			\
						glyph_row);			\
			    extent->parent->tem = extent->right_sibling;	\
			    extent = extent->parent;				\
			    extent_delta -= extent->start.row;			\
			}							\
			else							\
			    break;						\
			if(extent->tem != 0)					\
			{							\
			    next_extent = extent->tem->start;			\
			    next_extent.row += extent->start.row;		\
			}							\
			else							\
			    next_extent = extent->end;				\
			next_extent.row += extent_delta;			\
		    }								\
		} while(extent != old);						\
		if(extent != orig)						\
		{								\
		    attr = merge_faces(vw, extent, block_active, FALSE);	\
										\
		    /* Reload the glyph table for the new extent. */		\
		    glyph_tab = Fbuffer_symbol_value(Qglyph_table,		\
							rep_VAL(extent),	\
							Qnil, Qt);		\
		    if(!GLYPHTABP(glyph_tab))					\
			glyph_tab = rep_VAL(&default_glyph_table);		\
		    width_table = &VGLYPHTAB(glyph_tab)->gt_Widths;		\
		    glyph_table = &VGLYPHTAB(glyph_tab)->gt_Glyphs;		\
		}								\
	    } while (0)


	    if(in_block)
	    {
		block_row = (VROW(vw->block_start) <= char_row
			     && VROW(vw->block_end) >= char_row);
		if(!rect_block)
		{
		    /* Does the block start or end in this row? */
		    if(block_row)
		    {
			block_start = VROW(vw->block_start) == char_row;
			block_end = VROW(vw->block_end) == char_row;
			/* Is the block active in the first column
			   of this row? */
			if((char_row > VROW(vw->block_start)
			    || (char_row == VROW(vw->block_start)
				&& VCOL(vw->block_start) == 0))
			   && (char_row < VROW(vw->block_end)
			       || (char_row == VROW(vw->block_end)
				   && VCOL(vw->block_end) > 0)))
			{
			    block_active = TRUE;
			}
		    }
		    else
		    {
			block_active = FALSE;
			block_start = block_end = 0;
		    }
		}
		else
		{
		    /* Is the block active in the first column of this row? */
		    if(char_row >= VROW(vw->block_start)
		       && block_start == 0
		       && char_row < VROW(vw->block_end)
		       && block_end > 0)
		    {
			block_active = TRUE;
		    }
		}
	    }

	    attr = merge_faces(vw, extent, block_active, FALSE);

	    /* Start output. Two versions, dependent on whether
	       we wrap or truncate long lines. */
	    if(TX_WRAP_LINES_P(vw->tx))
	    {
		while(glyph_row < last_row && src_len-- > 0)
		{
		    register u_char *ptr;
		    register int width;

		    CHECK_EXTENT();

		    ptr = &(*glyph_table)[*src][0];
		    width = (*width_table)[*src++];
		    if(width == 0)
		    {
			/* TAB special case. */
			width = tab_size - (glyph_col % tab_size);
			ptr = spaces;
		    }
		    while(width-- > 0)
		    {
			if(in_block)
			    CHECK_BLOCK_ATTR(char_col, glyph_col);
			if(char_row > first_row
			   || glyph_col >= first_col)
			{
			    if(real_glyph_col >= vw->width - 1)
			    {
				*codes = '\\';
				*attrs = attr;
				real_glyph_col = 0;
				++glyph_row;
				codes = w->new_content->codes[glyph_row];
				attrs = w->new_content->attrs[glyph_row];
			    }
			    if(glyph_row < last_row)
				OUTPUT(*ptr++);
			    real_glyph_col++;
			}
			glyph_col++;
		    }
		    char_col++;
		}
	    }
	    else
	    {
		while(real_glyph_col < vw->width && src_len-- > 0)
		{
		    register u_char *ptr;
		    register int width;

		    CHECK_EXTENT();

		    ptr = &(*glyph_table)[*src][0];
		    width = (*width_table)[*src++];
		    if(width == 0)
		    {
			/* TAB special case. */
			width = tab_size - (glyph_col % tab_size);
			ptr = spaces;
		    }
		    while(width-- > 0)
		    {
			if(in_block)
			    CHECK_BLOCK_ATTR(char_col, glyph_col);
			if(glyph_col >= first_col
			   && real_glyph_col < vw->width)
			{
			    OUTPUT(*ptr++);
			    real_glyph_col++;
			}
			glyph_col++;
		    }
		    char_col++;
		}
		if(glyph_col < first_col)
		{
		    if(in_block)
		    {
			while(glyph_col < first_col)
			{
			    CHECK_BLOCK_ATTR(char_col, glyph_col);
			    glyph_col++; char_col++;
			}
		    }
		    else
		    {
			char_col += (first_col - glyph_col);
			glyph_col = first_col;
		    }
		}
	    }

	    /* In case the line ends before the last column in the
	       window -- fill with spaces. */
	    if(glyph_row < last_row)
	    {
		while(real_glyph_col < vw->width)
		{
		    if(in_block)
			CHECK_BLOCK_ATTR(char_col, glyph_col);
		    CHECK_EXTENT();
		    OUTPUT(' ');
		    glyph_col++;
		    real_glyph_col++;
		    char_col++;		/* in case the cursor is past EOL */
		}
		glyph_row++;
	    }
	    char_row++;
	}

	/* XXX this is wrong. the extent could stop at the
	   XXX end of the previous line. */
	while (extent != 0)
	{
	    end_visible_extent (vw, extent, 0, glyph_row);
	    extent = extent->parent;
	}

	/* In case the logical end of the buffer is before the
	   end of the view, fill with empty lines. */
	{
	    repv face = Fsymbol_value (Qdefault_face, Qt);
	    if(FACEP(face))
		attr = get_face_id(w, VFACE(face));
	}
	while(glyph_row < last_row)
	{
	    memset(w->new_content->codes[glyph_row], ' ', g->cols);
	    memset(w->new_content->attrs[glyph_row], attr, g->cols);
	    glyph_row++;
	}

	/* If we're not outputting a minibuffer view, output the status
	   line text. TODO: should use glyph tables for this */
	if((vw->car & VWFF_MINIBUF) == 0)
	{
	    repv face;
	    glyph_code *codes;
	    glyph_attr *attrs;

	    face = Fsymbol_value (Qmodeline_face, Qt);
	    if(FACEP(face))
		attr = get_face_id(w, VFACE(face));

	    glyph_row = vw->min_y + vw->height;
	    codes = w->new_content->codes[glyph_row];
	    attrs = w->new_content->attrs[glyph_row];

	    update_status_buffer(vw, (char *)codes, g->cols);
	    memset(attrs, attr, g->cols);
	    glyph_row++;
	}
    }

    if(w->car & WINFF_MESSAGE)
    {
	/* The minibuffer has a message [partially?] obscuring it. */
	make_message_glyphs(g, w);
    }
}

void
make_message_glyphs(glyph_buf *g, Lisp_Window *w)
{
    /* TODO: use glyph table to output message */

    repv face;
    glyph_attr attr;

    u_long msg_len = w->message_length;
    char *msg = w->message;
    int line = w->row_count - (ROUND_UP_INT(msg_len, g->cols-1) / (g->cols-1));
    if(line < w->mini_buffer_view->min_y)
    {
	line = w->mini_buffer_view->min_y;
	msg_len = (g->cols-1) * w->mini_buffer_view->height;
    }

    face = Fsymbol_value (Qdefault_face, Qt);
    if(FACEP(face))
	attr = get_face_id(w, VFACE(face));
    else
	attr = GA_Garbage;

    /* Output the message on the bottom-most lines. */
    while(line < w->row_count)
    {
	if(msg_len >= g->cols - 1)
	{
	    memcpy(g->codes[line], msg, g->cols - 1);
	    g->codes[line][g->cols - 1] = '\\';
	}
	else
	{
	    memcpy(g->codes[line], msg, msg_len);
	    memset(g->codes[line] + msg_len, ' ', g->cols - msg_len);
	}
	memset(g->attrs[line], attr, g->cols);
	msg_len -= g->cols - 1;
	msg += g->cols - 1;
	line++;
    }
}
    

/* Screen utility functions */

/* From glyph COL in buffer line ROW, move COUNT display lines forwards,
   storing the resulting position in *COLP and *ENDP. If there aren't
   enough lines in the buffer to move COUNT rows forwards, return false
   leaving COLP and ROWP unset, otherwise return true. */
bool
skip_glyph_rows_forwards(Lisp_View *vw, long count,
			 long col, long row,
			 long *colp, long *rowp)
{
    Lisp_Buffer *tx = vw->tx;
    if(TX_WRAP_LINES_P(tx))
    {
	long len = line_glyph_length(tx, row);
	while(count-- > 0)
	{
	    col += vw->width - 1;
	    if(col >= len)
	    {
		if(++row >= tx->logical_end)
		    return FALSE;
		col = 0;
		len = line_glyph_length(tx, row);
	    }
	}
	*colp = col;
	*rowp = row;
	return TRUE;
    }
    else
    {
	row += count;
	if(row >= tx->logical_end)
	    return FALSE;
	*colp = VCOL(vw->display_origin);
	*rowp = row;
	return TRUE;
    }
}

/* From glyph COL in buffer line ROW, move COUNT display lines backwards,
   storing the resulting position in *COLP and *ENDP. If there aren't
   enough lines in the buffer to move COUNT rows backwards, return false
   leaving COLP and ROWP unset, otherwise return true. */
bool
skip_glyph_rows_backwards(Lisp_View *vw, long count,
			  long col, long row,
			  long *colp, long *rowp)
{
    Lisp_Buffer *tx = vw->tx;
    if(TX_WRAP_LINES_P(tx))
    {
	while(count-- > 0)
	{
	    col -= vw->width - 1;
	    if(col <= 0)
	    {
		if(--row < tx->logical_start)
		    return FALSE;
		col = line_glyph_length(tx, row);
	    }
	}
	*colp = ROUND_DOWN_INT(col, vw->width - 1);
	*rowp = row;
	return TRUE;
    }
    else
    {
	row -= count;
	if(row < tx->logical_start)
	    return FALSE;
	*colp = VCOL(vw->display_origin);
	*rowp = row;
	return TRUE;
    }
}

/* Do the necessary checks to ensure that the cursor is within the
   visible region of the buffer (in this view)
   TODO: set VWFF_AT_BOTTOM when appropriate. */
void
recenter_cursor(Lisp_View *vw)
{
    Lisp_Buffer *tx = vw->tx;
    long start_col = VCOL(vw->display_origin);
    long start_row = VROW(vw->display_origin);

    /* First check that the cursor is within the current
       restriction, if not move the cursor until it is. */
    if(VROW(vw->cursor_pos) < tx->logical_start)
	vw->cursor_pos = Frestriction_start(rep_VAL(tx));
    if(VROW(vw->cursor_pos) >= tx->logical_end)
	vw->cursor_pos = Frestriction_end(rep_VAL(tx));
    
    /* Check how cursor is in relation to the viewable region of the
       buffer. Change the viewable region if necessary. */
    if(!TX_WRAP_LINES_P(tx))
    {
	/* First for the easy case when lines are never wrapped. */

	long offset = get_cursor_column(vw);
	long delta;

	/* Move horizontally if necessary */
	while((offset - start_col) >= vw->width)
	    start_col += vw->scroll_step_x;
	while(offset < start_col)
	{
	    start_col -= vw->scroll_step_x;
	    if(start_col < 0)
		start_col = 0;
	}

	/* Move vertically if necessary */
	delta = VROW(vw->cursor_pos) - start_row;
	if(delta < 0)
	{
	    if(-delta > vw->scroll_step_y)
		start_row = VROW(vw->cursor_pos) - (vw->height / 2);
	    else
		start_row -= vw->scroll_step_y;
	}
	else if(delta >= vw->height)
	{
	    if((vw->height + vw->scroll_step_y) <= delta)
		start_row = VROW(vw->cursor_pos) - (vw->height / 2);
	    else
		start_row += vw->scroll_step_y;
	}

	/* Finally do some sanity checks: ensure that nothing outside
	   the restriction is visible, and that there's no wasted space
	   when displaying the bottom of the restriction. */
	if(start_row < tx->logical_start)
	    start_row = tx->logical_start;
	/* Check for a `gap' at the bottom of the display. This may
	   seem to act a bit strangely---if the last screen of the
	   buffer is being displayed, and lines are deleted from this
	   screen, new data is scrolled into the _top_ of the screen,
	   the bottom part _isn't_ scrolled up. This makes sense I
	   think, since we want to keep as much of the window covered
	   with buffer-contents as possible. */
	else if(start_row >= tx->logical_end
		|| (tx->logical_end - start_row) < vw->height)
	    start_row = MAX(tx->logical_end - vw->height,
			    tx->logical_start);

	if(start_row >= tx->logical_end - vw->height)
	    vw->car |= VWFF_AT_BOTTOM;
	else
	    vw->car &= ~VWFF_AT_BOTTOM;
    }
    else
    {
	/* The more difficult case when lines may be wrapped. */

	long offset = get_cursor_column(vw);

	if(start_row < tx->logical_start)
	    start_row = tx->logical_start;
	else if(start_row >= tx->logical_end)
	    start_row = tx->logical_end - 1;

	/* First, is the cursor past the end of the last row of glyphs
	   that will be displayed for the line it's on? */
	{
	    long last_col = line_glyph_length(tx, VROW(vw->cursor_pos));
	    if(last_col == 0)
		/* Always display a line, even if there's no glyphs at all */
		last_col = vw->width - 1;
	    else
		last_col = ROUND_UP_INT(last_col, vw->width - 1);
	    if(offset > last_col)
	    {
		vw->cursor_pos = make_pos(tx->lines[VROW(vw->cursor_pos)].ln_Strlen - 1,
					    VROW(vw->cursor_pos));
		offset = get_cursor_column(vw);
	    }
	}

	/* Check the easiest case first; is the cursor before the start
	   of the viewable region? */
	if(VROW(vw->cursor_pos) < start_row
	   || (VROW(vw->cursor_pos) == start_row && offset < start_col))
	{
	    /* Yes. Try scrolling up scroll_step_y lines */
	    if(!skip_glyph_rows_backwards(vw, vw->scroll_step_y,
					 start_col, start_row,
					 &start_col, &start_row))
	    {
		start_col = 0; start_row = tx->logical_start;
	    }
	    else if(VROW(vw->cursor_pos) < start_row
		    || (VROW(vw->cursor_pos) == start_row
		        && offset < start_col))
	    {
		/* The scroll-step is too small. We need to recenter on
		   the cursor position. */
		if(!skip_glyph_rows_backwards(vw, vw->height / 2,
					      VCOL(vw->cursor_pos),
					      VROW(vw->cursor_pos),
					      &start_col, &start_row))
		{
		    start_col = 0; start_row = tx->logical_start;
		}
	    }
	}
	/* Check if the cursor is beyond the last visible line in the view. */
	else
	{
	    /* Find the position of the start of the glyph
	       row following the end of the view. */
	    long next_line_col, next_line_row;
	    if(skip_glyph_rows_forwards(vw, vw->height,
					start_col, start_row,
					&next_line_col, &next_line_row)
	       && (VROW(vw->cursor_pos) > next_line_row
		   || (VROW(vw->cursor_pos) == next_line_row
		       && offset >= next_line_col)))
	    {
		/* Yes, the cursor's past the end of the screen.
		   Try scrolling scroll_step_y lines forwards. */
		if(skip_glyph_rows_forwards(vw, vw->scroll_step_y,
					    next_line_col, next_line_row,
					    &next_line_col, &next_line_row))
		{
		    if(VROW(vw->cursor_pos) > next_line_row
		       || (VROW(vw->cursor_pos) == next_line_row
			   && offset >= next_line_col))
		    {
			/* The scroll step is too small. recenter */
			if(!skip_glyph_rows_backwards(vw, vw->height / 2,
						      VCOL(vw->cursor_pos),
						      VROW(vw->cursor_pos),
						      &start_col, &start_row))
			{
			    start_col = 0; start_row = tx->logical_start;
			}
		    }
		    else
		    {
			/* This is ok, but we need to find the new
			   start of the display. */
			skip_glyph_rows_forwards(vw, vw->scroll_step_y,
						 start_col, start_row,
						 &start_col, &start_row);
		    }
		}
		else
		{
		    /* Couldn't move forward YStep lines, so we must be
		       at the end of the buffer. This isn't very clean,
		       but by setting start_row==logical_end-1 the
		       gap detection code below should solve the problem. */
		    start_row = tx->logical_end - 1;
		    start_col = line_glyph_length(tx, start_row);
		}
	    }
	}

	if(start_row < tx->logical_start)
	{
	    start_row = tx->logical_start;
	    start_col = 0;
	}
	else if(start_row >= tx->logical_end)
	{
	    start_row = tx->logical_end - 1;
	    start_col = line_glyph_length(tx, start_row);
	}

	vw->car &= ~VWFF_AT_BOTTOM;
	if(start_row + vw->height >= tx->logical_end)
	{
	    /* There's the possibility of a gap at the bottom of
	       the view. If so, supress it. */
	    long row = tx->logical_end - 1;
	    long col = line_glyph_length(tx, row);
	    if(skip_glyph_rows_backwards(vw, vw->height - 1,
					 col, row, &col, &row))
	    {
		if(start_row > row
		   || (start_row == row && start_col >= col))
		{
		    /* The proposed origin would leave a gap. Change it */
		    start_col = col;
		    start_row = row;
		    vw->car |= VWFF_AT_BOTTOM;
		}
	    }
	    else
	    {
		/* Since we can't skip one screen backwards, we must be
		   at the start of the buffer. Make this the origin */
		start_col = 0;
		start_row = tx->logical_start;
		vw->car |= VWFF_AT_BOTTOM;
	    }
	}
    }
    if(start_col != VCOL(vw->display_origin)
       || start_row != VROW(vw->display_origin))
	vw->display_origin = make_pos(start_col, start_row);
}


/* Utility functions */

/* Returns the number of glyphs needed to draw the string SRC.
   TODO: this function is called a lot, should really cache its results */
static inline long
uncached_string_glyph_length(Lisp_Buffer *tx, const char *src, long srcLen)
{
    /* FIXME: This is wrong, it's necessary to traverse the extent
       tree on this line since the glyph-table can be changed. */
    repv gt = Fbuffer_symbol_value(Qglyph_table, Qnil,
				       rep_VAL(tx), Qt);
    register long w;
    glyph_widths_t *width_table;
    if(!GLYPHTABP(gt))
	gt = Fdefault_glyph_table();
    width_table = &VGLYPHTAB(gt)->gt_Widths;
    for(w = 0; srcLen-- > 0;)
    {
	register int w1 = (*width_table)[*(u_char *)src++];
	if(w1 != 0)
	    w += w1;
	else
	    w += tx->tab_size - (w % tx->tab_size);
    }
    return(w);
}

/* Return the total number of glyphs needed to display the whole of line
   LINE in buffer TX. This caches the results from recently examined lines */
static long
line_glyph_length(Lisp_Buffer *tx, long line)
{
    u_long set = GL_MAP_LINE(line);
    gl_cache_entry_t *set_data = GL_GET_SET(&gl_cache, set);

#if GL_CACHE_ASSOC == 1 /* Direct-mapped cache */

    if(set_data->line == line
       && set_data->tx == tx)
    {
	/* Found the entry. Is it still valid? */
	if(set_data->changes != tx->change_count)
	{
	    /* No. Recalculate */
	    set_data->glyphs
		= uncached_string_glyph_length(tx, tx->lines[line].ln_Line,
					       tx->lines[line].ln_Strlen - 1);
	    set_data->changes = tx->change_count;
	    gl_cache.invalid_hits++;
	}
	else
	    gl_cache.valid_hits++;
	return set_data->glyphs;
    }
    set_data->line = line;
    set_data->tx = tx;
    set_data->glyphs
        = uncached_string_glyph_length(tx, tx->lines[line].ln_Line,
				       tx->lines[line].ln_Strlen - 1);
    set_data->changes = tx->change_count;
    gl_cache.misses++;
    return set_data->glyphs;

#else /* Set associative cache */

    int i, lru_set = 0;
    u_long lru_time = ~0;
    for(i = 0; i < GL_CACHE_ASSOC; i++)
    {
	if(set_data[i].line == line
	   && set_data[i].tx == tx)
	{
	    /* Found the entry. Is it still valid? */
	    if(set_data[i].changes != tx->change_count)
	    {
		/* No. Recalculate */
		set_data[i].glyphs
		    = uncached_string_glyph_length(tx, tx->lines[line].ln_Line,
						   tx->lines[line].ln_Strlen - 1);
		set_data[i].changes = tx->change_count;
		gl_cache.invalid_hits++;
	    }
	    else
		gl_cache.valid_hits++;
	    set_data[i].lru_clock = ++gl_cache.lru_clock;
	    return set_data[i].glyphs;
	}
	if(set_data[i].lru_clock < lru_time)
	{
	    lru_set = i;
	    lru_time = set_data[i].lru_clock;
	}
    }
    /* Not in cache. Overwrite least-recently used entry */
    set_data = set_data + lru_set;
    set_data->line = line;
    set_data->tx = tx;
    set_data->glyphs
        = uncached_string_glyph_length(tx, tx->lines[line].ln_Line,
				       tx->lines[line].ln_Strlen - 1);
    set_data->changes = tx->change_count;
    set_data->lru_clock = ++gl_cache.lru_clock;
    gl_cache.misses++;
    return set_data->glyphs;

#endif
}

/* Return the glyph index of (COL,LINE) in TX.	*/
long
glyph_col(Lisp_Buffer *tx, long col, long linenum)
{
    if(col >= tx->lines[linenum].ln_Strlen)
    {
	return (line_glyph_length(tx, linenum)
		+ (col - (tx->lines[linenum].ln_Strlen - 1)));
    }
    else
	/* TODO: make this work with the cache */
	return uncached_string_glyph_length(tx, tx->lines[linenum].ln_Line,
					    col);
}

/* Find how many chars to glyph position col. */
long
char_col(Lisp_Buffer *tx, long col, long linenum)
{
    char *src = tx->lines[linenum].ln_Line;
    long srclen = tx->lines[linenum].ln_Strlen - 1;
    glyph_widths_t *width_table;
    register long w = 0;
    /* FIXME: This is wrong */
    repv gt = Fbuffer_symbol_value(Qglyph_table, rep_VAL(tx),
				       Qnil, Qt);
    if(rep_VOIDP(gt) || rep_NILP(gt))
	gt = rep_VAL(&default_glyph_table);
    width_table = &VGLYPHTAB(gt)->gt_Widths;
    while((w < col) && (srclen-- > 0))
    {
	register int w1 = (*width_table)[*(u_char *)src++];
	if(w1 == 0)
	    w += tx->tab_size - (w % tx->tab_size);
	else
	    w += w1;
    }
    if(srclen < 0)
	return((tx->lines[linenum].ln_Strlen - 1) + (col - w));
    else
	return(src - tx->lines[linenum].ln_Line);
}

/* Return the actual column on the screen that the cursor appears in. */
long
get_cursor_column(Lisp_View *vw)
{
    Lisp_Buffer *tx = vw->tx;
    if(!((tx == vw->last_cursor_tx)
	 && (tx->change_count == vw->last_cursor_change_count)
	 && POS_EQUAL_P(vw->cursor_pos, vw->last_cursor_pos)))
    {
	/* Have to recalculate the number of glyphs before the cursor. */
	vw->last_cursor_offset = glyph_col(tx, VCOL(vw->cursor_pos),
					    VROW(vw->cursor_pos));
	vw->last_cursor_tx = tx;
	vw->last_cursor_change_count = tx->change_count;
	vw->last_cursor_pos = vw->cursor_pos;
    }
    return vw->last_cursor_offset;
}

/* Sets the cursor_pos.pos_Col so that the cursor appears in line ROW,
   as near as possible to last_cursor_offset horizontally.  */
void
set_cursor_vertically(Lisp_View *vw, long row)
{
    long col = char_col(vw->tx, vw->last_cursor_offset, row);
    vw->cursor_pos = make_pos(col, row);
}


/* LISP interface */

DEFUN("glyph-table-p", Fglyph_table_p, Sglyph_table_p, (repv arg), rep_Subr1) /*
::doc:glyph-table-p::
glyph-table-p ARG

Returns t if ARG is a glyph-table.
::end:: */
{
    return(GLYPHTABP(arg) ? Qt : Qnil);
}

DEFUN("char-to-glyph-pos", Fchar_to_glyph_pos, Schar_to_glyph_pos, (repv pos, repv tx), rep_Subr2) /*
::doc:char-to-glyph-pos::
char-to-glyph-pos [POS] [BUFFER]

From the character position POS, find its true *physical* position when
rendered.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    if(check_line(VBUFFER(tx), pos))
	return make_pos(glyph_col(VBUFFER(tx), VCOL(pos), VROW(pos)), VROW(pos));
    else
	return rep_NULL;
}

DEFUN("glyph-to-char-pos", Fglyph_to_char_pos, Sglyph_to_char_pos, (repv pos, repv tx), rep_Subr2) /*
::doc:glyph-to-char-pos::
glyph-to-char-pos POS [BUFFER]

For the physical position POS, find the closest matching actual character
position.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    rep_DECLARE1(pos, POSP);
    if(check_line(VBUFFER(tx), pos))
	return make_pos(char_col(VBUFFER(tx), VCOL(pos), VROW(pos)), VROW(pos));
    else
	return rep_NULL;
}

DEFUN("display-to-char-pos", Fdisplay_to_char_pos,
      Sdisplay_to_char_pos, (repv pos, repv vw), rep_Subr2) /*
::doc:display-to-char-pos::
display-to-char-pos POSITION [VIEW]

Return the position of the character displayed in VIEW (or the current view)
at screen coordinate POSITION (relative to the upper-left glyph in VIEW).
Returns nil if no such character exists (i.e. POSITION is past the end of
the buffer).
::end:: */
{
    Lisp_Buffer *tx;
    long col, row;
    rep_DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    tx = VVIEW(vw)->tx;
    col = VCOL(pos);
    row = VROW(pos);
    if(!(col >= 0 && col < VVIEW(vw)->width
	 && row >= 0 && row < VVIEW(vw)->height))
	return rep_signal_arg_error(pos, 1);

    if(skip_glyph_rows_forwards(VVIEW(vw), row,
				VCOL(VVIEW(vw)->display_origin),
				VROW(VVIEW(vw)->display_origin),
				&col, &row))
    {
	/* Got the character at the start of the screen row. Now
	   find the exact character in this row. */
	col = char_col(tx, col + VCOL(pos), row);
	return make_pos(col, row);
    }
    return Qnil;
}

DEFUN("char-to-display-pos", Fchar_to_display_pos, Schar_to_display_pos,
      (repv pos, repv vw), rep_Subr2) /*
::doc:char-to-display-pos::
char-to-display-pos POSITION [VIEW]

Return the screen coordinates, relative to the upper left corner of VIEW,
that the character at POSITION in VIEW is displayed. If this character is
not currently being displayed, return nil.
::end:: */
{
    Lisp_Buffer *tx;
    long gcol, grow;

    rep_DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    tx = VVIEW(vw)->tx;
    if(!check_line(tx, pos))
	return rep_NULL;
    if(POS_LESS_P(pos, VVIEW(vw)->display_origin))
	return Qnil;

    if(TX_WRAP_LINES_P(tx))
    {
	long row = VROW(VVIEW(vw)->display_origin);
	grow = 0;
	while(row < VROW(pos) && grow < VVIEW(vw)->height)
	{
	    long len = line_glyph_length(tx, row);
	    if(len != 0)
	    {
		len = ROUND_UP_INT(len, VVIEW(vw)->width - 1);
		grow += len / (VVIEW(vw)->width - 1);
	    }
	    else
		grow++;
	    row++;
	}
	if(grow >= VVIEW(vw)->height)
	    return Qnil;
	gcol = glyph_col(tx, VCOL(pos), VROW(pos));
	while(gcol >= VVIEW(vw)->width - 1)
	{
	    gcol -= VVIEW(vw)->width - 1;
	    grow++;
	}
    }
    else
    {
	grow = VROW(pos) - VROW(VVIEW(vw)->display_origin);
	if(grow < 0 || grow >= VVIEW(vw)->height)
	    return Qnil;
	gcol = (glyph_col(tx, VCOL(pos), VROW(pos))
		- VCOL(VVIEW(vw)->display_origin));
	if(gcol < 0 || gcol >= VVIEW(vw)->width)
	    return Qnil;
    }
    return make_pos(gcol, grow);
}

DEFUN("default-glyph-table", Fdefault_glyph_table, Sdefault_glyph_table, (void), rep_Subr0) /*
::doc:default-glyph-table::
default-glyph-table

Returns the standard glyph-table.
::end:: */
{
    return(rep_VAL(&default_glyph_table));
}

DEFUN("make-glyph-table", Fmake_glyph_table, Smake_glyph_table, (repv src), rep_Subr1) /*
::doc:make-glyph-table::
make-glyph-table SRC

Creates a new glyph-table. If SRC is a glyph-table it will be copied, else if
SRC is a buffer that buffer's glyph-table will be copied. If SRC is nil the
default glyph-table will be copied.
::end:: */
{
    glyph_table_t *newgt = rep_ALLOC_CELL(sizeof(glyph_table_t));
    if(newgt)
    {
	glyph_table_t *srcgt;
	if(GLYPHTABP(src))
	    srcgt = VGLYPHTAB(src);
	else if(BUFFERP(src))
	{
	    repv tem = Fbuffer_symbol_value(Qglyph_table, Qnil,
						src, Qt);
	    if(GLYPHTABP(tem))
		srcgt = VGLYPHTAB(tem);
	    else
		srcgt = &default_glyph_table;
	}
	else
	    srcgt = &default_glyph_table;
	memcpy(newgt, srcgt, sizeof(glyph_table_t));
	newgt->gt_Car = glyph_table_type;
	newgt->gt_Next = gt_chain;
	gt_chain = newgt;
	rep_data_after_gc += sizeof(glyph_table_t);
	return(rep_VAL(newgt));
    }
    return rep_mem_error();
}

DEFUN("set-glyph", Fset_glyph, Sset_glyph, (repv gt, repv ch, repv glyph), rep_Subr3) /*
::doc:set-glyph::
set-glyph GLYPH-TABLE CHARACTER GLYPH-STRING

Make the renderer draw the string GLYPH-STRING (no more than 4 characters)
for each character CHARACTER in any buffers which use the GLYPH-TABLE.
::end:: */
{
    int glyphlen;
    rep_DECLARE1(gt, GLYPHTABP);
    rep_DECLARE2(ch, rep_INTP);
    rep_DECLARE3(glyph, rep_STRINGP);
    if((rep_INT(ch) < 0) || (rep_INT(ch) >= 256))
    {
	rep_signal_arg_error(ch, 1);
	return rep_NULL;
    }
    glyphlen = rep_STRING_LEN(glyph);
    if(glyphlen > 4)
    {
	rep_signal_arg_error(glyph, 2);
	return rep_NULL;
    }
    VGLYPHTAB(gt)->gt_Widths[rep_INT(ch)] = glyphlen;
    if(glyphlen == 0)
    {
	/* put a space in the first character */
	VGLYPHTAB(gt)->gt_Glyphs[rep_INT(ch)][0] = ' ';
    }
    else
	memcpy(&VGLYPHTAB(gt)->gt_Glyphs[rep_INT(ch)][0], rep_STR(glyph), glyphlen);

    return(Qt);
}

DEFUN("get-glyph", Fget_glyph, Sget_glyph, (repv gt, repv ch), rep_Subr2) /*
::doc:get-glyph::
get-glyph GLYPH-TABLE CHARACTER

Return the string which is the rendered representation of CHARACTER in
GLYPH-TABLE.
::end:: */
{
    rep_DECLARE1(gt, GLYPHTABP);
    rep_DECLARE2(ch, rep_INTP);
    if((rep_INT(ch) < 0) || (rep_INT(ch) >= 256))
    {
	rep_signal_arg_error(ch, 1);
	return rep_NULL;
    }
    return(rep_string_dupn((char *)&VGLYPHTAB(gt)->gt_Glyphs[rep_INT(ch)][0],
		       VGLYPHTAB(gt)->gt_Widths[rep_INT(ch)]));
}

static void
glyphtable_sweep(void)
{
    glyph_table_t *gt = gt_chain;
    gt_chain = NULL;
    while(gt)
    {
	glyph_table_t *nxt = gt->gt_Next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(gt)) && !(gt->gt_Car & GTF_STATIC))
	    rep_FREE_CELL(gt);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(gt));
	    gt->gt_Next = gt_chain;
	    gt_chain = gt;
	}
	gt = nxt;
    }
}

static void
glyphtable_prin(repv strm, repv obj)
{
    rep_stream_puts(strm, "#<glyph-table>", -1, FALSE);
}

void
glyphs_init(void)
{
    glyph_table_type = rep_register_new_type ("glyph-table", 0,
					      glyphtable_prin,
					      glyphtable_prin,
					      glyphtable_sweep,
					      0, 0, 0, 0, 0, 0, 0, 0);

    default_glyph_table.gt_Car |= glyph_table_type;

    rep_ADD_SUBR(Sglyph_table_p);
    rep_ADD_SUBR(Schar_to_glyph_pos);
    rep_ADD_SUBR(Sglyph_to_char_pos);
    rep_ADD_SUBR(Sdisplay_to_char_pos);
    rep_ADD_SUBR(Schar_to_display_pos);
    rep_ADD_SUBR(Sdefault_glyph_table);
    rep_ADD_SUBR(Smake_glyph_table);
    rep_ADD_SUBR(Sset_glyph);
    rep_ADD_SUBR(Sget_glyph);
    rep_INTERN_SPECIAL(glyph_table);

    Fset (Qglyph_table, rep_VAL(&default_glyph_table));
    Fmake_variable_buffer_local(Qglyph_table);
}

void
glyphs_kill(void)
{
    glyph_table_t *gt = gt_chain;
    while(gt)
    {
	glyph_table_t *nxt = gt->gt_Next;
	if(!(gt->gt_Car & GTF_STATIC))
	    rep_FREE_CELL(gt);
	gt = nxt;
    }
    gt_chain = NULL;
}
