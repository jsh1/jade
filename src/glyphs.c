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
#include "jade_protos.h"

#include <string.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR void make_window_glyphs(glyph_buf *g, WIN *w);
_PR void make_message_glyphs(glyph_buf *g, WIN *w);
_PR bool skip_glyph_rows_forwards(VW *, long, long, long, long *, long *);
_PR bool skip_glyph_rows_backwards(VW *, long, long, long, long *, long *);
_PR void recenter_cursor(VW *vw);

static long line_glyph_length(TX *tx, long line);

_PR long glyph_col(TX *, long, long);
_PR long char_col(TX *, long, long);
_PR long get_cursor_column(VW *);
_PR void set_cursor_vertically(VW *vw, long row);

_PR void glyphtable_sweep(void);
_PR void glyphtable_prin(VALUE, VALUE);
_PR void glyphs_init(void);
_PR void glyphs_kill(void);


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
    VALUE		gt_Car;
    struct glyph_table *gt_Next;
    glyph_widths_t	gt_Widths;
    glyph_glyphs_t	gt_Glyphs;
} glyph_table_t;
#define GTF_STATIC (1 << CELL8_TYPE_BITS)	/* Don't free() this table */

static glyph_table_t default_glyph_table = {
    V_GlyphTable | GTF_STATIC,
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
#define GL_CACHE_SETS	100		/* number of cache sets */
#define GL_CACHE_ASSOC	2		/* entries in each set */

/* Map a line number to a gl-cache set */
#define GL_MAP_LINE(l) ((l) % GL_CACHE_SETS)

typedef struct {
    u_long line;			/* line number */
    TX *tx;				/* buffer */
    u_long glyphs;			/* number of glyphs in line */
    u_long changes;			/* change-count at calc. time */
    u_long lru_clock;			/* last access time */
} gl_cache_entry_t;

typedef struct {
    gl_cache_entry_t data[GL_CACHE_SETS * GL_CACHE_ASSOC];
    u_long lru_clock;
    u_long misses, valid_hits, invalid_hits;
} gl_cache_t;

/* Get a pointer to the array of entries forming set S */
#define GL_GET_SET(gl, s) ((gl)->data + ((s) * GL_CACHE_ASSOC))

static gl_cache_t gl_cache;


/* Filling glyph buffers */

/* Assuming a block is active, check if it starts or ends at the current
   position, if so update the attribute value. CC is the positition in the
   line of the current character, GC is the position in the screen row of
   the next glyph to be output. */
#define CHECK_BLOCK_ATTR(cc, gc)			\
    do {						\
	if(!rect_block)					\
	{						\
	    /* check for a normal block */		\
	    if(attr == GA_Text && block_start		\
	       && (cc) == VCOL(vw->vw_BlockS))		\
		attr = GA_Block;			\
	    if(attr == GA_Block && block_end		\
	       && (cc) == VCOL(vw->vw_BlockE))		\
		attr = GA_Text;				\
	}						\
	else if(block_row)				\
	{						\
	    /* check for a rectangular block */		\
	    if((gc) == block_start)			\
		attr = GA_Block;			\
	    else if((gc) == block_end)			\
		attr = GA_Text;				\
	}						\
    } while(0)

/* Output a glyph CH with attribute defined by the variable "attr". */
#define OUTPUT(ch)					\
    do {						\
	if(!cursor_row)					\
	    *attrs++ = attr;				\
	else						\
	{						\
	    /* check for cursor pos. */			\
	    if(char_col == cursor_col)			\
	    {						\
		*attrs++ = cursor_attrs[attr];		\
		cursor_row = FALSE;			\
	    }						\
	    else					\
		*attrs++ = attr;			\
	}						\
	*codes++ = ch;					\
    } while(0)

/* Fill glyph buffer G with whatever should be displayed in window W. */
void
make_window_glyphs(glyph_buf *g, WIN *w)
{
    glyph_code *codes = GLYPH_BUF_CODES(w->w_NewContent, 0);
    glyph_attr *attrs = GLYPH_BUF_ATTRS(w->w_NewContent, 0);

    /* Lookup table for changing an attribute to the cursor's equivalent. */
    static const glyph_attr cursor_attrs[GA_MAX] =
	{ GA_Text_RV, GA_Text, GA_Block_RV, GA_Block };
    static char spaces[] = "                                                 "
"                                                                            ";

    VW *vw;
    int glyph_row = 0;

    for(vw = w->w_ViewList;
	vw != 0 && (!(vw->vw_Flags & VWFF_MINIBUF)
		    || !(w->w_Flags & WINFF_MESSAGE));
	vw = vw->vw_NextView)
    {
	glyph_widths_t *width_table
	    = &VGLYPHTAB(vw->vw_Tx->tx_GlyphTable)->gt_Widths;
	glyph_glyphs_t *glyph_table
	    = &VGLYPHTAB(vw->vw_Tx->tx_GlyphTable)->gt_Glyphs;

	glyph_attr attr;
	int tab_size = vw->vw_Tx->tx_TabSize;
	long first_col, first_row, first_char_col;
	long last_row, char_row;
	long cursor_col;

	bool in_block, rect_block = FALSE;
	long block_start = 0, block_end = 0;

	recenter_cursor(vw);

	/* First and last glyph columns of the viewable part of the buffer. */
	first_col = VCOL(vw->vw_DisplayOrigin);
	first_char_col = char_col(vw->vw_Tx, VCOL(vw->vw_DisplayOrigin),
				  VROW(vw->vw_DisplayOrigin));

	/* First and last viewable rows of the buffer. */
	first_row = VROW(vw->vw_DisplayOrigin);
	last_row = glyph_row + vw->vw_MaxY;

	/* Current row in the buffer. */
	char_row = first_row;
	cursor_col = VCOL(vw->vw_CursorPos);

	/* Memorise some facts about when the block starts and stops. */
	if(vw->vw_BlockStatus == 0)
	{
	    if((VROW(vw->vw_BlockE) < VROW(vw->vw_DisplayOrigin)
	        || (VROW(vw->vw_BlockE) == VROW(vw->vw_DisplayOrigin)
	            && VCOL(vw->vw_BlockE) <= first_char_col))
	       || VROW(vw->vw_BlockS) > first_row + vw->vw_MaxY)
		in_block = FALSE;
	    else
	    {
		in_block = TRUE;
		if(vw->vw_Flags & VWFF_RECTBLOCKS)
		{
		    if(VCOL(vw->vw_BlockS) == VCOL(vw->vw_BlockE))
			in_block = FALSE;
		    else
		    {
			rect_block = TRUE;
			block_start = glyph_col(vw->vw_Tx, VCOL(vw->vw_BlockS),
						VROW(vw->vw_BlockS));
			block_end = glyph_col(vw->vw_Tx, VCOL(vw->vw_BlockE),
					      VROW(vw->vw_BlockE));
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

	while(glyph_row < last_row && char_row < vw->vw_Tx->tx_LogicalEnd)
	{
	    /* Fill in the glyphs for CHAR_ROW */

	    u_char *src = vw->vw_Tx->tx_Lines[char_row].ln_Line;
	    long src_len = vw->vw_Tx->tx_Lines[char_row].ln_Strlen - 1;

	    /* Position in current screen row, logical glyph position in
	       current buffer line, actual character in buffer line. */
	    long real_glyph_col = 0, glyph_col = 0, char_col = 0;

	    /* Is the cursor in this row? */
	    bool cursor_row = (vw->vw_Win == curr_win
			       && vw == w->w_CurrVW
			       && VROW(vw->vw_CursorPos) == char_row);
	    bool block_row = FALSE;
	    attr = GA_Text;

	    if(in_block)
	    {
		block_row = (VROW(vw->vw_BlockS) <= char_row
			     && VROW(vw->vw_BlockE) >= char_row);
		if(!rect_block)
		{
		    /* Does the block start or end in this row? */
		    if(block_row)
		    {
			block_start = VROW(vw->vw_BlockS) == char_row;
			block_end = VROW(vw->vw_BlockE) == char_row;
			/* Is the block active in the first column
			   of this row? */
			if((char_row > VROW(vw->vw_BlockS)
			    || (char_row == VROW(vw->vw_BlockS)
				&& VCOL(vw->vw_BlockS) == 0))
			   && (char_row < VROW(vw->vw_BlockE)
			       || (char_row == VROW(vw->vw_BlockE)
				   && VCOL(vw->vw_BlockE) > 0)))
			    attr = GA_Block;
		    }
		    else
			block_start = block_end = 0;
		}
		else
		{
		    /* Is the block active in the first column of this row? */
		    if(char_row >= VROW(vw->vw_BlockS)
		       && block_start == 0
		       && char_row < VROW(vw->vw_BlockE)
		       && block_end > 0)
			attr = GA_Block;
		}
	    }

	    /* Start output. Two versions, dependent on whether
	       we wrap or truncate long lines. */
	    if(TX_WRAP_LINES_P(vw->vw_Tx))
	    {
		while(glyph_row < last_row && src_len-- > 0)
		{
		    register u_char *ptr = &(*glyph_table)[*src][0];
		    register int width = (*width_table)[*src++];
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
			    if(real_glyph_col >= vw->vw_MaxX - 1)
			    {
				*codes++ = '\\';
				*attrs++ = attr;
				real_glyph_col = 0;
				++glyph_row;
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
		while(real_glyph_col < vw->vw_MaxX && src_len-- > 0)
		{
		    register u_char *ptr = &(*glyph_table)[*src][0];
		    register int width = (*width_table)[*src++];
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
			   && real_glyph_col < vw->vw_MaxX)
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
		while(real_glyph_col < vw->vw_MaxX)
		{
		    if(in_block)
			CHECK_BLOCK_ATTR(char_col, glyph_col);
		    OUTPUT(' ');
		    glyph_col++;
		    real_glyph_col++;
		    char_col++;		/* in case the cursor is past EOL */
		}
	    }

	    glyph_row++;
	    char_row++;
	}

	/* In case the logical end of the buffer is before the
	   end of the view, fill with empty lines. */
	while(glyph_row < last_row)
	{
	    memset(codes, ' ', g->cols);
	    memset(attrs, GA_Text, g->cols);
	    codes += g->cols;
	    attrs += g->cols;
	    glyph_row++;
	}

	/* If we're not outputting a minibuffer view, output the status
	   line text. TODO: should use glyph tables for this */
	if((vw->vw_Flags & VWFF_MINIBUF) == 0)
	{
	    int len;
	    update_status_buffer(vw);
	    len = strlen(vw->vw_StatusBuf);
	    memcpy(codes, vw->vw_StatusBuf, MIN(len, g->cols));
	    if(len < g->cols)
		memset(codes + len, ' ', g->cols - len);
	    memset(attrs, GA_Text_RV, g->cols);
	    codes += g->cols;
	    attrs += g->cols;
	    glyph_row++;
	}
    }

    if(vw != 0)
    {
	/* A minibuffer with a message obscuring it. */
	make_message_glyphs(g, w);
    }
}

void
make_message_glyphs(glyph_buf *g, WIN *w)
{
    /* TODO: use glyph table to output message */
    memcpy(GLYPH_BUF_CODES(g, w->w_MaxY - 1),
	   w->w_Message,
	   MIN(w->w_MessageLen, g->cols));
    if(w->w_MessageLen < g->cols)
	memset(GLYPH_BUF_CODES(g, w->w_MaxY - 1) + w->w_MessageLen,
	       ' ', g->cols - w->w_MessageLen);
    memset(GLYPH_BUF_ATTRS(g, w->w_MaxY - 1), GA_Text, g->cols);
}
    

/* Screen utility functions */

/* From glyph COL in buffer line ROW, move COUNT display lines forwards,
   storing the resulting position in *COLP and *ENDP. If there aren't
   enough lines in the buffer to move COUNT rows forwards, return false
   leaving COLP and ROWP unset, otherwise return true. */
bool
skip_glyph_rows_forwards(VW *vw, long count,
			 long col, long row,
			 long *colp, long *rowp)
{
    TX *tx = vw->vw_Tx;
    if(TX_WRAP_LINES_P(tx))
    {
	long len = line_glyph_length(tx, row);
	while(count-- > 0)
	{
	    col += vw->vw_MaxX - 1;
	    if(col >= len)
	    {
		if(++row >= tx->tx_LogicalEnd)
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
	if(row >= tx->tx_LogicalEnd)
	    return FALSE;
	*colp = VCOL(vw->vw_DisplayOrigin);
	*rowp = row;
	return TRUE;
    }
}

/* From glyph COL in buffer line ROW, move COUNT display lines backwards,
   storing the resulting position in *COLP and *ENDP. If there aren't
   enough lines in the buffer to move COUNT rows backwards, return false
   leaving COLP and ROWP unset, otherwise return true. */
bool
skip_glyph_rows_backwards(VW *vw, long count,
			  long col, long row,
			  long *colp, long *rowp)
{
    TX *tx = vw->vw_Tx;
    if(TX_WRAP_LINES_P(tx))
    {
	while(count-- > 0)
	{
	    col -= vw->vw_MaxX - 1;
	    if(col <= 0)
	    {
		if(--row < tx->tx_LogicalStart)
		    return FALSE;
		col = line_glyph_length(tx, row);
	    }
	}
	*colp = ROUND_DOWN_INT(col, vw->vw_MaxX - 1);
	*rowp = row;
	return TRUE;
    }
    else
    {
	row -= count;
	if(row < tx->tx_LogicalStart)
	    return FALSE;
	*colp = VCOL(vw->vw_DisplayOrigin);
	*rowp = row;
	return TRUE;
    }
}

/* Do the necessary checks to ensure that the cursor is within the
   visible region of the buffer (in this view)
   TODO: set VWFF_AT_BOTTOM when appropriate. */
void
recenter_cursor(VW *vw)
{
    TX *tx = vw->vw_Tx;
    long start_col = VCOL(vw->vw_DisplayOrigin);
    long start_row = VROW(vw->vw_DisplayOrigin);

    /* First check that the cursor is within the current
       restriction, if not move the cursor until it is. */
    if(VROW(vw->vw_CursorPos) < tx->tx_LogicalStart)
	vw->vw_CursorPos = cmd_restriction_start(VAL(tx));
    if(VROW(vw->vw_CursorPos) >= tx->tx_LogicalEnd)
	vw->vw_CursorPos = cmd_restriction_end(VAL(tx));
    
    /* Check how cursor is in relation to the viewable region of the
       buffer. Change the viewable region if necessary. */
    if(!TX_WRAP_LINES_P(tx))
    {
	/* First for the easy case when lines are never wrapped. */

	long offset = get_cursor_column(vw);
	long delta;

	/* Move horizontally if necessary */
	while((offset - start_col) >= vw->vw_MaxX)
	    start_col += vw->vw_XStep;
	while(offset < start_col)
	{
	    start_col -= vw->vw_XStep;
	    if(start_col < 0)
		start_col = 0;
	}

	/* Move vertically if necessary */
	delta = VROW(vw->vw_CursorPos) - start_row;
	if(delta < 0)
	{
	    if(-delta > vw->vw_YStep)
		start_row = VROW(vw->vw_CursorPos) - (vw->vw_MaxY / 2);
	    else
		start_row -= vw->vw_YStep;
	}
	else if(delta >= vw->vw_MaxY)
	{
	    if((vw->vw_MaxY + vw->vw_YStep) <= delta)
		start_row = VROW(vw->vw_CursorPos) - (vw->vw_MaxY / 2);
	    else
		start_row += vw->vw_YStep;
	}

	/* Finally do some sanity checks: ensure that nothing outside
	   the restriction is visible, and that there's no wasted space
	   when displaying the bottom of the restriction. */
	if(start_row < tx->tx_LogicalStart)
	    start_row = tx->tx_LogicalStart;
	/* Check for a `gap' at the bottom of the display. This may
	   seem to act a bit strangely---if the last screen of the
	   buffer is being displayed, and lines are deleted from this
	   screen, new data is scrolled into the _top_ of the screen,
	   the bottom part _isn't_ scrolled up. This makes sense I
	   think, since we want to keep as much of the window covered
	   with buffer-contents as possible. */
	else if(start_row >= tx->tx_LogicalEnd
		|| (tx->tx_LogicalEnd - start_row) < vw->vw_MaxY)
	    start_row = MAX(tx->tx_LogicalEnd - vw->vw_MaxY,
			    tx->tx_LogicalStart);

	if(start_row >= tx->tx_LogicalEnd - vw->vw_MaxY)
	    vw->vw_Flags |= VWFF_AT_BOTTOM;
	else
	    vw->vw_Flags &= ~VWFF_AT_BOTTOM;
    }
    else
    {
	/* The more difficult case when lines may be wrapped. */

	long offset = get_cursor_column(vw);

	if(start_row < tx->tx_LogicalStart)
	    start_row = tx->tx_LogicalStart;
	else if(start_row >= tx->tx_LogicalEnd)
	    start_row = tx->tx_LogicalEnd - 1;

	/* First, is the cursor past the end of the last row of glyphs
	   that will be displayed for the line it's on? */
	{
	    LINE *line = tx->tx_Lines + VROW(vw->vw_CursorPos);
	    long last_col = line_glyph_length(tx, VROW(vw->vw_CursorPos));
	    if(last_col == 0)
		/* Always display a line, even if there's no glyphs at all */
		last_col = vw->vw_MaxX - 1;
	    else
		last_col = ROUND_UP_INT(last_col, vw->vw_MaxX - 1);
	    if(offset > last_col)
	    {
		vw->vw_CursorPos = make_pos(line->ln_Strlen - 1,
					    VROW(vw->vw_CursorPos));
		offset = get_cursor_column(vw);
	    }
	}

	/* Check the easiest case first; is the cursor before the start
	   of the viewable region? */
	if(VROW(vw->vw_CursorPos) < start_row
	   || (VROW(vw->vw_CursorPos) == start_row && offset < start_col))
	{
	    /* Yes. Try scrolling up vw_YStep lines */
	    if(!skip_glyph_rows_backwards(vw, vw->vw_YStep,
					 start_col, start_row,
					 &start_col, &start_row))
	    {
		start_col = 0; start_row = tx->tx_LogicalStart;
	    }
	    else if(VROW(vw->vw_CursorPos) < start_row
		    || (VROW(vw->vw_CursorPos) == start_row
		        && offset < start_col))
	    {
		/* The scroll-step is too small. We need to recenter on
		   the cursor position. */
		if(!skip_glyph_rows_backwards(vw, vw->vw_MaxY / 2,
					      VCOL(vw->vw_CursorPos),
					      VROW(vw->vw_CursorPos),
					      &start_col, &start_row))
		{
		    start_col = 0; start_row = tx->tx_LogicalStart;
		}
	    }
	}
	/* Check if the cursor is beyond the last visible line in the view. */
	else
	{
	    /* Find the position of the start of the glyph
	       row following the end of the view. */
	    long next_line_col, next_line_row;
	    if(skip_glyph_rows_forwards(vw, vw->vw_MaxY,
					start_col, start_row,
					&next_line_col, &next_line_row)
	       && (VROW(vw->vw_CursorPos) > next_line_row
		   || (VROW(vw->vw_CursorPos) == next_line_row
		       && offset >= next_line_col)))
	    {
		/* Yes, the cursor's past the end of the screen.
		   Try scrolling vw_YStep lines forwards. */
		if(skip_glyph_rows_forwards(vw, vw->vw_YStep,
					    next_line_col, next_line_row,
					    &next_line_col, &next_line_row)
		   && (VROW(vw->vw_CursorPos) > next_line_row
		       || (VROW(vw->vw_CursorPos) == next_line_row
			   && offset >= next_line_col)))
		{
		    /* The scroll step is too small. recenter */
		    if(!skip_glyph_rows_backwards(vw, vw->vw_MaxY / 2,
						  VCOL(vw->vw_CursorPos),
						  VROW(vw->vw_CursorPos),
						  &start_col, &start_row))
		    {
			start_col = 0; start_row = tx->tx_LogicalStart;
		    }
		}
		else
		{
		    /* This is ok, but we need to find the new
		       start of the display. */
		    skip_glyph_rows_forwards(vw, vw->vw_YStep,
					     start_col, start_row,
					     &start_col, &start_row);
		}
	    }
	}

	if(start_row < tx->tx_LogicalStart)
	    start_row = tx->tx_LogicalStart;
	else if(start_row >= tx->tx_LogicalEnd)
	    start_row = tx->tx_LogicalEnd - 1;

	vw->vw_Flags &= ~VWFF_AT_BOTTOM;
	if(start_row + vw->vw_MaxY >= tx->tx_LogicalEnd)
	{
	    /* There's the possibility of a gap at the bottom of
	       the view. If so, supress it. */
	    long row = tx->tx_LogicalEnd - 1;
	    long col = tx->tx_Lines[row].ln_Strlen - 1;
	    if(skip_glyph_rows_backwards(vw, vw->vw_MaxY - 1,
					 col, row, &col, &row))
	    {
		if(start_row > row
		   || (start_row == row && start_col >= col))
		{
		    /* The proposed origin would leave a gap. Change it */
		    start_col = col;
		    start_row = row;
		    vw->vw_Flags |= VWFF_AT_BOTTOM;
		}
	    }
	    else
	    {
		/* Since we can't skip one screen backwards, we must be
		   at the start of the buffer. Make this the origin */
		start_col = 0;
		start_row = tx->tx_LogicalStart;
		vw->vw_Flags |= VWFF_AT_BOTTOM;
	    }
	}
    }
    if(start_col != VCOL(vw->vw_DisplayOrigin)
       || start_row != VROW(vw->vw_DisplayOrigin))
	vw->vw_DisplayOrigin = make_pos(start_col, start_row);
}


/* Utility functions */

/* Returns the number of glyphs needed to draw the string SRC.
   TODO: this function is called a lot, should really cache its results */
static inline long
uncached_string_glyph_length(TX *tx, const u_char *src, long srcLen)
{
    glyph_widths_t *width_table = &VGLYPHTAB(tx->tx_GlyphTable)->gt_Widths;
    register long w;
    for(w = 0; srcLen-- > 0;)
    {
	register int w1 = (*width_table)[*src++];
	if(w1 != 0)
	    w += w1;
	else
	    w += tx->tx_TabSize - (w % tx->tx_TabSize);
    }
    return(w);
}

/* Return the total number of glyphs needed to display the whole of line
   LINE in buffer TX. This caches the results from recently examined
   lines (the cache handling is copied from my libsim.a cache simulator) */
static long
line_glyph_length(TX *tx, long line)
{
    u_long set = GL_MAP_LINE(line);
    gl_cache_entry_t *set_data = GL_GET_SET(&gl_cache, set);
    int i, lru_set = 0;
    u_long lru_time = ~0;
    for(i = 0; i < GL_CACHE_ASSOC; i++)
    {
	if(set_data[i].line == line
	   && set_data[i].tx == tx)
	{
	    /* Found the entry. Is it still valid? */
	    if(set_data[i].changes != tx->tx_Changes)
	    {
		/* No. Recalculate */
		LINE *l = tx->tx_Lines + line;
		set_data[i].glyphs
		    = uncached_string_glyph_length(tx, l->ln_Line,
						   l->ln_Strlen - 1);
		set_data[i].changes = tx->tx_Changes;
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
        = uncached_string_glyph_length(tx, tx->tx_Lines[line].ln_Line,
				       tx->tx_Lines[line].ln_Strlen - 1);
    set_data->changes = tx->tx_Changes;
    set_data->lru_clock = ++gl_cache.lru_clock;
    gl_cache.misses++;
    return set_data->glyphs;
}

/* Return the glyph index of (COL,LINE) in TX.	*/
long
glyph_col(TX *tx, long col, long linenum)
{
    LINE *line = tx->tx_Lines + linenum;
    if(col >= line->ln_Strlen)
    {
	return (line_glyph_length(tx, linenum)
		+ (col - (line->ln_Strlen - 1)));
    }
    else
	/* TODO: make this work with the cache */
	return uncached_string_glyph_length(tx, line->ln_Line, col);
}

/* Find how many chars to glyph position col. */
long
char_col(TX *tx, long col, long linenum)
{
    LINE *line = tx->tx_Lines + linenum;
    u_char *src = line->ln_Line;
    long srclen = line->ln_Strlen - 1;
    glyph_widths_t *width_table = &VGLYPHTAB(tx->tx_GlyphTable)->gt_Widths;
    register long w = 0;
    while((w < col) && (srclen-- > 0))
    {
	register int w1 = (*width_table)[*src++];
	if(w1 == 0)
	    w += tx->tx_TabSize - (w % tx->tx_TabSize);
	else
	    w += w1;
    }
    if(srclen < 0)
	return((line->ln_Strlen - 1) + (col - w));
    else
	return(src - line->ln_Line);
}

/* Return the actual column on the screen that the cursor appears in. */
long
get_cursor_column(VW *vw)
{
    TX *tx = vw->vw_Tx;
    if(!((tx == vw->vw_LastCursorTx)
	 && (tx->tx_Changes == vw->vw_LastCursorChanges)
	 && POS_EQUAL_P(vw->vw_CursorPos, vw->vw_LastCursorPos)))
    {
	/* Have to recalculate the number of glyphs before the cursor. */
	vw->vw_LastCursorOffset = glyph_col(tx, VCOL(vw->vw_CursorPos),
					    VROW(vw->vw_CursorPos));
	vw->vw_LastCursorTx = tx;
	vw->vw_LastCursorChanges = tx->tx_Changes;
	vw->vw_LastCursorPos = vw->vw_CursorPos;
    }
    return vw->vw_LastCursorOffset;
}

/* Sets the vw_CursorPos.pos_Col so that the cursor appears in line ROW,
   as near as possible to vw_LastCursorOffset horizontally.  */
void
set_cursor_vertically(VW *vw, long row)
{
    long col = char_col(vw->vw_Tx, vw->vw_LastCursorOffset, row);
    vw->vw_CursorPos = make_pos(col, row);
}


/* LISP interface */

_PR VALUE cmd_glyph_table_p(VALUE arg);
DEFUN("glyph-table-p", cmd_glyph_table_p, subr_glyph_table_p, (VALUE arg), V_Subr1, DOC_glyph_table_p) /*
::doc:glyph_table_p::
glyph-table-p ARG

Returns t if ARG is a glyph-table.
::end:: */
{
    return(GLYPHTABP(arg) ? sym_t : sym_nil);
}

_PR VALUE cmd_char_to_glyph_pos(VALUE pos, VALUE tx);
DEFUN("char-to-glyph-pos", cmd_char_to_glyph_pos, subr_char_to_glyph_pos, (VALUE pos, VALUE tx), V_Subr2, DOC_char_to_glyph_pos) /*
::doc:char_to_glyph_pos::
char-to-glyph-pos [POS] [BUFFER]

From the character position POS, find its true *physical* position when
rendered.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(check_line(VTX(tx), pos))
	return make_pos(glyph_col(VTX(tx), VCOL(pos), VROW(pos)), VROW(pos));
    else
	return LISP_NULL;
}

_PR VALUE cmd_glyph_to_char_pos(VALUE pos, VALUE tx);
DEFUN("glyph-to-char-pos", cmd_glyph_to_char_pos, subr_glyph_to_char_pos, (VALUE pos, VALUE tx), V_Subr2, DOC_glyph_to_char_pos) /*
::doc:glyph_to_char_pos::
glyph-to-char-pos POS [BUFFER]

For the physical position POS, find the closest matching actual character
position.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    DECLARE1(pos, POSP);
    if(check_line(VTX(tx), pos))
	return make_pos(char_col(VTX(tx), VCOL(pos), VROW(pos)), VROW(pos));
    else
	return LISP_NULL;
}

_PR VALUE cmd_display_to_char_pos(VALUE pos, VALUE vw);
DEFUN("display-to-char-pos", cmd_display_to_char_pos,
      subr_display_to_char_pos, (VALUE pos, VALUE vw),
      V_Subr2, DOC_display_to_char_pos) /*
::doc:display_to_char_pos::
display-to-char-pos POSITION [VIEW]

Return the position of the character displayed in VIEW (or the current view)
at screen coordinate POSITION (relative to the upper-left glyph in VIEW).
Returns nil if no such character exists (i.e. POSITION is past the end of
the buffer).
::doc:: */
{
    TX *tx;
    long col, row;
    DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    tx = VVIEW(vw)->vw_Tx;
    col = VCOL(pos);
    row = VROW(pos);
    if(!(col >= 0 && col < VVIEW(vw)->vw_MaxX
	 && row >= 0 && row < VVIEW(vw)->vw_MaxY))
	return signal_arg_error(pos, 1);

    if(skip_glyph_rows_forwards(VVIEW(vw), row,
				VCOL(VVIEW(vw)->vw_DisplayOrigin),
				VROW(VVIEW(vw)->vw_DisplayOrigin),
				&col, &row))
    {
	/* Got the character at the start of the screen row. Now
	   find the exact character in this row. */
	col = char_col(tx, col + VCOL(pos), row);
	return make_pos(col, row);
    }
    return sym_nil;
}

_PR VALUE cmd_char_to_display_pos(VALUE pos, VALUE vw);
DEFUN("char-to-display-pos", cmd_char_to_display_pos, subr_char_to_display_pos,
      (VALUE pos, VALUE vw), V_Subr2, DOC_char_to_display_pos) /*
::doc:char_to_display_pos::
char-to-display-pos POSITION [VIEW]

Return the screen coordinates, relative to the upper left corner of VIEW,
that the character at POSITION in VIEW is displayed. If this character is
not currently being displayed, return nil.
::end:: */
{
    TX *tx;
    long gcol, grow;

    DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    tx = VVIEW(vw)->vw_Tx;
    if(!check_line(tx, pos))
	return LISP_NULL;
    if(POS_LESS_P(pos, VVIEW(vw)->vw_DisplayOrigin))
	return sym_nil;

    if(TX_WRAP_LINES_P(tx))
    {
	long row = VROW(VVIEW(vw)->vw_DisplayOrigin);
	grow = 0;
	while(row < VROW(pos) && grow < VVIEW(vw)->vw_MaxY)
	{
	    long len = line_glyph_length(tx, row);
	    if(len != 0)
	    {
		len = ROUND_UP_INT(len, VVIEW(vw)->vw_MaxX - 1);
		grow += len / (VVIEW(vw)->vw_MaxX - 1);
	    }
	    else
		grow++;
	    row++;
	}
	if(grow >= VVIEW(vw)->vw_MaxY)
	    return sym_nil;
	gcol = glyph_col(tx, VCOL(pos), VROW(pos));
	while(gcol >= VVIEW(vw)->vw_MaxX - 1)
	{
	    gcol -= VVIEW(vw)->vw_MaxX - 1;
	    grow++;
	}
    }
    else
    {
	grow = VROW(pos) - VROW(VVIEW(vw)->vw_DisplayOrigin);
	if(grow < 0 || grow >= VVIEW(vw)->vw_MaxY)
	    return sym_nil;
	gcol = (glyph_col(tx, VCOL(pos), VROW(pos))
		- VCOL(VVIEW(vw)->vw_DisplayOrigin));
	if(gcol < 0 || gcol >= VVIEW(vw)->vw_MaxX)
	    return sym_nil;
    }
    return make_pos(gcol, grow);
}

_PR VALUE cmd_default_glyph_table(void);
DEFUN("default-glyph-table", cmd_default_glyph_table, subr_default_glyph_table, (void), V_Subr0, DOC_default_glyph_table) /*
::doc:default_glyph_table::
default-glyph-table

Returns the standard glyph-table.
::end:: */
{
    return(VAL(&default_glyph_table));
}

_PR VALUE cmd_make_glyph_table(VALUE src);
DEFUN("make-glyph-table", cmd_make_glyph_table, subr_make_glyph_table, (VALUE src), V_Subr1, DOC_make_glyph_table) /*
::doc:make_glyph_table::
make-glyph-table SRC

Creates a new glyph-table. If SRC is a glyph-table it will be copied, else if
SRC is a buffer that buffer's glyph-table will be copied. If SRC is nil the
default glyph-table will be copied.
::end:: */
{
    glyph_table_t *newgt = ALLOC_OBJECT(sizeof(glyph_table_t));
    if(newgt)
    {
	glyph_table_t *srcgt;
	if(GLYPHTABP(src))
	    srcgt = VGLYPHTAB(src);
	else if(BUFFERP(src))
	    srcgt = VGLYPHTAB(VTX(src)->tx_GlyphTable);
	else
	    srcgt = &default_glyph_table;
	memcpy(newgt, srcgt, sizeof(glyph_table_t));
	newgt->gt_Car = V_GlyphTable;
	newgt->gt_Next = gt_chain;
	gt_chain = newgt;
	data_after_gc += sizeof(glyph_table_t);
	return(VAL(newgt));
    }
    return mem_error();
}

_PR VALUE cmd_set_glyph(VALUE gt, VALUE ch, VALUE glyph);
DEFUN("set-glyph", cmd_set_glyph, subr_set_glyph, (VALUE gt, VALUE ch, VALUE glyph), V_Subr3, DOC_set_glyph) /*
::doc:set_glyph::
set-glyph GLYPH-TABLE CHARACTER GLYPH-STRING

Make the renderer draw the string GLYPH-STRING (no more than 4 characters)
for each character CHARACTER in any buffers which use the GLYPH-TABLE.
::end:: */
{
    int glyphlen;
    DECLARE1(gt, GLYPHTABP);
    DECLARE2(ch, INTP);
    DECLARE3(glyph, STRINGP);
    if((VINT(ch) < 0) || (VINT(ch) >= 256))
    {
	signal_arg_error(ch, 1);
	return LISP_NULL;
    }
    glyphlen = STRING_LEN(glyph);
    if(glyphlen > 4)
    {
	signal_arg_error(glyph, 2);
	return LISP_NULL;
    }
    VGLYPHTAB(gt)->gt_Widths[VINT(ch)] = glyphlen;
    if(glyphlen == 0)
    {
	/* put a space in the first character */
	VGLYPHTAB(gt)->gt_Glyphs[VINT(ch)][0] = ' ';
    }
    else
	memcpy(&VGLYPHTAB(gt)->gt_Glyphs[VINT(ch)][0], VSTR(glyph), glyphlen);

    return(sym_t);
}

_PR VALUE cmd_get_glyph(VALUE gt, VALUE ch);
DEFUN("get-glyph", cmd_get_glyph, subr_get_glyph, (VALUE gt, VALUE ch), V_Subr2, DOC_get_glyph) /*
::doc:get_glyph::
get-glyph GLYPH-TABLE CHARACTER

Return the string which is the rendered representation of CHARACTER in
GLYPH-TABLE.
::end:: */
{
    DECLARE1(gt, GLYPHTABP);
    DECLARE2(ch, INTP);
    if((VINT(ch) < 0) || (VINT(ch) >= 256))
    {
	signal_arg_error(ch, 1);
	return LISP_NULL;
    }
    return(string_dupn(&VGLYPHTAB(gt)->gt_Glyphs[VINT(ch)][0],
		       VGLYPHTAB(gt)->gt_Widths[VINT(ch)]));
}

_PR VALUE cmd_buffer_glyph_table(VALUE tx);
DEFUN("buffer-glyph-table", cmd_buffer_glyph_table, subr_buffer_glyph_table, (VALUE tx), V_Subr1, DOC_buffer_glyph_table) /*
::doc:buffer_glyph_table::
buffer-glyph-table [BUFFER]

Returns the glyph-table being used in BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return(VTX(tx)->tx_GlyphTable);
}

_PR VALUE cmd_set_buffer_glyph_table(VALUE tx, VALUE gt);
DEFUN("set-buffer-glyph-table", cmd_set_buffer_glyph_table, subr_set_buffer_glyph_table, (VALUE tx, VALUE gt), V_Subr2, DOC_set_buffer_glyph_table) /*
::doc:set_buffer_glyph_table::
set-buffer-glyph-table [BUFFER] GLYPH-TABLE

Sets the glyph-table being used in BUFFER to GLYPH-TABLE.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    DECLARE2(gt, GLYPHTABP);
    VTX(tx)->tx_GlyphTable = gt;
    return(gt);
}

void
glyphtable_sweep(void)
{
    glyph_table_t *gt = gt_chain;
    gt_chain = NULL;
    while(gt)
    {
	glyph_table_t *nxt = gt->gt_Next;
	if(!GC_CELL_MARKEDP(VAL(gt)) && !(gt->gt_Car & GTF_STATIC))
	    FREE_OBJECT(gt);
	else
	{
	    GC_CLR_CELL(VAL(gt));
	    gt->gt_Next = gt_chain;
	    gt_chain = gt;
	}
	gt = nxt;
    }
}

void
glyphtable_prin(VALUE strm, VALUE obj)
{
    stream_puts(strm, "#<glyph-table>", -1, FALSE);
}

void
glyphs_init(void)
{
    ADD_SUBR(subr_glyph_table_p);
    ADD_SUBR(subr_char_to_glyph_pos);
    ADD_SUBR(subr_glyph_to_char_pos);
    ADD_SUBR(subr_display_to_char_pos);
    ADD_SUBR(subr_char_to_display_pos);
    ADD_SUBR(subr_default_glyph_table);
    ADD_SUBR(subr_make_glyph_table);
    ADD_SUBR(subr_set_glyph);
    ADD_SUBR(subr_get_glyph);
    ADD_SUBR(subr_buffer_glyph_table);
    ADD_SUBR(subr_set_buffer_glyph_table);
}

void
glyphs_kill(void)
{
    glyph_table_t *gt = gt_chain;
    while(gt)
    {
	glyph_table_t *nxt = gt->gt_Next;
	if(!(gt->gt_Car & GTF_STATIC))
	    FREE_OBJECT(gt);
	gt = nxt;
    }
    gt_chain = NULL;
}
