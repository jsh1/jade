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

_PR long string_glyph_length(TX *, const u_char *, long);
_PR long glyph_col(TX *, long, long);
_PR long char_col(TX *, long, long);
_PR void calc_cursor_offset(VW *);
_PR void adjust_cursor_to_glyph(VW *);
_PR u_char *char_glyphs(TX *, u_char);

_PR void glyphtable_sweep(void);
_PR void glyphtable_prin(VALUE, VALUE);
_PR void glyphs_init(void);
_PR void glyphs_kill(void);

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


/* Filling glyph buffers */

/* Output a glyph CH. Check for attribute changes, output
   attribute, and advance glyph_col variable */
#define OUTPUT(ch)					\
    do {						\
	if(!in_block && !cursor_row)			\
	    *attrs++ = attr;				\
	else if(!in_block)				\
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
	else						\
	{						\
	    if(!rect_block)				\
	    {						\
		/* check for a normal block */		\
		if(attr == GA_Text && block_start	\
		   && char_col == VCOL(vw->vw_BlockS))	\
		    attr = GA_Block;			\
		if(attr == GA_Block && block_end	\
		   && char_col == VCOL(vw->vw_BlockE))	\
		    attr = GA_Text;			\
	    }						\
	    else if(block_row)				\
	    {						\
		/* check for a rectangular block */	\
		if(glyph_col == block_start)		\
		    attr = GA_Block;			\
		else if(glyph_col == block_end)		\
		    attr = GA_Text;			\
	    }						\
	    if(cursor_row && char_col == cursor_col)	\
	    {						\
		*attrs++ = cursor_attrs[attr];		\
		cursor_row = FALSE;			\
	    }						\
	    else					\
		*attrs++ = attr;			\
	}						\
	*codes++ = ch;					\
	glyph_col++;					\
    } while(0)

/* Same as above, but check for overflowing past
   the last column in the window. */
#define OUTPUT_CHK(ch)					\
    do {						\
	if(glyph_col < last_col)			\
	    OUTPUT(ch);					\
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
	long first_col = VCOL(vw->vw_DisplayOrigin);
	long first_row = VROW(vw->vw_DisplayOrigin);
	long last_col = first_col + vw->vw_MaxX;
	long last_row = MIN(first_row + vw->vw_MaxY, vw->vw_Tx->tx_LogicalEnd);
	long cursor_col = VCOL(vw->vw_CursorPos);
	long char_row = first_row;

	bool in_block, rect_block = FALSE;
	long block_start = 0, block_end = 0;

	update_status_buffer(vw);

	/* Memorise some facts about when the block starts and stops. */
	if(vw->vw_BlockStatus == 0)
	{
	    if(POS_LESS_EQUAL_P(vw->vw_BlockE, vw->vw_DisplayOrigin)
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
			block_start = MIN(VCOL(vw->vw_BlockS),
					  VCOL(vw->vw_BlockE));
			block_start = MAX(block_start, first_col);
			block_end = MAX(VCOL(vw->vw_BlockS),
					VCOL(vw->vw_BlockE));
			block_end = MIN(block_end, first_col + vw->vw_MaxX);
		    }
		}
		else
		    rect_block = FALSE;
	    }
	}
	else
	    in_block = FALSE;

	while(char_row < last_row)
	{
	    /* Fill in the glyphs for GLYPH_ROW */
	    u_char *src = vw->vw_Tx->tx_Lines[char_row].ln_Line;
	    long src_len = vw->vw_Tx->tx_Lines[char_row].ln_Strlen - 1;
	    long glyph_col = 0, char_col = 0;

	    /* Is the cursor in this row? */
	    bool cursor_row = (vw == w->w_CurrVW
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
		    block_start = VROW(vw->vw_BlockS) == char_row;
		    block_end = VROW(vw->vw_BlockE) == char_row;
		    if((char_row > VROW(vw->vw_BlockS)
			|| (char_row == VROW(vw->vw_BlockS)
			    && char_col >= VCOL(vw->vw_BlockS)))
		       && (char_row < VROW(vw->vw_BlockE)
			   || (char_row == VROW(vw->vw_BlockE)
			       && char_col < VCOL(vw->vw_BlockE))))
			attr = GA_Block;
		}
		else
		{
		    if(char_row >= VROW(vw->vw_BlockS)
		       && first_col >= VCOL(vw->vw_BlockS)
		       && char_row < VROW(vw->vw_BlockE)
		       && first_col < VCOL(vw->vw_BlockE))
			attr = GA_Block;
		}
	    }

	    if(first_col > 0)
	    {
		/* First, skip glyphs up to the first displayed column. */
		while(glyph_col < first_col && src_len-- > 0)
		{
		    int width = (*width_table)[*src++];
		    if(width == 0)
		    {
			/* TAB */
			glyph_col += tab_size - (glyph_col % tab_size);
			if(glyph_col > first_col)
			{
			    int i = glyph_col - first_col;
			    glyph_col = first_col;
			    while(i-- > 0)
				OUTPUT(' ');
			}
		    }
		    else
		    {
			glyph_col += width;
			if(glyph_col > first_col)
			{
			    int i = glyph_col - first_col;
			    u_char *out = &(*glyph_table)[src[-1]][width - i];
			    glyph_col = first_col;
			    while(i-- > 0)
				OUTPUT(*out++);
			}
		    }
		    char_col++;
		}
	    }

	    /* Now start outputting properly. */
	    while(glyph_col < last_col && src_len-- > 0)
	    {
		u_char *ptr = &(*glyph_table)[*src][0];
		switch((*width_table)[*src++])
		{
		case 0:
		    {
			/* TAB */
			int i = tab_size - (glyph_col % tab_size);
			while(glyph_col < last_col && i-- > 0)
			    OUTPUT(' ');
			break;
		    }

		    /* Do some funky fall through stuff to minimise
		       calls to the big OUTPUT macro. */
		case 4:
		    OUTPUT_CHK(*ptr++);
		case 3:
		    OUTPUT_CHK(*ptr++);
		case 2:
		    OUTPUT_CHK(*ptr++);
		case 1:
		    OUTPUT_CHK(*ptr);
		}
		char_col++;
	    }

	    if(glyph_col < first_col)
		/* in case EOL before first column */
		char_col = glyph_col = first_col;

	    while(glyph_col < last_col)
	    {
		/* This line ends before the last column in the
		   window -- fill with spaces. */
		OUTPUT(' ');
		char_col++;		/* in case the cursor is past EOL */
	    }
	    glyph_row++;
	    char_row++;
	}
	while(glyph_row < vw->vw_FirstY + vw->vw_MaxY)
	{
	    /* We reached the [logical] end of the buffer. Fill with
	       blank lines */
	    memset(codes, ' ', g->cols);
	    memset(attrs, GA_Text, g->cols);
	    codes += g->cols;
	    attrs += g->cols;
	    glyph_row++;
	}
	if((vw->vw_Flags & VWFF_MINIBUF) == 0)
	{
	    /* Do status line
	       TODO: use glyph table to output status buffer */
	    int len = strlen(vw->vw_StatusBuf);
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
	/* A minibuffer with a message obscuring it.
           TODO: use glyph table to output message */
	memcpy(codes, w->w_Message, MIN(w->w_MessageLen, g->cols));
	if(w->w_MessageLen < g->cols)
	    memset(codes + w->w_MessageLen, ' ', g->cols - w->w_MessageLen);
	memset(attrs, GA_Text, g->cols);
    }
}


/* Utility functions */

/* Returns the number of glyphs needed to draw the string SRC.	*/
long
string_glyph_length(TX *tx, const u_char *src, long srcLen)
{
    glyph_widths_t *width_table = &VGLYPHTAB(tx->tx_GlyphTable)->gt_Widths;
    register long w;
    for(w = 0; srcLen-- > 0;)
    {
	register int w1 = (*width_table)[*src++];
	if(w1 == 0)
	    w += tx->tx_TabSize - (w % tx->tx_TabSize);
	else
	    w += w1;
    }
    return(w);
}

/* Return the glyph index of (COL,LINE) in TX.	*/
long
glyph_col(TX *tx, long col, long linenum)
{
    LINE *line = tx->tx_Lines + linenum;
    if(col >= line->ln_Strlen)
    {
	return((string_glyph_length(tx, line->ln_Line, line->ln_Strlen - 1)
		+ (col - (line->ln_Strlen - 1))));
    }
    else
	return(string_glyph_length(tx, line->ln_Line, col));
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

void
calc_cursor_offset(VW *vw)
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
}

/* Sets the vw_CursorPos.pos_Col so that the cursor's glyph pos is the same
   as vw_LastCursorOffset.  */
void
adjust_cursor_to_glyph(VW *vw)
{
    vw->vw_CursorPos = make_pos(char_col(vw->vw_Tx, vw->vw_LastCursorOffset,
					 VROW(vw->vw_CursorPos)),
				VROW(vw->vw_CursorPos));
}

u_char *
char_glyphs(TX *tx, u_char ch)
{
    return(&VGLYPHTAB(tx->tx_GlyphTable)->gt_Glyphs[ch][0]);
}

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
    ADD_SUBR(subr_glyph_to_char_pos);
    ADD_SUBR(subr_char_to_glyph_pos);
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
