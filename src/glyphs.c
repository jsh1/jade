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

_PR long make_glyph_array(TX *, const u_char *, long, long, u_char *, long, long);
_PR long string_glyph_length(TX *, const u_char *, long);
_PR long glyph_col(TX *, long, long);
_PR long char_col(TX *, long, long);
_PR void calc_cursor_offset(VW *);
_PR void adjust_cursor_to_glyph(VW *);
_PR u_char *char_glyphs(TX *, u_char);
_PR long expand_tabs(TX *, u_char *, long, long, long, long *);

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

typedef struct _GlyphTable{
    u_char		gt_Type;
    u_char		gt_Flags;
    struct _GlyphTable *gt_Next;
    glyph_widths_t	gt_Widths;
    glyph_glyphs_t	gt_Glyphs;
} GlyphTable;
#define GTF_STATIC 1		/* Don't free() this table */

static GlyphTable default_glyph_table = {
    V_GlyphTable,
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

static GlyphTable *gt_chain = &default_glyph_table;

/* From the array of characters SRC, build an array of glyph codes DST.
   SRC contains at least SRC-LEN characters. On return, there is guaranteed
   to be no more than DST-LEN glyph codes in DST, the actual number is
   the return value of the function. No glyphs are copied to DST until
   DST-START glyphs have gone before. SRC-ORIG is the *true glyph* position
   of SRC, for calculating tab sizes.  */
long
make_glyph_array(TX *tx, const u_char *src, long srcOrig, long srcLen,
		 register u_char *dst, long dstStart, long dstLen)
{
    glyph_widths_t *width_table = &VGLYPHTAB(tx->tx_GlyphTable)->gt_Widths;
    glyph_glyphs_t *glyph_table = &VGLYPHTAB(tx->tx_GlyphTable)->gt_Glyphs;
    register long i = 0;
    if(dstStart != 0)
    {
	/* Skip DSTSTART glyphs */
	register long j = 0;
	while((j < dstStart) && (srcLen-- > 0))
	{
	    register int w = (*width_table)[*src++];
	    if(w == 0)
	    {
		j += tx->tx_TabSize - ((j + srcOrig) % tx->tx_TabSize);
		if(j > dstStart)
		{
		    i = j - dstStart;
		    memset(dst, ' ', i);
		    dst += i;
		    break;
		}
	    }
	    else
	    {
		j += w;
		if(j > dstStart)
		{
		    i = j - dstStart;
		    memcpy(dst, &(*glyph_table)[src[-1]][w-i], i);
		    dst += i;
		    break;
		}
	    }
	}
    }
    while((i < dstLen) && (srcLen-- > 0))
    {
	register u_char c;
	switch((*width_table)[c = *src++])
	{
	case 0:
	    {
		/* TAB special case */
		register int size = tx->tx_TabSize - ((i + srcOrig + dstStart)
						      % tx->tx_TabSize);
		memset(dst, ' ', size);
		dst += size;
		i += size;
	    }
	    break;
	case 1:
	    *dst++ = (*glyph_table)[c][0];
	    i++;
	    break;
	case 2:
	    *dst++ = (*glyph_table)[c][0];
	    *dst++ = (*glyph_table)[c][1];
	    i += 2;
	    break;
	case 3:
	    *dst++ = (*glyph_table)[c][0];
	    *dst++ = (*glyph_table)[c][1];
	    *dst++ = (*glyph_table)[c][2];
	    i += 3;
	    break;
	case 4:
	    *dst++ = (*glyph_table)[c][0];
	    *dst++ = (*glyph_table)[c][1];
	    *dst++ = (*glyph_table)[c][2];
	    *dst++ = (*glyph_table)[c][3];
	    i += 4;
	    break;
	}
    }
    return(i);
}

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
    while((w < col) && (srclen-- != 0))
    {
	register int w1 = (*width_table)[*src++];
	if(w1 == 0)
	    w += tx->tx_TabSize - (w % tx->tx_TabSize);
	else
	    w += w1;
    }
    if(srclen == -1)
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
	 && POS_EQUAL_P(&vw->vw_CursorPos, &vw->vw_LastCursorPos)))
    {
	/* Have to recalculate the number of glyphs before the cursor. */
	vw->vw_LastCursorOffset = glyph_col(tx, vw->vw_CursorPos.pos_Col,
					    vw->vw_CursorPos.pos_Line);
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
    vw->vw_CursorPos.pos_Col = char_col(vw->vw_Tx, vw->vw_LastCursorOffset,
					vw->vw_CursorPos.pos_Line);
}

u_char *
char_glyphs(TX *tx, u_char ch)
{
    return(&VGLYPHTAB(tx->tx_GlyphTable)->gt_Glyphs[ch][0]);
}

/* Expands any tabs in the line LINE-NUM in TX, between the glyph
   positions START-GLYPH and END-GLYPH into the buffer DST. The
   actual number of characters copied into DST (won't be any more
   than (END-GLYPH - START-GLYPH) is returned.	If WIDTH-P is non-NULL
   the true width (in glyphs) of the copied portion is stored there. */
long
expand_tabs(TX *tx, u_char *dst, long start_glyph, long end_glyph,
	    long linenum, long *width_p)
{
    LINE *line = tx->tx_Lines + linenum;
    u_char *src = line->ln_Line;
    u_char *orig_dst = dst;
    long srclen = line->ln_Strlen - 1;
    glyph_widths_t *width_table = &VGLYPHTAB(tx->tx_GlyphTable)->gt_Widths;
    register long i = 0;
    if(start_glyph > 0)
    {
	while(srclen-- != 0)
	{
	    register int w1 = (*width_table)[*src++];
	    if(w1 == 0)
	    {
		i += tx->tx_TabSize - (i % tx->tx_TabSize);
		if(i > start_glyph)
		{
		    memset(dst, ' ', i - start_glyph);
		    dst += i - start_glyph;
		    break;
		}
	    }
	    else
	    {
		i += w1;
		if(i > start_glyph)
		{
		    *dst++ = src[-1];
		    break;
		}
	    }
	}
    }
    if(srclen == -1)
	return(dst - orig_dst);
    while(srclen-- != 0)
    {
	u_char c = *src++;
	register int w1 = (*width_table)[c];
	if(w1 == 0)
	{
	    w1 = tx->tx_TabSize - (i % tx->tx_TabSize);
	    if(w1 + i >= end_glyph)
	    {

		memset(dst, ' ', end_glyph - i);
		dst += end_glyph - i;
		break;
	    }
	    memset(dst, ' ', w1);
	    dst += w1;
	    i += w1;
	}
	else
	{
	    *dst++ = c;
	    i += w1;
	    if(i >= end_glyph)
		break;
	}
    }
    if(width_p)
	*width_p = i - start_glyph;
    return(dst - orig_dst);
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

_PR VALUE cmd_char_to_glyph_pos(VALUE vpos, VALUE tx);
DEFUN("char-to-glyph-pos", cmd_char_to_glyph_pos, subr_char_to_glyph_pos, (VALUE vpos, VALUE tx), V_Subr2, DOC_char_to_glyph_pos) /*
::doc:char_to_glyph_pos::
char-to-glyph-pos [POS] [BUFFER]

From the character position POS, find its true *physical* position when
rendered.
::end:: */
{
    POS pos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(vpos))
	pos = VPOS(vpos);
    else
	pos = *get_tx_cursor(VTX(tx));
    pos.pos_Col = glyph_col(VTX(tx), pos.pos_Col, pos.pos_Line);
    return(make_lpos(&pos));
}

_PR VALUE cmd_glyph_to_char_pos(VALUE vpos, VALUE tx);
DEFUN("glyph-to-char-pos", cmd_glyph_to_char_pos, subr_glyph_to_char_pos, (VALUE vpos, VALUE tx), V_Subr2, DOC_glyph_to_char_pos) /*
::doc:glyph_to_char_pos::
glyph-to-char-pos POS [BUFFER]

For the physical position POS, find the closest matching actual character
position.
::end:: */
{
    POS pos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    DECLARE1(vpos, POSP);
    pos = VPOS(vpos);
    pos.pos_Col = char_col(VTX(tx), pos.pos_Col, pos.pos_Line);
    return(make_lpos(&pos));
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
    GlyphTable *newgt = mymalloc(sizeof(GlyphTable));
    if(newgt)
    {
	GlyphTable *srcgt;
	if(GLYPHTABP(src))
	    srcgt = VGLYPHTAB(src);
	else if(BUFFERP(src))
	    srcgt = VGLYPHTAB(VTX(src)->tx_GlyphTable);
	else
	    srcgt = &default_glyph_table;
	memcpy(newgt, srcgt, sizeof(GlyphTable));
	newgt->gt_Flags &= ~GTF_STATIC;
	newgt->gt_Next = gt_chain;
	gt_chain = newgt;
	data_after_gc += sizeof(GlyphTable);
	return(VAL(newgt));
    }
    return(sym_nil);
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
    TX *tx;
    DECLARE1(gt, GLYPHTABP);
    DECLARE2(ch, NUMBERP);
    DECLARE3(glyph, STRINGP);
    if((VNUM(ch) < 0) || (VNUM(ch) >= 256))
    {
	signal_arg_error(ch, 1);
	return(NULL);
    }
    glyphlen = STRING_LEN(glyph);
    if(glyphlen > 4)
    {
	signal_arg_error(glyph, 2);
	return(NULL);
    }
    VGLYPHTAB(gt)->gt_Widths[VNUM(ch)] = glyphlen;
    if(glyphlen == 0)
    {
	/* put a space in the first character */
	VGLYPHTAB(gt)->gt_Glyphs[VNUM(ch)][0] = ' ';
    }
    else
	memcpy(&VGLYPHTAB(gt)->gt_Glyphs[VNUM(ch)][0], VSTR(glyph), glyphlen);

    tx = buffer_chain;
    while(tx)
    {
	/* refresh all buffers which use this glyph table */
	if(tx->tx_GlyphTable == gt)
	    tx->tx_Flags |= TXFF_REFRESH_ALL;
	tx = tx->tx_Next;
    }

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
    DECLARE2(ch, NUMBERP);
    if((VNUM(ch) < 0) || (VNUM(ch) >= 256))
    {
	signal_arg_error(ch, 1);
	return(NULL);
    }
    return(string_dupn(&VGLYPHTAB(gt)->gt_Glyphs[VNUM(ch)][0],
		       VGLYPHTAB(gt)->gt_Widths[VNUM(ch)]));
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
    VTX(tx)->tx_Flags |= TXFF_REFRESH_ALL;
    return(gt);
}

void
glyphtable_sweep(void)
{
    GlyphTable *gt = gt_chain;
    gt_chain = NULL;
    while(gt)
    {
	GlyphTable *nxt = gt->gt_Next;
	if(!GC_MARKEDP(VAL(gt)) && !(gt->gt_Flags & GTF_STATIC))
	    myfree(gt);
	else
	{
	    GC_CLR(VAL(gt));
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
    GlyphTable *gt = gt_chain;
    while(gt)
    {
	GlyphTable *nxt = gt->gt_Next;
	if(!(gt->gt_Flags & GTF_STATIC))
	    myfree(gt);
	gt = nxt;
    }
    gt_chain = NULL;
}
