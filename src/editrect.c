/* editrect.c -- Manipulating `rectangles' of text
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR bool rect_insert(TX *, const u_char *, long, POS *);
_PR bool rect_delete(TX *, POS *, POS *);
_PR void order_rect(TX *, POS *, POS *, POS *, POS *);
_PR VALUE rect_copy(TX *, POS *, POS *);
_PR void editrect_init(void);

bool
rect_insert(TX *tx, const u_char *text, long textLen, POS *pos)
{
    long len, col = pos->pos_Col;
    LINE *line = tx->tx_Lines + pos->pos_Line;
    POS orig;
    while(textLen > 0 && pos->pos_Line < tx->tx_NumLines)
    {
	const u_char *end = memchr(text, '\n', textLen);
	if(!end)
	{
	    end = text + textLen;
	    len = textLen;
	    textLen = 0;
	}
	else
	{
	    len = end - text;
	    textLen -= len + 1;
	}
	if(!pad_pos(tx, pos))
	    goto nomem;
	orig = *pos;
	if(!insert_str_n(tx, text, len, pos))
	    goto nomem;
	undo_record_insertion(tx, &orig, pos);
	flag_insertion(tx, &orig, pos);
	pos->pos_Line++;
	pos->pos_Col = col;
	line++;
	text = (*end == '\n') ? end + 1 : end;
    }
    if(textLen > 0 && pos->pos_Line == tx->tx_NumLines)
    {
	/*
	 * Now start expanding off the bottom of the file.
	 */
	orig = *pos;
	while(textLen > 0)
	{
	    const u_char *end = memchr(text, '\n', textLen);
	    u_char *new;
	    if(!end)
	    {
		end = text + textLen;
		len = textLen;
		textLen = 0;
	    }
	    else
	    {
		len = end - text;
		textLen -= len + 1;
	    }
	    if(!resize_line_list(tx, +1, pos->pos_Line))
		goto nomem;
	    if(!(new = str_alloc(col + len + 1)))
		goto nomem;
	    memset(new, ' ', col);
	    memcpy(new + col, text, len);
	    new[col + len] = 0;
	    tx->tx_Lines[pos->pos_Line].ln_Strlen = col + len + 1;
	    tx->tx_Lines[pos->pos_Line].ln_Line = new;
	    adjust_marks_add_y(tx, +1, pos->pos_Line);
	    pos->pos_Line++;
	    line++;
	    text = (*end == '\n') ? end + 1 : end;
	}
	if(len > 0)
	    pos->pos_Col += len;
	pos->pos_Line--;
	undo_record_insertion(tx, &orig, pos);
	flag_insertion(tx, &orig, pos);
    }
    return(TRUE);
nomem:
    mem_error();
    return(FALSE);
}

bool
rect_delete(TX *tx, POS *startPos, POS *endPos)
{
    LINE *line = tx->tx_Lines + startPos->pos_Line;
    while(startPos->pos_Line <= endPos->pos_Line)
    {
	POS start;
	start.pos_Col = char_col(tx, startPos->pos_Col, startPos->pos_Line);
	if(line->ln_Strlen > start.pos_Col)
	{
	    long del_len, spc_len;
	    POS end;
	    long start_glyph = glyph_col(tx, start.pos_Col,
					 startPos->pos_Line);
	    if((start_glyph != startPos->pos_Col) && (start.pos_Col > 0))
	    {
		start.pos_Col--;
		start_glyph = glyph_col(tx, start.pos_Col, startPos->pos_Line);
	    }
	    end.pos_Col = char_col(tx, endPos->pos_Col, startPos->pos_Line);
	    if(end.pos_Col >= line->ln_Strlen)
		end.pos_Col = line->ln_Strlen - 1;
	    del_len = end.pos_Col - start.pos_Col;
	    spc_len = startPos->pos_Col - start_glyph;
	    start.pos_Line = end.pos_Line = startPos->pos_Line;
	    undo_record_deletion(tx, &start, &end);
	    if(!delete_chars(tx, &start, del_len))
		return(FALSE);
	    flag_deletion(tx, &start, &end);

	    /* May need to insert some spaces to compensate for a deleted
	       TAB which crossed the start of the deletion.  */
	    if(spc_len > 0)
	    {
		if(!insert_gap(tx, spc_len, &start))
		    return(FALSE);
		memset(line->ln_Line + start.pos_Col, ' ', spc_len);
		end.pos_Col = start.pos_Col + spc_len;
		undo_record_insertion(tx, &start, &end);
		flag_insertion(tx, &start, &end);
	    }
	}
	startPos->pos_Line++;
	line++;
    }
    return(TRUE);
}

/* Makes sure that the corners of a rectangle are how I want them. Converts
   from character positions to glyph positions.  */
void
order_rect(TX *tx, POS *dstart, POS *dend, POS *sstart, POS *send)
{
    long start_col = glyph_col(tx, sstart->pos_Col, sstart->pos_Line);
    long end_col = glyph_col(tx, send->pos_Col, send->pos_Line);
    if(start_col < end_col)
    {
	dstart->pos_Col = start_col;
	dend->pos_Col = end_col;
    }
    else
    {
	dstart->pos_Col = end_col;
	dend->pos_Col = start_col;
    }
    if(sstart->pos_Line < send->pos_Line)
    {
	dstart->pos_Line = sstart->pos_Line;
	dend->pos_Line = send->pos_Line;
    }
    else
    {
	dstart->pos_Line = send->pos_Line;
	dend->pos_Line = sstart->pos_Line;
    }
}

/* Copies a rectangle to a string, newlines are inserted between lines. */
VALUE
rect_copy(TX *tx, POS *startPos, POS *endPos)
{
    VALUE res;
    u_char *buf = str_alloc(128);
    long len = 0, buflen = 128;
    long linenum = startPos->pos_Line;
    LINE *line = tx->tx_Lines + linenum;
    long width = (endPos->pos_Col - startPos->pos_Col) + 1;
    if(!buf)
	return(mem_error());
    while(linenum <= endPos->pos_Line)
    {
	long glyphs;
	if(len + width >= buflen)
	{
	    long newlen = buflen * 2;
	    u_char *new = str_alloc(newlen);
	    if(!new)
	    {
		str_free(buf);
		return(mem_error());
	    }
	    memcpy(new, buf, len);
	    str_free(buf);
	    buf = new;
	    buflen = newlen;
	}
	len += expand_tabs(tx, buf + len, startPos->pos_Col, endPos->pos_Col,
			   linenum, &glyphs);
	if(glyphs < width - 1)
	{
	    glyphs = width - glyphs - 1;
	    memset(buf + len, ' ', glyphs);
	    len += glyphs;
	}
	buf[len++] = '\n';
	linenum++;
	line++;
    }
    res = string_dupn(buf, len);
    str_free(buf);
    return(res);
}

_PR VALUE cmd_insert_rect(VALUE str, VALUE pos, VALUE buff);
DEFUN_INT("insert-rect", cmd_insert_rect, subr_insert_rect, (VALUE str, VALUE lpos, VALUE buff), V_Subr3, DOC_insert_rect, "sString to insert rectangularly") /*
::doc:insert_rect::
insert-rect STRING [POS] [BUFFER]

Inserts STRING into BUFFER at POS treating it as a ``rectangle'' of
text -- that is, each separate line in STRING (separated by newlines) is
inserted at the *same* column in successive lines.

Returns the position of the character following the end of the inserted
text.
::end:: */
{
    POS pos;
    DECLARE1(str, STRINGP);
    if(POSP(lpos))
	pos = VPOS(lpos);
    else
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff)) && pad_pos(VTX(buff), &pos))
    {
	if(rect_insert(VTX(buff), VSTR(str), STRING_LEN(str), &pos))
	    return(make_lpos(&pos));
    }
    return(sym_nil);
}

_PR VALUE cmd_delete_rect(VALUE lstart, VALUE lend, VALUE buff);
DEFUN("delete-rect", cmd_delete_rect, subr_delete_rect, (VALUE lstart, VALUE lend, VALUE buff), V_Subr3, DOC_delete_rect) /*
::doc:delete_rect::
delete-rect START-POS END-POS [BUFFER]

Deletes the rectangle of text from one corner, START-POS, to the opposite
corner, END-POS.
::end:: */
{
    POS start, end;
    DECLARE1(lstart, POSP);
    DECLARE2(lend, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    order_rect(VTX(buff), &start, &end, &VPOS(lstart), &VPOS(lend));
    if(!read_only(VTX(buff)) && check_line(VTX(buff), &end))
    {
	rect_delete(VTX(buff), &start, &end);
	return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_copy_rect(VALUE lstart, VALUE lend, VALUE buff);
DEFUN("copy-rect", cmd_copy_rect, subr_copy_rect, (VALUE lstart, VALUE lend, VALUE buff), V_Subr3, DOC_copy_rect) /*
::doc:copy_rect::
copy-rect START-POS END-POS [BUFFER]

Returns the rectangle of text marked out by START-POS and END-POS.
::end:: */
{
    POS start, end;
    DECLARE1(lstart, POSP);
    DECLARE2(lend, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    order_rect(VTX(buff), &start, &end, &VPOS(lstart), &VPOS(lend));
    if(check_line(VTX(buff), &end))
	return(rect_copy(VTX(buff), &start, &end));
    return(sym_nil);
}

_PR VALUE cmd_cut_rect(VALUE lstart, VALUE lend, VALUE buff);
DEFUN("cut-rect", cmd_cut_rect, subr_cut_rect, (VALUE lstart, VALUE lend, VALUE buff), V_Subr3, DOC_cut_rect) /*
::doc:cut_rect::
cut-rect START-POS END-POS [BUFFER]

The same as `copy-rect' except that the section of text copied (START-POS
to END-POS) is deleted from the file after being duplicated.
::end:: */
{
    POS start, end;
    DECLARE1(lstart, POSP);
    DECLARE2(lend, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    order_rect(VTX(buff), &start, &end, &VPOS(lstart), &VPOS(lend));
    if(!read_only(VTX(buff)) && check_line(VTX(buff), &end))
    {
	VALUE str = rect_copy(VTX(buff), &start, &end);
	if(str)
	    rect_delete(VTX(buff), &start, &end);
	return(str);
    }
    return(sym_nil);
}

void
editrect_init(void)
{
    ADD_SUBR(subr_insert_rect);
    ADD_SUBR(subr_delete_rect);
    ADD_SUBR(subr_copy_rect);
    ADD_SUBR(subr_cut_rect);
}
