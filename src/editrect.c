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

_PR VALUE rect_insert(TX *, const u_char *, long, VALUE);
_PR bool rect_delete(TX *, VALUE, VALUE);
_PR void order_rect(TX *, VALUE *, VALUE *);
_PR VALUE rect_copy(TX *, VALUE, VALUE);
_PR void editrect_init(void);

VALUE
rect_insert(TX *tx, const u_char *text, long textLen, VALUE pos)
{
    long len, col = VCOL(pos);
    LINE *line = tx->tx_Lines + VROW(pos);
    VALUE orig;
    while(textLen > 0 && VROW(pos) < tx->tx_NumLines)
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
	orig = pos;
	pos = insert_bytes(tx, text, len, pos);
	if(pos == NULL)
	    goto nomem;
	undo_record_insertion(tx, orig, pos);
	flag_insertion(tx, orig, pos);
	pos = make_pos(col, VROW(pos) + 1);
	line++;
	text = (*end == '\n') ? end + 1 : end;
    }
    if(textLen > 0 && VROW(pos) == tx->tx_NumLines)
    {
	/*
	 * Now start expanding off the bottom of the file.
	 */
	orig = pos;
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
	    if(!resize_line_list(tx, +1, VROW(pos)))
		goto nomem;
	    if(!(new = str_alloc(col + len + 1)))
		goto nomem;
	    memset(new, ' ', col);
	    memcpy(new + col, text, len);
	    new[col + len] = 0;
	    tx->tx_Lines[VROW(pos)].ln_Strlen = col + len + 1;
	    tx->tx_Lines[VROW(pos)].ln_Line = new;
	    adjust_marks_add_y(tx, +1, VROW(pos));
	    pos = make_pos(VCOL(pos), VROW(pos) + 1);
	    line++;
	    text = (*end == '\n') ? end + 1 : end;
	}
	if(len > 0)
	    pos = make_pos(VCOL(pos) + len, VROW(pos) - 1);
	else
	    pos = make_pos(VCOL(pos), VROW(pos) - 1);
	undo_record_insertion(tx, orig, pos);
	flag_insertion(tx, orig, pos);
    }
    return pos;

nomem:
    mem_error();
    return NULL;
}

bool
rect_delete(TX *tx, VALUE startPos, VALUE endPos)
{
    LINE *line = tx->tx_Lines + VROW(startPos);
    long linenum = VROW(startPos);
    while(linenum <= VROW(endPos))
    {
	long startCol = char_col(tx, VCOL(startPos), linenum);
	long endCol;
	if(line->ln_Strlen > startCol)
	{
	    long del_len, spc_len;
	    VALUE start, end;
	    long start_glyph = glyph_col(tx, startCol, linenum);
	    if((start_glyph != VCOL(startPos)) && (startCol > 0))
	    {
		startCol--;
		start_glyph = glyph_col(tx, startCol, linenum);
	    }
	    endCol = char_col(tx, VCOL(endPos), linenum);
	    if(endCol >= line->ln_Strlen)
		endCol = line->ln_Strlen - 1;
	    del_len = endCol - startCol;
	    spc_len = VCOL(startPos) - start_glyph;
	    start = make_pos(startCol, linenum);
	    end = make_pos(endCol, linenum);
	    undo_record_deletion(tx, start, end);
	    if(!delete_chars(tx, start, del_len))
		return FALSE;
	    flag_deletion(tx, start, end);

	    /* May need to insert some spaces to compensate for a deleted
	       TAB which crossed the start of the deletion.  */
	    if(spc_len > 0)
	    {
		if(!insert_gap(tx, spc_len, start))
		    return(FALSE);
		memset(line->ln_Line + startCol, ' ', spc_len);
		end = make_pos(startCol + spc_len, VROW(end));
		undo_record_insertion(tx, start, end);
		flag_insertion(tx, start, end);
	    }
	}
	linenum++;
	line++;
    }
    return TRUE;
}

/* Makes sure that the corners of a rectangle are how I want them. Converts
   from character positions to glyph positions.  */
void
order_rect(TX *tx, VALUE *start, VALUE *end)
{
    long start_col = glyph_col(tx, VCOL(*start), VROW(*start));
    long end_col = glyph_col(tx, VCOL(*end), VROW(*end));
    long start_row = VROW(start);
    long end_row = VROW(end);
    if(start_col <= end_col)
    {
	if(start_row <= end_row)
	{
	    /* all ok */
	}
	else
	{
	    *start = make_pos(start_col, end_row);
	    *end = make_pos(end_col, start_row);
	}
    }
    else
    {
	if(start_row < end_row)
	{
	    *start = make_pos(end_col, start_row);
	    *end = make_pos(start_col, end_row);
	}
	else
	{
	    VALUE tem = *start;
	    *start = *end;
	    *end = tem;
	}
    }
}

/* Copies a rectangle to a string, newlines are inserted between lines. */
VALUE
rect_copy(TX *tx, VALUE startPos, VALUE endPos)
{
    VALUE res;
    u_char *buf = str_alloc(128);
    long len = 0, buflen = 128;
    long linenum = VROW(startPos);
    LINE *line = tx->tx_Lines + linenum;
    long width = (VCOL(endPos) - VCOL(startPos)) + 1;
    if(!buf)
	return(mem_error());
    while(linenum <= VROW(endPos))
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
	len += expand_tabs(tx, buf + len, VCOL(startPos), VCOL(endPos),
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
DEFUN_INT("insert-rect", cmd_insert_rect, subr_insert_rect, (VALUE str, VALUE pos, VALUE buff), V_Subr3, DOC_insert_rect, "sString to insert rectangularly") /*
::doc:insert_rect::
insert-rect STRING [POS] [BUFFER]

Inserts STRING into BUFFER at POS treating it as a ``rectangle'' of
text -- that is, each separate line in STRING (separated by newlines) is
inserted at the *same* column in successive lines.

Returns the position of the character following the end of the inserted
text.
::end:: */
{
    DECLARE1(str, STRINGP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff)) && pad_pos(VTX(buff), pos))
    {
	VALUE res = rect_insert(VTX(buff), VSTR(str), STRING_LEN(str), pos);
	if(res != NULL)
	    return res;
    }
    return(sym_nil);
}

_PR VALUE cmd_delete_rect(VALUE start, VALUE end, VALUE buff);
DEFUN("delete-rect", cmd_delete_rect, subr_delete_rect, (VALUE start, VALUE end, VALUE buff), V_Subr3, DOC_delete_rect) /*
::doc:delete_rect::
delete-rect START-POS END-POS [BUFFER]

Deletes the rectangle of text from one corner, START-POS, to the opposite
corner, END-POS.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    order_rect(VTX(buff), &start, &end);
    if(!read_only(VTX(buff)) && check_line(VTX(buff), end))
    {
	rect_delete(VTX(buff), start, end);
	return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_copy_rect(VALUE start, VALUE end, VALUE buff);
DEFUN("copy-rect", cmd_copy_rect, subr_copy_rect, (VALUE start, VALUE end, VALUE buff), V_Subr3, DOC_copy_rect) /*
::doc:copy_rect::
copy-rect START-POS END-POS [BUFFER]

Returns the rectangle of text marked out by START-POS and END-POS.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    order_rect(VTX(buff), &start, &end);
    if(check_line(VTX(buff), end))
	return(rect_copy(VTX(buff), start, end));
    return(sym_nil);
}

_PR VALUE cmd_cut_rect(VALUE start, VALUE end, VALUE buff);
DEFUN("cut-rect", cmd_cut_rect, subr_cut_rect, (VALUE start, VALUE end, VALUE buff), V_Subr3, DOC_cut_rect) /*
::doc:cut_rect::
cut-rect START-POS END-POS [BUFFER]

The same as `copy-rect' except that the section of text copied (START-POS
to END-POS) is deleted from the file after being duplicated.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    order_rect(VTX(buff), &start, &end);
    if(!read_only(VTX(buff)) && check_line(VTX(buff), end))
    {
	VALUE str = rect_copy(VTX(buff), start, end);
	if(str)
	    rect_delete(VTX(buff), start, end);
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
