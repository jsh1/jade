/* edit.c -- Editing buffers
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

_PR bool clear_line_list(TX *);
_PR void kill_line_list(TX *);
_PR LINE *resize_line_list(TX *, long, long);
_PR bool insert_gap(TX *, long, const POS *);
_PR bool insert_str(TX *, const u_char *, POS *);
_PR bool insert_str_n(TX *, const u_char *, long, POS *);
_PR bool insert_string(TX *, const u_char *, long, POS *);
_PR bool delete_chars(TX *, const POS *, long);
_PR bool delete_lines(TX *, long, long);
_PR bool delete_section(TX *, POS *, POS *);
_PR bool split_line(TX *, POS *);
static bool join_lines(TX *, POS *);
_PR bool pad_pos(TX *, POS *);
_PR bool pad_cursor(VW *);
_PR void order_pos(POS *, POS *);
_PR bool check_section(TX *, POS *, POS *);
_PR void check_pos(TX *, POS *);
_PR bool check_line(TX *, POS *);
_PR long section_length(TX *, POS *, POS *);
_PR void copy_section(TX *, POS *, POS *, u_char *);
_PR bool pos_in_block(VW *, long, long);
_PR bool cursor_in_block(VW *);
_PR bool page_in_block(VW *);
_PR short line_in_block(VW *, long);
_PR void order_block(VW *);
_PR void set_block_refresh(VW *);
_PR bool read_only(TX *);

#define ALLOC_LL(n)   mymalloc(sizeof(LINE) * (n))
#define FREE_LL(l)    myfree(l)

/*
 * This copies a line list (or part of one).
 * d = destination
 * s = source
 * n = number of LINE's to copy.
 */
#define MOV_LL(d,s,n) memcpy((d), (s), (n) * sizeof(LINE))
/*
 * void
 * MOV_LL(LINE *dst, LINE *src, long number)
 * {
 *     memcpy(dst, src, number * sizeof(LINE));
 * }
 */

/*
 * Makes file empty (null string in first line)
 */
bool
clear_line_list(TX *tx)
{
    if(tx->tx_Lines)
	kill_line_list(tx);
    tx->tx_Lines = ALLOC_LL(1);
    if(tx->tx_Lines)
    {
	tx->tx_Lines[0].ln_Line = str_dupn("", 0);
	if(tx->tx_Lines[0].ln_Line)
	    tx->tx_Lines[0].ln_Strlen = 1;
	else
	    tx->tx_Lines[0].ln_Strlen = 0;
	tx->tx_NumLines = 1;
	tx->tx_LogicalStart = 0;
	tx->tx_LogicalEnd = 1;
	return(TRUE);
    }
    return(FALSE);
}

/*
 * deallocates all lines and their list
 */
void
kill_line_list(TX *tx)
{
    if(tx->tx_Lines)
    {
	LINE *line;
	long i;
	for(i = 0, line = tx->tx_Lines; i < tx->tx_NumLines; i++, line++)
	{
	    if(line->ln_Strlen)
		str_free(line->ln_Line);
	}
	FREE_LL(tx->tx_Lines);
	tx->tx_Lines = NULL;
	tx->tx_NumLines = 0;
    }
}

/*
 * deallocates some lines (but not the list)
 */
static void
kill_some_lines(TX *tx, long start, long number)
{
    LINE *line = tx->tx_Lines + start;
    long i;
    for(i = 0; i < number; i++, line++)
    {
	if(line->ln_Strlen)
	{
	    str_free(line->ln_Line);
	    line->ln_Strlen = 0;
	    line->ln_Line = NULL;
	}
    }
}

/*
 * Creates blank entries or removes existing lines starting from line 'where'
 * 'change' is the number of lines to insert, negative numbers mean delete
 * that number of lines starting at the cursor line.
 * If lines are deleted the actual text is also deleted.
 * NOTE: You can't have a line list of zero lines.
 */
LINE *
resize_line_list(TX *tx, long change, long where)
{
    LINE *newlines;
    long newsize = tx->tx_NumLines + change;
    if((newsize > 0) && (newlines = ALLOC_LL(newsize)))
    {
	if(tx->tx_Lines)
	{
	    if(change > 0)
	    {
		MOV_LL(newlines, tx->tx_Lines, where);
		MOV_LL(newlines + where + change, tx->tx_Lines + where,
		       tx->tx_NumLines - where);
	    }
	    else
	    {
		MOV_LL(newlines, tx->tx_Lines, where);
		MOV_LL(newlines + where, tx->tx_Lines + where - change,
		       tx->tx_NumLines - where + change);
		kill_some_lines(tx, where, -change);
	    }
	    FREE_LL(tx->tx_Lines);
	}
	if(change > 0)
	    memset(newlines + where, 0, change * sizeof(LINE));
	tx->tx_Lines = newlines;
	tx->tx_NumLines = newsize;
	return(newlines);
    }
    return(FALSE);
}

#if 0
/*
 * Pastes a line into the current view at line num.
 * a LINE should have been made if it is wanted.
 * text is not copied, it should have been mymalloc()'ed (or savestring()'ed)
 */
bool
stuffline(TX *tx, u_char *text, long lineNum)
{
    LINE *line = tx->tx_Lines + lineNum;
    if(line->ln_Strlen)
	str_free(line->ln_Line);
    line->ln_Strlen = strlen(text) + 1;
    line->ln_Line = text;
    return(TRUE);
}
#endif

/*
 * Inserts some `space' at pos. The gap will be filled with random garbage.
 * pos is *not* altered.
 */
bool
insert_gap(TX *tx, long len, const POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    u_char *newline = str_alloc(len + line->ln_Strlen);
    if(newline)
    {
	if(line->ln_Strlen)
	{
	    memcpy(newline, line->ln_Line, pos->pos_Col);
	    memcpy(newline + pos->pos_Col + len, line->ln_Line + pos->pos_Col,
		   line->ln_Strlen - pos->pos_Col);
	    str_free(line->ln_Line);
	}
	else
	    newline[len] = 0;
	line->ln_Line = newline;
	line->ln_Strlen += len;
	adjust_marks_add_x(tx, len, pos->pos_Col, pos->pos_Line);
	return(TRUE);
    }
    mem_error();
    return(FALSE);
}

/*
 * Inserts a string into the current line at the cursor pos
 *
 * IMPORTANT: For any of the next functions which insert text the pos
 * argument must not be one which will be fiddled with by the keeppos*()
 * functions (specifically a direct pointer to vw_CursorPos).
 */
bool
insert_str(TX *tx, const u_char *text, POS *pos)
{
    return(insert_str_n(tx, text, strlen(text), pos));
}

bool
insert_str_n(TX *tx, const u_char *text, long textLen, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(insert_gap(tx, textLen, pos))
    {
	memcpy(line->ln_Line + pos->pos_Col, text, textLen);
	pos->pos_Col += textLen;
	return(TRUE);
    }
    return(FALSE);
}

/*
 * Inserts a null teminated string, this routine acts on any '\n' characters
 * that it finds. I expect that this routine will be incredibly slow.
 */
bool
insert_string(TX *tx, const u_char *text, long textLen, POS *pos)
{
    const u_char *eol;
    POS orig = *pos;
    while((eol = memchr(text, '\n', textLen)))
    {
	long len = eol - text;
	if(pos->pos_Col)
	{
	    if(len > 0)
	    {
		if(!insert_str_n(tx, text, len, pos))
		    goto abort;
	    }
	    if(!split_line(tx, pos))
		goto abort;
	    pos->pos_Col = 0;
	}
	else
	{
	    u_char *copy;
	    if(!resize_line_list(tx, +1, pos->pos_Line))
		goto abort;
	    if(!(copy = str_dupn(text, len)))
		goto abort;
	    tx->tx_Lines[pos->pos_Line].ln_Strlen = len + 1;
	    tx->tx_Lines[pos->pos_Line].ln_Line = copy;
	    adjust_marks_add_y(tx, +1, pos->pos_Line);
	}
	textLen -= len + 1;
	text = eol + 1;
	pos->pos_Line++;
    }
    if(textLen > 0)
    {
	if(!insert_str_n(tx, text, textLen, pos))
	{
	abort:
	    return(FALSE);
	}
    }
    undo_record_insertion(tx, &orig, pos);
    flag_insertion(tx, &orig, pos);
    return(TRUE);
}

/*
 * Deletes some text (this line only)
 */
bool
delete_chars(TX *tx, const POS *pos, long size)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(line->ln_Strlen)
    {
	u_char *newline;
	if(size >= line->ln_Strlen)
	    size = line->ln_Strlen - 1;
	newline = str_alloc(line->ln_Strlen - size);
	if(newline)
	{
#if 1
            memcpy(newline, line->ln_Line, pos->pos_Col);
            memcpy(newline + pos->pos_Col, line->ln_Line + pos->pos_Col + size,
                   line->ln_Strlen - pos->pos_Col - size);
#else
	    strncpy(newline, line->ln_Line, pos->pos_Col);
	    strcpy(newline + pos->pos_Col, line->ln_Line + pos->pos_Col + size);
#endif
	    str_free(line->ln_Line);
	    line->ln_Strlen -= size;
	    line->ln_Line = newline;
	    adjust_marks_sub_x(tx, size, pos->pos_Col, pos->pos_Line);
	    return(TRUE);
	}
	mem_error();
	return(FALSE);
    }
    return(TRUE);
}

/*
 * deletes some lines
 */
bool
delete_lines(TX *tx, long linenum, long size)
{
    bool rc = FALSE;
    if(resize_line_list(tx, -size, linenum))
	rc = TRUE;
    adjust_marks_sub_y(tx, size, linenum);
    return(rc);
}

/*
 * Deletes from startPos to endPos
 */
bool
delete_section(TX *tx, POS *startPos, POS *endPos)
{
    undo_record_deletion(tx, startPos, endPos);
    if(endPos->pos_Line == startPos->pos_Line)
    {
	delete_chars(tx, startPos, endPos->pos_Col - startPos->pos_Col);
	flag_deletion(tx, startPos, endPos);
    }
    else
    {
	long middle;
	bool joinflag = FALSE;
	POS orig = *startPos, oend = *endPos;
	if(startPos->pos_Col)
	{
	    long start = tx->tx_Lines[startPos->pos_Line].ln_Strlen - startPos->pos_Col - 1;
	    if(start)
		delete_chars(tx, startPos, start);
	    startPos->pos_Col = 0;
	    startPos->pos_Line++;
	    joinflag = TRUE;
	}
	middle = endPos->pos_Line - startPos->pos_Line;
	if(middle != 0)
	{
	    delete_lines(tx, startPos->pos_Line, middle);
	    endPos->pos_Line -= middle;
	}
	if(endPos->pos_Col)
	    delete_chars(tx, startPos, endPos->pos_Col);
	if(joinflag && startPos->pos_Line)
	{
	    startPos->pos_Line--;
	    startPos->pos_Col = tx->tx_Lines[startPos->pos_Line].ln_Strlen - 1;
	    join_lines(tx, startPos);
	}
	flag_deletion(tx, &orig, &oend);
    }
    return(TRUE);
}

/*
 * splits the line into two
 */
bool
split_line(TX *tx, POS *pos)
{
    if(resize_line_list(tx, +1, pos->pos_Line + 1))
    {
	LINE *line = tx->tx_Lines + pos->pos_Line;
	u_char *newline1 = str_dupn(line->ln_Line, pos->pos_Col);
	if(newline1)
	{
	    long nl2len = line->ln_Strlen - pos->pos_Col;
	    u_char *newline2 = str_dupn(line->ln_Line + pos->pos_Col,
					 nl2len - 1);
	    if(newline2)
	    {
		if(line[0].ln_Line)
		    str_free(line[0].ln_Line);
		line[0].ln_Strlen = pos->pos_Col + 1;
		line[0].ln_Line = newline1;
		if(line[1].ln_Line)
		    str_free(line[1].ln_Line);
		line[1].ln_Strlen = nl2len;
		line[1].ln_Line = newline2;
		adjust_marks_split_y(tx, pos->pos_Col, pos->pos_Line);
		return(TRUE);
	    }
	    str_free(newline1);
	}
    }
    mem_error();
    return(FALSE);
}

/*
 * joins the current line to the line below it.
 */
static bool
join_lines(TX *tx, POS *pos)
{
    if((pos->pos_Line + 1) < tx->tx_LogicalEnd)
    {
	LINE *line1 = tx->tx_Lines + pos->pos_Line;
	LINE *line2 = line1 + 1;
	int newlen = line1->ln_Strlen + line2->ln_Strlen - 1;
	u_char *newstr = str_alloc(newlen);
	if(newstr)
	{
            memcpy(newstr, line1->ln_Line, line1->ln_Strlen - 1);
            memcpy(newstr + (line1->ln_Strlen - 1), line2->ln_Line,
		   line2->ln_Strlen);
	    resize_line_list(tx, -1, pos->pos_Line);
	    line1 = tx->tx_Lines + pos->pos_Line;
	    if(line1->ln_Line)
		str_free(line1->ln_Line);
	    line1->ln_Strlen = newlen;
	    line1->ln_Line = newstr;
	    adjust_marks_join_y(tx, pos->pos_Col, pos->pos_Line);
	    return(TRUE);
	}
	mem_error();
    }
    return(FALSE);
}

/*
 * Inserts spaces from end of line to pos
 */
bool
pad_pos(TX *tx, POS *pos)
{
    if(pos->pos_Line < tx->tx_LogicalEnd)
    {
	LINE *line = tx->tx_Lines + pos->pos_Line;
	if(line->ln_Strlen < (pos->pos_Col + 1))
	{
	    u_char *newline = str_alloc(pos->pos_Col + 1);
	    if(newline)
	    {
		long i;
		POS start;
		start.pos_Line = pos->pos_Line;
		start.pos_Col = line->ln_Strlen - 1;
		undo_record_insertion(tx, &start, pos);
		/* No need to call flag_insertion() since there's nothing
		   that needs redrawing. */
                memcpy(newline, line->ln_Line, line->ln_Strlen - 1);
		for(i = line->ln_Strlen - 1; i < pos->pos_Col; i++)
		    newline[i] = ' ';
		newline[i] = 0;
		if(line->ln_Line)
		    str_free(line->ln_Line);
		line->ln_Strlen = pos->pos_Col + 1;
		line->ln_Line = newline;
		return(TRUE);
	    }
	    mem_error();
	    pos->pos_Col = line->ln_Strlen - 1;
	    return(FALSE);
	}
	return(TRUE);
    }
    return(FALSE);
}

bool
pad_cursor(VW *vw)
{
    return(pad_pos(vw->vw_Tx, &vw->vw_CursorPos));
}

/*
 * if end is before start then swap the two
 */
void
order_pos(POS *start, POS *end)
{
    if(((start->pos_Line == end->pos_Line) && (start->pos_Col > end->pos_Col))
       || (start->pos_Line > end->pos_Line))
    {
	POS temp;
	temp = *start;
	*start = *end;
	*end = temp;
    }
}

bool
check_section(TX *tx, POS *start, POS *end)
{
    order_pos(start, end);
    if((start->pos_Line >= tx->tx_LogicalEnd)
       || (end->pos_Line >= tx->tx_LogicalEnd)
       || (start->pos_Line < tx->tx_LogicalStart)
       || (end->pos_Line < tx->tx_LogicalStart))
	return(FALSE);
    if(start->pos_Col >= tx->tx_Lines[start->pos_Line].ln_Strlen)
	start->pos_Col = tx->tx_Lines[start->pos_Line].ln_Strlen - 1;
    if(end->pos_Col >= tx->tx_Lines[end->pos_Line].ln_Strlen)
	end->pos_Col = tx->tx_Lines[end->pos_Line].ln_Strlen - 1;
    return(TRUE);
}

void
check_pos(TX *tx, POS *pos)
{
    if(pos->pos_Line >= tx->tx_LogicalEnd)
	pos->pos_Line = tx->tx_LogicalEnd - 1;
    else if(pos->pos_Line < tx->tx_LogicalStart)
	pos->pos_Line = tx->tx_LogicalStart;
    if(pos->pos_Col >= tx->tx_Lines[pos->pos_Line].ln_Strlen)
	pos->pos_Col = tx->tx_Lines[pos->pos_Line].ln_Strlen - 1;
}

bool
check_line(TX *tx, POS *pos)
{
    if((pos->pos_Line >= tx->tx_LogicalEnd)
       || (pos->pos_Line < tx->tx_LogicalStart)
       || (pos->pos_Col < 0))
	return(FALSE);
    return(TRUE);
}

/*
 * Returns the number of bytes needed to store a section, doesn't include
 * a zero terminator but does include all newline chars.
 */
long
section_length(TX *tx, POS *startPos, POS *endPos)
{
    long linenum = startPos->pos_Line;
    LINE *line = tx->tx_Lines + linenum;
    long length;
    if(startPos->pos_Line == endPos->pos_Line)
	length = endPos->pos_Col - startPos->pos_Col;
    else
    {
	length = line->ln_Strlen - startPos->pos_Col;
	linenum++;
	line++;
	while(linenum < endPos->pos_Line)
	{
	    length += line->ln_Strlen;
	    linenum++;
	    line++;
	}
	length += endPos->pos_Col;
    }
    return(length);
}

/*
 * Copies a section to a buffer.
 * end of copy does NOT have a zero appended to it.
 */
void
copy_section(TX *tx, POS *startPos, POS *endPos, u_char *buff)
{
    long linenum = startPos->pos_Line;
    LINE *line = tx->tx_Lines + linenum;
    long copylen;
    if(startPos->pos_Line == endPos->pos_Line)
    {
	copylen = endPos->pos_Col - startPos->pos_Col;
	memcpy(buff, line->ln_Line + startPos->pos_Col, copylen);
	buff[copylen] = 0;
    }
    else
    {
	copylen = line->ln_Strlen - startPos->pos_Col - 1;
	memcpy(buff, line->ln_Line + startPos->pos_Col, copylen);
	buff[copylen] = '\n';
	buff += copylen + 1;
	linenum++;
	line++;
	while(linenum < endPos->pos_Line)
	{
	    copylen = line->ln_Strlen - 1;
	    memcpy(buff, line->ln_Line, copylen);
	    buff[copylen] = '\n';
	    buff += copylen + 1;
	    linenum++;
	    line++;
	}
	memcpy(buff, line->ln_Line, endPos->pos_Col);
    }
}

/*
 * returns TRUE if the specified position is inside a block
 */
bool
pos_in_block(VW *vw, long col, long line)
{
    if((vw->vw_BlockStatus)
       || (line < vw->vw_BlockS.pos_Line)
       || (line > vw->vw_BlockE.pos_Line))
	return(FALSE);
    if(vw->vw_Flags & VWFF_RECTBLOCKS)
    {
	long start_col = glyph_col(vw->vw_Tx, vw->vw_BlockS.pos_Col,
				   vw->vw_BlockS.pos_Line);

	long end_col = glyph_col(vw->vw_Tx, vw->vw_BlockE.pos_Col,
				 vw->vw_BlockE.pos_Line);
	col = glyph_col(vw->vw_Tx, col, line);
	if(start_col < end_col)
	{
	    if((col < start_col) || (col >= end_col))
		return(FALSE);
	}
	else
	{
	    if((col < end_col) || (col >= start_col))
		return(FALSE);
	}
    }
    else
    {
	if(((line == vw->vw_BlockS.pos_Line) && (col < vw->vw_BlockS.pos_Col))
	  || ((line == vw->vw_BlockE.pos_Line) && (col >= vw->vw_BlockE.pos_Col)))
	return(FALSE);
    }
    return(TRUE);
}

bool
cursor_in_block(VW *vw)
{
    return(pos_in_block(vw, vw->vw_CursorPos.pos_Col, vw->vw_CursorPos.pos_Line));
}


bool
page_in_block(VW *vw)
{
    if((vw->vw_BlockStatus)
      || ((vw->vw_BlockE.pos_Line + 1) < vw->vw_StartLine)
      || (vw->vw_BlockS.pos_Line > vw->vw_StartLine + vw->vw_MaxY))
	return(FALSE);
    return(TRUE);
}

/*
 * these returns,
 *
 *	0   line not in block
 *	1   whole line in block
 *	2   start of line in block
 *	3   end of line in block
 *	4   middle of line in block
 *
 * note:
 *  this isn't very intelligent (but it works :-).
 *
 * now handles rectangular blocks (VWFF_RECTBLOCKS)
 */
short
line_in_block(VW *vw, long line)
{
    bool startin = FALSE;
    bool endin = FALSE;

    if((vw->vw_BlockStatus)
      || (line < vw->vw_BlockS.pos_Line)
      || (line > vw->vw_BlockE.pos_Line))
	return(0);
    if(vw->vw_Flags & VWFF_RECTBLOCKS)
	return(4);
    if(line == vw->vw_BlockE.pos_Line)
	startin = TRUE;
    if(line == vw->vw_BlockS.pos_Line)
	endin = TRUE;
    if(startin)
    {
	if(endin)
	    return(4);
	return(2);
    }
    if(endin)
	return(3);
    return(1);
}

/*
 * makes sure that the marked block is valid
 */
void
order_block(VW *vw)
{
    if(!vw->vw_BlockStatus)
    {
	if(vw->vw_BlockS.pos_Line > vw->vw_BlockE.pos_Line)
	{
	    POS temp = vw->vw_BlockE;
	    vw->vw_BlockE = vw->vw_BlockS;
	    vw->vw_BlockS = temp;
	}
	else if(vw->vw_BlockS.pos_Line == vw->vw_BlockE.pos_Line)
	{
	    if(vw->vw_BlockS.pos_Col > vw->vw_BlockE.pos_Col)
	    {
		POS temp = vw->vw_BlockE;
		vw->vw_BlockE = vw->vw_BlockS;
		vw->vw_BlockS = temp;
	    }
	}
    }
}

/*
 * Set up the refresh flags to refresh the block in the most efficient manner.
 */
void
set_block_refresh(VW *vw)
{
    long endline = vw->vw_StartLine + vw->vw_MaxY;
    if((vw->vw_BlockS.pos_Line > endline)
       || (vw->vw_BlockE.pos_Line < vw->vw_StartLine))
	return;
    if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
	/*
	 * If the all-powerful bit is already set I have no way of telling
	 * myself to refresh two blocked areas, stupid huh? Anyway I just
	 * blast in the whole window instead.
	 */
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
    else
	vw->vw_Flags |= VWFF_REFRESH_BLOCK;
}

bool
read_only(TX *tx)
{
    if(tx->tx_Flags & TXFF_RDONLY)
    {
	VALUE tmp = cmd_symbol_value(sym_inhibit_read_only, sym_t);
	if(VOIDP(tmp) || NILP(tmp))
	{
	    cmd_signal(sym_buffer_read_only, LIST_1(VAL(tx)));
	    return(TRUE);
	}
    }
    return(FALSE);
}
