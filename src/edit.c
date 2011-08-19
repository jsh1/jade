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
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

/* The maximum number of unused line entries at the end of the
   array of lines in each buffer. 128 == 1k. */
#define MAX_SPARE_LINES 128

/*  When allocating new line arrays allocate this many extra lines,
    in case it grows. */
#define ALLOC_SPARE_LINES 32

/* Strings stored in LINEs are allocated using stringmem.c, this means
   that small strings are always allocated in chunks of eight bytes.
   So it makes sense to allocate lines in multiples of this number;
   allowing many unnecessary re-allocations to be avoided. */

/* For a piece of memory of size X, this is the number of bytes we'll
   actually ask to be allocated. */
#define LINE_BUF_SIZE(x) ROUND_UP_INT(x, 8)

/* Allocate a chunk of memory to store a string of size X (including
   terminating zero). */
#define ALLOC_LINE_BUF(tx, x) rep_alloc(LINE_BUF_SIZE(x))

/* Free something allocated with the previous macro. */
#define FREE_LINE_BUF(tx, p)  rep_free(p)

/* Makes buffer TX empty (null string in first line) */
bool
clear_line_list(TX *tx)
{
    if(tx->tx_Lines)
	kill_line_list(tx);
    tx->tx_Lines = rep_alloc(sizeof(LINE) * ALLOC_SPARE_LINES);
    if(tx->tx_Lines)
    {
	tx->tx_Lines[0].ln_Line = ALLOC_LINE_BUF(tx, 1);
	if(tx->tx_Lines[0].ln_Line)
	{
	    tx->tx_Lines[0].ln_Line[0] = 0;
	    tx->tx_Lines[0].ln_Strlen = 1;
	}
	else
	    tx->tx_Lines[0].ln_Strlen = 0;
	tx->tx_NumLines = 1;
	tx->tx_TotalLines = ALLOC_SPARE_LINES;
	tx->tx_LogicalStart = 0;
	tx->tx_LogicalEnd = 1;
	return(TRUE);
    }
    return(FALSE);
}

/* deallocates all lines and their list */
void
kill_line_list(TX *tx)
{
    if(tx->tx_Lines)
    {
	long i;
	for(i = 0; i < tx->tx_NumLines; i++)
	{
	    if(tx->tx_Lines[i].ln_Strlen)
		FREE_LINE_BUF(tx, tx->tx_Lines[i].ln_Line);
	}
	rep_free(tx->tx_Lines);
	tx->tx_Lines = 0;
	tx->tx_NumLines = 0;
	tx->tx_TotalLines = 0;
    }
}

/* deallocates some lines (but not the list) */
static void
kill_some_lines(TX *tx, long start, long number)
{
    long i;
    for(i = start; i < number + start; i++)
    {
	if(tx->tx_Lines[i].ln_Strlen)
	{
	    FREE_LINE_BUF(tx, tx->tx_Lines[i].ln_Line);
	    tx->tx_Lines[i].ln_Strlen = 0;
	    tx->tx_Lines[i].ln_Line = NULL;
	}
    }
}

/* Creates blank entries or removes existing lines starting from line
   WHERE. CHANGE is the number of lines to insert, negative numbers mean
   delete that number of lines starting at the cursor line. If lines are
   deleted the actual text is also deleted.
   NOTE: A line list of zero lines is not allowed. */
LINE *
resize_line_list(TX *tx, long change, long where)
{
    long newsize = tx->tx_NumLines + change;
    if(newsize <= 0)
	return NULL;
    if(change < 0)
    {
	assert(tx->tx_Lines != 0);
	kill_some_lines(tx, where, -change);
	memmove(tx->tx_Lines + where,
		tx->tx_Lines + where - change,
		(tx->tx_NumLines - where + change) * sizeof(LINE));
    }
    if(tx->tx_Lines == 0
       || newsize > tx->tx_TotalLines
       || (tx->tx_TotalLines - newsize) > MAX_SPARE_LINES)
    {
	/* Only reallocate if there's not enough space in the array */
	long actual_size = newsize + ALLOC_SPARE_LINES;
	if(tx->tx_Lines != 0)
	{
	    LINE *tem = rep_realloc(tx->tx_Lines, sizeof(LINE) * actual_size);
	    if (tem != 0)
		tx->tx_Lines = tem;
	    else
	    {
		rep_free(tx->tx_Lines);
		tx->tx_Lines = 0;
	    }
	}
	else
	    tx->tx_Lines = rep_alloc(sizeof(LINE) * actual_size);
	if(tx->tx_Lines == 0)
	    return 0;
	tx->tx_TotalLines = actual_size;
    }
    if(change > 0)
    {
	memmove(tx->tx_Lines + where + change,
		tx->tx_Lines + where,
		(tx->tx_NumLines - where) * sizeof(LINE));
	memset(tx->tx_Lines + where, 0, sizeof(LINE) * change);
    }
    tx->tx_NumLines = newsize;
    return tx->tx_Lines;
}

char *
alloc_line_buf(TX *tx, long length)
{
    return ALLOC_LINE_BUF(tx, length);
}

void
free_line_buf(TX *tx, char *line)
{
    FREE_LINE_BUF(tx, line);
}

/* Inserts LEN characters of `space' at pos. The gap will be filled
   with random garbage. */
bool
insert_gap(TX *tx, long len, long col, long row)
{
    long new_length = tx->tx_Lines[row].ln_Strlen + len;
    if(LINE_BUF_SIZE(new_length) == LINE_BUF_SIZE(tx->tx_Lines[row].ln_Strlen))
    {
	/* Absorb the insertion in the current buffer */
	memmove(tx->tx_Lines[row].ln_Line + col + len,
		tx->tx_Lines[row].ln_Line + col,
		tx->tx_Lines[row].ln_Strlen - col);
    }
    else
    {
	/* Need to allocate a new buffer */
	char *newline = ALLOC_LINE_BUF(tx, new_length);
	if(newline != NULL)
	{
	    if(tx->tx_Lines[row].ln_Strlen != 0)
	    {
		memcpy(newline, tx->tx_Lines[row].ln_Line, col);
		memcpy(newline + col + len, tx->tx_Lines[row].ln_Line + col,
		       tx->tx_Lines[row].ln_Strlen - col);
		FREE_LINE_BUF(tx, tx->tx_Lines[row].ln_Line);
	    }
	    else
		newline[len] = 0;
	    tx->tx_Lines[row].ln_Line = newline;
	}
	else
	{
	    rep_mem_error();
	    return FALSE;
	}
    }
    tx->tx_Lines[row].ln_Strlen += len;
    adjust_marks_add_x(tx, len, col, row);
    return TRUE;
}

/* Inserts a piece of memory into the current line at POS.
   No line-breaking is performed, the TEXTLEN bytes of TEXT are simply
   inserted into the current line. Returns the position of the character
   after the end of the inserted text. */
repv
insert_bytes(TX *tx, const char *text, long textLen, repv pos)
{
    if(insert_gap(tx, textLen, VCOL(pos), VROW(pos)))
    {
	memcpy(tx->tx_Lines[VROW(pos)].ln_Line + VCOL(pos), text, textLen);
	return make_pos(VCOL(pos) + textLen, VROW(pos));
    }
    else
	return rep_NULL;
}

/* Inserts a string, this routine acts on any '\n' characters that it
   finds. */
repv
insert_string(TX *tx, const char *text, long textLen, repv pos)
{
    const char *eol;
    Pos tpos;
    COPY_VPOS(&tpos, pos);
    while((eol = memchr(text, '\n', textLen)))
    {
	long len = eol - text;
	if(PCOL(&tpos) != 0)
	{
	    if(len > 0)
	    {
		if(insert_gap(tx, len, PCOL(&tpos), PROW(&tpos)))
		{
		    memcpy(tx->tx_Lines[PROW(&tpos)].ln_Line + PCOL(&tpos),
			   text, len);
		    PCOL(&tpos) += len;
		}
		else
		    goto abort;
	    }
	    /* Split line at TPOS */
	    if(resize_line_list(tx, +1, PROW(&tpos) + 1))
	    {
		long row = PROW(&tpos);

		/* First do the new line */
		tx->tx_Lines[row+1].ln_Line
		    = ALLOC_LINE_BUF(tx, tx->tx_Lines[row].ln_Strlen
				     - PCOL(&tpos));
		if(tx->tx_Lines[row+1].ln_Line != NULL)
		{
		    tx->tx_Lines[row+1].ln_Strlen
			= tx->tx_Lines[row].ln_Strlen - PCOL(&tpos);
		    memcpy(tx->tx_Lines[row+1].ln_Line,
			   tx->tx_Lines[row].ln_Line + PCOL(&tpos),
			   tx->tx_Lines[row+1].ln_Strlen - 1);
		    tx->tx_Lines[row+1].ln_Line[tx->tx_Lines[row+1].ln_Strlen - 1] = 0;
		}
		else
		    goto abort;

		/* Then chop the end off the old one */
		if(LINE_BUF_SIZE(PCOL(&tpos) + 1)
		   == LINE_BUF_SIZE(tx->tx_Lines[row].ln_Strlen))
		{
		    /* Use the old buffer */
		    tx->tx_Lines[row].ln_Strlen = PCOL(&tpos) + 1;
		    tx->tx_Lines[row].ln_Line[tx->tx_Lines[row].ln_Strlen - 1] = 0;
		}
		else
		{
		    /* Allocate a new buffer */
		    char *new = ALLOC_LINE_BUF(tx, PCOL(&tpos) + 1);
		    if(new != NULL)
		    {
			memcpy(new, tx->tx_Lines[row].ln_Line, PCOL(&tpos));
			new[PCOL(&tpos)] = 0;
			FREE_LINE_BUF(tx, tx->tx_Lines[row].ln_Line);
			tx->tx_Lines[row].ln_Line = new;
			tx->tx_Lines[row].ln_Strlen = PCOL(&tpos) + 1;
		    }
		    else
			goto abort;
		}
		adjust_marks_split_y(tx, PCOL(&tpos), PROW(&tpos));
		PCOL(&tpos) = 0;
		PROW(&tpos)++;
	    }
	    else
		goto abort;
	}
	else
	{
	    char *copy;
	    if(!resize_line_list(tx, +1, PROW(&tpos)))
		goto abort;
	    copy = ALLOC_LINE_BUF(tx, len + 1);
	    if(copy == NULL)
		goto abort;
	    memcpy(copy, text, len);
	    copy[len] = 0;
	    tx->tx_Lines[PROW(&tpos)].ln_Strlen = len + 1;
	    tx->tx_Lines[PROW(&tpos)].ln_Line = copy;
	    adjust_marks_add_y(tx, +1, PROW(&tpos));
	    PROW(&tpos)++;
	}
	textLen -= len + 1;
	text = eol + 1;
    }
    if(textLen > 0)
    {
	if(insert_gap(tx, textLen, PCOL(&tpos), PROW(&tpos)))
	{
	    memcpy(tx->tx_Lines[PROW(&tpos)].ln_Line + PCOL(&tpos),
		   text, textLen);
	    PCOL(&tpos) += textLen;
	}
	else
	abort:
	    return rep_NULL;
    }

    {
	repv end = make_pos(PCOL(&tpos), PROW(&tpos));
	undo_record_insertion(tx, pos, end);
	flag_insertion(tx, pos, end);
	return end;
    }
}

/* Deletes some SIZE bytes from line at (COL,ROW). Returns true if okay.
   This won't delete past the end of the line at (COL,ROW). */
bool
delete_chars(TX *tx, long col, long row, long size)
{
    if(tx->tx_Lines[row].ln_Strlen)
    {
	long new_length;
	if(size >= tx->tx_Lines[row].ln_Strlen - col)
	    size = tx->tx_Lines[row].ln_Strlen - col - 1;
	if(size <= 0)
	    return FALSE;
	new_length = tx->tx_Lines[row].ln_Strlen - size;
	if(LINE_BUF_SIZE(new_length)
	   == LINE_BUF_SIZE(tx->tx_Lines[row].ln_Strlen))
	{
	    /* Absorb the deletion */
	    memmove(tx->tx_Lines[row].ln_Line + col,
		    tx->tx_Lines[row].ln_Line + col + size,
		    tx->tx_Lines[row].ln_Strlen - (col + size));
	}
	else
	{
	    /* Allocate a new line */
	    char *new_line = ALLOC_LINE_BUF(tx, new_length);
	    if(new_line == NULL)
	    {
		rep_mem_error();
		return FALSE;
	    }
            memcpy(new_line, tx->tx_Lines[row].ln_Line, col);
            memcpy(new_line + col, tx->tx_Lines[row].ln_Line + col + size,
		   tx->tx_Lines[row].ln_Strlen - col - size);
	    FREE_LINE_BUF(tx, tx->tx_Lines[row].ln_Line);
	    tx->tx_Lines[row].ln_Line = new_line;
	}
	tx->tx_Lines[row].ln_Strlen -= size;
	adjust_marks_sub_x(tx, size, col, row);
	return TRUE;
    }
    return FALSE;
}

/* Deletes from START to END; returns END if okay. */
repv
delete_section(TX *tx, repv start, repv end)
{
    undo_record_deletion(tx, start, end);
    if(VROW(end) == VROW(start))
    {
	delete_chars(tx, VCOL(start), VROW(start),
		     VCOL(end) - VCOL(start));
	flag_deletion(tx, start, end);
    }
    else
    {
	long middle_lines;
	bool joinflag = FALSE;
	Pos tstart, tend;
	COPY_VPOS(&tstart, start); COPY_VPOS(&tend, end);
	if(PCOL(&tstart) != 0)
	{
	    long start_col = (tx->tx_Lines[PROW(&tstart)].ln_Strlen
			      - PCOL(&tstart) - 1);
	    if(start_col != 0)
		delete_chars(tx, PCOL(&tstart), PROW(&tstart), start_col);
	    PCOL(&tstart) = 0;
	    PROW(&tstart)++;
	    joinflag = TRUE;
	}
	middle_lines = PROW(&tend) - PROW(&tstart);
	if(middle_lines != 0)
	{
	    if(!resize_line_list(tx, -middle_lines, PROW(&tstart)))
		rep_mem_error();
	    adjust_marks_sub_y(tx, middle_lines, PROW(&tstart));
	    PROW(&tend) = PROW(&tend) - middle_lines;
	}
	if(PCOL(&tend) != 0)
	    delete_chars(tx, PCOL(&tstart), PROW(&tstart), PCOL(&tend));
	if(joinflag && PROW(&tstart) != 0)
	{
	    PROW(&tstart)--;
	    PCOL(&tstart) = tx->tx_Lines[PROW(&tstart)].ln_Strlen - 1;

	    /* Join the two lines at TSTART */
	    if((PROW(&tstart) + 1) < tx->tx_LogicalEnd)
	    {
		long row = PROW(&tstart);

		if(tx->tx_Lines[row].ln_Strlen == 1
		   || tx->tx_Lines[row+1].ln_Strlen == 1)
		{
		    /* One (or both) of the lines being joined is
		       empty; so just use the other line */
		    if(tx->tx_Lines[row+1].ln_Strlen == 1)
		    {
			char *tem = tx->tx_Lines[row].ln_Line;
			tx->tx_Lines[row].ln_Line = tx->tx_Lines[row+1].ln_Line;
			tx->tx_Lines[row+1].ln_Line = tem;
			tx->tx_Lines[row+1].ln_Strlen = tx->tx_Lines[row].ln_Strlen;
			tx->tx_Lines[row].ln_Strlen = 1;
		    }
		}
		else
		{
		    /* Allocate a new line;
		       TODO: see if the join can be absorbed into one
		       of the existing lines.. */
		    int new_length = (tx->tx_Lines[row].ln_Strlen
				      + tx->tx_Lines[row+1].ln_Strlen - 1);
		    char *new_line = ALLOC_LINE_BUF(tx, new_length);
		    if(new_line == NULL)
		    {
			rep_mem_error();
			return rep_NULL;
		    }
		    memcpy(new_line, tx->tx_Lines[row].ln_Line,
			   tx->tx_Lines[row].ln_Strlen - 1);
		    memcpy(new_line + (tx->tx_Lines[row].ln_Strlen - 1),
			   tx->tx_Lines[row+1].ln_Line,
			   tx->tx_Lines[row+1].ln_Strlen);
		    FREE_LINE_BUF(tx, tx->tx_Lines[row+1].ln_Line);
		    tx->tx_Lines[row+1].ln_Line = new_line;
		    tx->tx_Lines[row+1].ln_Strlen = new_length;
		}
		resize_line_list(tx, -1, PROW(&tstart));
		adjust_marks_join_y(tx, PCOL(&tstart), PROW(&tstart));
	    }
	    else
		abort();		/* shouldn't happen :-) */
	}
	flag_deletion(tx, start, end);
    }
    return end;
}

/* Inserts spaces from end of line to pos */
bool
pad_pos(TX *tx, repv pos)
{
    if(VROW(pos) < tx->tx_LogicalEnd && !read_only_pos(tx, pos))
    {
	if(tx->tx_Lines[VROW(pos)].ln_Strlen < (VCOL(pos) + 1))
	{
	    repv point = make_pos(tx->tx_Lines[VROW(pos)].ln_Strlen - 1,
				   VROW(pos));
	    if(insert_gap(tx, VCOL(pos) - VCOL(point),
			  VCOL(point), VROW(point)))
	    {
		undo_record_insertion(tx, point, pos);
		memset(tx->tx_Lines[VROW(pos)].ln_Line + VCOL(point), ' ',
		       VCOL(pos) - VCOL(point));
		return TRUE;
	    }
	    rep_mem_error();
	    return FALSE;
	}
	return TRUE;
    }
    return FALSE;
}

bool
pad_cursor(VW *vw)
{
    repv old_cursor = vw->vw_CursorPos;
    if(pad_pos(vw->vw_Tx, vw->vw_CursorPos))
    {
	/* Need to reinstall the old cursor position, since it
	   may have been changed by the insertion of spaces
	   before it. */
	vw->vw_CursorPos = old_cursor;
	return TRUE;
    }
    else
	return FALSE;
}

/* if end is before start then swap the two */
void
order_pos(repv *start, repv *end)
{
    if(POS_GREATER_P(*start, *end))
    {
	repv tem = *end;
	*end = *start;
	*start = tem;
    }
}

bool
check_section(TX *tx, repv *start, repv *end)
{
    order_pos(start, end);
    if((VROW(*start) >= tx->tx_LogicalEnd)
       || (VROW(*end) >= tx->tx_LogicalEnd)
       || (VROW(*start) < tx->tx_LogicalStart)
       || (VROW(*end) < tx->tx_LogicalStart))
    {
	Fsignal(Qinvalid_area, rep_list_3(rep_VAL(tx), *start, *end));
	return(FALSE);
    }
    if(VCOL(*start) >= tx->tx_Lines[VROW(*start)].ln_Strlen)
	*start = make_pos(tx->tx_Lines[VROW(*start)].ln_Strlen - 1,
			  VROW(*start));
    if(VCOL(*end) >= tx->tx_Lines[VROW(*end)].ln_Strlen)
	*end = make_pos(tx->tx_Lines[VROW(*end)].ln_Strlen - 1, VROW(*end));
    return TRUE;
}

/* Check that POSITION is in the current restriction of buffer TX.
   If not an error is signalled and the function returns null. If the
   column specified by POSITION is past the end of its line, the value
   returned will be the position of the end of the line, otherwise
   POSITION is returned. */
repv
check_pos(TX *tx, repv pos)
{
    if(VROW(pos) >= tx->tx_LogicalEnd
       || VROW(pos) < tx->tx_LogicalStart)
    {
	Fsignal(Qinvalid_pos, rep_list_2(rep_VAL(tx), pos));
	return rep_NULL;
    }
    if(VCOL(pos) >= tx->tx_Lines[VROW(pos)].ln_Strlen)
	pos = make_pos(tx->tx_Lines[VROW(pos)].ln_Strlen - 1, VROW(pos));
    return pos;
}

/* Check that POSITION is in the current restriction of buffer TX.
   If not an error is signalled and the function returns false. */
bool
check_line(TX *tx, repv pos)
{
    if((VROW(pos) >= tx->tx_LogicalEnd)
       || (VROW(pos) < tx->tx_LogicalStart)
       || (VCOL(pos) < 0))
    {
	Fsignal(Qinvalid_pos, rep_list_2(rep_VAL(tx), pos));
	return FALSE;
    }
    return TRUE;
}

/* Check that row LINE is in the current restriction of buffer TX.
   If not an error is signalled and the function returns false. */
bool
check_row(TX *tx, long line)
{
    if(line >= tx->tx_LogicalEnd || line < tx->tx_LogicalStart)
    {
	Fsignal(Qinvalid_pos, rep_list_2(rep_VAL(tx), make_pos(0, line)));
	return FALSE;
    }
    else
	return TRUE;
}

/* Returns the number of bytes needed to store a section, doesn't include
   a zero terminator but does include all newline chars. */
long
section_length(TX *tx, repv startPos, repv endPos)
{
    long linenum = VROW(startPos);
    long length;
    if(VROW(startPos) == VROW(endPos))
	length = VCOL(endPos) - VCOL(startPos);
    else
    {
	length = tx->tx_Lines[linenum++].ln_Strlen - VCOL(startPos);
	while(linenum < VROW(endPos))
	    length += tx->tx_Lines[linenum++].ln_Strlen;
	length += VCOL(endPos);
    }
    return length;
}

/* Copies a section to a buffer.
   End of copy does NOT have a zero appended to it. */
void
copy_section(TX *tx, repv startPos, repv endPos, char *buff)
{
    long linenum = VROW(startPos);
    long copylen;
    if(VROW(startPos) == VROW(endPos))
    {
	copylen = VCOL(endPos) - VCOL(startPos);
	memcpy(buff, tx->tx_Lines[linenum].ln_Line + VCOL(startPos), copylen);
	buff[copylen] = 0;
    }
    else
    {
	copylen = tx->tx_Lines[linenum].ln_Strlen - VCOL(startPos) - 1;
	memcpy(buff, tx->tx_Lines[linenum].ln_Line + VCOL(startPos), copylen);
	buff[copylen] = '\n';
	buff += copylen + 1;
	linenum++;
	while(linenum < VROW(endPos))
	{
	    copylen = tx->tx_Lines[linenum].ln_Strlen - 1;
	    memcpy(buff, tx->tx_Lines[linenum].ln_Line, copylen);
	    buff[copylen] = '\n';
	    buff += copylen + 1;
	    linenum++;
	}
	memcpy(buff, tx->tx_Lines[linenum].ln_Line, VCOL(endPos));
    }
}

/* Ensures that the marked block is valid */
void
order_block(VW *vw)
{
    if(!vw->vw_BlockStatus)
    {
	if(VROW(vw->vw_BlockS) > VROW(vw->vw_BlockE)
	   || (VROW(vw->vw_BlockS) == VROW(vw->vw_BlockE)
	       && VCOL(vw->vw_BlockS) > VCOL(vw->vw_BlockE)))
	{
	    repv tem = vw->vw_BlockE;
	    vw->vw_BlockE = vw->vw_BlockS;
	    vw->vw_BlockS = tem;
	}
    }
}

/* Returns TRUE and signals an error if buffer TX is currently read-only,
   otherwise returns FALSE. */
bool
read_only_pos(TX *tx, repv pos)
{
    repv tmp = Fbuffer_symbol_value(Qread_only, pos, rep_VAL(tx), Qt);
    if(rep_VOIDP(tmp))
	tmp = Fdefault_value(Qread_only, Qt);
    if(!rep_VOIDP(tmp) && !rep_NILP(tmp))
    {
	repv tmp = Fsymbol_value(Qinhibit_read_only, Qt);
	if(rep_VOIDP(tmp) || rep_NILP(tmp))
	{
	    Fsignal(Qbuffer_read_only, rep_LIST_2(pos, rep_VAL(tx)));
	    return TRUE;
	}
    }
    return FALSE;
}

static void
read_only_callback (Lisp_Extent *e, void *data)
{
    bool *read_onlyp = data;
    repv val = Fbuffer_symbol_value(Qread_only, rep_VAL(e), Qnil, Qt);
    if(!rep_VOIDP(val) && !rep_NILP(val))
	*read_onlyp = TRUE;
}

bool
read_only_section(TX *tx, repv start, repv end)
{
    bool read_only = FALSE;
    Pos p_start, p_end;

    COPY_VPOS(&p_start, start);
    COPY_VPOS(&p_end, end);
    map_section_extents(read_only_callback,
			tx->tx_GlobalExtent, &p_start, &p_end, &read_only);
    if(read_only)
    {
	repv tmp = Fsymbol_value(Qinhibit_read_only, Qt);
	if(rep_VOIDP(tmp) || rep_NILP(tmp))
	{
	    Fsignal(Qbuffer_read_only, rep_list_3(start, end, rep_VAL(tx)));
	    return TRUE;
	}
    }
    return FALSE;
}
