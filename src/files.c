/* files.c -- Extendable file handling
   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

/* Define this symbol to disable the code that tries to predict how many
   lines a file of length N will contain. */
/* #define NO_ADAPTIVE_LOADING */

/* When loading files into buffers, read this many lines before trying to
   predict how many lines the file actually contains, based on the average
   line length up to this point. This prediction is re-calibrated each time
   new lines subsequently need to be allocated. */
#define ADAPTIVE_INITIAL_SAMPLES 64

#include "jade.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

/* List of operations. If there's a file handler defined for the file
   being manipulated it will be called to execute the operation.

   (write-buffer-contents FILE-OR-NAME START END)
   (read-file-contents FILE-OR-NAME)
   (insert-file-contents FILE-OR-NAME) */

DEFSYM(write_buffer_contents, "write-buffer-contents");
DEFSYM(read_file_contents, "read-file-contents");
DEFSYM(insert_file_contents, "insert-file-contents");


/* Low level stuff */

/* Read a file into a tx structure, the line list should have been
   killed. FILE-LENGTH is the length of the file to be loaded, or -1
   if the length is unknown. */
static bool
read_file_into_tx(TX *tx, FILE *fh, long file_length)
{
    bool rc = FALSE;
    u_char buf[BUFSIZ];
    long len, row, alloced_lines, chars_read = 0;

    /* For the first N allocations, use the standard resize_line_list method,
       even if we know the length of the file.. */
    if(!resize_line_list(tx, 1, 0))
	goto abortmem;
    alloced_lines = 1;
    row = 0;

    while((len = fread(buf, 1, BUFSIZ, fh)) > 0)
    {
	u_char *new;
	long newlen;
	u_char *eol, *cur = buf;
	while((eol = memchr(cur, '\n', (buf + len) - cur)))
	{
	    if(tx->tx_Lines[row].ln_Strlen != 0)
	    {
		newlen = tx->tx_Lines[row].ln_Strlen + (eol - cur);
		new = alloc_line_buf(tx, newlen);
		memcpy(new, tx->tx_Lines[row].ln_Line,
		       tx->tx_Lines[row].ln_Strlen);
		memcpy(new + tx->tx_Lines[row].ln_Strlen - 1, cur, eol - cur);
		new[newlen-1] = 0;
		free_line_buf(tx, tx->tx_Lines[row].ln_Line);
		tx->tx_Lines[row].ln_Line = new;
		tx->tx_Lines[row].ln_Strlen = newlen;
	    }
	    else
	    {
		newlen = eol - cur;
		new = alloc_line_buf(tx, newlen + 1);
		if(new == NULL)
		    goto abortmem;
		memcpy(new, cur, newlen);
		new[newlen] = 0;
		tx->tx_Lines[row].ln_Line = new;
		tx->tx_Lines[row].ln_Strlen = newlen+1;
	    }
	    chars_read += tx->tx_Lines[row].ln_Strlen;

	    if(++row >= alloced_lines)
	    {
		/* Need to grow the line list. */
#ifndef NO_ADAPTIVE_LOADING
		if(file_length >= 0 && row > ADAPTIVE_INITIAL_SAMPLES)
		{
		    /* We know the file_length, and the average bytes-per-line
		       so far. Re-calibrate our prediction of the total
		       number of lines. */
		    long predicted_lines = file_length * row / chars_read;
		    /* Ensure that at least some new lines are going
		       to be allocated.. */
		    if(predicted_lines < row + 32)
			predicted_lines = row + 32;
		    if(!resize_line_list(tx, predicted_lines - alloced_lines,
					 row))
			goto abortmem;
		    alloced_lines = predicted_lines;
		}
		else
#endif
		{
		    /*  We don't know the size of the file;
		        just pass it off to resize_line_list().. */
		    if(!resize_line_list(tx, 1, row))
			goto abortmem;
		    alloced_lines++;
		}
	    }
	    cur = eol + 1;
	}
	if(cur < buf + len)
	{
            if(tx->tx_Lines[row].ln_Strlen)
	    {
                /* Only way we can get here is if there were *no* newlines in
                   the chunk we just read. */
		newlen = tx->tx_Lines[row].ln_Strlen + len;
		new = alloc_line_buf(tx, newlen);
		if(!new)
		    goto abortmem;
		memcpy(new, tx->tx_Lines[row].ln_Line,
		       tx->tx_Lines[row].ln_Strlen - 1);
		memcpy(new + (tx->tx_Lines[row].ln_Strlen - 1), buf, len);
		new[newlen-1] = 0;
		free_line_buf(tx, tx->tx_Lines[row].ln_Line);
		tx->tx_Lines[row].ln_Line = new;
		tx->tx_Lines[row].ln_Strlen = newlen;
	    }
            else
	    {
		newlen = (buf + len) - cur;
		tx->tx_Lines[row].ln_Line = alloc_line_buf(tx, newlen + 1);
		if(!tx->tx_Lines[row].ln_Line)
		    goto abortmem;
		memcpy(tx->tx_Lines[row].ln_Line, cur, newlen);
		tx->tx_Lines[row].ln_Line[newlen] = 0;
		tx->tx_Lines[row].ln_Strlen = newlen + 1;
	    }
	}
    }
    if(tx->tx_Lines[row].ln_Strlen == 0)
    {
	tx->tx_Lines[row].ln_Line = alloc_line_buf(tx, 1);
	if(tx->tx_Lines[row].ln_Line == NULL)
	    goto abortmem;
	tx->tx_Lines[row].ln_Line[0] = 0;
	tx->tx_Lines[row].ln_Strlen = 1;
    }
    else
	chars_read += tx->tx_Lines[row].ln_Strlen;
    row++;

    if(!resize_line_list(tx, row - alloced_lines, row))
	goto abortmem;

    tx->tx_LogicalStart = 0;
    tx->tx_LogicalEnd = tx->tx_NumLines;

    tx->tx_Changes++;
    rc = TRUE;

    if(0)
    {
	/* This only gets executed if we aborted while reading the file. */
abortmem:
	rep_mem_error();
	clear_line_list(tx);
    }

    return(rc);
}


/* Buffer-file functions */

DEFUN_INT("write-buffer-contents", Fwrite_buffer_contents,
	  Swrite_buffer_contents, (repv file, repv start, repv end),
	  rep_Subr3, "-FWrite block to file:" rep_DS_NL "m" rep_DS_NL "M") /*
::doc:Swrite-buffer-contents::
write-buffer-contents FILE-OR-NAME [START] [END]

Writes the text between positions START and END in the current buffer
to FILE-OR-NAME, which may be either a string naming a file to overwrite,
or a file object.

If START or END aren't defined they are taken from the start and end of
the buffer (ignoring the current restriction).
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    repv handler;
    rep_GC_root gc_start, gc_end;

    rep_PUSHGC(gc_start, start);
    rep_PUSHGC(gc_end, end);
    handler = rep_get_handler_from_file_or_name(&file, rep_op_write_buffer_contents);
    rep_POPGC; rep_POPGC;
    if(handler == rep_NULL)
	return handler;

    if(!POSP(start))
	start = Fstart_of_buffer(rep_VAL(tx), Qt);
    if(!POSP(end))
	end = Fend_of_buffer(rep_VAL(tx), Qt);

    if(rep_NILP(handler))
    {
	long row, col;
	FILE *fh;

	/* Don't call check_section() since that looks at the restriction. */
	if(POS_LESS_P(end, start) || VROW(start) < 0
	   || VROW(end) > tx->tx_NumLines)
	    return(Fsignal(Qinvalid_area, rep_list_3(rep_VAL(tx), start, end)));

	if(rep_FILEP(file))
	    fh = rep_FILE(file)->file.fh;
	else
	{
	    fh = fopen(rep_STR(file), "w");
	    if(fh == 0)
		return rep_signal_file_error(file);
	}

	row = VROW(start);
	col = MIN(VCOL(start), tx->tx_Lines[row].ln_Strlen - 1);

	while(row <= VROW(end))
	{
	    int len = (((row == VROW(end))
			? VCOL(end) : tx->tx_Lines[row].ln_Strlen - 1) - col);
	    if(len > 0
	       && fwrite(tx->tx_Lines[row].ln_Line + col, 1, len, fh) != len) 
	    {
		return rep_signal_file_error(file);
	    }
	    if(row != VROW(end))
		fputc('\n', fh);
	    col = 0;
	    row++;
	}
	if(!rep_FILEP(file))
	    fclose(fh);
	return file;
    }
    else
	return rep_call_file_handler(handler, rep_op_write_buffer_contents,
				     Qwrite_buffer_contents,
				     3, file, start, end);
}

DEFUN("read-file-contents", Fread_file_contents, Sread_file_contents,
      (repv file), rep_Subr1) /*
::doc:Sread-file-contents::
read-file-contents FILE-OR-NAME

Overwrites the text in BUFFER with that from the file FILE-OR-NAME.

FILE-OR-NAME is either a string naming the file to be opened or a Lisp file
object to be used. Also removes any restriction on BUFFER.
::end:: */
{
    repv handler
	= rep_get_handler_from_file_or_name(&file, rep_op_read_file_contents);
    TX *tx = curr_vw->vw_Tx;
    if(handler == rep_NULL)
	return handler;

    Funrestrict_buffer(rep_VAL(tx));

    if(rep_NILP(handler))
    {
	FILE *fh;
	u_long file_length;
	repv res = rep_NULL;
	repv start;

	if(rep_FILEP(file))
	{
	    fh = rep_FILE(file)->file.fh;
	    file_length = rep_file_length(rep_FILE(file)->handler_data);
	}
	else
	{
	    fh = fopen(rep_STR(file), "r");
	    if(fh == 0)
		return rep_signal_file_error(file);
	    file_length = rep_file_length(file);
	}

	start = make_pos(0, 0);
	undo_record_deletion(tx, start, Fend_of_buffer(rep_VAL(tx), Qt));
	kill_line_list(tx);
	if(read_file_into_tx(tx, fh, file_length))
	{
	    undo_record_insertion(tx, start, Fend_of_buffer(rep_VAL(tx), Qt));
	    res = rep_VAL(tx);
	}
	else
	    clear_line_list(tx);
	reset_global_extent(tx);

	if(!rep_FILEP(file))
	    fclose(fh);
	
	return res;
    }
    else
	return rep_call_file_handler(handler, rep_op_read_file_contents,
				     Qread_file_contents, 1, file);
}

DEFUN("insert-file-contents", Finsert_file_contents,
      Sinsert_file_contents, (repv file), rep_Subr1) /*
::doc:Sinsert-file-contents::
insert-file-contents FILE-OR-NAME

Insert the contents of FILE-OR-NAME (the name of a file, or a file object)
before the cursor in the current buffer.
::end:: */
{
    repv handler
	= rep_get_handler_from_file_or_name(&file, rep_op_insert_file_contents);
    if(handler == rep_NULL)
	return handler;
    if(rep_NILP(handler))
    {
	TX *tx = curr_vw->vw_Tx;
	FILE *fh;
	u_char buf[BUFSIZ];
	long len;
	repv pos = curr_vw->vw_CursorPos;

	if(rep_FILEP(file))
	    fh = rep_FILE(file)->file.fh;
	else
	{
	    fh = fopen(rep_STR(file), "r");
	    if(fh == 0)
		return rep_signal_file_error(file);
	}

	while(pos != rep_NULL && (len = fread(buf, 1, BUFSIZ, fh)) > 0)
	    pos = insert_string(tx, buf, len, pos);
	if(!rep_FILEP(file))
	    fclose(fh);
	return rep_VAL(tx);
    }
    else
	return rep_call_file_handler(handler, rep_op_insert_file_contents,
				     Qinsert_file_contents, 1, file);
}


/* init */

void
files_init(void)
{
    Fmake_variable_buffer_local(Qdefault_directory);

    rep_INTERN(write_buffer_contents);
    rep_INTERN(read_file_contents);
    rep_INTERN(insert_file_contents);

    rep_ADD_SUBR_INT(Swrite_buffer_contents);
    rep_ADD_SUBR(Sread_file_contents);
    rep_ADD_SUBR(Sinsert_file_contents);
}
