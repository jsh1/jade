/* io.c -- Loading & saving files, etc...
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <string.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#if defined( HAVE_UNIX )
# include <unistd.h>
#elif defined( HAVE_AMIGA )
  /* my Amiga compiler has chdir() etc in <stdio.h> */
#endif

_PR bool file_exists2(u_char *, u_char *);
_PR bool file_exists3(u_char *, u_char *, u_char *);
_PR VALUE signal_file_error(VALUE cdr);
_PR void io_init(void);

/* The average number of chars-per-line in the last file read. At first
   I tried an average over all files ever loaded; this seems better since
   likely use will involve locality in loading similar files.
   Initialised to a guessed value */
static u_long last_avg_line_length = 40;

/* Read a file into a tx structure, the line list should have been
   killed. FILE-LENGTH is the length of the file to be loaded, or -1
   if the length is unknown. */
static bool
read_tx(TX *tx, FILE *fh, long file_length)
{
    bool rc = FALSE;
    u_char buf[BUFSIZ];
    long len, linenum, alloced_lines, chars_read = 0;
    LINE *line;

    /* First calculate the rate at which the line list is allocated. */
    if(file_length > 0)
    {
	/* We know the length of the file, and the average chars-per-line
	   of the last file loaded. */
	long predicted_lines = file_length / last_avg_line_length;
	if(predicted_lines < 64)
	    predicted_lines = 64;
	if(predicted_lines > 1024)
	    predicted_lines = 1024;
	if(!resize_line_list(tx, predicted_lines, 0))
	    goto abortmem;
	alloced_lines = predicted_lines;
    }
    else
    {
	/* Don't know the length of the file. Let resize_line_list()
	   take care of everything. */
	if(!resize_line_list(tx, 1, 0))
	    goto abortmem;
	alloced_lines = 1;
    }
    linenum = 0;
    line = tx->tx_Lines;

    while((len = fread(buf, 1, BUFSIZ, fh)) > 0)
    {
	u_char *new;
	long newlen;
	u_char *eol, *cur = buf;
	while((eol = memchr(cur, '\n', (buf + len) - cur)))
	{
	    if(line->ln_Strlen != 0)
	    {
		newlen = line->ln_Strlen + (eol - cur);
		new = alloc_line_buf(tx, newlen);
		memcpy(new, line->ln_Line, line->ln_Strlen);
		memcpy(new + line->ln_Strlen - 1, cur, eol - cur);
		new[newlen-1] = 0;
		free_line_buf(tx, line->ln_Line);
		line->ln_Line = new;
		line->ln_Strlen = newlen;
	    }
	    else
	    {
		newlen = eol - cur;
		new = alloc_line_buf(tx, newlen + 1);
		if(new == NULL)
		    goto abortmem;
		memcpy(new, cur, newlen);
		new[newlen] = 0;
		line->ln_Line = new;
		line->ln_Strlen = newlen+1;
	    }
	    chars_read += line->ln_Strlen;

	    if(++linenum >= alloced_lines)
	    {
		/* Need to grow the line list. If we don't know the size
		   of the file just pass it off to resize_line_list().. */
		if(file_length < 0)
		{
		    if(!resize_line_list(tx, 1, linenum))
			goto abortmem;
		    alloced_lines++;
		}
		else
		{
		    /* We know the file_length, and the average bytes-per-line
		       so far. Re-calibrate our prediction of the total
		       number of lines. */
		    long predicted_lines = file_length * linenum / chars_read;
		    /* Some restrictions on the growth rate */
		    if(predicted_lines < linenum + 32)
			predicted_lines = linenum + 32;
		    if(predicted_lines > linenum + 1024)
			predicted_lines = linenum + 1024;
		    if(!resize_line_list(tx, predicted_lines - alloced_lines,
					 linenum))
			goto abortmem;
		    alloced_lines = predicted_lines;
		}
		line = tx->tx_Lines + linenum;
	    }
	    else
		line++;
	    cur = eol + 1;
	}
	if(cur < buf + len)
	{
            if(line->ln_Strlen)
	    {
                /* Only way we can get here is if there were *no* newlines in
                   the chunk we just read. */
		newlen = line->ln_Strlen + len;
		new = alloc_line_buf(tx, newlen);
		if(!new)
		    goto abortmem;
		memcpy(new, line->ln_Line, line->ln_Strlen - 1);
		memcpy(new + (line->ln_Strlen - 1), buf, len);
		new[newlen-1] = 0;
		free_line_buf(tx, line->ln_Line);
		line->ln_Line = new;
		line->ln_Strlen = newlen;
	    }
            else
	    {
		newlen = (buf + len) - cur;
		line->ln_Line = alloc_line_buf(tx, newlen + 1);
		if(!line->ln_Line)
		    goto abortmem;
		memcpy(line->ln_Line, cur, newlen);
		line->ln_Line[newlen] = 0;
		line->ln_Strlen = newlen + 1;
	    }
	}
    }
    if(line->ln_Strlen == 0)
    {
	line->ln_Line = alloc_line_buf(tx, 1);
	if(line->ln_Line == NULL)
	    goto abortmem;
	line->ln_Line[0] = 0;
	line->ln_Strlen = 1;
    }
    else
	chars_read += line->ln_Strlen;
    linenum++;

    if(!resize_line_list(tx, linenum - alloced_lines, linenum))
	goto abortmem;

    /* If the average line length seems sensible, set it as the
       length of the "last-read" file */
    if(chars_read / linenum > 10 && chars_read / linenum < 100)
	last_avg_line_length = chars_read / linenum;

    tx->tx_LogicalStart = 0;
    tx->tx_LogicalEnd = tx->tx_NumLines;

    tx->tx_Changes++;
    rc = TRUE;

    if(0)
    {
	/* This only gets executed if we aborted while reading the file. */
abortmem:
	mem_error();
	clear_line_list(tx);
    }

    tx->tx_Flags |= TXFF_REFRESH_ALL;
    return(rc);
}

_PR VALUE cmd_read_file_contents(VALUE file, VALUE tx);
DEFUN("read-file-contents", cmd_read_file_contents, subr_read_file_contents, (VALUE file, VALUE tx), V_Subr2, DOC_read_file_contents) /*
::doc:read_file_contents::
read-file-contents FILE [BUFFER]

Overwrites the text in BUFFER with that from the file FILE.

FILE is either a string naming the file to be opened or a Lisp file object
(from `open') to be used. Also removes any restriction on BUFFER.
::end:: */
{
    VALUE res = sym_nil;
    FILE *fh;
    bool closefh;
    long file_length = -1;
    VALUE start, end;
    if(FILEP(file) && VFILE(file)->name)
    {
	fh = VFILE(file)->file;
	closefh = FALSE;
	file_length = sys_file_length(VSTR(VFILE(file)->name));
    }
    else
    {
	DECLARE1(file, STRINGP);
	if(!(fh = fopen(VSTR(file), "r")))
	    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
	closefh = TRUE;
	file_length = sys_file_length(VSTR(file));
    }
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    cmd_unrestrict_buffer(tx);
    start = make_pos(0, 0);
    end = cmd_end_of_buffer(tx, sym_t);
    undo_record_deletion(VTX(tx), start, end);
    kill_line_list(VTX(tx));
    if(read_tx(VTX(tx), fh, file_length))
    {
	end = cmd_end_of_buffer(tx, sym_t);
	undo_record_insertion(VTX(tx), start, end);
	res = tx;
    }
    else
	clear_line_list(VTX(tx));	 /* hope for some mem left */
    if(closefh)
	fclose(fh);
    reset_all_views(VTX(tx));
    sys_reset_sleep_titles(VTX(tx));
    return(res);
}

_PR VALUE cmd_insert_file_contents(VALUE file, VALUE pos, VALUE tx);
DEFUN("insert-file-contents", cmd_insert_file_contents, subr_insert_file_contents, (VALUE file, VALUE pos, VALUE tx), V_Subr3, DOC_insert_file_contents) /*
::doc:insert_file_contents::
insert-file-contents FILE [POSITION] [BUFFER]

Insert the contents of FILE (the name of a file, or a file object) before
the character at POSITION in BUFFER (or before the cursor).
::end:: */
{
    FILE *fh;
    u_char buf[BUFSIZ];
    long len;

    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);

    if(FILEP(file) && VFILE(file)->name)
	fh = VFILE(file)->file;
    else
    {
	DECLARE1(file, STRINGP);
	if(!(fh = fopen(VSTR(file), "r")))
	    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
    }

    while(pos != LISP_NULL && (len = fread(buf, 1, BUFSIZ, fh)) > 0)
	pos = insert_string(VTX(tx), buf, len, pos);

    if(STRINGP(file))
	fclose(fh);

    return pos;
}

_PR VALUE cmd_write_buffer_contents(VALUE file, VALUE start, VALUE end, VALUE tx);
DEFUN_INT("write-buffer-contents", cmd_write_buffer_contents, subr_write_buffer_contents, (VALUE file, VALUE start, VALUE end, VALUE tx), V_Subr4, DOC_write_buffer_contents, "FWrite marked block to file:" DS_NL "m" DS_NL "M") /*
::doc:write_buffer_contents::
write-buffer-contents FILE [START] [END] [BUFFER]

Writes the text between positions START and END in BUFFER to FILE,
which may be either a string naming a file to overwrite, or a standard
stream object.

If START or END are undefined they are taken as the start and end of
the buffer respectively (ignoring the current restriction).
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(start))
	start = cmd_start_of_buffer(tx, sym_t);
    if(!POSP(end))
	end = cmd_end_of_buffer(tx, sym_t);

    if(!check_section(VTX(tx), &start, &end))
	return(cmd_signal(sym_invalid_area, list_3(tx, start, end)));

    if(STRINGP(file))
    {
	FILE *fh;
	fh = fopen(VSTR(file), "w");
	if(fh)
	{
	    long lineno = VROW(start), col = VCOL(start);
	    LINE *line = VTX(tx)->tx_Lines + lineno;
	    while(lineno <= VROW(end))
	    {
		int len = (((lineno == VROW(end))
			    ? VCOL(end) : line->ln_Strlen - 1) - col);
		if(fwrite(line->ln_Line + col, 1, len, fh) != len) 
		{
		    fclose(fh);
		    goto file_error;
		}
		if(lineno != VROW(end))
		    fputc('\n', fh);
		lineno++;
		col = 0;
		line++;
	    }
	    fclose(fh);
	}
	else
	{
	file_error:
	    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
	}
	return(file);
    }
    else if(NILP(cmd_streamp(file)))
    {
	/* file is a stream */
	long lineno = VROW(start), col = VCOL(start);
	LINE *line = VTX(tx)->tx_Lines + lineno;
	while(lineno <= VROW(end))
	{
	    int len = (((lineno == VROW(end))
			? VCOL(end) : line->ln_Strlen - 1) - col);
	    if(stream_puts(file, line->ln_Line + col, len, FALSE) != len) 
		goto stream_error;
	    if(lineno != VROW(end))
		stream_putc(file, '\n');
	    lineno++;
	    col = 0;
	    line++;
	}
	return file;
    }
    else
    {
    stream_error:
	return cmd_signal(sym_invalid_stream, list_1(file));
    }
}

_PR VALUE cmd_cd(VALUE dir);
DEFUN("cd", cmd_cd, subr_cd, (VALUE dir), V_Subr1, DOC_cd) /*
::doc:cd::
cd [DIRECTORY]

If DIRECTORY is given set the editor's current directory to it, else
return the name of the current directory.
::end:: */
{
    VALUE res = sym_nil;
    if(STRINGP(dir))
    {
	if(chdir(VSTR(dir)))
	    messagef("can't chdir to %s", VSTR(dir));
	else
	    res = dir;
    }
    else
    {
	u_char buff[256];
	if(getcwd(buff, 256))
	    res = string_dup(buff);
    }
    return(res);
}

DEFSTRING(cant_open, "Can't open file");
_PR VALUE cmd_read_file_from_to(VALUE file, VALUE offset, VALUE ch);
DEFUN("read-file-from-to", cmd_read_file_from_to, subr_read_file_from_to, (VALUE file, VALUE offset, VALUE ch), V_Subr3, DOC_read_file_from_to) /*
::doc:read_file_from_to::
read-file-from-to FILENAME OFFSET CHAR
::end:: */
{
    FILE *fh;
    VALUE str = LISP_NULL;
    DECLARE1(file, STRINGP);
    DECLARE2(offset, INTP);
    DECLARE3(ch, INTP);
    if((fh = fopen(VSTR(file), "r")) && !fseek(fh, VINT(offset), 0 /*SEEK_SET*/))
    {
	int buflen = 128, i = 0, c;
	u_char *buf = str_alloc(buflen);
	if(buf)
	{
	    while(((c = getc(fh)) != VINT(ch)) && (c != EOF))
	    {
		if(i >= buflen)
		{
		    u_char *newbuf;
		    int newbuflen = buflen * 2;
		    if(!(newbuf = str_alloc(newbuflen)))
			goto error;
		    memcpy(newbuf, buf, i);
		    str_free(buf);
		    buf = newbuf;
		    buflen = newbuflen;
		}
		buf[i++] = c;
	    }
	    str = string_dupn(buf, i);
error:
	    str_free(buf);
	}
	fclose(fh);
    }
    else
    {
	return(cmd_signal(sym_file_error, list_2(VAL(&cant_open), file)));
    }
    return(str);
}

_PR VALUE cmd_write_clip(VALUE unit, VALUE str);
DEFUN("write-clip", cmd_write_clip, subr_write_clip, (VALUE unit, VALUE str), V_Subr2, DOC_write_clip) /*
::doc:write_clip::
write-clip UNIT STRING

Writes STRING to unit UNIT of the standard clipboard.
::end:: */
{
    DECLARE1(unit, INTP);
    DECLARE2(str, STRINGP);
    if(write_clip(VINT(unit), VSTR(str), STRING_LEN(str)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_read_clip(VALUE unit);
DEFUN("read-clip", cmd_read_clip, subr_read_clip, (VALUE unit), V_Subr1, DOC_read_clip) /*
::doc:read_clip::
read-clip UNIT

Returns the string which unit UNIT of the clipboard holds.
::end:: */
{
    DECLARE1(unit, INTP);
    return(read_clip(VINT(unit)));
}

bool
file_exists2(u_char *s1, u_char *s2)
{
    u_char buf[256];
    stpcpy(stpcpy(buf, s1), s2);
    return(file_exists(buf));
}
bool
file_exists3(u_char *s1, u_char *s2, u_char *s3)
{
    u_char buf[256];
    stpcpy(stpcpy(stpcpy(buf, s1), s2), s3);
    return(file_exists(buf));
}

VALUE
signal_file_error(VALUE cdr)
{
    VALUE data = cmd_cons(lookup_errno(), sym_nil);
    if(cdr)
    {
	if(CONSP(cdr) || NILP(cdr))
	    VCDR(data) = cdr;
	else
	    VCDR(data) = cmd_cons(cdr, sym_nil);
    }
    return(cmd_signal(sym_file_error, data));
}

void
io_init(void)
{
    ADD_SUBR(subr_read_file_contents);
    ADD_SUBR(subr_insert_file_contents);
    ADD_SUBR_INT(subr_write_buffer_contents);
    ADD_SUBR(subr_cd);
    ADD_SUBR(subr_read_file_from_to);
    ADD_SUBR(subr_write_clip);
    ADD_SUBR(subr_read_clip);
}
