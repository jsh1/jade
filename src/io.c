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

/* Read a file into a tx structure, the line list should have been
   killed.  */
static bool
read_tx(TX *tx, FILE *fh)
{
#define SIZESTEP  50	/* size at which line list grows by */
    bool rc = FALSE;
    u_char buf[BUFSIZ];
    long len;
    long linenum, allocedlines;
    LINE *line;
#ifdef HAVE_AMIGA
    message("loading...");
#endif
    if(!resize_line_list(tx, SIZESTEP, 0))
	goto abortmem;
    allocedlines = SIZESTEP;
    linenum = 0;
    line = tx->tx_Lines;
    while((len = fread(buf, 1, BUFSIZ, fh)) > 0)
    {
	u_char *new;
	long newlen;
	u_char *eol, *cur = buf;
	while((eol = memchr(cur, '\n', (buf + len) - cur)))
	{
	    if(line->ln_Strlen)
	    {
		newlen = line->ln_Strlen + (eol - cur);
		new = str_alloc(newlen);
		memcpy(new, line->ln_Line, line->ln_Strlen);
		memcpy(new + line->ln_Strlen - 1, cur, eol - cur);
		new[newlen-1] = 0;
		str_free(line->ln_Line);
		line->ln_Line = new;
		line->ln_Strlen = newlen;
	    }
	    else
	    {
		newlen = eol - cur;
		new = str_dupn(cur, newlen);
		line->ln_Line = new;
		line->ln_Strlen = newlen+1;
	    }
	    if(++linenum >= allocedlines)
	    {
		if(!resize_line_list(tx, SIZESTEP, linenum))
		    goto abortmem;
		allocedlines += SIZESTEP;
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
		new = str_alloc(newlen);
		if(!new)
		    goto abortmem;
		memcpy(new, line->ln_Line, line->ln_Strlen - 1);
		memcpy(new + (line->ln_Strlen - 1), buf, len);
		new[newlen-1] = 0;
		str_free(line->ln_Line);
		line->ln_Line = new;
		line->ln_Strlen = newlen;
	    }
            else
	    {
		newlen = (buf + len) - cur;
		line->ln_Line = str_alloc(newlen + 1);
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
	line->ln_Line = str_dupn("", 0);
	line->ln_Strlen = 1;
    }
    linenum++;

    if(!resize_line_list(tx, linenum - allocedlines, linenum))
	goto abortmem;

    tx->tx_LogicalStart = 0;
    tx->tx_LogicalEnd = tx->tx_NumLines;

    tx->tx_Changes++;
    rc = TRUE;

#ifdef HAVE_AMIGA
    message("OK");
#endif

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

_PR VALUE cmd_read_buffer(VALUE file, VALUE tx);
DEFUN("read-buffer", cmd_read_buffer, subr_read_buffer, (VALUE file, VALUE tx), V_Subr2, DOC_read_buffer) /*
::doc:read_buffer::
read-buffer FILE [BUFFER]

Overwrites the text in BUFFER with that from the file FILE.
FILE is either a string naming the file to be opened or a Lisp file object
(from `open') to be used. Also removes any restriction on BUFFER.
::end:: */
{
    VALUE res = sym_nil;
    FILE *fh;
    bool closefh;
    POS start, end;
    if(FILEP(file) && VFILE(file)->lf_Name)
    {
	fh = VFILE(file)->lf_File;
	closefh = FALSE;
    }
    else
    {
	DECLARE1(file, STRINGP);
	if(!(fh = fopen(VSTR(file), "r")))
	    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
	closefh = TRUE;
    }
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    cmd_unrestrict_buffer(tx);
    start.pos_Col = start.pos_Line = 0;
    end.pos_Line = VTX(tx)->tx_NumLines - 1;
    end.pos_Col = VTX(tx)->tx_Lines[end.pos_Line].ln_Strlen - 1;
    if(!(end.pos_Line == 0 && end.pos_Line == 0))
	undo_record_deletion(VTX(tx), &start, &end);
    kill_line_list(VTX(tx));
    if(read_tx(VTX(tx), fh))
    {
	end.pos_Line = VTX(tx)->tx_NumLines - 1;
	end.pos_Col = VTX(tx)->tx_Lines[end.pos_Line].ln_Strlen - 1;
	undo_record_insertion(VTX(tx), &start, &end);
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

_PR VALUE cmd_write_buffer(VALUE file, VALUE tx, VALUE urp);
DEFUN("write-buffer", cmd_write_buffer, subr_write_buffer, (VALUE file, VALUE tx, VALUE urp), V_Subr3, DOC_write_buffer) /*
::doc:write_buffer::
write-buffer [FILE-NAME] [BUFFER] [USE-RESTRICTION-P]

Saves the contents of BUFFER to file FILE-NAME. Normally any restriction to
BUFFER is ignored, and the full contents of the buffer written. If USE-
RESTRICTION-P is non-nil only the restricted area is written.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!STRINGP(file))
	file = VTX(tx)->tx_FileName;
    if(file)
    {
	FILE *fh = fopen(VSTR(file), "w");
	if(fh)
	{
	    long i;
	    long min = NILP(urp) ? 0 : VTX(tx)->tx_LogicalStart;
	    long max = NILP(urp) ? VTX(tx)->tx_NumLines
				 : VTX(tx)->tx_LogicalEnd;
	    LINE *line = VTX(tx)->tx_Lines;
	    for(i = min; i < max; i++, line++)
	    {
		if(fwrite(line->ln_Line, 1, line->ln_Strlen - 1, fh)
		   != (line->ln_Strlen - 1))
		    goto error;
		if(i != max - 1)
		    fputc('\n', fh);
	    }
	    fclose(fh);
	}
	else
error:
	    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
	return(file);
    }
    return(cmd_signal(sym_bad_arg, list_2(file, make_number(1))));
}

_PR VALUE cmd_write_buffer_area(VALUE vstart, VALUE vend, VALUE file, VALUE tx);
DEFUN("write-buffer-area", cmd_write_buffer_area, subr_write_buffer_area, (VALUE vstart, VALUE vend, VALUE file, VALUE tx), V_Subr4, DOC_write_buffer_area) /*
::doc:write_buffer_area::
write-buffer-area START-POS END-POS [FILE-NAME] [BUFFER]

Writes the text between START-POS and END-POS in BUFFER to file
FILE-NAME.
::end:: */
{
    POS start, end;
    DECLARE1(vstart, POSP);
    DECLARE2(vend, POSP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!STRINGP(file))
	file = VTX(tx)->tx_FileName;
    start = VPOS(vstart);
    end = VPOS(vend);
    if(!check_section(VTX(tx), &start, &end))
	return(cmd_signal(sym_invalid_area, list_3(tx, vstart, vend)));
    if(file)
    {
	FILE *fh = fopen(VSTR(file), "w");
	if(fh)
	{
	    LINE *line = VTX(tx)->tx_Lines + start.pos_Line;
	    while(start.pos_Line <= end.pos_Line)
	    {
		int len = (((start.pos_Line == end.pos_Line)
			    ? end.pos_Col : line->ln_Strlen)
			   - start.pos_Col - 1);
		if(fwrite(line->ln_Line + start.pos_Col,
			  1, len, fh) != len) 
		    goto error;
		if(start.pos_Line != end.pos_Line)
		    fputc('\n', fh);
		start.pos_Line++;
		start.pos_Col = 0;
		line++;
	    }
	    fclose(fh);
	}
	else
error:
	    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
	return(file);
    }
    return(cmd_signal(sym_bad_arg, list_2(file, make_number(1))));
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

_PR VALUE cmd_write_file(VALUE file, VALUE str);
DEFUN("write-file", cmd_write_file, subr_write_file, (VALUE file, VALUE str), V_Subr2, DOC_write_file) /*
::doc:write_file::
write-file FILE-NAME STRING

Writes STRING to file FILE-NAME.
::end:: */
{
    FILE *fh;
    VALUE res = sym_nil;
    DECLARE1(file, STRINGP);
    DECLARE2(str, STRINGP);
    fh = fopen(VSTR(file), "w");
    if(fh)
    {
	int len = STRING_LEN(str);
	if(fwrite(VSTR(str), 1, len, fh) != len)
	    res = cmd_signal(sym_file_error, list_2(lookup_errno(), file));
	else
	    res = sym_t;
	fclose(fh);
    }
    else
	res = cmd_signal(sym_file_error, list_2(lookup_errno(), file));
    return(res);
}

_PR VALUE cmd_read_file(VALUE file);
DEFUN("read-file", cmd_read_file, subr_read_file, (VALUE file), V_Subr1, DOC_read_file) /*
::doc:read_file::
read-file FILE-NAME

Return the contents of file FILE-NAME.
::end:: */
{
    DECLARE1(file, STRINGP);
    return(read_file(VSTR(file)));
}

_PR VALUE cmd_read_file_from_to(VALUE file, VALUE offset, VALUE ch);
DEFUN("read-file-from-to", cmd_read_file_from_to, subr_read_file_from_to, (VALUE file, VALUE offset, VALUE ch), V_Subr3, DOC_read_file_from_to) /*
::doc:read_file_from_to::
read-file-from-to FILENAME OFFSET CHAR
::end:: */
{
    FILE *fh;
    VALUE str = NULL;
    DECLARE1(file, STRINGP);
    DECLARE2(offset, NUMBERP);
    DECLARE3(ch, CHARP);
    if((fh = fopen(VSTR(file), "r")) && !fseek(fh, VNUM(offset), 0 /*SEEK_SET*/))
    {
	int buflen = 128, i = 0, c;
	u_char *buf = str_alloc(buflen);
	if(buf)
	{
	    while(((c = getc(fh)) != VCHAR(ch)) && (c != EOF))
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
	return(cmd_signal(sym_file_error, list_2(MKSTR("Can't open file"), file)));
    return(str);
}

_PR VALUE cmd_write_clip(VALUE unit, VALUE str);
DEFUN("write-clip", cmd_write_clip, subr_write_clip, (VALUE unit, VALUE str), V_Subr2, DOC_write_clip) /*
::doc:write_clip::
write-clip UNIT STRING

Writes STRING to unit UNIT of the standard clipboard.
::end:: */
{
    DECLARE1(unit, NUMBERP);
    DECLARE2(str, STRINGP);
    if(write_clip(VNUM(unit), VSTR(str), STRING_LEN(str)))
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
    DECLARE1(unit, NUMBERP);
    return(read_clip(VNUM(unit)));
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
    ADD_SUBR(subr_read_buffer);
    ADD_SUBR(subr_write_buffer);
    ADD_SUBR(subr_write_buffer_area);
    ADD_SUBR(subr_cd);
    ADD_SUBR(subr_write_file);
    ADD_SUBR(subr_read_file);
    ADD_SUBR(subr_read_file_from_to);
    ADD_SUBR(subr_write_clip);
    ADD_SUBR(subr_read_clip);
}
