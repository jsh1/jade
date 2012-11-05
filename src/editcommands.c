/* editcommands.c -- Lisp functions for editing
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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
#include <string.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#include <fcntl.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

DEFSYM(block_status_hook, "block-status-hook");
DEFSYM(inhibit_read_only, "inhibit-read-only");
DEFSYM(read_only, "read-only");

/* Some doc strings
::doc:inhibit-read-only::
When bound and non-nil this variable cancels the effect of the set-buffer-
read-only command; in that the buffer is always writeable. This is
intended to be bound while a command is executing that is allowed to
modify a buffer.
::end::
::doc:block-status-hook::
The hook called when the status of the block changes. It will be called
with a single argument, t if the block is now marked, nil if it isn't.
::end:: */


/* Positions */

repv
make_pos(intptr_t col, intptr_t row)
{
    return MAKE_POS(col, row);
}

DEFUN("pos", Fpos, Spos, (repv x, repv y), rep_Subr2) /*
::doc:pos::
pos COLUMN ROW

Returns a new position object with coordinates (COLUMN , ROW).
::end:: */
{
    intptr_t col = rep_INTP(x) ? rep_INT(x) : VCOL(curr_vw->cursor_pos);
    intptr_t row = rep_INTP(y) ? rep_INT(y) : VROW(curr_vw->cursor_pos);
    return MAKE_POS(col ,row);
}


DEFUN_INT("insert", Finsert, Sinsert, (repv string, repv pos, repv buff), rep_Subr3, "sString to insert:") /*
::doc:insert::
insert STRING [POS] [BUFFER]

Inserts STRING into BUFFER at POS. Returns the first position of the first
character after the end of the inserted text.
::end:: */
{
    rep_DECLARE1(string, rep_STRINGP);
    if(!BUFFERP(buff))
	buff = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(buff));
    if(pad_pos(VBUFFER(buff), pos))
    {
	pos = insert_string(VBUFFER(buff), rep_STR(string), rep_STRING_LEN(string), pos);
	if(pos != 0)
	    return pos;
    }
    return(Qnil);
}

DEFUN_INT("delete-area", Fdelete_area, Sdelete_area, (repv start, repv end, repv buff), rep_Subr3, "-m" rep_DS_NL "M") /*
::doc:delete-area::
delete-area START-POS END-POS [BUFFER]

Deletes from START-POS up to (but not including) END-POS.
::end:: */
{
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = rep_VAL(curr_vw->tx);
    if(check_section(VBUFFER(buff), &start, &end)
       && !read_only_section(VBUFFER(buff), start, end))
    {
	delete_section(VBUFFER(buff), start, end);
	return start;
    }
    return(Qnil);
}

DEFUN("copy-area", Fcopy_area, Scopy_area, (repv start, repv end, repv buff), rep_Subr3) /*
::doc:copy-area::
copy-area START-POS END-POS [BUFFER]

Returns the string from START-POS up to END-POS.
::end:: */
{
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = rep_VAL(curr_vw->tx);
    if(check_section(VBUFFER(buff), &start, &end))
    {
	intptr_t tlen = section_length(VBUFFER(buff), start, end) + 1;
	repv str = rep_make_string(tlen);
	if(str)
	{
	    copy_section(VBUFFER(buff), start, end, rep_STR(str));
	    rep_STR(str)[tlen - 1] = 0;
	    return(str);
	}
    }
    return(Qnil);
}

DEFUN("cut-area", Fcut_area, Scut_area, (repv start, repv end, repv buff), rep_Subr3) /*
::doc:cut-area::
cut-area START-POS END-POS [BUFFER]

The same as `copy-area' except that the section of text copied (START-POS to
END-POS) is deleted from the file after being duplicated.
::end:: */
{
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = rep_VAL(curr_vw->tx);
    if(check_section(VBUFFER(buff), &start, &end)
       && !read_only_section(VBUFFER(buff), start, end))
    {
	/* Only one copy is made. */
	repv str = undo_push_deletion(VBUFFER(buff), start, end);
	if(str)
	{
	    delete_section(VBUFFER(buff), start, end);
	    return(str);
	}
    }
    return(Qnil);
}

DEFUN_INT("block-toggle", Fblock_toggle, Sblock_toggle, (void), rep_Subr0, "") /*
::doc:block-toggle::
block-toggle
::end:: */
{
    Lisp_View *vw = curr_vw;
    switch(vw->block_state)
    {
	case 0:
	    vw->block_state = -1;
	    break;
	case 1:
	    vw->block_end = vw->cursor_pos;
	    vw->block_state = 0;
	    order_block(vw);
	    break;
	case 2:
	    vw->block_start = vw->cursor_pos;
	    vw->block_state = 0;
	    order_block(vw);
	    break;
	case -1:
	    vw->block_start = vw->cursor_pos;
	    vw->block_state = 1;
	    break;
    }
    Fcall_hook(Qblock_status_hook, Qnil, Qnil);
    return(Qt);
}

DEFUN("block-start", Fblock_start, Sblock_start, (repv pos), rep_Subr1) /*
::doc:block-start::
block-start [POS]

Always returns the position of the block-start as it is, if POS is given
it is used as the new position of the start of the block.
::end:: */
{
    Lisp_View *vw = curr_vw;
    repv res;
    if(!vw->block_state || (vw->block_state == 1))
	res = vw->block_start;
    else
	res = Qnil;
    if(POSP(pos) && check_line(vw->tx, pos))
    {
	switch(vw->block_state)
	{
	    rep_GC_root gc_res;
	    case 0:
		vw->block_start = pos;
		order_block(vw);
		break;
	    case 2:
		vw->block_start = pos;
		vw->block_state = 0;
		order_block(vw);
		rep_PUSHGC(gc_res, res);
		Fcall_hook(Qblock_status_hook, Qnil, Qnil);
		rep_POPGC;
		break;
	    case -1:
		vw->block_state = 1;
		/* FALL THROUGH */
	    case 1:
		vw->block_start = pos;
		break;
	}
    }
    return res;
}

DEFUN("block-end", Fblock_end, Sblock_end, (repv pos), rep_Subr1) /*
::doc:block-end::
block-end [POS]

Always returns the position of the block-end as it is, if POS is given
it is used as the new position of the end of the block.
::end:: */
{
    Lisp_View *vw = curr_vw;
    repv res;
    if(!vw->block_state || (vw->block_state == 2))
	res = vw->block_end;
    else
	res = Qnil;
    if(POSP(pos) && check_line(vw->tx, pos))
    {
	switch(vw->block_state)
	{
	    rep_GC_root gc_res;
	    case 0:
		vw->block_end = pos;
		order_block(vw);
		break;
	    case 1:
		vw->block_end = pos;
		vw->block_state = 0;
		order_block(vw);
		rep_PUSHGC(gc_res, res);
		Fcall_hook(Qblock_status_hook, Qnil, Qnil);
		rep_POPGC;
		break;
	    case -1:
		vw->block_state = 2;
		/* FALL THROUGH */
	    case 2:
		vw->block_end = pos;
		break;
	}
    }
    return res;
}

DEFUN_INT("block-kill", Fblock_kill, Sblock_kill, (void), rep_Subr0, "") /*
::doc:block-kill::
block-kill

Unmarks the block.
::end:: */
{
    Lisp_View *vw = curr_vw;
    if(vw->block_state == 0)
    {
	vw->block_state = -1;
	Fcall_hook(Qblock_status_hook, Qnil, Qnil);
    }
    return(Qt);
}

DEFUN("blockp", Fblockp, Sblockp, (void), rep_Subr0) /*
::doc:blockp::
blockp

Returns true if a block is currently marked.
::end:: */
{
    if(curr_vw->block_state == 0)
	return(Qt);
    return(Qnil);
}

DEFUN("translate-area", Ftranslate_area, Stranslate_area, (repv start, repv end, repv table, repv tx), rep_Subr4) /*
::doc:translate-area:
translate-area START-POS END-POS TRANSLATION-TABLE [BUFFER]

Applies the TRANSLATION-TABLE to the text between START-POS and END-POS.
TRANSLATION-TABLE is a string, each character represents the translation
for an ascii character of that characters position in the string. If the
string is less than 256 chars long any undefined characters will remain
unchanged.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);
    rep_DECLARE3(table, rep_STRINGP);
    if(check_section(VBUFFER(tx), &start, &end)
       && !read_only_section(VBUFFER(tx), start, end))
    {
	intptr_t linenum = VROW(start), col;
	int tablen = rep_STRING_LEN(table);
	char *str;
	undo_record_modification(VBUFFER(tx), start, end);
	flag_modification(VBUFFER(tx), start, end);
	while(linenum < VROW(end))
	{
	    int llen = VBUFFER(tx)->lines[linenum].ln_Strlen - 1;
	    col = (linenum == VROW(start) ? VCOL(start) : 0);
	    str = VBUFFER(tx)->lines[linenum].ln_Line + col;
	    while(col++ < llen)
	    {
		unsigned int c = *str;
		*str++ = (c < tablen) ? rep_STR(table)[c] : c;
	    }
	    linenum++;
	}
	col = (linenum == VROW(start) ? VCOL(start) : 0);
	str = VBUFFER(tx)->lines[linenum].ln_Line + col;
	while(col++ < VCOL(end))
	{
	    unsigned int c = *str;
	    *str++ = (c < tablen) ? rep_STR(table)[c] : c;
	}
	return(Qt);
    }
    return 0;
}

DEFUN("get-char", Fget_char, Sget_char, (repv pos, repv tx), rep_Subr2) /*
::doc:get-char::
get-char [POS] [BUFFER]

Returns the numerical value of the character at position POS in BUFFER. If no
character exists at that position, nil is returned.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    if(!check_line(VBUFFER(tx), pos))
	return(Qnil);
    if(VCOL(pos) >= VBUFFER(tx)->lines[VROW(pos)].ln_Strlen)
	return(Qnil);
    else if(VCOL(pos) == VBUFFER(tx)->lines[VROW(pos)].ln_Strlen - 1)
    {
	if(VROW(pos) == VBUFFER(tx)->logical_end - 1)
	    return(Qnil);
	else
	    return(rep_MAKE_INT('\n'));
    }
    else
	return(rep_MAKE_INT(VBUFFER(tx)->lines[VROW(pos)].ln_Line[VCOL(pos)]));
}

DEFUN_INT("set-char", Fset_char, Sset_char, (repv ch, repv pos, repv tx), rep_Subr3, "cCharacter:") /*
::doc:set-char::
set-char CHARACTER [POS] [BUFFER]

Sets the character at position POS in BUFFER to CHARACTER.
::end:: */
{
    /* FIXME: make this handle insertion of newlines */
    repv end;
    rep_DECLARE1(ch, rep_INTP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    if(!check_line(VBUFFER(tx), pos))
	return(Qnil);
    end = make_pos(VCOL(pos) + 1, VROW(pos));
    if(pad_pos(VBUFFER(tx), end))
    {
	undo_record_modification(VBUFFER(tx), pos, end);
	VBUFFER(tx)->lines[VROW(pos)].ln_Line[VCOL(pos)] = rep_INT(ch);
	flag_modification(VBUFFER(tx), pos, end);
	return(ch);
    }
    return 0;
}

DEFUN("posp", Fposp, Sposp, (repv arg), rep_Subr1) /*
::doc:posp::
posp ARG

Returns t if ARG is a position object.
::end:: */
{
    if(POSP(arg))
	return(Qt);
    return(Qnil);
}

DEFUN("cursor-pos", Fcursor_pos, Scursor_pos, (void), rep_Subr0) /*
::doc:cursor-pos::
cursor-pos

Returns the position of the cursor in the current window.
::end:: */
{
    return curr_vw->cursor_pos;
}

DEFUN("empty-line-p", Fempty_line_p, Sempty_line_p, (repv pos, repv tx), rep_Subr2) /*
::doc:empty-line-p::
empty-line-p [POS] [BUFFER]

Returns t if the line pointer to by POS (or the cursor) in BUFFER is
empty, ie, blank or only containing spaces.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    if(check_line(VBUFFER(tx), pos))
    {
	if(VBUFFER(tx)->lines[VROW(pos)].ln_Strlen == 1)
	    return(Qt);
	else
	{
	    char *s = VBUFFER(tx)->lines[VROW(pos)].ln_Line;
	    while(*s && isspace(*s))
		s++;
	    if(!(*s))
		return(Qt);
	}
	return(Qnil);
    }
    else
	return 0;
}

DEFUN("indent-pos", Findent_pos, Sindent_pos, (repv pos, repv tx), rep_Subr2) /*
::doc:indent-pos::
indent-pos [POS] [BUFFER]

Returns the glyph position of the first non-space character in the line
pointed to by POS (or the cursor), in BUFFER.
::end:: */
{
    Lisp_View *vw = curr_vw;
    intptr_t len;
    char *line;
    if(!BUFFERP(tx))
	tx = rep_VAL(vw->tx);
    if(POSP(pos) && check_line(VBUFFER(tx), pos))
	;
    else
	pos = vw->cursor_pos;
    line = VBUFFER(tx)->lines[VROW(pos)].ln_Line;
    for(len = 0; *line && isspace(*line); len++, line++)
	;
    len = glyph_col(VBUFFER(tx), len, VROW(pos));
    return make_pos(len, VROW(pos));
}

DEFUN("%set-indent-pos", Fset_indent_pos, Sset_indent_pos, (repv indpos, repv tx, repv spaces_p), rep_Subr3) /*
::doc:%set-indent-pos::
%set-indent-pos POS [BUFFER] [ONLY-SPACES]

Sets the indentation of the line pointed to by POS to the column pointed
to by POS by putting the optimal sequence of TAB and SPC characters at the
start of the line.

If ONLY-SPACES in non-nil no tab characters are used.
::end:: */
{
    rep_DECLARE1(indpos, POSP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    /* FIXME: should check if the region is read-only. */
    if(!read_only_pos(VBUFFER(tx), indpos) && check_line(VBUFFER(tx), indpos))
    {
	intptr_t row = VROW(indpos);
	char *s = VBUFFER(tx)->lines[row].ln_Line;
	repv pos = indpos;
	intptr_t oldind, diff;
	intptr_t tabs, spaces;
	while(*s && isspace(*s))
	    s++;
	oldind = s - VBUFFER(tx)->lines[row].ln_Line;
	if(rep_NILP(spaces_p))
	{
	    tabs = VCOL(pos) / VBUFFER(tx)->tab_size;
	    spaces = VCOL(pos) % VBUFFER(tx)->tab_size;
	}
	else
	{
	    tabs = 0;
	    spaces = VCOL(pos);
	}
	diff = oldind - (tabs + spaces);
	pos = make_pos(0, VROW(pos));
	if(diff > 0)
	{
	    repv end = make_pos(diff, VROW(pos));
	    undo_record_deletion(VBUFFER(tx), pos, end);
	    delete_chars(VBUFFER(tx), VCOL(pos), VROW(pos), diff);
	    flag_deletion(VBUFFER(tx), pos, end);
	    end = make_pos(tabs + spaces, VROW(end));
	    undo_record_modification(VBUFFER(tx), pos, end);
	    memset(VBUFFER(tx)->lines[row].ln_Line, '\t', tabs);
	    memset(VBUFFER(tx)->lines[row].ln_Line + tabs, ' ', spaces);
	    flag_modification(VBUFFER(tx), pos, end);
	}
	else if(diff < 0)
	{
	    repv end;
	    diff = -diff;
	    end = make_pos(diff, VROW(pos));
	    insert_gap(VBUFFER(tx), diff, VCOL(pos), VROW(pos));
	    undo_record_insertion(VBUFFER(tx), pos, end);
	    flag_insertion(VBUFFER(tx), pos, end);
	    pos = make_pos(diff, VROW(pos));
	    end = make_pos(tabs + spaces, VROW(end));
	    undo_record_modification(VBUFFER(tx), pos, end);
	    memset(VBUFFER(tx)->lines[row].ln_Line, '\t', tabs);
	    memset(VBUFFER(tx)->lines[row].ln_Line + tabs, ' ', spaces);
	    flag_modification(VBUFFER(tx), pos, end);
	}
	else
	{
	    char *s = VBUFFER(tx)->lines[row].ln_Line;
	    intptr_t i;
	    repv end = make_pos(tabs + spaces, VROW(pos));
	    for(i = 0; i < tabs; i++)
	    {
		if(*s++ != '\t')
		{
		    undo_record_modification(VBUFFER(tx), pos, end);
		    memset(VBUFFER(tx)->lines[row].ln_Line, '\t', tabs);
		    memset(VBUFFER(tx)->lines[row].ln_Line + tabs, ' ', spaces);
		    flag_modification(VBUFFER(tx), pos, end);
		    return indpos;
		}
	    }
	    for(i = 0; i < spaces; i++)
	    {
		if(*s++ != ' ')
		{
		    pos = make_pos(tabs, VROW(pos));
		    undo_record_modification(VBUFFER(tx), pos, end);
		    memset(VBUFFER(tx)->lines[row].ln_Line + tabs, ' ', spaces);
		    flag_modification(VBUFFER(tx), pos, end);
		    return indpos;
		}
	    }
	    /* No modifications required. */
	}
	return indpos;
    }
    return Qnil;
}

DEFUN_INT("%indent-to", Findent_to, Sindent_to, (repv col, repv spaces_p), rep_Subr2, "NIndent to column:") /*
::doc:%indent-to::
%indent-to COLUMN [ONLY-SPACES]

Inserts enough tabs and spaces to move the cursor to glyph column COLUMN.
If ONLY-SPACES is non-nil no tabs are used.
COLUMN counts from zero.
::end:: */
{
    Lisp_View *vw = curr_vw;
    Lisp_Buffer *tx = vw->tx;
    rep_DECLARE1(col, rep_INTP);
    if(pad_cursor(vw))
    {
	int spaces, tabs;
	intptr_t curr_col, dest_col;
        curr_col = get_cursor_column(vw);
        dest_col = rep_INT(col);
        if(dest_col <= curr_col)
            return(Qt);
	if(rep_NILP(spaces_p))
	{
	    tabs = (dest_col / tx->tab_size) - (curr_col / tx->tab_size);
	    if(tabs == 0)
		spaces = dest_col - curr_col;
	    else
		spaces = dest_col - ((dest_col / tx->tab_size) * tx->tab_size);
	}
	else
	{
	    tabs = 0;
	    spaces = dest_col - curr_col;
	}
	if(spaces + tabs > 0)
	{
	    repv tmp = vw->cursor_pos;
	    if(insert_gap(tx, spaces + tabs, VCOL(tmp), VROW(tmp)))
	    {
		char *line = tx->lines[VROW(tmp)].ln_Line;
		memset(line + VCOL(tmp), '\t', tabs);
		memset(line + VCOL(tmp) + tabs, ' ', spaces);
		undo_record_insertion(tx, tmp, vw->cursor_pos);
		flag_insertion(tx, tmp, vw->cursor_pos);
		return(col);
	    }
	}
	return(Qnil);
    }
    return 0;
}
	    
DEFUN_INT("clear-buffer", Fclear_buffer, Sclear_buffer, (repv tx), rep_Subr1, "") /*
::doc:clear-buffer::
clear-buffer [BUFFER]

Remove all text from BUFFER, leaving just one empty line. Also removes
any restriction on the buffer.
::end:: */
{
    repv start, end;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    Funrestrict_buffer(tx);
    start = make_pos(0, 0);
    end = Fend_of_buffer(rep_VAL(tx), Qt);
    undo_record_deletion(VBUFFER(tx), start, end);
    if(clear_line_list(VBUFFER(tx)))
    {
	reset_global_extent(VBUFFER(tx));
	reset_all_views(VBUFFER(tx));
	return tx;
    }
    return Qnil;
}

DEFUN("pos-to-offset", Fpos_to_offset, Spos_to_offset, (repv pos, repv tx), rep_Subr2) /*
::doc:pos-to-offset::
pos-to-offset [POS] [BUFFER]

Returns the number of characters (counting from zero) that POS (or the cursor)
is from the beginning of the buffer.
::end:: */
{
    intptr_t offset, line_num;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    if(check_pos(VBUFFER(tx), pos))
    {
	offset = 0;
	for(line_num = 0; line_num < VROW(pos); line_num++)
	    offset += VBUFFER(tx)->lines[line_num].ln_Strlen;
	offset += VCOL(pos);
	return rep_MAKE_INT(offset);
    }
    else
	return 0;
}

DEFUN("offset-to-pos", Foffset_to_pos, Soffset_to_pos, (repv voffset, repv tx), rep_Subr2) /*
::doc:offset-to-pos::
offset-to-pos OFFSET [BUFFER]

Returns the position which is OFFSET characters from the start of the buffer.
::end:: */
{
    intptr_t offset;
    intptr_t col, row;
    rep_DECLARE1(voffset, rep_INTP);
    offset = rep_INT(voffset);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    row = 0;
    while(offset >= VBUFFER(tx)->lines[row].ln_Strlen)
	offset -= VBUFFER(tx)->lines[row++].ln_Strlen;
    col = offset;
    return make_pos(col, row);
}

DEFUN("call-process-area", Fcall_process_area,
      Scall_process_area, (repv arg_list), rep_SubrN) /*
::doc:call-process-area::
call-process-area [PROCESS] START END DELETEP [PROGRAM] [ARGS...]

Starts a process running on process-object PROCESS. Waits for the child to
exit, then returns the exit-value of the child. If PROCESS is unspecified
the make-process function will be called (with zero arguments) to create one.

The area of the current buffer between START and END is used as the
input stream of the new process. If DELETE-P is non-nil the area will
be deleted from the buffer before the process is started.

PROGRAM is the filename of the binary image, it will be searched for in
all directories listed in the `PATH' environment variable.
ARGS are the arguments to give to the process.

If any of the optional parameters are unspecified they should have been
set in the PROCESS prior to calling this function.
::end:: */
{
    if(rep_CONSP(arg_list))
    {
	repv proc = rep_CAR(arg_list);
	arg_list = rep_CDR(arg_list);
	if(rep_CONSP(arg_list) && POSP(rep_CAR(arg_list)))
	{
	    repv start = rep_CAR(arg_list);
	    arg_list = rep_CDR(arg_list);
	    if(rep_CONSP(arg_list) && POSP(rep_CAR(arg_list)))
	    {
		repv end = rep_CAR(arg_list);
		arg_list = rep_CDR(arg_list);
		if(rep_CONSP(arg_list))
		{
		    bool deletep = (rep_CAR(arg_list) != Qnil);
		    repv temp_file;
		    repv ret;
		    arg_list = rep_CDR(arg_list);
		    temp_file = Fmake_temp_name();
		    if(temp_file && rep_STRINGP(temp_file))
		    {
			/* Open the file to make it private. */
			int fd = open(rep_STR(temp_file),
				      O_RDWR | O_CREAT | O_TRUNC,
				      S_IRUSR | S_IWUSR);
			if(fd < 0)
			    return rep_signal_file_error(temp_file);
			close(fd);
		    }
		    ret = Fwrite_buffer_contents(temp_file, start, end);
		    if(ret && ret != Qnil)
		    {
			rep_GC_root gc_temp_file;

			if(deletep)
			{
			    ret = Fdelete_area(start, end, Qnil);
			    if(!ret || ret == Qnil)
				goto error;
			}
			/* Splice together the arguments to call-process.
			   PROC FILE-NAME REST.. */
			arg_list = Fcons(proc, Fcons(temp_file, arg_list));
			rep_PUSHGC(gc_temp_file, temp_file);
			ret = Fcall_process(arg_list);
			rep_POPGC;
		    }
		error:
		    unlink(rep_STR(temp_file));	/* ignore errors! */
		    return ret;
		}
	    }
	}
    }
    return Fsignal(Qbad_arg, Fcons(arg_list, Qnil));
}

void
edit_init(void)
{
    rep_ADD_SUBR(Spos);
    rep_ADD_SUBR_INT(Sinsert);
    rep_ADD_SUBR_INT(Sdelete_area);
    rep_ADD_SUBR(Scopy_area);
    rep_ADD_SUBR(Scut_area);
    rep_ADD_SUBR_INT(Sblock_toggle);
    rep_ADD_SUBR(Sblock_start);
    rep_ADD_SUBR(Sblock_end);
    rep_ADD_SUBR_INT(Sblock_kill);
    rep_ADD_SUBR(Sblockp);
    rep_ADD_SUBR(Stranslate_area);
    rep_ADD_SUBR(Sget_char);
    rep_ADD_SUBR_INT(Sset_char);
    rep_ADD_SUBR(Sposp);
    rep_ADD_SUBR(Scursor_pos);
    rep_ADD_SUBR(Sempty_line_p);
    rep_ADD_SUBR(Sindent_pos);
    rep_ADD_SUBR(Sset_indent_pos);
    rep_ADD_SUBR_INT(Sindent_to);
    rep_ADD_SUBR_INT(Sclear_buffer);
    rep_ADD_SUBR(Soffset_to_pos);
    rep_ADD_SUBR(Spos_to_offset);
    rep_ADD_SUBR(Scall_process_area);

    rep_INTERN_SPECIAL(inhibit_read_only);
    rep_INTERN_SPECIAL(read_only);
    Fset (Qread_only, Qnil);
    Fmake_variable_buffer_local(Qread_only);
    rep_INTERN_SPECIAL(block_status_hook);
}
