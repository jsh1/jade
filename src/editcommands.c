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
#include "jade_protos.h"

#include <string.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR void edit_init(void);

_PR VALUE sym_upcase_table, sym_downcase_table;
_PR VALUE sym_flatten_table, sym_block_status_hook;
DEFSYM(upcase_table, "upcase-table");
DEFSYM(downcase_table, "downcase-table");
DEFSYM(flatten_table, "flatten-table");
DEFSYM(block_status_hook, "block-status-hook");

_PR VALUE sym_inhibit_read_only;
DEFSYM(inhibit_read_only, "inhibit-read-only");

/* Some doc strings
::doc:upcase_table::
256-byte string holding translations to turn each character into its
upper-case equivalent.
::end::
::doc:downcase_table::
256-byte string holding translations to turn each character into its
lower-case equivalent.
::end::
::doc:flatten_table::
Translation table to convert newline characters to spaces.
::end::
::doc:inhibit_read_only::
When bound and non-nil this variable cancels the effect of the set-buffer-
read-only command; in that the buffer is always writeable. This is
intended to be bound while a command is executing that is allowed to
modify a buffer.
::end::
::doc:block_status_hook::
The hook called when the status of the block changes. It will be called
with a single argument, t if the block is now marked, nil if it isn't.
::end:: */


_PR VALUE cmd_insert(VALUE string, VALUE pos, VALUE buff);
DEFUN_INT("insert", cmd_insert, subr_insert, (VALUE string, VALUE pos, VALUE buff), V_Subr3, DOC_insert, "sString to insert:") /*
::doc:insert::
insert STRING [POS] [BUFFER]

Inserts STRING into BUFFER at POS. Returns the first position of the first
character after the end of the inserted text.
::end:: */
{
    DECLARE1(string, STRINGP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff)) && pad_pos(VTX(buff), pos))
    {
	pos = insert_string(VTX(buff), VSTR(string), STRING_LEN(string), pos);
	if(pos != LISP_NULL)
	    return pos;
    }
    return(sym_nil);
}

_PR VALUE cmd_delete_area(VALUE start, VALUE end, VALUE buff);
DEFUN_INT("delete-area", cmd_delete_area, subr_delete_area, (VALUE start, VALUE end, VALUE buff), V_Subr3, DOC_delete_area, "-m" DS_NL "M") /*
::doc:delete_area::
delete-area START-POS END-POS [BUFFER]

Deletes from START-POS up to (but not including) END-POS.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff))
       && pad_pos(VTX(buff), start)
       && pad_pos(VTX(buff), end)
       && check_section(VTX(buff), &start, &end))
    {
	delete_section(VTX(buff), start, end);
	return start;
    }
    return(sym_nil);
}

_PR VALUE cmd_copy_area(VALUE start, VALUE end, VALUE buff);
DEFUN("copy-area", cmd_copy_area, subr_copy_area, (VALUE start, VALUE end, VALUE buff), V_Subr3, DOC_copy_area) /*
::doc:copy_area::
copy-area START-POS END-POS [BUFFER]

Returns the string from START-POS up to END-POS.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(check_section(VTX(buff), &start, &end))
    {
	long tlen = section_length(VTX(buff), start, end) + 1;
	VALUE str = make_string(tlen);
	if(str)
	{
	    copy_section(VTX(buff), start, end, VSTR(str));
	    VSTR(str)[tlen - 1] = 0;
	    return(str);
	}
    }
    return(sym_nil);
}

_PR VALUE cmd_cut_area(VALUE start, VALUE end, VALUE buff);
DEFUN("cut-area", cmd_cut_area, subr_cut_area, (VALUE start, VALUE end, VALUE buff), V_Subr3, DOC_cut_area) /*
::doc:cut_area::
cut-area START-POS END-POS [BUFFER]

The same as `copy-area' except that the section of text copied (START-POS to
END-POS) is deleted from the file after being duplicated.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff))
       && pad_pos(VTX(buff), start)
       && pad_pos(VTX(buff), end)
       && check_section(VTX(buff), &start, &end))
    {
	/* Only one copy is made. */
	VALUE str = undo_push_deletion(VTX(buff), start, end);
	if(str)
	{
	    delete_section(VTX(buff), start, end);
	    return(str);
	}
    }
    return(sym_nil);
}

_PR VALUE cmd_block_toggle(void);
DEFUN_INT("block-toggle", cmd_block_toggle, subr_block_toggle, (void), V_Subr0, DOC_block_toggle, "") /*
::doc:block_toggle::
block-toggle
::end:: */
{
    VW *vw = curr_vw;
    switch(vw->vw_BlockStatus)
    {
	case 0:
	    vw->vw_BlockStatus = -1;
	    break;
	case 1:
	    vw->vw_BlockE = vw->vw_CursorPos;
	    vw->vw_BlockStatus = 0;
	    order_block(vw);
	    break;
	case 2:
	    vw->vw_BlockS = vw->vw_CursorPos;
	    vw->vw_BlockStatus = 0;
	    order_block(vw);
	    break;
	case -1:
	    vw->vw_BlockS = vw->vw_CursorPos;
	    vw->vw_BlockStatus = 1;
	    break;
    }
    cmd_call_hook(sym_block_status_hook, sym_nil, sym_nil);
    return(sym_t);
}

_PR VALUE cmd_block_start(VALUE pos);
DEFUN("block-start", cmd_block_start, subr_block_start, (VALUE pos), V_Subr1, DOC_block_start) /*
::doc:block_start::
block-start [POS]

Always returns the position of the block-start as it is, if POS is given
it is used as the new position of the start of the block.
::end:: */
{
    VW *vw = curr_vw;
    VALUE res;
    if(!vw->vw_BlockStatus || (vw->vw_BlockStatus == 1))
	res = vw->vw_BlockS;
    else
	res = sym_nil;
    if(POSP(pos) && check_line(vw->vw_Tx, pos))
    {
	switch(vw->vw_BlockStatus)
	{
	    GC_root gc_res;
	    case 0:
		vw->vw_BlockS = pos;
		order_block(vw);
		break;
	    case 2:
		vw->vw_BlockS = pos;
		vw->vw_BlockStatus = 0;
		order_block(vw);
		PUSHGC(gc_res, res);
		cmd_call_hook(sym_block_status_hook, sym_nil, sym_nil);
		POPGC;
		break;
	    case -1:
		vw->vw_BlockStatus = 1;
		/* FALL THROUGH */
	    case 1:
		vw->vw_BlockS = pos;
		break;
	}
    }
    return res;
}

_PR VALUE cmd_block_end(VALUE pos);
DEFUN("block-end", cmd_block_end, subr_block_end, (VALUE pos), V_Subr1, DOC_block_end) /*
::doc:block_end::
block-end [POS]

Always returns the position of the block-end as it is, if POS is given
it is used as the new position of the end of the block.
::end:: */
{
    VW *vw = curr_vw;
    VALUE res;
    if(!vw->vw_BlockStatus || (vw->vw_BlockStatus == 2))
	res = vw->vw_BlockE;
    else
	res = sym_nil;
    if(POSP(pos) && check_line(vw->vw_Tx, pos))
    {
	switch(vw->vw_BlockStatus)
	{
	    GC_root gc_res;
	    case 0:
		vw->vw_BlockE = pos;
		order_block(vw);
		break;
	    case 1:
		vw->vw_BlockE = pos;
		vw->vw_BlockStatus = 0;
		order_block(vw);
		PUSHGC(gc_res, res);
		cmd_call_hook(sym_block_status_hook, sym_nil, sym_nil);
		POPGC;
		break;
	    case -1:
		vw->vw_BlockStatus = 2;
		/* FALL THROUGH */
	    case 2:
		vw->vw_BlockE = pos;
		break;
	}
    }
    return res;
}

_PR VALUE cmd_block_kill(void);
DEFUN_INT("block-kill", cmd_block_kill, subr_block_kill, (void), V_Subr0, DOC_block_kill, "") /*
::doc:block_kill::
block-kill

Unmarks the block.
::end:: */
{
    VW *vw = curr_vw;
    if(vw->vw_BlockStatus == 0)
    {
	vw->vw_BlockStatus = -1;
	cmd_call_hook(sym_block_status_hook, sym_nil, sym_nil);
    }
    return(sym_t);
}

_PR VALUE cmd_blockp(void);
DEFUN("blockp", cmd_blockp, subr_blockp, (void), V_Subr0, DOC_blockp) /*
::doc:blockp::
blockp

Returns true if a block is currently marked.
::end:: */
{
    if(curr_vw->vw_BlockStatus == 0)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_translate_area(VALUE start, VALUE end, VALUE table, VALUE tx);
DEFUN("translate-area", cmd_translate_area, subr_translate_area, (VALUE start, VALUE end, VALUE table, VALUE tx), V_Subr4, DOC_translate_area) /*
::doc:translate_area:
translate-area START-POS END-POS TRANSLATION-TABLE [BUFFER]

Applies the TRANSLATION-TABLE to the text between START-POS and END-POS.
TRANSLATION-TABLE is a string, each character represents the translation
for an ascii character of that characters position in the string. If the
string is less than 256 chars long any undefined characters will remain
unchanged.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    DECLARE3(table, STRINGP);
    if(!read_only(VTX(tx)) && check_section(VTX(tx), &start, &end))
    {
	LINE *line = VTX(tx)->tx_Lines + VROW(start);
	long linenum = VROW(start), col;
	int tablen = STRING_LEN(table);
	register u_char *str;
	undo_record_modification(VTX(tx), start, end);
	flag_modification(VTX(tx), start, end);
	while(linenum < VROW(end))
	{
	    int llen = line->ln_Strlen - 1;
	    col = (linenum == VROW(start) ? VCOL(start) : 0);
	    str = line->ln_Line + col;
	    while(col++ < llen)
	    {
		register u_char c = *str;
		*str++ = (c < tablen) ? VSTR(table)[c] : c;
	    }
	    linenum++;
	    line++;
	}
	col = (linenum == VROW(start) ? VCOL(start) : 0);
	str = line->ln_Line + col;
	while(col++ < VCOL(end))
	{
	    register u_char c = *str;
	    *str++ = (c < tablen) ? VSTR(table)[c] : c;
	}
	return(sym_t);
    }
    return LISP_NULL;
}

_PR VALUE cmd_translate_string(VALUE string, VALUE table);
DEFUN("translate-string", cmd_translate_string, subr_translate_string, (VALUE string, VALUE table), V_Subr2, DOC_translate_string) /*
::doc:translate_string:
translate-string STRING TRANSLATION-TABLE

Applies the TRANSLATION-TABLE to each character in the string STRING.
TRANSLATION-TABLE is a string, each character represents the translation
for an ascii character of that characters position in the string. If the
string is less than 256 chars long any undefined characters will remain
unchanged.
Note that the STRING really is modified, no copy is made!
::end:: */
{
    int tablen, slen;
    register u_char *str;
    DECLARE1(string, STRINGP);
    DECLARE2(table, STRINGP);
    tablen = STRING_LEN(table);
    if(!STRING_WRITABLE_P(string))
	return(signal_arg_error(string, 1));
    str = VSTR(string);
    slen = STRING_LEN(string);
    while(slen-- > 0)
    {
	register u_char c = *str;
	*str++ = (c < tablen) ? VSTR(table)[c] : c;
    }
    return(string);
}

_PR VALUE cmd_get_char(VALUE pos, VALUE tx);
DEFUN("get-char", cmd_get_char, subr_get_char, (VALUE pos, VALUE tx), V_Subr2, DOC_get_char) /*
::doc:get_char::
get-char [POS] [BUFFER]

Returns the numerical value of the character at position POS in BUFFER. If no
character exists at that position, nil is returned.
::end:: */
{
    LINE *line;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(!check_line(VTX(tx), pos))
	return(sym_nil);
    line = VTX(tx)->tx_Lines + VROW(pos);
    if(VCOL(pos) >= line->ln_Strlen)
	return(sym_nil);
    else if(VCOL(pos) == line->ln_Strlen - 1)
    {
	if(VROW(pos) == VTX(tx)->tx_LogicalEnd - 1)
	    return(sym_nil);
	else
	    return(MAKE_INT('\n'));
    }
    else
	return(MAKE_INT(line->ln_Line[VCOL(pos)]));
}

_PR VALUE cmd_set_char(VALUE ch, VALUE pos, VALUE tx);
DEFUN_INT("set-char", cmd_set_char, subr_set_char, (VALUE ch, VALUE pos, VALUE tx), V_Subr3, DOC_set_char, "cCharacter:") /*
::doc:set_char::
set-char CHARACTER [POS] [BUFFER]

Sets the character at position POS in BUFFER to CHARACTER.
::end:: */
{
    /* FIXME: make this handle insertion of newlines */
    VALUE end;
    DECLARE1(ch, INTP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(!check_line(VTX(tx), pos))
	return(sym_nil);
    end = make_pos(VCOL(pos) + 1, VROW(pos));
    if(pad_pos(VTX(tx), end))
    {
	LINE *line = VTX(tx)->tx_Lines + VROW(pos);
	undo_record_modification(VTX(tx), pos, end);
	line->ln_Line[VCOL(pos)] = VINT(ch);
	flag_modification(VTX(tx), pos, end);
	return(ch);
    }
    return LISP_NULL;
}

_PR VALUE cmd_alpha_char_p(VALUE ch);
DEFUN("alpha-char-p", cmd_alpha_char_p, subr_alpha_char_p, (VALUE ch), V_Subr1, DOC_alpha_char_p) /*
::doc:alpha_char_p::
alpha-char-p CHAR

Returns t if CHAR is an alphabetic character.
::end:: */
{
    if(INTP(ch) && isalpha(VINT(ch)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_upper_case_p(VALUE ch);
DEFUN("upper-case-p", cmd_upper_case_p, subr_upper_case_p, (VALUE ch), V_Subr1, DOC_upper_case_p) /*
::doc:upper_case_p::
upper-case-p CHAR

Returns t if CHAR is upper case.
::end:: */
{
    if(INTP(ch) && isupper(VINT(ch)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_lower_case_p(VALUE ch);
DEFUN("lower-case-p", cmd_lower_case_p, subr_lower_case_p, (VALUE ch), V_Subr1, DOC_lower_case_p) /*
::doc:lower_case_p::
lower-case-p CHAR

Returns t if CHAR is lower case.
::end:: */
{
    if(INTP(ch) && islower(VINT(ch)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_digit_char_p(VALUE ch);
DEFUN("digit-char-p", cmd_digit_char_p, subr_digit_char_p, (VALUE ch), V_Subr1, DOC_digit_char_p) /*
::doc:digit_char_p::
digit-char-p CHAR

Returns t if CHAR is a digit.
::end:: */
{
    if(INTP(ch) && isdigit(VINT(ch)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_alphanumericp(VALUE ch);
DEFUN("alphanumericp", cmd_alphanumericp, subr_alphanumericp, (VALUE ch), V_Subr1, DOC_alphanumericp) /*
::doc:alphanumericp::
alphanumericp CHAR

Returns t if CHAR is alpha-numeric.
::end:: */
{
    if(INTP(ch) && isalnum(VINT(ch)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_space_char_p(VALUE ch);
DEFUN("space-char-p", cmd_space_char_p, subr_space_char_p, (VALUE ch), V_Subr1, DOC_space_char_p) /*
::doc:space_char_p::
space-char-p CHAR

Returns t if CHAR is whitespace.
::end:: */
{
    if(INTP(ch) && isspace(VINT(ch)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_char_upcase(VALUE ch);
DEFUN("char-upcase", cmd_char_upcase, subr_char_upcase, (VALUE ch), V_Subr1, DOC_char_upcase) /*
::doc:char_upcase::
char-upcase CHAR

Returns the upper-case equivalent of CHAR.
::end:: */
{
    DECLARE1(ch, INTP);
    return(MAKE_INT(toupper(VINT(ch))));
}

_PR VALUE cmd_char_downcase(VALUE ch);
DEFUN("char-downcase", cmd_char_downcase, subr_char_downcase, (VALUE ch), V_Subr1, DOC_char_downcase) /*
::doc:char_downcase::
char-downcase CHAR

Returns the lower-case equivalent of CHAR.
::end:: */
{
    DECLARE1(ch, INTP);
    return(MAKE_INT(toupper(VINT(ch))));
}

_PR VALUE cmd_posp(VALUE arg);
DEFUN("posp", cmd_posp, subr_posp, (VALUE arg), V_Subr1, DOC_posp) /*
::doc:posp::
posp ARG

Returns t if ARG is a position object.
::end:: */
{
    if(POSP(arg))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_cursor_pos(void);
DEFUN("cursor-pos", cmd_cursor_pos, subr_cursor_pos, (void), V_Subr0, DOC_cursor_pos) /*
::doc:cursor_pos::
cursor-pos

Returns the position of the cursor in the current window.
::end:: */
{
    return curr_vw->vw_CursorPos;
}

_PR VALUE cmd_empty_line_p(VALUE pos, VALUE tx);
DEFUN("empty-line-p", cmd_empty_line_p, subr_empty_line_p, (VALUE pos, VALUE tx), V_Subr2, DOC_empty_line_p) /*
::doc:empty_line_p::
empty-line-p [POS] [BUFFER]

Returns t if the line pointer to by POS (or the cursor) in BUFFER is
empty, ie, blank or only containing spaces.
::end:: */
{
    VW *vw = curr_vw;
    LINE *line;
    if(!POSP(pos))
	pos = vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(vw->vw_Tx);
    line = VTX(tx)->tx_Lines + VROW(pos);
    if(line->ln_Strlen == 1)
	return(sym_t);
    else
    {
	u_char *s = line->ln_Line;
	while(*s && isspace(*s))
	    s++;
	if(!(*s))
	    return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_indent_pos(VALUE pos, VALUE tx);
DEFUN("indent-pos", cmd_indent_pos, subr_indent_pos, (VALUE pos, VALUE tx), V_Subr2, DOC_indent_pos) /*
::doc:indent_pos::
indent-pos [POS] [BUFFER]

Returns the glyph position of the first non-space character in the line
pointed to by POS (or the cursor), in BUFFER.
::end:: */
{
    VW *vw = curr_vw;
    long len;
    u_char *line;
    if(!BUFFERP(tx))
	tx = VAL(vw->vw_Tx);
    if(POSP(pos) && check_line(VTX(tx), pos))
	;
    else
	pos = vw->vw_CursorPos;
    line = VTX(tx)->tx_Lines[VROW(pos)].ln_Line;
    for(len = 0; *line && isspace(*line); len++, line++)
	;
    len = glyph_col(VTX(tx), len, VROW(pos));
    return make_pos(len, VROW(pos));
}

_PR VALUE cmd_set_indent_pos(VALUE indpos, VALUE tx, VALUE spaces_p);
DEFUN("set-indent-pos", cmd_set_indent_pos, subr_set_indent_pos, (VALUE indpos, VALUE tx, VALUE spaces_p), V_Subr3, DOC_set_indent_pos) /*
::doc:set_indent_pos::
set-indent-pos POS [BUFFER] [ONLY-SPACES]

Sets the indentation of the line pointed to by POS to the column pointed
to by POS by putting the optimal sequence of TAB and SPC characters at the
start of the line.

If ONLY-SPACES in non-nil no tab characters are used.
::end:: */
{
    DECLARE1(indpos, POSP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if((!read_only(VTX(tx))) && check_line(VTX(tx), indpos))
    {
	LINE *line = VTX(tx)->tx_Lines + VROW(indpos);
	u_char *s = line->ln_Line;
	VALUE pos = indpos;
	long oldind, diff;
	long tabs, spaces;
	while(*s && isspace(*s))
	    s++;
	oldind = s - line->ln_Line;
	if(NILP(spaces_p))
	{
	    tabs = VCOL(pos) / VTX(tx)->tx_TabSize;
	    spaces = VCOL(pos) % VTX(tx)->tx_TabSize;
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
	    VALUE end = make_pos(diff, VROW(pos));
	    undo_record_deletion(VTX(tx), pos, end);
	    delete_chars(VTX(tx), VCOL(pos), VROW(pos), diff);
	    flag_deletion(VTX(tx), pos, end);
	    end = make_pos(tabs + spaces, VROW(end));
	    undo_record_modification(VTX(tx), pos, end);
	    memset(line->ln_Line, '\t', tabs);
	    memset(line->ln_Line + tabs, ' ', spaces);
	    flag_modification(VTX(tx), pos, end);
	}
	else if(diff < 0)
	{
	    VALUE end;
	    diff = -diff;
	    end = make_pos(diff, VROW(pos));
	    insert_gap(VTX(tx), diff, VCOL(pos), VROW(pos));
	    undo_record_insertion(VTX(tx), pos, end);
	    flag_insertion(VTX(tx), pos, end);
	    pos = make_pos(diff, VROW(pos));
	    end = make_pos(tabs + spaces, VROW(end));
	    undo_record_modification(VTX(tx), pos, end);
	    memset(line->ln_Line, '\t', tabs);
	    memset(line->ln_Line + tabs, ' ', spaces);
	    flag_modification(VTX(tx), pos, end);
	}
	else
	{
	    u_char *s = line->ln_Line;
	    long i;
	    VALUE end = make_pos(tabs + spaces, VROW(pos));
	    for(i = 0; i < tabs; i++)
	    {
		if(*s++ != '\t')
		{
		    undo_record_modification(VTX(tx), pos, end);
		    memset(line->ln_Line, '\t', tabs);
		    memset(line->ln_Line + tabs, ' ', spaces);
		    flag_modification(VTX(tx), pos, end);
		    return indpos;
		}
	    }
	    for(i = 0; i < spaces; i++)
	    {
		if(*s++ != ' ')
		{
		    pos = make_pos(tabs, VROW(pos));
		    undo_record_modification(VTX(tx), pos, end);
		    memset(line->ln_Line + tabs, ' ', spaces);
		    flag_modification(VTX(tx), pos, end);
		    return indpos;
		}
	    }
	    /* No modifications required. */
	}
	return indpos;
    }
    return sym_nil;
}

_PR VALUE cmd_indent_to(VALUE col, VALUE spaces_p);
DEFUN_INT("indent-to", cmd_indent_to, subr_indent_to, (VALUE col, VALUE spaces_p), V_Subr2, DOC_indent_to, "NIndent to column:") /*
::doc:indent_to::
indent-to COLUMN [ONLY-SPACES]

Inserts enough tabs and spaces to move the cursor to glyph column COLUMN.
If ONLY-SPACES is non-nil no tabs are used.
COLUMN counts from zero.
::end:: */
{
    VW *vw = curr_vw;
    TX *tx = vw->vw_Tx;
    DECLARE1(col, INTP);
    if(!read_only(tx) && pad_cursor(vw))
    {
	int spaces, tabs;
	long curr_col, dest_col;
        curr_col = get_cursor_column(vw);
        dest_col = VINT(col);
        if(dest_col <= curr_col)
            return(sym_t);
	if(NILP(spaces_p))
	{
	    tabs = (dest_col / tx->tx_TabSize) - (curr_col / tx->tx_TabSize);
	    if(tabs == 0)
		spaces = dest_col - curr_col;
	    else
		spaces = dest_col - ((dest_col / tx->tx_TabSize) * tx->tx_TabSize);
	}
	else
	{
	    tabs = 0;
	    spaces = dest_col - curr_col;
	}
	if(spaces + tabs > 0)
	{
	    VALUE tmp = vw->vw_CursorPos;
	    if(insert_gap(tx, spaces + tabs, VCOL(tmp), VROW(tmp)))
	    {
		u_char *line = tx->tx_Lines[VROW(tmp)].ln_Line;
		memset(line + VCOL(tmp), '\t', tabs);
		memset(line + VCOL(tmp) + tabs, ' ', spaces);
		undo_record_insertion(tx, tmp, vw->vw_CursorPos);
		flag_insertion(tx, tmp, vw->vw_CursorPos);
		return(col);
	    }
	}
	return(sym_nil);
    }
    return LISP_NULL;
}
	    
_PR VALUE cmd_clear_buffer(VALUE tx);
DEFUN_INT("clear-buffer", cmd_clear_buffer, subr_clear_buffer, (VALUE tx), V_Subr1, DOC_clear_buffer, "") /*
::doc:clear_buffer::
clear-buffer [BUFFER]

Remove all text from BUFFER, leaving just one empty line. Also removes
any restriction on the buffer.
::end:: */
{
    VALUE start, end;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    cmd_unrestrict_buffer(tx);
    start = make_pos(0, 0);
    end = cmd_end_of_buffer(VAL(tx), sym_t);
    undo_record_deletion(VTX(tx), start, end);
    if(clear_line_list(VTX(tx)))
    {
	reset_all_views(VTX(tx));
	return tx;
    }
    return sym_nil;
}

_PR VALUE cmd_pos_to_offset(VALUE pos, VALUE tx);
DEFUN("pos-to-offset", cmd_pos_to_offset, subr_pos_to_offset, (VALUE pos, VALUE tx), V_Subr2, DOC_pos_to_offset) /*
::doc:pos_to_offset::
pos-to-offset [POS] [BUFFER]

Returns the number of characters (counting from zero) that POS (or the cursor)
is from the beginning of the buffer.
::end:: */
{
    LINE *line;
    long offset, line_num;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(check_pos(VTX(tx), pos))
    {
	line = VTX(tx)->tx_Lines;
	for(offset = line_num = 0; line_num < VROW(pos); line++, line_num++)
	    offset += line->ln_Strlen; /* includes the theoretical '\n' */
	offset += VCOL(pos);
	return MAKE_INT(offset);
    }
    else
	return LISP_NULL;
}

_PR VALUE cmd_offset_to_pos(VALUE voffset, VALUE tx);
DEFUN("offset-to-pos", cmd_offset_to_pos, subr_offset_to_pos, (VALUE voffset, VALUE tx), V_Subr2, DOC_offset_to_pos) /*
::doc:offset_to_pos::
offset-to-pos OFFSET [BUFFER]

Returns the position which is OFFSET characters from the start of the buffer.
::end:: */
{
    long offset;
    long col, row;
    LINE *line;
    DECLARE1(voffset, INTP);
    offset = VINT(voffset);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    row = 0;
    line = VTX(tx)->tx_Lines;
    while(offset >= line->ln_Strlen)
    {
	offset -= line->ln_Strlen;
	row++;
	line++;
    }
    col = offset;
    return make_pos(col, row);
}

void
edit_init(void)
{
    int i;
    ADD_SUBR_INT(subr_insert);
    ADD_SUBR_INT(subr_delete_area);
    ADD_SUBR(subr_copy_area);
    ADD_SUBR(subr_cut_area);
    ADD_SUBR_INT(subr_block_toggle);
    ADD_SUBR(subr_block_start);
    ADD_SUBR(subr_block_end);
    ADD_SUBR_INT(subr_block_kill);
    ADD_SUBR(subr_blockp);
    ADD_SUBR(subr_translate_area);
    ADD_SUBR(subr_translate_string);
    ADD_SUBR(subr_get_char);
    ADD_SUBR_INT(subr_set_char);
    ADD_SUBR(subr_alpha_char_p);
    ADD_SUBR(subr_upper_case_p);
    ADD_SUBR(subr_lower_case_p);
    ADD_SUBR(subr_digit_char_p);
    ADD_SUBR(subr_alphanumericp);
    ADD_SUBR(subr_space_char_p);
    ADD_SUBR(subr_char_upcase);
    ADD_SUBR(subr_char_downcase);
    ADD_SUBR(subr_posp);
    ADD_SUBR(subr_cursor_pos);
    ADD_SUBR(subr_empty_line_p);
    ADD_SUBR(subr_indent_pos);
    ADD_SUBR(subr_set_indent_pos);
    ADD_SUBR_INT(subr_indent_to);
    ADD_SUBR_INT(subr_clear_buffer);
    ADD_SUBR(subr_offset_to_pos);
    ADD_SUBR(subr_pos_to_offset);

    INTERN(upcase_table); DOC(upcase_table);
    VSYM(sym_upcase_table)->value = make_string(257);
    INTERN(downcase_table); DOC(downcase_table);
    VSYM(sym_downcase_table)->value = make_string(257);
    for(i = 0; i < 256; i++)
    {
	VSTR(VSYM(sym_upcase_table)->value)[i] = toupper(i);
	VSTR(VSYM(sym_downcase_table)->value)[i] = tolower(i);
    }
    VSTR(VSYM(sym_upcase_table)->value)[256] = 0;
    VSTR(VSYM(sym_downcase_table)->value)[256] = 0;

    INTERN(flatten_table); DOC(flatten_table);
    VSYM(sym_flatten_table)->value = make_string(12);
    for(i = 0; i < 10; i++)
	VSTR(VSYM(sym_flatten_table)->value)[i] = i;
    VSTR(VSYM(sym_flatten_table)->value)[10] = ' ';
    VSTR(VSYM(sym_flatten_table)->value)[11] = 0;
    
    INTERN(inhibit_read_only); DOC(inhibit_read_only);
    INTERN(block_status_hook); DOC(block_status_hook);
}
