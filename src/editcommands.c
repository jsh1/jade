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

static VALUE sym_upcase_table, sym_downcase_table;

_PR VALUE sym_inhibit_read_only;
VALUE sym_inhibit_read_only;

_PR VALUE sym_block_status_hook;
VALUE sym_block_status_hook;

/* Some doc strings
::doc:upcase_table::
256-byte string holding translations to turn each character into its
upper-case equivalent.
::end::
::doc:downcase_table::
256-byte string holding translations to turn each character into its
lower-case equivalent.
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

_PR VALUE cmd_split_line(void);
DEFUN_INT("split-line", cmd_split_line, subr_split_line, (void), V_Subr0, DOC_split_line, "") /*
::doc:split_line::
split-line

Splits the line into two at the cursor position.
::end:: */
{
    VW *vw = curr_vw;
    TX *tx = vw->vw_Tx;
    if(!read_only(tx))
    {
	POS old = vw->vw_CursorPos;
	if(pad_cursor(vw))
	{
	    if(split_line(tx, &vw->vw_CursorPos))
	    {
		undo_record_insertion(tx, &old, &vw->vw_CursorPos);
		flag_insertion(tx, &old, &vw->vw_CursorPos);
		return(sym_t);
	    }
	}
    }
    return(sym_nil);
}

_PR VALUE cmd_insert(VALUE string, VALUE pos, VALUE buff);
DEFUN_INT("insert", cmd_insert, subr_insert, (VALUE string, VALUE lpos, VALUE buff), V_Subr3, DOC_insert, "sString to insert:") /*
::doc:insert::
insert STRING [POS] [BUFFER]

Inserts STRING into BUFFER at POS. Returns the first position of the first
character after the end of the inserted text.
::end:: */
{
    POS pos;
    DECLARE1(string, STRINGP);
    if(POSP(lpos))
	pos = VPOS(lpos);
    else
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff)) && pad_pos(VTX(buff), &pos))
    {
	if(insert_string(VTX(buff), VSTR(string), STRING_LEN(string), &pos))
	    return(make_lpos(&pos));
    }
    return(sym_nil);
}

_PR VALUE cmd_delete_area(VALUE start, VALUE end, VALUE buff);
DEFUN("delete-area", cmd_delete_area, subr_delete_area, (VALUE lstart, VALUE lend, VALUE buff), V_Subr3, DOC_delete_area) /*
::doc:delete_area::
delete-area START-POS END-POS [BUFFER]

Deletes from START-POS up to (but not including) END-POS.
::end:: */
{
    POS start, end;
    DECLARE1(lstart, POSP);
    DECLARE2(lend, POSP);
    start = VPOS(lstart);
    end = VPOS(lend);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff)) && pad_pos(VTX(buff), &start)
       && pad_pos(VTX(buff), &end) && check_section(VTX(buff), &start, &end))
    {
	delete_section(VTX(buff), &start, &end);
	return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_copy_area(VALUE lstart, VALUE lend, VALUE buff);
DEFUN("copy-area", cmd_copy_area, subr_copy_area, (VALUE lstart, VALUE lend, VALUE buff), V_Subr3, DOC_copy_area) /*
::doc:copy_area::
copy-area START-POS END-POS [BUFFER]

Returns the string from START-POS up to END-POS.
::end:: */
{
    POS start, end;
    DECLARE1(lstart, POSP);
    DECLARE2(lend, POSP);
    start = VPOS(lstart);
    end = VPOS(lend);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(check_section(VTX(buff), &start, &end))
    {
	long tlen = section_length(VTX(buff), &start, &end) + 1;
	VALUE str = make_string(tlen);
	if(str)
	{
	    copy_section(VTX(buff), &start, &end, VSTR(str));
	    VSTR(str)[tlen - 1] = 0;
	    return(str);
	}
    }
    return(sym_nil);
}

_PR VALUE cmd_cut_area(VALUE lstart, VALUE lend, VALUE buff);
DEFUN("cut-area", cmd_cut_area, subr_cut_area, (VALUE lstart, VALUE lend, VALUE buff), V_Subr3, DOC_cut_area) /*
::doc:cut_area::
cut-area START-POS END-POS [BUFFER]

The same as `copy-area' except that the section of text copied (START-POS to
END-POS) is deleted from the file after being duplicated.
::end:: */
{
    POS start, end;
    DECLARE1(lstart, POSP);
    DECLARE2(lend, POSP);
    start = VPOS(lstart);
    end = VPOS(lend);
    if(!BUFFERP(buff))
	buff = VAL(curr_vw->vw_Tx);
    if(!read_only(VTX(buff)) && pad_pos(VTX(buff), &start)
       && pad_pos(VTX(buff), &end) && check_section(VTX(buff), &start, &end))
    {
	/* Only one copy is made. */
	VALUE str = undo_push_deletion(VTX(buff), &start, &end);
	if(str)
	{
	    delete_section(VTX(buff), &start, &end);
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
	    set_block_refresh(vw);
	    break;
	case 1:
	    vw->vw_BlockE = vw->vw_CursorPos;
	    vw->vw_BlockStatus = 0;
	    order_block(vw);
	    set_block_refresh(vw);
	    break;
	case 2:
	    vw->vw_BlockS = vw->vw_CursorPos;
	    vw->vw_BlockStatus = 0;
	    order_block(vw);
	    set_block_refresh(vw);
	    break;
	case -1:
	    vw->vw_BlockS = vw->vw_CursorPos;
	    vw->vw_BlockStatus = 1;
	    break;
    }
    cmd_eval_hook2(sym_block_status_hook,
		   vw->vw_BlockStatus == 0 ? sym_t : sym_nil);
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
	res = make_lpos(&vw->vw_BlockS);
    else
	res = sym_nil;
    if(POSP(pos) && check_line(vw->vw_Tx, &VPOS(pos)))
    {
	switch(vw->vw_BlockStatus)
	{
	    GCVAL gcv_res;
	    case 0:
		set_block_refresh(vw);
		vw->vw_BlockS = VPOS(pos);
		order_block(vw);
		set_block_refresh(vw);
		break;
	    case 2:
		vw->vw_BlockS = VPOS(pos);
		vw->vw_BlockStatus = 0;
		order_block(vw);
		set_block_refresh(vw);
		PUSHGC(gcv_res, res);
		cmd_eval_hook2(sym_block_status_hook,
			       vw->vw_BlockStatus == 0 ? sym_t : sym_nil);
		POPGC;
		break;
	    case -1:
		vw->vw_BlockStatus = 1;
		/* FALL THROUGH */
	    case 1:
		vw->vw_BlockS = VPOS(pos);
		break;
	}
    }
    return(res);
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
	res = make_lpos(&vw->vw_BlockE);
    else
	res = sym_nil;
    if(POSP(pos) && check_line(vw->vw_Tx, &VPOS(pos)))
    {
	switch(vw->vw_BlockStatus)
	{
	    GCVAL gcv_res;
	    case 0:
		set_block_refresh(vw);
		vw->vw_BlockE = VPOS(pos);
		order_block(vw);
		set_block_refresh(vw);
		break;
	    case 1:
		vw->vw_BlockE = VPOS(pos);
		vw->vw_BlockStatus = 0;
		order_block(vw);
		set_block_refresh(vw);
		PUSHGC(gcv_res, res);
		cmd_eval_hook2(sym_block_status_hook,
			       vw->vw_BlockStatus == 0 ? sym_t : sym_nil);
		POPGC;
		break;
	    case -1:
		vw->vw_BlockStatus = 2;
		/* FALL THROUGH */
	    case 2:
		vw->vw_BlockE = VPOS(pos);
		break;
	}
    }
    return(res);
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
	set_block_refresh(vw);
	vw->vw_BlockStatus = -1;
	cmd_eval_hook2(sym_block_status_hook,
		       vw->vw_BlockStatus == 0 ? sym_t : sym_nil);
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

_PR VALUE cmd_translate_area(VALUE vstart, VALUE vend, VALUE table, VALUE tx);
DEFUN("translate-area", cmd_translate_area, subr_translate_area, (VALUE vstart, VALUE vend, VALUE table, VALUE tx), V_Subr4, DOC_translate_area) /*
::doc:translate_area:
translate-area START-POS END-POS TRANSLATION-TABLE [BUFFER]

Applies the TRANSLATION-TABLE to the text between START-POS and END-POS.
TRANSLATION-TABLE is a string, each character represents the translation
for an ascii character of that characters position in the string. If the
string is less than 256 chars long any undefined characters will remain
unchanged.
::end:: */
{
    POS start, end;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    DECLARE1(vstart, POSP);
    DECLARE2(vend, POSP);
    DECLARE3(table, STRINGP);
    start = VPOS(vstart);
    end = VPOS(vend);
    if(!read_only(VTX(tx)) && check_section(VTX(tx), &start, &end))
    {
	LINE *line = VTX(tx)->tx_Lines + start.pos_Line;
	int tablen = STRING_LEN(table);
	register u_char *str;
	undo_record_modification(VTX(tx), &start, &end);
	flag_modification(VTX(tx), &start, &end);
	while(start.pos_Line < end.pos_Line)
	{
	    int llen = line->ln_Strlen - 1;
	    str = line->ln_Line + start.pos_Col;
	    while(start.pos_Col++ < llen)
	    {
		register u_char c = *str;
		*str++ = (c < tablen) ? VSTR(table)[c] : c;
	    }
	    start.pos_Col = 0;
	    start.pos_Line++;
	    line++;
	}
	str = line->ln_Line + start.pos_Col;
	while(start.pos_Col++ < end.pos_Col)
	{
	    register u_char c = *str;
	    *str++ = (c < tablen) ? VSTR(table)[c] : c;
	}
	return(sym_t);
    }
    return(NULL);
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
    if(!STRING_WRITEABLE_P(string))
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

_PR VALUE cmd_get_char(VALUE vpos, VALUE tx);
DEFUN("get-char", cmd_get_char, subr_get_char, (VALUE vpos, VALUE tx), V_Subr2, DOC_get_char) /*
::doc:get_char::
get-char [POS] [BUFFER]

Returns the numerical value of the character at position POS in BUFFER. If no
character exists at that position, nil is returned.
::end:: */
{
    POS pos;
    LINE *line;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(vpos))
	pos = VPOS(vpos);
    else
	pos = *(get_tx_cursor(VTX(tx)));
    if(!check_line(VTX(tx), &pos))
	return(sym_nil);
    line = VTX(tx)->tx_Lines + pos.pos_Line;
    if(pos.pos_Col >= line->ln_Strlen)
	return(sym_nil);
    else if(pos.pos_Col == line->ln_Strlen - 1)
    {
	if(pos.pos_Line == VTX(tx)->tx_LogicalEnd - 1)
	    return(sym_nil);
	else
	    return(make_number('\n'));
    }
    else
	return(make_number(line->ln_Line[pos.pos_Col]));
}

_PR VALUE cmd_set_char(VALUE ch, VALUE vpos, VALUE tx);
DEFUN_INT("set-char", cmd_set_char, subr_set_char, (VALUE ch, VALUE vpos, VALUE tx), V_Subr3, DOC_set_char, "cCharacter:") /*
::doc:set_char::
set-char CHARACTER [POS] [BUFFER]

Sets the character at position POS in BUFFER to CHARACTER.
::end:: */
{
    POS pos, end;
    DECLARE1(ch, CHARP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(vpos))
	pos = VPOS(vpos);
    else
	pos = *(get_tx_cursor(VTX(tx)));
    if(!check_line(VTX(tx), &pos))
	return(sym_nil);
    end.pos_Col = pos.pos_Col + 1;
    end.pos_Line = pos.pos_Line;
    if(pad_pos(VTX(tx), &end))
    {
	LINE *line = VTX(tx)->tx_Lines + pos.pos_Line;
	undo_record_modification(VTX(tx), &pos, &end);
	line->ln_Line[pos.pos_Col] = VCHAR(ch);
	flag_modification(VTX(tx), &pos, &end);
	return(ch);
    }
    return(NULL);
}

_PR VALUE cmd_alpha_char_p(VALUE ch);
DEFUN("alpha-char-p", cmd_alpha_char_p, subr_alpha_char_p, (VALUE ch), V_Subr1, DOC_alpha_char_p) /*
::doc:alpha_char_p::
alpha-char-p CHAR

Returns t if CHAR is an alphabetic character.
::end:: */
{
    if(CHARP(ch) && isalpha(VCHAR(ch)))
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
    if(CHARP(ch) && isupper(VCHAR(ch)))
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
    if(CHARP(ch) && islower(VCHAR(ch)))
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
    if(CHARP(ch) && isdigit(VCHAR(ch)))
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
    if(CHARP(ch) && isalnum(VCHAR(ch)))
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
    if(CHARP(ch) && isspace(VCHAR(ch)))
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
    DECLARE1(ch, CHARP);
    return(make_number(toupper(VCHAR(ch))));
}

_PR VALUE cmd_char_downcase(VALUE ch);
DEFUN("char-downcase", cmd_char_downcase, subr_char_downcase, (VALUE ch), V_Subr1, DOC_char_downcase) /*
::doc:char_downcase::
char-downcase CHAR

Returns the lower-case equivalent of CHAR.
::end:: */
{
    DECLARE1(ch, CHARP);
    return(make_number(toupper(VCHAR(ch))));
}

_PR VALUE cmd_pos_line(VALUE pos);
DEFUN("pos-line", cmd_pos_line, subr_pos_line, (VALUE pos), V_Subr1, DOC_pos_line) /*
::doc:pos_line::
pos-line POS

Returns the line number which POS points to.
::end:: */
{
    DECLARE1(pos, POSP);
    return(make_number(VPOS(pos).pos_Line));
}

_PR VALUE cmd_pos_col(VALUE pos);
DEFUN("pos-col", cmd_pos_col, subr_pos_col, (VALUE pos), V_Subr1, DOC_pos_col) /*
::doc:pos_col::
pos-col POS

Return the column number which POS points to.
::end:: */
{
    DECLARE1(pos, POSP);
    return(make_number(VPOS(pos).pos_Col));
}

_PR VALUE cmd_set_pos_line(VALUE pos, VALUE line);
DEFUN("set-pos-line", cmd_set_pos_line, subr_set_pos_line, (VALUE pos, VALUE line), V_Subr2, DOC_set_pos_line) /*
::doc:set_pos_line::
set-pos-line POS LINE

Sets the line number of POS to LINE.
::end:: */
{
    DECLARE1(pos, POSP);
    DECLARE2(line, NUMBERP);
    VPOS(pos).pos_Line = VNUM(line);
    return(line);
}

_PR VALUE cmd_set_pos_col(VALUE pos, VALUE col);
DEFUN("set-pos-col", cmd_set_pos_col, subr_set_pos_col, (VALUE pos, VALUE col), V_Subr2, DOC_set_pos_col) /*
::doc:set_pos_col::
set-pos-col POS COL

Sets the column number of POS to COL.
::end:: */
{
    DECLARE1(pos, POSP);
    DECLARE2(col, NUMBERP);
    VPOS(pos).pos_Col = VNUM(col);
    return(col);
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
    return(make_lpos(&curr_vw->vw_CursorPos));
}

_PR VALUE cmd_empty_line_p(VALUE lpos, VALUE tx);
DEFUN("empty-line-p", cmd_empty_line_p, subr_empty_line_p, (VALUE lpos, VALUE tx), V_Subr2, DOC_empty_line_p) /*
::doc:empty_line_p::
empty-line-p [POS] [BUFFER]

Returns t if the line pointer to by POS (or the cursor) in BUFFER is
empty, ie, blank or only containing spaces.
::end:: */
{
    VW *vw = curr_vw;
    POS pos;
    LINE *line;
    if(POSP(lpos))
	pos = VPOS(lpos);
    else
	pos = vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(vw->vw_Tx);
    line = VTX(tx)->tx_Lines + pos.pos_Line;
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

_PR VALUE cmd_indent_pos(VALUE lpos, VALUE tx);
DEFUN("indent-pos", cmd_indent_pos, subr_indent_pos, (VALUE lpos, VALUE tx), V_Subr2, DOC_indent_pos) /*
::doc:indent_pos::
indent-pos [POS] [BUFFER]

Returns the glyph position of the first non-space character in the line
pointed to by POS (or the cursor), in BUFFER.
::end:: */
{
    VW *vw = curr_vw;
    POS pos;
    long len;
    u_char *line;
    if(!BUFFERP(tx))
	tx = VAL(vw->vw_Tx);
    if(POSP(lpos) && check_line(VTX(tx), &VPOS(lpos)))
	pos = VPOS(lpos);
    else
	pos = vw->vw_CursorPos;
    line = VTX(tx)->tx_Lines[pos.pos_Line].ln_Line;
    for(len = 0; *line && isspace(*line); len++, line++)
	;
    len = glyph_col(VTX(tx), len, pos.pos_Line);
    return(make_lpos2(len, pos.pos_Line));
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
    if((!read_only(VTX(tx))) && check_line(VTX(tx), &VPOS(indpos)))
    {
	LINE *line = VTX(tx)->tx_Lines + VPOS(indpos).pos_Line;
	u_char *s = line->ln_Line;
	POS pos = VPOS(indpos), end;
	long oldind, diff;
	long tabs, spaces;
	while(*s && isspace(*s))
	    s++;
	oldind = s - line->ln_Line;
	if(NILP(spaces_p))
	{
	    tabs = pos.pos_Col / VTX(tx)->tx_TabSize;
	    spaces = pos.pos_Col % VTX(tx)->tx_TabSize;
	}
	else
	{
	    tabs = 0;
	    spaces = pos.pos_Col;
	}
	diff = oldind - (tabs + spaces);
	pos.pos_Col = 0;
	end.pos_Col = diff;
	end.pos_Line = pos.pos_Line;
	if(diff > 0)
	{
	    undo_record_deletion(VTX(tx), &pos, &end);
	    delete_chars(VTX(tx), &pos, diff);
	    flag_deletion(VTX(tx), &pos, &end);
	    end.pos_Col = tabs + spaces;
	    undo_record_modification(VTX(tx), &pos, &end);
	    memset(line->ln_Line, '\t', tabs);
	    memset(line->ln_Line + tabs, ' ', spaces);
	    flag_modification(VTX(tx), &pos, &end);
	}
	else if(diff < 0)
	{
	    diff = -diff;
	    end.pos_Col = diff;
	    insert_gap(VTX(tx), diff, &pos);
	    undo_record_insertion(VTX(tx), &pos, &end);
	    flag_insertion(VTX(tx), &pos, &end);
	    pos.pos_Col = diff;
	    end.pos_Col = tabs + spaces;
	    memset(line->ln_Line, '\t', tabs);
	    memset(line->ln_Line + tabs, ' ', spaces);
	    undo_record_modification(VTX(tx), &pos, &end);
	    flag_modification(VTX(tx), &pos, &end);
	}
	else
	{
	    u_char *s = line->ln_Line;
	    long i;
	    end.pos_Col = tabs + spaces;
	    for(i = 0; i < tabs; i++)
	    {
		if(*s++ != '\t')
		{
		    undo_record_modification(VTX(tx), &pos, &end);
		    memset(line->ln_Line, '\t', tabs);
		    memset(line->ln_Line + tabs, ' ', spaces);
		    flag_modification(VTX(tx), &pos, &end);
		    return(indpos);
		}
	    }
	    for(i = 0; i < spaces; i++)
	    {
		if(*s++ != ' ')
		{
		    pos.pos_Col = tabs;
		    undo_record_modification(VTX(tx), &pos, &end);
		    memset(line->ln_Line + tabs, ' ', spaces);
		    flag_modification(VTX(tx), &pos, &end);
		    return(indpos);
		}
	    }
	    /* No modifications required. */
	}
	return(indpos);
    }
    return(sym_nil);
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
    DECLARE1(col, NUMBERP);
    if(!read_only(tx) && pad_cursor(vw))
    {
	int spaces, tabs;
	long curr_col, dest_col;
        calc_cursor_offset(vw);
        curr_col = vw->vw_LastCursorOffset;
        dest_col = VNUM(col);
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
	    POS tmp = vw->vw_CursorPos;
	    if(insert_gap(tx, spaces + tabs, &tmp))
	    {
		u_char *line = tx->tx_Lines[tmp.pos_Line].ln_Line;
		memset(line + tmp.pos_Col, '\t', tabs);
		memset(line + tmp.pos_Col + tabs, ' ', spaces);
		undo_record_insertion(tx, &tmp, &vw->vw_CursorPos);
		flag_insertion(tx, &tmp, &vw->vw_CursorPos);
		return(col);
	    }
	}
	return(sym_nil);
    }
    return(NULL);
}
	    
_PR VALUE cmd_clear_buffer(VALUE tx);
DEFUN_INT("clear-buffer", cmd_clear_buffer, subr_clear_buffer, (VALUE tx), V_Subr1, DOC_clear_buffer, "") /*
::doc:clear_buffer::
clear-buffer [BUFFER]

Remove all text from BUFFER, leaving just one empty line. Also removes
any restriction on the buffer.
::end:: */
{
    POS start, end;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    cmd_unrestrict_buffer(tx);
    start.pos_Col = start.pos_Line = 0;
    end.pos_Line = VTX(tx)->tx_NumLines - 1;
    end.pos_Col = VTX(tx)->tx_Lines[end.pos_Line].ln_Strlen - 1;
    undo_record_deletion(VTX(tx), &start, &end);
    if(clear_line_list(VTX(tx)))
    {
	reset_all_views(VTX(tx));
	return(tx);
    }
    return(sym_nil);
}

_PR VALUE cmd_pos_to_offset(VALUE vpos, VALUE tx);
DEFUN("pos-to-offset", cmd_pos_to_offset, subr_pos_to_offset, (VALUE vpos, VALUE tx), V_Subr2, DOC_pos_to_offset) /*
::doc:pos_to_offset::
pos-to-offset [POS] [BUFFER]

Returns the number of characters (counting from zero) that POS (or the cursor)
is from the beginning of the buffer.
::end:: */
{
    POS pos;
    LINE *line;
    long offset, line_num;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(vpos))
	pos = VPOS(vpos);
    else
	pos = *get_tx_cursor(VTX(tx));
    check_pos(VTX(tx), &pos);
    line = VTX(tx)->tx_Lines;
    for(offset = line_num = 0; line_num < pos.pos_Line; line++, line_num++)
	offset += line->ln_Strlen; /* includes the theoretical '\n' */
    offset += pos.pos_Col;
    return(make_number(offset));
}

_PR VALUE cmd_offset_to_pos(VALUE voffset, VALUE tx);
DEFUN("offset-to-pos", cmd_offset_to_pos, subr_offset_to_pos, (VALUE voffset, VALUE tx), V_Subr2, DOC_offset_to_pos) /*
::doc:offset_to_pos::
offset-to-pos OFFSET [BUFFER]

Returns the position which is OFFSET characters from the start of the buffer.
::end:: */
{
    POS pos;
    long offset;
    LINE *line;
    DECLARE1(voffset, NUMBERP);
    offset = VNUM(voffset);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    pos.pos_Col = 0;
    pos.pos_Line = 0;
    line = VTX(tx)->tx_Lines + pos.pos_Line;
    while(offset >= line->ln_Strlen)
    {
	offset -= line->ln_Strlen;
	pos.pos_Line++;
	line++;
    }
    pos.pos_Col = offset;
    return(make_lpos(&pos));
}

void
edit_init(void)
{
    int i;
    ADD_SUBR(subr_split_line);
    ADD_SUBR(subr_insert);
    ADD_SUBR(subr_delete_area);
    ADD_SUBR(subr_copy_area);
    ADD_SUBR(subr_cut_area);
    ADD_SUBR(subr_block_toggle);
    ADD_SUBR(subr_block_start);
    ADD_SUBR(subr_block_end);
    ADD_SUBR(subr_block_kill);
    ADD_SUBR(subr_blockp);
    ADD_SUBR(subr_translate_area);
    ADD_SUBR(subr_translate_string);
    ADD_SUBR(subr_get_char);
    ADD_SUBR(subr_set_char);
    ADD_SUBR(subr_alpha_char_p);
    ADD_SUBR(subr_upper_case_p);
    ADD_SUBR(subr_lower_case_p);
    ADD_SUBR(subr_digit_char_p);
    ADD_SUBR(subr_alphanumericp);
    ADD_SUBR(subr_space_char_p);
    ADD_SUBR(subr_char_upcase);
    ADD_SUBR(subr_char_downcase);
    ADD_SUBR(subr_pos_line);
    ADD_SUBR(subr_pos_col);
    ADD_SUBR(subr_set_pos_line);
    ADD_SUBR(subr_set_pos_col);
    ADD_SUBR(subr_posp);
    ADD_SUBR(subr_cursor_pos);
    ADD_SUBR(subr_empty_line_p);
    ADD_SUBR(subr_indent_pos);
    ADD_SUBR(subr_set_indent_pos);
    ADD_SUBR(subr_indent_to);
    ADD_SUBR(subr_clear_buffer);
    ADD_SUBR(subr_offset_to_pos);
    ADD_SUBR(subr_pos_to_offset);

    INTERN(sym_upcase_table, "upcase-table");
    DOC_VAR(sym_upcase_table, DOC_upcase_table);
    VSYM(sym_upcase_table)->sym_Value = make_string(257);
    INTERN(sym_downcase_table, "downcase-table");
    DOC_VAR(sym_downcase_table, DOC_downcase_table);
    VSYM(sym_downcase_table)->sym_Value = make_string(257);
    for(i = 0; i < 256; i++)
    {
	VSTR(VSYM(sym_upcase_table)->sym_Value)[i] = toupper(i);
	VSTR(VSYM(sym_downcase_table)->sym_Value)[i] = tolower(i);
    }
    VSTR(VSYM(sym_upcase_table)->sym_Value)[256] = 0;
    VSTR(VSYM(sym_downcase_table)->sym_Value)[256] = 0;

    INTERN(sym_inhibit_read_only, "inhibit-read-only");
    DOC_VAR(sym_inhibit_read_only, DOC_inhibit_read_only);

    INTERN(sym_block_status_hook, "block-status-hook");
    DOC_VAR(sym_block_status_hook, DOC_block_status_hook);
}
