/* movement.c -- Positioning the cursor
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <ctype.h>

_PR void movement_init(void);

DEFSYM(next_screen_context_lines, "next-screen-context-lines"); /*
::doc:next_screen_context_lines::
This variable controls the number of lines of "overlap" when scrolling
by screenfuls of text.
::end:: */

_PR VALUE cmd_goto(VALUE pos);
DEFUN("goto", cmd_goto, subr_goto, (VALUE pos), V_Subr1, DOC_goto) /*
::doc:goto::
goto POSITION

Set the cursor position in the current window to the character position
POSITION.
::end:: */
{
    VW *vw = curr_vw;
    DECLARE1(pos, POSP);
    if(check_line(vw->vw_Tx, pos))
    {
	vw->vw_CursorPos = pos;
	return(pos);
    }
    else
	return(sym_nil);
}

_PR VALUE cmd_goto_glyph(VALUE pos);
DEFUN("goto-glyph", cmd_goto_glyph, subr_goto_glyph, (VALUE pos), V_Subr1, DOC_goto_glyph) /*
::doc:goto_glyph::
goto-glyph POSITION

Set the cursor position in the current window to the glyph position POSITION.
::end:: */
{
    VW *vw = curr_vw;
    DECLARE1(pos, POSP);
    if(check_line(vw->vw_Tx, pos))
    {
	vw->vw_CursorPos = make_pos(char_col(vw->vw_Tx, VCOL(pos), VROW(pos)),
				    VROW(pos));
	return(pos);
    }
    else
	return(sym_nil);
}

_PR VALUE cmd_center_display(VALUE vw, VALUE arg);
DEFUN_INT("center-display", cmd_center_display, subr_center_display, (VALUE vw, VALUE arg), V_Subr2, DOC_center_display, DS_NL "P") /*
::doc:center_display::
center-display [VIEW] [ARG]

When ARG is nil arrange it so that the line that the cursor is on is
displayed in the middle of the view (if possible).

If ARG is non-nil it should be a number, negative numbers mean that many
lines from the bottom of the view, other numbers count from the top of the
view.
::end:: */
{
    long offset;
    long col, row;

    if(!VIEWP(vw))
	vw = VAL(curr_vw);

    if(NILP(arg) || CONSP(arg))
	offset = VVIEW(vw)->vw_MaxY / 2;
    else if(SYMBOLP(arg))
	offset = VVIEW(vw)->vw_MaxY - 1;
    else if(INTP(arg))
    {
	if(VINT(arg) < 0)
	    offset = VVIEW(vw)->vw_MaxY + VINT(arg);
	else
	    offset = VINT(arg);
    }
    else
	offset = 0;

    if(!skip_glyph_rows_backwards(VVIEW(vw), offset,
				  VCOL(VVIEW(vw)->vw_CursorPos),
				  VROW(VVIEW(vw)->vw_CursorPos),
				  &col, &row))
    {
	col = 0;
	row = 0;
    }

    if(row < VVIEW(vw)->vw_Tx->tx_LogicalStart)
	row = VVIEW(vw)->vw_Tx->tx_LogicalStart;
    if(row >= VVIEW(vw)->vw_Tx->tx_LogicalEnd)
	row = VVIEW(vw)->vw_Tx->tx_LogicalEnd - 1;
    VVIEW(vw)->vw_DisplayOrigin = make_pos(col, row);
    return VVIEW(vw)->vw_DisplayOrigin;
}

_PR VALUE cmd_next_screen(VALUE number);
DEFUN_INT("next-screen", cmd_next_screen, subr_next_screen, (VALUE number), V_Subr1, DOC_next_screen, "p") /*
::doc:next_screen::
next-screen [NUMBER]

Move NUMBER (default: 1) screens forwards in the current window.
::end:: */
{
    long lines = (INTP(number) ? VINT(number) : 1) * curr_vw->vw_MaxY;
    long col, row;
    VALUE context;
    if(lines < 0)
	return cmd_prev_screen(MAKE_INT(-lines / curr_vw->vw_MaxY));
    context = cmd_symbol_value(sym_next_screen_context_lines, sym_t);
    if(INTP(context) && lines > VINT(context) + 1)
	lines -= VINT(context);

    if(VROW(curr_vw->vw_CursorPos) == curr_vw->vw_Tx->tx_LogicalEnd - 1)
	return sym_nil;
    else if(curr_vw->vw_Flags & VWFF_AT_BOTTOM)
    {
	set_cursor_vertically(curr_vw, curr_vw->vw_Tx->tx_LogicalEnd - 1);
	return curr_vw->vw_DisplayOrigin;
    }
    else if(skip_glyph_rows_forwards(curr_vw, lines,
				     VCOL(curr_vw->vw_DisplayOrigin),
				     VROW(curr_vw->vw_DisplayOrigin),
				     &col, &row))
    {
	curr_vw->vw_DisplayOrigin = make_pos(col, row);
	if(POS_GREATER_P(curr_vw->vw_DisplayOrigin, curr_vw->vw_CursorPos))
	    set_cursor_vertically(curr_vw, VROW(curr_vw->vw_DisplayOrigin));
	return curr_vw->vw_DisplayOrigin;
    }
    else
	return sym_nil;
}

_PR VALUE cmd_prev_screen(VALUE number);
DEFUN_INT("prev-screen", cmd_prev_screen, subr_prev_screen, (VALUE number), V_Subr1, DOC_prev_screen, "p") /*
::doc:prev_screen::
prev-screen [NUMBER]

Move NUMBER (default: 1) screens backwards in the current window.
::end:: */
{
    long lines = (INTP(number) ? VINT(number) : 1) * curr_vw->vw_MaxY;
    long col, row;
    VALUE context, new_origin;
    if(lines < 0)
	return cmd_next_screen(MAKE_INT(-lines / curr_vw->vw_MaxY));

    context = cmd_symbol_value(sym_next_screen_context_lines, sym_t);
    if(INTP(context) && lines > VINT(context) + 1)
	lines -= VINT(context);
    if(skip_glyph_rows_backwards(curr_vw, lines,
				 VCOL(curr_vw->vw_DisplayOrigin),
				 VROW(curr_vw->vw_DisplayOrigin),
				 &col, &row))
	new_origin = make_pos(col, row);
    else if(VROW(curr_vw->vw_DisplayOrigin) != curr_vw->vw_Tx->tx_LogicalStart)
	new_origin = make_pos(0, curr_vw->vw_Tx->tx_LogicalStart);
    else if(VROW(curr_vw->vw_CursorPos) != curr_vw->vw_Tx->tx_LogicalStart)
    {
	set_cursor_vertically(curr_vw, curr_vw->vw_Tx->tx_LogicalStart);
	return curr_vw->vw_DisplayOrigin;
    }
    else
	return sym_nil;

    /* Now fix the cursor position. */
    if(skip_glyph_rows_forwards(curr_vw, curr_vw->vw_MaxY - 1,
				VCOL(new_origin), VROW(new_origin),
				&col, &row))
    {
	long curs_offset = get_cursor_column(curr_vw);
	if(VROW(curr_vw->vw_CursorPos) > row
	   || (VROW(curr_vw->vw_CursorPos) == row
	       && curs_offset > col))
	{
	    /* TODO: the column isn't correct */
	    curr_vw->vw_CursorPos = make_pos(col, row);
	}
    }

    curr_vw->vw_DisplayOrigin = new_origin;
    return curr_vw->vw_DisplayOrigin;
}

_PR VALUE cmd_end_of_buffer(VALUE tx, VALUE irp);
DEFUN_INT("end-of-buffer", cmd_end_of_buffer, subr_end_of_buffer,
	  (VALUE tx, VALUE irp), V_Subr2, DOC_end_of_buffer, "!@") /*
::doc:end_of_buffer::
end-of-buffer [BUFFER] [IGNORE-RESTRICTION-P]

Return the position of the last character in BUFFER. Unless
IGNORE-RESTRICTION-P is non-nil the position returned is the end
of the buffer's restriction.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!NILP(irp))
    {
	long x, y;
	y = VTX(tx)->tx_NumLines - 1;
	x = VTX(tx)->tx_Lines[y].ln_Strlen - 1;
	return make_pos(x, y);
    }
    else
	return cmd_restriction_end(tx);
}

_PR VALUE cmd_start_of_buffer(VALUE tx, VALUE irp);
DEFUN_INT("start-of-buffer", cmd_start_of_buffer, subr_start_of_buffer,
	  (VALUE tx, VALUE irp), V_Subr2, DOC_start_of_buffer, "!@") /*
::doc:start_of_buffer::
start-of-buffer [BUFFER] [IGNORE-RESTRICTION-P]

Return the position of the start of the buffer. Unless
IGNORE-RESTRICTION-P is non-nil the position returned is the start
of the buffer's restriction.
::end:: */
{
    if(!NILP(irp))
	return make_pos(0, 0);
    else
	return cmd_restriction_start(tx);
}

_PR VALUE cmd_end_of_line(VALUE pos, VALUE tx);
DEFUN_INT("end-of-line", cmd_end_of_line, subr_end_of_line,
	  (VALUE pos, VALUE tx), V_Subr2, DOC_end_of_line, "@") /*
::doc:end_of_line::
end-of-line [POS] [BUFFER]

Return the position of the last character in the line pointed to by POS (or
the cursor).
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(VROW(pos) < VTX(tx)->tx_NumLines)
	return make_pos(VTX(tx)->tx_Lines[VROW(pos)].ln_Strlen - 1, VROW(pos));
    else
	return sym_nil;
}

_PR VALUE cmd_start_of_line(VALUE pos);
DEFUN_INT("start-of-line", cmd_start_of_line, subr_start_of_line,
	  (VALUE pos), V_Subr1, DOC_start_of_line, "@") /*
::doc:start_of_line::
start-of-line [POS]

Return the position of the first character in the line pointed to by POS
(or the cursor).
::end:: */
{
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(VCOL(pos) != 0)
	return make_pos(0, VROW(pos));
    else
	return pos;
}

_PR VALUE cmd_forward_line(VALUE lines, VALUE pos);
DEFUN_INT("forward-line", cmd_forward_line, subr_forward_line,
	  (VALUE lines, VALUE pos), V_Subr2, DOC_forward_line, "@p") /*
::doc:forward_line::
forward-line [NUMBER] [POS]

Return the position of the NUMBER'th (by default the next) line below
that pointed to by POS (or the cursor).

Negative NUMBERs move backwards, if the first line is passed (i.e. a negative
line number is made) nil is returned.
::end:: */
{
    long row;
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    row = VROW(pos) + (INTP(lines) ? VINT(lines) : 1);
    if(row < 0)
	return sym_nil;
    else
	return make_pos(VCOL(pos), row);
}

_PR VALUE cmd_forward_char(VALUE count, VALUE pos, VALUE tx);
DEFUN_INT("forward-char", cmd_forward_char, subr_forward_char,
	  (VALUE count, VALUE pos, VALUE tx), V_Subr3, DOC_forward_char, "@p") /*
::doc:forward_char::
forward-char [COUNT] [POS] [BUFFER]

Returns the position of the character COUNT characters (by default the next)
after POS (or the cursor). Negative COUNTs move backwards. If either the
beginning or the end of the buffer is passed, nil is returned.
::end:: */
{
    long dist;
    Pos tem;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    else
    {
	if(!check_pos(VTX(tx), pos))
	    return LISP_NULL;
    }
    dist = INTP(count) ? VINT(count) : 1;
    if(dist == 0)
	return pos;
    COPY_VPOS(&tem, pos);
    if((dist > 0 && forward_char(dist, VTX(tx), &tem))
       || backward_char(-dist, VTX(tx), &tem))
	return COPY_POS(&tem);
    else
	return sym_nil;
}

_PR VALUE cmd_forward_tab(VALUE num, VALUE pos, VALUE size);
DEFUN_INT("forward-tab", cmd_forward_tab, subr_forward_tab,
	  (VALUE num, VALUE pos, VALUE size), V_Subr3, DOC_forward_tab, "@p") /*
::doc:forward_tab::
forward-tab [COUNT] [POS] [TAB-SIZE]

Return the glyph position of the COUNT'th next tab stop to the right of
the character position POS (or the cursor). COUNT is assumed 1 when
undefined; negative values move towards the left hand side of the screen.
::end:: */
{
    int tabs = INTP(num) ? VINT(num) : 1;
    VW *vw = curr_vw;
    int tabsize = INTP(size) ? VINT(size) : vw->vw_Tx->tx_TabSize;
    long col;
    if(!POSP(pos))
    {
	pos = curr_vw->vw_CursorPos;
	col = get_cursor_column(vw);
    }
    else
	col = VCOL(pos);
    while(tabs > 0)
    {
	col = ((col / tabsize) + 1) * tabsize;
	tabs--;
    }
    while(tabs < 0)
    {
	col = (((col - 1) / tabsize)) * tabsize;
	tabs++;
    }
    if(col >= 0)
	return make_pos(col, VROW(pos));
    else
	return(sym_nil);
}

DEFSTRING(no_brac, "No matching bracket");
DEFSTRING(no_open_brac, "No opening bracket");

static int
find_matching_bracket(Pos *pos, TX *tx, u_char esc)
{
#define NUM_BRAC_TYPES 10
    static u_char bracs[] =
    {
	'{', '}',
	'(', ')',
	'[', ']',
	'`', '\'',
	'<', '>'
    };

/* Test for an escape character preceding COL in the string LINE. Beware
   that COL is referenced more than once, so no side effects please!   */
#define TST_ESC(line, col) ((col) > 0 && (line)[(col)-1] == esc)

    LINE *line = tx->tx_Lines + PROW(pos);
    if(PCOL(pos) < line->ln_Strlen)
    {
	u_char startc = line->ln_Line[PCOL(pos)];
	long i;
	for(i = 0; i < NUM_BRAC_TYPES; i++)
	{
	    if(startc == bracs[i])
		break;
	}
	if(!TST_ESC(line->ln_Line, PCOL(pos)) && (i < NUM_BRAC_TYPES))
	{
	    long x = PCOL(pos);
	    long y = PROW(pos);
	    long braccount = 1;
	    bool found = FALSE;
	    if(i & 1)
	    {
		/* search backwards */
		u_char endc = bracs[i - 1];
		while(!found)
		{
		    u_char c;
		    if(--x < 0)
		    {
			if(--y < tx->tx_LogicalStart)
			{
			    cmd_signal(sym_error, LIST_1(VAL(&no_brac)));
			    return(FALSE);
			}
			line--;
			x = line->ln_Strlen - 1;
		    }
		    c = line->ln_Line[x];
		    if(c == startc)
		    {
			if(!TST_ESC(line->ln_Line, x))
			    braccount++;
		    }
		    else if(c == endc)
		    {
			if(!TST_ESC(line->ln_Line, x) && !(--braccount))
			    found = TRUE;
		    }
		}
	    }
	    else
	    {
		/* search forwards */
		u_char endc = bracs[i + 1];
		while(!found)
		{
		    u_char c;
		    if(++x >= line->ln_Strlen)
		    {
			if(++y >= tx->tx_LogicalEnd)
			{
			    cmd_signal(sym_error, LIST_1(VAL(&no_brac)));
			    return(FALSE);
			}
			line++;
			x = 0;
		    }
		    c = line->ln_Line[x];
		    if(c == startc)
		    {
			if(!TST_ESC(line->ln_Line, x))
			    braccount++;
		    }
		    else if(c == endc)
		    {
			if(!TST_ESC(line->ln_Line, x) && !(--braccount))
			    found = TRUE;
		    }
		}
	    }
	    PCOL(pos) = x;
	    PROW(pos) = y;
	    return TRUE;
	}
    }
    cmd_signal(sym_error, LIST_1(VAL(&no_open_brac)));
    return FALSE;
}

_PR VALUE cmd_find_matching_bracket(VALUE pos, VALUE tx, VALUE esc);
DEFUN_INT("find-matching-bracket", cmd_find_matching_bracket,
	  subr_find_matching_bracket, (VALUE pos, VALUE tx, VALUE esc),
	  V_Subr3, DOC_find_matching_bracket, "!@") /*
::doc:find_matching_bracket::
find-matching-bracket [POS] [BUFFER] [ESCAPE-CHAR]

Find a bracket matching the one at POS (or the cursor). The things that match
each other are,  { }, ( ), [ ], ` ', < >. POS is altered.
Brackets preceded by ESCAPE-CHAR (`\' by default) are not counted.
::end:: */
{
    u_char esc_char = INTP(esc) ? VINT(esc) : '\\';
    Pos tem;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    else
    {
	if(!check_pos(VTX(tx), pos))
	    return LISP_NULL;
    }
    COPY_VPOS(&tem, pos);
    if(find_matching_bracket(&tem, VTX(tx), esc_char))
	return COPY_POS(&tem);
    return(sym_nil);
}

_PR VALUE cmd_raw_mouse_pos(void);
DEFUN("raw-mouse-pos", cmd_raw_mouse_pos, subr_raw_mouse_pos, (void), V_Subr0, DOC_raw_mouse_pos) /*
::doc:raw_mouse_pos::
raw-mouse-pos

Return the glyph position of the mouse, relative to the current window.
::end:: */
{
    VALUE pos = sys_get_mouse_pos(curr_win);
    if(pos != LISP_NULL)
	return pos;
    else
	return sym_nil;
}

void
movement_init(void)
{
    ADD_SUBR(subr_goto);
    ADD_SUBR(subr_goto_glyph);
    ADD_SUBR_INT(subr_center_display);
    ADD_SUBR_INT(subr_next_screen);
    ADD_SUBR_INT(subr_prev_screen);
    ADD_SUBR_INT(subr_end_of_buffer);
    ADD_SUBR_INT(subr_start_of_buffer);
    ADD_SUBR_INT(subr_end_of_line);
    ADD_SUBR_INT(subr_start_of_line);
    ADD_SUBR_INT(subr_forward_line);
    ADD_SUBR_INT(subr_forward_char);
    ADD_SUBR_INT(subr_forward_tab);
    ADD_SUBR_INT(subr_find_matching_bracket);
    ADD_SUBR(subr_raw_mouse_pos);

    INTERN(next_screen_context_lines);
    DOC(next_screen_context_lines);
    VSYM(sym_next_screen_context_lines)->value = MAKE_INT(2);
}
