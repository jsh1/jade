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

static long move_down_screens(long);
static long move_up_screens(long);
static bool prev_char(long, POS *, TX *);
static bool next_char(long, POS *, TX *);
static int find_matching_bracket(POS *, TX *, u_char esc);
_PR void movement_init(void);

_PR VALUE cmd_screen_top_line(void);
DEFUN("screen-top-line", cmd_screen_top_line, subr_screen_top_line, (void), V_Subr0, DOC_screen_top_line) /*
::doc:screen_top_line::
screen-top-line

Returns the line number of the first line being shown in the current window.
::end:: */
{
    return(make_number(curr_vw->vw_StartLine));
}

_PR VALUE cmd_screen_bottom_line(void);
DEFUN("screen-bottom-line", cmd_screen_bottom_line, subr_screen_bottom_line, (void), V_Subr0, DOC_screen_bottom_line) /*
::doc:screen_bottom_line::
screen-bottom-line

Returns the line number of the last line being shown in the current window.
::end:: */
{
    return(make_number(curr_vw->vw_StartLine + curr_vw->vw_MaxY - 1));
}

_PR VALUE cmd_screen_first_column(void);
DEFUN("screen-first-column", cmd_screen_first_column, subr_screen_first_column, (void), V_Subr0, DOC_screen_first_column) /*
::doc:screen_first_column::
screen-first-column

Returns the line number of the first column being shown in the current window.
::end:: */
{
    return(make_number(curr_vw->vw_StartCol));
}

_PR VALUE cmd_screen_last_column(void);
DEFUN("screen-last-column", cmd_screen_last_column, subr_screen_last_column, (void), V_Subr0, DOC_screen_last_column) /*
::doc:screen_last_column::
screen-last-column

Returns the line number of the last column being shown in the current window.
::end:: */
{
    return(make_number(curr_vw->vw_StartCol + curr_vw->vw_MaxX - 1));
}

_PR VALUE cmd_goto_char(VALUE pos);
DEFUN("goto-char", cmd_goto_char, subr_goto_char, (VALUE pos), V_Subr1, DOC_goto_char) /*
::doc:goto_char::
goto-char POS

Set the cursor position in the current window to the character position POS.
::end:: */
{
    VW *vw = curr_vw;
    DECLARE1(pos, POSP);
    if(check_line(vw->vw_Tx, &VPOS(pos)))
    {
	vw->vw_CursorPos = VPOS(pos);
	return(pos);
    }
    return(sym_nil);
}

_PR VALUE cmd_goto_glyph(VALUE pos);
DEFUN("goto-glyph", cmd_goto_glyph, subr_goto_glyph, (VALUE pos), V_Subr1, DOC_goto_glyph) /*
::doc:goto_glyph::
goto-glyph POS

Set the cursor position in the current window to the glyph position POS.
::end:: */
{
    VW *vw = curr_vw;
    DECLARE1(pos, POSP);
    if(check_line(vw->vw_Tx, &VPOS(pos)))
    {
	vw->vw_CursorPos.pos_Col = char_col(vw->vw_Tx, VPOS(pos).pos_Col,
					    VPOS(pos).pos_Line);
	vw->vw_CursorPos.pos_Line = VPOS(pos).pos_Line;
	return(pos);
    }
    return(sym_nil);
}

_PR VALUE cmd_centre_display(VALUE vw);
DEFUN_INT("centre-display", cmd_centre_display, subr_centre_display, (VALUE vw), V_Subr1, DOC_centre_display, "") /*
::doc:centre_display::
centre-display [VIEW]

Arrange it so that the line that the cursor is on is displayed in the
middle of the view (if possible).
::end:: */
{
    long start_line;
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    start_line = VVIEW(vw)->vw_CursorPos.pos_Line - (VVIEW(vw)->vw_MaxY / 2);
    if(start_line < VVIEW(vw)->vw_Tx->tx_LogicalStart)
	start_line = VVIEW(vw)->vw_Tx->tx_LogicalStart;
    if(start_line >= VVIEW(vw)->vw_Tx->tx_LogicalEnd)
	start_line = VVIEW(vw)->vw_Tx->tx_LogicalEnd - 1;
    VVIEW(vw)->vw_StartLine = start_line;
    return(vw);
}

_PR VALUE cmd_next_screen(VALUE number);
DEFUN_INT("next-screen", cmd_next_screen, subr_next_screen, (VALUE number), V_Subr1, DOC_next_screen, "p") /*
::doc:next_screen::
next-screen [NUMBER]

Move NUMBER (default: 1) screens forwards in the current window.
::end:: */
{
    if(move_down_screens(NUMBERP(number) ? VNUM(number) : 1))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_prev_screen(VALUE number);
DEFUN_INT("prev-screen", cmd_prev_screen, subr_prev_screen, (VALUE number), V_Subr1, DOC_prev_screen, "p") /*
::doc:prev_screen::
prev-screen [NUMBER]

Move NUMBER (default: 1) screens backwards in the current window.
::end:: */
{
    if(move_up_screens(NUMBERP(number) ? VNUM(number) : 1))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_buffer_end(VALUE tx, VALUE irp);
DEFUN("buffer-end", cmd_buffer_end, subr_buffer_end, (VALUE tx, VALUE irp), V_Subr2, DOC_buffer_end) /*
::doc:buffer_end::
buffer-end [BUFFER] [IGNORE-RESTRICTION-P]

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
	return(make_lpos2(x, y));
    }
    else
	return cmd_restriction_end(tx);
}

_PR VALUE cmd_goto_buffer_end(void);
DEFUN_INT("goto-buffer-end", cmd_goto_buffer_end, subr_goto_buffer_end, (void), V_Subr0, DOC_goto_buffer_end, "") /*
::doc:goto_buffer_end::
goto-buffer-end

Move to the last character in the current window.
::end:: */
{
    VW *vw = curr_vw;
    vw->vw_CursorPos.pos_Line = vw->vw_Tx->tx_LogicalEnd - 1;
    vw->vw_CursorPos.pos_Col
        = vw->vw_Tx->tx_Lines[vw->vw_CursorPos.pos_Line].ln_Strlen - 1;
    return(sym_t);
}

_PR VALUE cmd_buffer_start(VALUE tx, VALUE irp);
DEFUN("buffer-start", cmd_buffer_start, subr_buffer_start, (VALUE tx, VALUE irp), V_Subr2, DOC_buffer_start) /*
::doc:buffer_start::
buffer-start [BUFFER] [IGNORE-RESTRICTION-P]

Return the position of the start of the buffer. Unless
IGNORE-RESTRICTION-P is non-nil the position returned is the start
of the buffer's restriction.
::end:: */
{
    if(!NILP(irp))
	return(make_lpos2(0, 0));
    else
	return cmd_restriction_start(tx);
}

_PR VALUE cmd_goto_buffer_start(void);
DEFUN_INT("goto-buffer-start", cmd_goto_buffer_start, subr_goto_buffer_start, (void), V_Subr0, DOC_goto_buffer_start, "") /*
::doc:goto_buffer_start::
goto-buffer-start

Move to the first character in the buffer displayed in the current window.
::end:: */
{
    curr_vw->vw_CursorPos.pos_Col = 0;
    curr_vw->vw_CursorPos.pos_Line = curr_vw->vw_Tx->tx_LogicalStart;
    return(sym_t);
}

_PR VALUE cmd_line_end(VALUE pos, VALUE tx);
DEFUN("line-end", cmd_line_end, subr_line_end, (VALUE pos, VALUE tx), V_Subr2, DOC_line_end) /*
::doc:line_end::
line-end [POS] [BUFFER]

Return the position of the last character in the line pointed to by POS (or
the cursor).
::end:: */
{
    POS res;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(pos))
	res.pos_Line = VPOS(pos).pos_Line;
    else
	res.pos_Line = get_tx_cursor(VTX(tx))->pos_Line;
    if(res.pos_Line < VTX(tx)->tx_NumLines)
    {
	res.pos_Col = VTX(tx)->tx_Lines[res.pos_Line].ln_Strlen - 1;
	return(make_lpos(&res));
    }
    return(sym_nil);
}

_PR VALUE cmd_goto_line_end(void);
DEFUN_INT("goto-line-end", cmd_goto_line_end, subr_goto_line_end, (void), V_Subr0, DOC_goto_line_end, "") /*
::doc:goto_line_end::
goto-line-end

Move to the last character in the line.
::end:: */
{
    VW *vw = curr_vw;
    vw->vw_CursorPos.pos_Col = vw->vw_Tx->tx_Lines[vw->vw_CursorPos.pos_Line].ln_Strlen - 1;
    return(sym_t);
}

_PR VALUE cmd_line_start(VALUE pos);
DEFUN("line-start", cmd_line_start, subr_line_start, (VALUE pos), V_Subr1, DOC_line_start) /*
::doc:line_start::
line-start [POS]

Return the position of the first character in the line pointed to by POS
(or the cursor).
::end:: */
{
    POS res;
    if(POSP(pos))
	res.pos_Line = VPOS(pos).pos_Line;
    else
	res.pos_Line = curr_vw->vw_CursorPos.pos_Line;
    res.pos_Col = 0;
    return(make_lpos(&res));
}

_PR VALUE cmd_goto_line_start(void);
DEFUN_INT("goto-line-start", cmd_goto_line_start, subr_goto_line_start, (void), V_Subr0, DOC_goto_line_start, "") /*
::doc:goto_line_start::
goto-line-start

Move to the start of the current line.
::end:: */
{
    VW *vw = curr_vw;
    vw->vw_CursorPos.pos_Col = 0;
    return(sym_t);
}

_PR VALUE cmd_next_line(VALUE lines, VALUE pos);
DEFUN("next-line", cmd_next_line, subr_next_line, (VALUE lines, VALUE pos), V_Subr2, DOC_next_line) /*
::doc:next_line::
next-line [NUMBER] [POS]

Return the position of the NUMBERth (def: 1) line down from that pointed to
by POS (or the cursor). POS is altered.
::end:: */
{
    if(!POSP(pos))
	pos = make_lpos(&curr_vw->vw_CursorPos);
    VPOS(pos).pos_Line += NUMBERP(lines) ? VNUM(lines) : 1;
    if(VPOS(pos).pos_Line > 0)
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_goto_next_line(VALUE lines);
DEFUN_INT("goto-next-line", cmd_goto_next_line, subr_goto_next_line, (VALUE lines), V_Subr1, DOC_goto_next_line, "p") /*
::doc:goto_next_line::
goto-next-line [NUMBER]

Move NUMBER lines (def: 1) downwards. Adjusts the cursor's column position
so that it is drawn as near to where it was previously as possible.
::end:: */
{
    VW *vw = curr_vw;
    VALUE res = sym_nil;
    vw->vw_CursorPos.pos_Line += NUMBERP(lines) ? VNUM(lines) : 1;
    if(vw->vw_CursorPos.pos_Line >= vw->vw_Tx->tx_LogicalEnd)
	vw->vw_CursorPos.pos_Line = vw->vw_Tx->tx_LogicalEnd - 1;
    else if(vw->vw_CursorPos.pos_Line < vw->vw_Tx->tx_LogicalStart)
	vw->vw_CursorPos.pos_Line = vw->vw_Tx->tx_LogicalStart;
    else
	res = sym_t;
    adjust_cursor_to_glyph(vw);
    return(res);
}

_PR VALUE cmd_prev_line(VALUE lines, VALUE pos);
DEFUN("prev-line", cmd_prev_line, subr_prev_line, (VALUE lines, VALUE pos), V_Subr2, DOC_prev_line) /*
::doc:prev_line::
prev-line [NUMBER] [POS]

Return the position of the NUMBERth (def: 1) line up from that pointed to
by POS (or the cursor). POS is altered.
::end:: */
{
    if(!POSP(pos))
	pos = make_lpos(&curr_vw->vw_CursorPos);
    VPOS(pos).pos_Line -= NUMBERP(lines) ? VNUM(lines) : 1;
    if(VPOS(pos).pos_Line >= 0)
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_goto_prev_line(VALUE lines);
DEFUN_INT("goto-prev-line", cmd_goto_prev_line, subr_goto_prev_line, (VALUE lines), V_Subr1, DOC_goto_prev_line, "p") /*
::doc:goto_prev_line::
goto-next-line [NUMBER]

Move NUMBER lines (def: 1) upwards. Adjusts the cursor's column position
so that it is drawn as near to where it was previously as possible.
::end:: */
{
    VW *vw = curr_vw;
    VALUE res = sym_nil;
    vw->vw_CursorPos.pos_Line -= NUMBERP(lines) ? VNUM(lines) : 1;
    if(vw->vw_CursorPos.pos_Line >= vw->vw_Tx->tx_LogicalEnd)
	vw->vw_CursorPos.pos_Line = vw->vw_Tx->tx_LogicalEnd - 1;
    else if(vw->vw_CursorPos.pos_Line < vw->vw_Tx->tx_LogicalStart)
	vw->vw_CursorPos.pos_Line = vw->vw_Tx->tx_LogicalStart;
    else
	res = sym_t;
    adjust_cursor_to_glyph(vw);
    return(res);
}

_PR VALUE cmd_left_char(VALUE chars, VALUE pos);
DEFUN("left-char", cmd_left_char, subr_left_char, (VALUE chars, VALUE pos), V_Subr2, DOC_left_char) /*
::doc:left_char::
left-char [NUMBER] [POS]

Return the position of the NUMBERth character (def: 1) to the left of the
one pointed to by POS (or the cursor). If that position is before the
beginning of the line, returns nil. POS is altered.
::end:: */
{
    if(!POSP(pos))
	pos = make_lpos(&curr_vw->vw_CursorPos);
    VPOS(pos).pos_Col -= NUMBERP(chars) ? VNUM(chars) : 1;
    if(VPOS(pos).pos_Col >= 0)
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_goto_left_char(VALUE chars);
DEFUN_INT("goto-left-char", cmd_goto_left_char, subr_goto_left_char, (VALUE chars), V_Subr1, DOC_goto_left_char, "p") /*
::doc:goto_left_char::
goto-left-char [NUMBER]

Move NUMBER chars (def: 1) to the left.
::end:: */
{
    VW *vw = curr_vw;
    vw->vw_CursorPos.pos_Col -= NUMBERP(chars) ? VNUM(chars) : 1;
    if(vw->vw_CursorPos.pos_Col > 0)
	return(sym_t);
    vw->vw_CursorPos.pos_Col = 0;
    return(sym_nil);
}

_PR VALUE cmd_right_char(VALUE chars, VALUE pos);
DEFUN("right-char", cmd_right_char, subr_right_char, (VALUE chars, VALUE pos), V_Subr2, DOC_right_char) /*
::doc:right_char::
right-char [NUMBER] [POS]

Return the position of the NUMBERth character (def: 1) to the right of the
one pointed to by POS (or the cursor). Doesn't pay any attention to newlines.
POS is altered.
::end:: */
{
    if(!POSP(pos))
	pos = make_lpos(&curr_vw->vw_CursorPos);
    VPOS(pos).pos_Col += NUMBERP(chars) ? VNUM(chars) : 1;
    if(VPOS(pos).pos_Col >= 0)
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_goto_right_char(VALUE chars);
DEFUN_INT("goto-right-char", cmd_goto_right_char, subr_goto_right_char, (VALUE chars), V_Subr1, DOC_goto_right_char, "p") /*
::doc:goto_right_char::
goto-right-char [NUMBER]

Move NUMBER chars (def: 1) to the right
::end:: */
{
    VW *vw = curr_vw;
    vw->vw_CursorPos.pos_Col += NUMBERP(chars) ? VNUM(chars) : 1;
    if(vw->vw_CursorPos.pos_Col > 0)
	return(sym_t);
    vw->vw_CursorPos.pos_Col = 0;
    return(sym_nil);
}

_PR VALUE cmd_prev_tab(VALUE num, VALUE pos, VALUE size);
DEFUN("prev-tab", cmd_prev_tab, subr_prev_tab, (VALUE num, VALUE pos, VALUE size), V_Subr3, DOC_prev_tab) /*
::doc:prev_tab::
prev-tab [NUMBER] [POS] [TAB-SIZE]

Return the position of the NUMBERth (def: 1) tab stop to the left of POS (or
the cursor). Returns nil if that position is past the beginning of the line.
Note that this doesn't return the actual *character* position of the tab
stop, but the number of glyphs which have to be displayed. POS is altered.
::end:: */
{
    int tabs = 1;
    VW *vw = curr_vw;
    int tabsize = NUMBERP(size) ? VNUM(size) : vw->vw_Tx->tx_TabSize;
    if(!POSP(pos))
    {
	pos = make_lpos(&curr_vw->vw_CursorPos);
	calc_cursor_offset(vw);
	VPOS(pos).pos_Col = vw->vw_LastCursorOffset;
    }
    if(NUMBERP(num))
	tabs = VNUM(num);
    if(tabs > 0)
    {
	while(tabs--)
	    VPOS(pos).pos_Col = (((VPOS(pos).pos_Col - 1) / tabsize)) * tabsize;
    }
    else if(tabs < 0)
    {
	while(tabs++)
	    VPOS(pos).pos_Col = ((VPOS(pos).pos_Col / tabsize) + 1) * tabsize;
    }
    if(VPOS(pos).pos_Col >= 0)
	return(pos);
    VPOS(pos).pos_Col = 0;
    return(sym_nil);
}

_PR VALUE cmd_goto_prev_tab(VALUE num, VALUE size);
DEFUN_INT("goto-prev-tab", cmd_goto_prev_tab, subr_goto_prev_tab, (VALUE num, VALUE size), V_Subr2, DOC_goto_prev_tab, "p") /*
::doc:goto_prev_tab::
goto-prev-tab [NUMBER] [TAB-SIZE]

Move NUMBER (def: 1) tab stops to the left.
::end:: */
{
    int tabs = 1;
    VW *vw = curr_vw;
    int tabsize = NUMBERP(size) ? VNUM(size) : vw->vw_Tx->tx_TabSize;
    long x = glyph_col(vw->vw_Tx, vw->vw_CursorPos.pos_Col,
		       vw->vw_CursorPos.pos_Line);
    if(NUMBERP(num))
	tabs = VNUM(num);
    if(tabs > 0)
    {
	while(tabs--)
	    x = (((x - 1) / tabsize)) * tabsize;
    }
    else if(tabs < 0)
    {
	while(tabs++)
	    x = ((x / tabsize) + 1) * tabsize;
    }
    if(x >= 0)
    {
	vw->vw_CursorPos.pos_Col = char_col(vw->vw_Tx, x,
					    vw->vw_CursorPos.pos_Line);
	return(sym_t);
    }
    vw->vw_CursorPos.pos_Col = 0;
    return(sym_nil);
}

_PR VALUE cmd_next_tab(VALUE num, VALUE pos, VALUE size);
DEFUN("next-tab", cmd_next_tab, subr_next_tab, (VALUE num, VALUE pos, VALUE size), V_Subr3, DOC_next_tab) /*
::doc:next_tab::
next-tab [NUMBER] [POS] [TAB-SIZE]

Return the position of the NUMBERth (def: 1) tab stop to the right of POS (or
the cursor).
Note that this doesn't return the actual *character* position of the tab
stop, but the number of glyphs which have to be displayed. POS is altered.
::end:: */
{
    int tabs = 1;
    VW *vw = curr_vw;
    int tabsize = NUMBERP(size) ? VNUM(size) : vw->vw_Tx->tx_TabSize;
    if(!POSP(pos))
    {
	pos = make_lpos(&curr_vw->vw_CursorPos);
	calc_cursor_offset(vw);
	VPOS(pos).pos_Col = vw->vw_LastCursorOffset;
    }
    if(NUMBERP(num))
	tabs = VNUM(num);
    if(tabs > 0)
    {
	while(tabs--)
	    VPOS(pos).pos_Col = ((VPOS(pos).pos_Col / tabsize) + 1) * tabsize;
    }
    else if(tabs < 0)
    {
	while(tabs++)
	    VPOS(pos).pos_Col = (((VPOS(pos).pos_Col - 1) / tabsize))
		                 * tabsize;
    }
    if(VPOS(pos).pos_Col >= 0)
	return(pos);
    VPOS(pos).pos_Col = 0;
    return(sym_nil);
}

_PR VALUE cmd_goto_next_tab(VALUE num, VALUE size);
DEFUN_INT("goto-next-tab", cmd_goto_next_tab, subr_goto_next_tab, (VALUE num, VALUE size), V_Subr2, DOC_goto_next_tab, "p") /*
::doc:goto_next_tab::
goto-next-tab [NUMBER] [TAB-SIZE]

Move NUMBER (def: 1) tab stops to the right
::end:: */
{
    int tabs = 1;
    VW *vw = curr_vw;
    int tabsize = NUMBERP(size) ? VNUM(size) : vw->vw_Tx->tx_TabSize;
    long x = glyph_col(vw->vw_Tx, vw->vw_CursorPos.pos_Col,
		       vw->vw_CursorPos.pos_Line);
    if(NUMBERP(num))
	tabs = VNUM(num);
    if(tabs > 0)
    {
	while(tabs--)
	    x = ((x / tabsize) + 1) * tabsize;
    }
    else if(tabs < 0)
    {
	while(tabs++)
	    x = (((x - 1) / tabsize)) * tabsize;
    }
    if(x >= 0)
    {
	vw->vw_CursorPos.pos_Col = char_col(vw->vw_Tx, x,
					    vw->vw_CursorPos.pos_Line);
	return(sym_t);
    }
    vw->vw_CursorPos.pos_Col = 0;
    return(sym_nil);
}

static bool
prev_char(long count, POS *pos, TX *tx)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(count < 0)
	return(next_char(-count, pos, tx));
    while(count > 0)
    {
	if(count <= pos->pos_Col)
	{
	    pos->pos_Col -= count;
	    count = 0;
	}
	else
	{
	    count -= pos->pos_Col + 1; /* `+ 1' for the assumed '\n' */
	    line--;
	    pos->pos_Line--;
	    if(pos->pos_Line < 0)
		return(FALSE);
	    pos->pos_Col = line->ln_Strlen - 1;
	}
    }
    return(TRUE);
}

static bool
next_char(long count, POS *pos, TX *tx)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(count < 0)
	return(prev_char(-count, pos, tx));
    if(pos->pos_Col >= line->ln_Strlen)
	pos->pos_Col = line->ln_Strlen - 1;
    while(count > 0)
    {
	if(count < (line->ln_Strlen - pos->pos_Col))
	{
	    pos->pos_Col += count;
	    count = 0;
	}
	else
	{
	    count -= line->ln_Strlen - pos->pos_Col;
	    pos->pos_Col = 0;
	    pos->pos_Line++;
	    line++;
	    if(pos->pos_Line >= tx->tx_NumLines)
		return(FALSE);
	}
    }
    return(TRUE);
}

_PR VALUE cmd_next_char(VALUE count, VALUE pos, VALUE tx);
DEFUN("next-char", cmd_next_char, subr_next_char, (VALUE count, VALUE pos, VALUE tx), V_Subr3, DOC_next_char) /*
::doc:next_char::
next-char [COUNT] [POS] [BUFFER]

Returns the position of the character COUNT (default: 1) characters after POS
(or the cursor). POS is altered.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = make_lpos(get_tx_cursor(VTX(tx)));
    else
	check_pos(VTX(tx), &VPOS(pos));
    if(next_char(NUMBERP(count) ? VNUM(count) : 1, &VPOS(pos), VTX(tx)))
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_goto_next_char(VALUE count);
DEFUN_INT("goto-next-char", cmd_goto_next_char, subr_goto_next_char, (VALUE count), V_Subr1, DOC_goto_next_char, "p") /*
::doc:goto_next_char::
goto-next-char [COUNT]

Moves to the character COUNT characters after the cursor.
::end:: */
{
    VW *vw = curr_vw;
    POS tmp = vw->vw_CursorPos;
    if(next_char(NUMBERP(count) ? VNUM(count) : 1, &tmp, vw->vw_Tx))
    {
	vw->vw_CursorPos = tmp;
	return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_prev_char(VALUE count, VALUE pos, VALUE tx);
DEFUN("prev-char", cmd_prev_char, subr_prev_char, (VALUE count, VALUE pos, VALUE tx), V_Subr3, DOC_prev_char) /*
::doc:prev_char::
prev-char [COUNT] [POS] [BUFFER]

Returns the position of the character COUNT characters before POS (or the
cursor). POS is altered.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = make_lpos(get_tx_cursor(VTX(tx)));
    else
	check_pos(VTX(tx), &VPOS(pos));
    if(prev_char(NUMBERP(count) ? VNUM(count) : 1, &VPOS(pos), VTX(tx)))
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_goto_prev_char(VALUE count);
DEFUN_INT("goto-prev-char", cmd_goto_prev_char, subr_goto_prev_char, (VALUE count), V_Subr1, DOC_goto_prev_char, "p") /*
::doc:goto_prev_char::
goto-prev-char [COUNT]

Moves to the character COUNT characters before the cursor.
::end:: */
{
    VW *vw = curr_vw;
    POS tmp = vw->vw_CursorPos;
    if(prev_char(NUMBERP(count) ? VNUM(count) : 1, &tmp, vw->vw_Tx))
    {
	vw->vw_CursorPos = tmp;
	return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_match_brackets(VALUE pos, VALUE tx, VALUE esc);
DEFUN("match-brackets", cmd_match_brackets, subr_match_brackets, (VALUE pos, VALUE tx, VALUE esc), V_Subr3, DOC_match_brackets) /*
::doc:match_brackets::
match-brackets [POS] [BUFFER] [ESCAPE-CHAR]

Find a bracket matching the one at POS (or the cursor). The things that match
each other are,  { }, ( ), [ ], ` ', < >. POS is altered.
Brackets preceded by ESCAPE-CHAR (`\' by default) are not counted.
::end:: */
{
    u_char esc_char;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = make_lpos(get_tx_cursor(VTX(tx)));
    else
	check_pos(VTX(tx), &VPOS(pos));
    if(NUMBERP(esc))
	esc_char = VNUM(esc);
    else
	esc_char = '\\';
    if(find_matching_bracket(&VPOS(pos), VTX(tx), esc_char))
	return(pos);
    return(sym_nil);
}

_PR VALUE cmd_mouse_pos(void);
DEFUN("mouse-pos", cmd_mouse_pos, subr_mouse_pos, (void), V_Subr0, DOC_mouse_pos) /*
::doc:mouse_pos::
mouse-pos

Return the position of the mouse pointer, relative to the display origin of
the buffer in the current window.
::end:: */
{
    VALUE pos;
    if((pos = make_lpos2(0, 0)) && sys_get_mouse_pos(&VPOS(pos), curr_win))
    {
	/* POS is relative to the window frame, not the buffer in the
	   current view. Translate it. */
	VPOS(pos).pos_Line = VPOS(pos).pos_Line
			     - (curr_vw->vw_TopPix / curr_vw->vw_Win->w_FontY)
			     + curr_vw->vw_StartLine;
	if(VPOS(pos).pos_Line < 0
	   || VPOS(pos).pos_Line >= curr_vw->vw_Tx->tx_NumLines)
	    return sym_nil;
	VPOS(pos).pos_Col = char_col(curr_vw->vw_Tx, VPOS(pos).pos_Col,
				     VPOS(pos).pos_Line);
	return(pos);
    }
    return(sym_nil);
}

static long
move_down_screens(long pages)
{
    VW *vw = curr_vw;
    long newline, rc;
    newline = vw->vw_CursorPos.pos_Line + (pages * vw->vw_MaxY);
    if(newline >= vw->vw_Tx->tx_LogicalEnd)
    {
	newline = vw->vw_Tx->tx_LogicalEnd - 1;
	rc = FALSE;
    }
    else
    {
	vw->vw_StartLine += (pages * vw->vw_MaxY);
	rc = TRUE;
    }
    vw->vw_CursorPos.pos_Line = newline;
    adjust_cursor_to_glyph(vw);
    return(rc);
}

static long
move_up_screens(long pages)
{
    VW *vw = curr_vw;
    long newline, rc;
    newline = vw->vw_CursorPos.pos_Line - (pages * vw->vw_MaxY);
    if(newline < vw->vw_Tx->tx_LogicalStart)
    {
	newline = vw->vw_Tx->tx_LogicalStart;
	rc = FALSE;
    }
    else
    {
	vw->vw_StartLine -= (pages * vw->vw_MaxY);
	if(vw->vw_StartLine < vw->vw_Tx->tx_LogicalStart)
	    vw->vw_StartLine = vw->vw_Tx->tx_LogicalStart;
	rc = TRUE;
    }
    vw->vw_CursorPos.pos_Line = newline;
    adjust_cursor_to_glyph(vw);
    return(rc);
}

static int
find_matching_bracket(POS *pos, TX *tx, u_char esc)
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

    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col < line->ln_Strlen)
    {
	u_char startc = line->ln_Line[pos->pos_Col];
	long i;
	for(i = 0; i < NUM_BRAC_TYPES; i++)
	{
	    if(startc == bracs[i])
		break;
	}
	if(!TST_ESC(line->ln_Line, pos->pos_Col) && (i < NUM_BRAC_TYPES))
	{
	    long x = pos->pos_Col;
	    long y = pos->pos_Line;
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
			if(--y < 0)
			{
			    cmd_signal(sym_error,
				       LIST_1(MKSTR("No matching bracket")));
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
			if(++y >= tx->tx_NumLines)
			{
			    cmd_signal(sym_error,
				       LIST_1(MKSTR("No matching bracket")));
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
	    pos->pos_Col = x;
	    pos->pos_Line = y;
	    return(TRUE);
	}
    }
    cmd_signal(sym_error, LIST_1(MKSTR("No opening bracket")));
    return(FALSE);
}

void
movement_init(void)
{
    ADD_SUBR(subr_screen_top_line);
    ADD_SUBR(subr_screen_bottom_line);
    ADD_SUBR(subr_screen_first_column);
    ADD_SUBR(subr_screen_last_column);
    ADD_SUBR(subr_goto_char);
    ADD_SUBR(subr_goto_glyph);
    ADD_SUBR(subr_centre_display);
    ADD_SUBR(subr_next_screen);
    ADD_SUBR(subr_prev_screen);
    ADD_SUBR(subr_buffer_end);
    ADD_SUBR(subr_goto_buffer_end);
    ADD_SUBR(subr_buffer_start);
    ADD_SUBR(subr_goto_buffer_start);
    ADD_SUBR(subr_line_end);
    ADD_SUBR(subr_goto_line_end);
    ADD_SUBR(subr_line_start);
    ADD_SUBR(subr_goto_line_start);
    ADD_SUBR(subr_next_line);
    ADD_SUBR(subr_goto_next_line);
    ADD_SUBR(subr_prev_line);
    ADD_SUBR(subr_goto_prev_line);
    ADD_SUBR(subr_left_char);
    ADD_SUBR(subr_goto_left_char);
    ADD_SUBR(subr_right_char);
    ADD_SUBR(subr_goto_right_char);
    ADD_SUBR(subr_prev_tab);
    ADD_SUBR(subr_goto_prev_tab);
    ADD_SUBR(subr_next_tab);
    ADD_SUBR(subr_goto_next_tab);
    ADD_SUBR(subr_next_char);
    ADD_SUBR(subr_goto_next_char);
    ADD_SUBR(subr_prev_char);
    ADD_SUBR(subr_goto_prev_char);
    ADD_SUBR(subr_match_brackets);
    ADD_SUBR(subr_mouse_pos);
}
