/* buffers.c -- Buffer handling
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
#include <stdlib.h>

_PR void buffer_sweep(void);
_PR void buffer_prin(VALUE, VALUE);
_PR TX *first_buffer(void);
_PR void swap_buffers(VW *, TX *);
_PR TX *swap_buffers_tmp(VW *, TX *);
_PR VALUE *get_tx_cursor_ptr(TX *tx);
_PR VALUE get_tx_cursor(TX *);
_PR int auto_save_buffers(bool);
_PR void make_marks_resident(TX *);
_PR void make_marks_non_resident(TX *);
_PR void mark_sweep(void);
_PR int mark_cmp(VALUE, VALUE);
_PR void mark_prin(VALUE, VALUE);
_PR void buffers_init(void);
_PR void buffers_kill(void);

/*
 * Chain of all allocated TXs.
 */
_PR TX *buffer_chain;
TX *buffer_chain;

static DEFSYM(auto_save_function, "auto-save-function");

_PR VALUE cmd_make_buffer_name(VALUE);
DEFUN("make-buffer-name", cmd_make_buffer_name, subr_make_buffer_name, (VALUE rawName), V_Subr1, DOC_make_buffer_name) /*
::doc:make_buffer_name::
make-buffer-name NAME

Construct a unique buffer-name from NAME.
::end:: */
{
    int suffix = 1;
    DECLARE1(rawName, STRINGP);
    while(TRUE)
    {
	u_char buf[256];
	u_char *thistry;
	TX *tx = buffer_chain;
	if(suffix != 1)
	{
	    sprintf(buf, "%s<%d>", VSTR(rawName), suffix);
	    thistry = buf;
	}
	else
	    thistry = VSTR(rawName);
	while(tx)
	{
	    if(tx->tx_BufferName && !strcmp(thistry, VSTR(tx->tx_BufferName)))
		break;
	    tx = tx->tx_Next;
	}
	if(!tx)
	{
	    if(suffix == 1)
		return(rawName);
	    return(string_dup(buf));
	}
	suffix++;
    }
}

_PR VALUE cmd_make_buffer(VALUE, VALUE, VALUE);
DEFUN("make-buffer", cmd_make_buffer, subr_make_buffer, (VALUE name, VALUE oldTx, VALUE litName), V_Subr3, DOC_make_buffer) /*
::doc:make_buffer::
make-buffer NAME

Return a new buffer, it's name is the result of (make-buffer-name NAME).
::end:: */
{
    TX *tx;
    DECLARE1(name, STRINGP);
    if(!BUFFERP(oldTx))
    {
	if(curr_vw)
	    oldTx = VAL(curr_vw->vw_Tx);
	else
	    oldTx = LISP_NULL;
    }
    tx = ALLOC_OBJECT(sizeof(TX));
    if(tx != NULL)
    {
	memset(tx, 0, sizeof(TX));
	if(clear_line_list(tx))
	{
	    tx->tx_Car = V_Buffer;
	    tx->tx_BufferName = NILP(litName)
	        ? cmd_make_buffer_name(name) : name;
	    if(tx->tx_BufferName)
	    {
		tx->tx_Next = buffer_chain;
		buffer_chain = tx;
		data_after_gc += sizeof(TX);

		tx->tx_FileName = null_string();
		tx->tx_MinorModeNameList = sym_nil;
		tx->tx_MinorModeNameString = null_string();
		tx->tx_SavedBlockStatus = -1;
		tx->tx_TabSize = 8;
		tx->tx_LocalVariables = sym_nil;
		tx->tx_GlyphTable = (oldTx
				     ? VTX(oldTx)->tx_GlyphTable
				     : cmd_default_glyph_table());
		tx->tx_LastSaveTime = sys_time();
		tx->tx_UndoList = sym_nil;
		tx->tx_ToUndoList = LISP_NULL;
		tx->tx_UndoneList = sym_nil;
		tx->tx_SavedCPos = make_pos(0, 0);
		tx->tx_SavedWPos = tx->tx_SavedCPos;
		return(VAL(tx));
	    }
	}
	FREE_OBJECT(tx);
    }
    return LISP_NULL;
}

_PR VALUE cmd_destroy_buffer(VALUE);
DEFUN("destroy-buffer", cmd_destroy_buffer, subr_destroy_buffer, (VALUE tx), V_Subr1, DOC_destroy_buffer) /*
::doc:destroy_buffer::
destory-buffer BUFFER

Throw away everything associated with buffer. All resident marks are made
non-resident.
::end:: */
{
    DECLARE1(tx, BUFFERP);
    make_marks_non_resident(VTX(tx));
    clear_line_list(VTX(tx));
    VTX(tx)->tx_FileName = null_string();
    VTX(tx)->tx_BufferName = null_string();
    VTX(tx)->tx_ModeName = LISP_NULL;
    VTX(tx)->tx_MinorModeNameList = sym_nil;
    VTX(tx)->tx_MinorModeNameString = null_string();
    VTX(tx)->tx_Changes = 0;
    VTX(tx)->tx_LocalVariables = sym_nil;
    VTX(tx)->tx_GlyphTable = cmd_default_glyph_table();
    VTX(tx)->tx_Flags |= TXFF_RDONLY | TXFF_REFRESH_ALL | TXFF_NO_UNDO;
    VTX(tx)->tx_UndoList = sym_nil;
    VTX(tx)->tx_ToUndoList = LISP_NULL;
    VTX(tx)->tx_UndoneList = sym_nil;
#if 0
    sm_flush(&main_strmem);
#endif
    return(sym_t);
}

void
buffer_sweep(void)
{
    TX *tx = buffer_chain;
    buffer_chain = NULL;
    while(tx)
    {
	TX *nxt = tx->tx_Next;
	if(!GC_CELL_MARKEDP(VAL(tx)))
	{
	    make_marks_non_resident(tx);
	    kill_line_list(tx);
	    FREE_OBJECT(tx);
	}
	else
	{
	    GC_CLR_CELL(VAL(tx));
	    tx->tx_Next = buffer_chain;
	    buffer_chain = tx;
	}
	tx = nxt;
    }
}
void
buffer_prin(VALUE strm, VALUE obj)
{
    stream_puts(strm, "#<buffer ", -1, FALSE);
    stream_puts(strm, VPTR(VTX(obj)->tx_BufferName), -1, TRUE);
    stream_putc(strm, '>');
}

DEFSTRING(first_buffer_name, "*jade*");

TX *
first_buffer(void)
{
    TX *tx = VTX(cmd_make_buffer(VAL(&first_buffer_name), sym_nil, sym_t));
    if(!curr_win)
    {
	curr_win = VWIN(cmd_make_window(sym_nil, sym_nil, sym_nil, sym_nil));
	if(!curr_win)
	    return(NULL);
    }
    if(tx)
    {
	swap_buffers(curr_vw, tx);
#ifndef NOSCRLBAR
	sys_update_scroller(curr_vw);
#endif
	return(tx);
    }
    return(NULL);
}

/*
 * Makes `new' the file being shown in window `vw'
 */
void
swap_buffers(VW *vw, TX *new)
{
    TX *old = vw->vw_Tx;
    if(old != new)
    {
	if(old)
	{
	    old->tx_SavedCPos = vw->vw_CursorPos;
	    old->tx_SavedWPos = vw->vw_DisplayOrigin;
	    old->tx_SavedBlockPos[0] = vw->vw_BlockS;
	    old->tx_SavedBlockPos[1] = vw->vw_BlockE;
	    old->tx_SavedBlockStatus = vw->vw_BlockStatus;
	}
	vw->vw_Tx = new;
	vw->vw_CursorPos = new->tx_SavedCPos;
	vw->vw_DisplayOrigin = new->tx_SavedWPos;
	vw->vw_BlockS = new->tx_SavedBlockPos[0];
	vw->vw_BlockE = new->tx_SavedBlockPos[1];
	vw->vw_BlockStatus = new->tx_SavedBlockStatus;
	vw->vw_LastRefTx = NULL;
    }
}
/*
 * "nd" means non-destructive. refcount's of buffers are not changed and
 * previous buffer shown is returned.
 * This is intended to allow *temporary* switching of buffers before
 * reinstalling the original.
 * ** this is kind of obsolete but never mind **
 */
TX *
swap_buffers_tmp(VW *vw, TX *new)
{
    TX *old = vw->vw_Tx;
    if(old != new)
    {
	if(old)
	{
	    old->tx_SavedCPos = vw->vw_CursorPos;
	    old->tx_SavedWPos = vw->vw_DisplayOrigin;
	    old->tx_SavedBlockPos[0] = vw->vw_BlockS;
	    old->tx_SavedBlockPos[1] = vw->vw_BlockE;
	    old->tx_SavedBlockStatus = vw->vw_BlockStatus;
	}
	vw->vw_Tx = new;
	vw->vw_CursorPos = new->tx_SavedCPos;
	vw->vw_DisplayOrigin = new->tx_SavedWPos;
	vw->vw_BlockS = new->tx_SavedBlockPos[0];
	vw->vw_BlockE = new->tx_SavedBlockPos[1];
	vw->vw_BlockStatus = new->tx_SavedBlockStatus;
    }
    return(old);
}

_PR VALUE cmd_get_file_buffer(VALUE);
DEFUN("get-file-buffer", cmd_get_file_buffer, subr_get_file_buffer, (VALUE name), V_Subr1, DOC_get_file_buffer) /*
::doc:get_file_buffer::
get-file-buffer NAME

Scan all buffers for one containing the file NAME.
::end:: */
{
    TX *tx = buffer_chain;
    DECLARE1(name, STRINGP);
    while(tx)
    {
	if(same_files(VSTR(name), VSTR(tx->tx_FileName)))
	    return(VAL(tx));
	tx = tx->tx_Next;
    }
    return(sym_nil);
}

_PR VALUE cmd_get_buffer(VALUE);
DEFUN("get-buffer", cmd_get_buffer, subr_get_buffer, (VALUE name), V_Subr1, DOC_get_buffer) /*
::doc:get_buffer::
get-buffer NAME

Scan all buffers for one whose name is NAME.
::end:: */
{
    TX *tx = buffer_chain;
    if(BUFFERP(name))
	return(name);
    DECLARE1(name, STRINGP);
    while(tx)
    {
	if(!strcmp(VSTR(name), VSTR(tx->tx_BufferName)))
	    return(VAL(tx));
	tx = tx->tx_Next;
    }
    return(sym_nil);
}

VALUE *
get_tx_cursor_ptr(TX *tx)
{
    VW *vw;

    /* Check active view first */
    if(curr_vw->vw_Tx == tx)
	return(&curr_vw->vw_CursorPos);

    /* Then other views in the same window. */
    for(vw = curr_win->w_ViewList; vw != 0; vw = vw->vw_NextView)
    {
	if(vw->vw_Win && vw->vw_Win->w_Window && (vw->vw_Tx == tx))
	    return(&vw->vw_CursorPos);
    }

    /* Finally all other windows */
    for(vw = view_chain; vw != 0; vw = vw->vw_Next)
    {
	if(vw->vw_Win && vw->vw_Win->w_Window && (vw->vw_Tx == tx))
	    return(&vw->vw_CursorPos);
    }

    return(&tx->tx_SavedCPos);
}    

VALUE
get_tx_cursor(TX *tx)
{
    return *get_tx_cursor_ptr(tx);
}

/* returns the number of buffers saved.
   (maybe should only save one buffer at a time, then wait to be called
   again to save next in line? This could be less intrusive: yes.)

   If force_save is true, don't worry about the time between saves,
   just save the next buffer */
int
auto_save_buffers(bool force_save)
{
    /*
     * Stops me entering here more than once at the same time. This
     * can happen when tracing the `auto-save-function'
     */
    static bool Exclusion;
    if(!Exclusion)
    {
	TX *tx = buffer_chain;
	u_long time = sys_time();
	Exclusion = TRUE;
	while(tx)
	{
	    if(tx->tx_Changes
	       && tx->tx_AutoSaveInterval
	       && (tx->tx_LastSaveChanges != tx->tx_Changes)
	       && (force_save
	           || (time > (tx->tx_LastSaveTime + tx->tx_AutoSaveInterval))))
	    {
		VALUE val_tx = VAL(tx);
		GC_root gc_tx;
		PUSHGC(gc_tx, val_tx);
		call_lisp1(sym_auto_save_function, VAL(tx));
		POPGC;
		tx->tx_LastSaveTime = time;
		tx->tx_LastSaveChanges = tx->tx_Changes;
		Exclusion = FALSE;
		return(1);
	    }
	    tx = tx->tx_Next;
	}
	Exclusion = FALSE;
    }
    return(0);
}

_PR VALUE cmd_current_buffer(VALUE);
DEFUN("current-buffer", cmd_current_buffer, subr_current_buffer, (VALUE vw), V_Subr1, DOC_current_buffer) /*
::doc:current_buffer::
current-buffer [VIEW]

Return the buffer that VIEW (or the current view) is displaying.
::end:: */
{
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    return(VAL(VVIEW(vw)->vw_Tx));
}

_PR VALUE cmd_set_current_buffer(VALUE, VALUE);
DEFUN("set-current-buffer", cmd_set_current_buffer, subr_set_current_buffer, (VALUE tx, VALUE vw), V_Subr2, DOC_set_current_buffer) /*
::doc:set_current_buffer::
set-current-buffer BUFFER [VIEW]

Set the buffer that VIEW (or the current view) is displaying. Returns
the buffer which was being displayed before.
::end:: */
{
    VALUE old;
    DECLARE1(tx, BUFFERP);
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    old = VAL(VVIEW(vw)->vw_Tx);
    swap_buffers(VVIEW(vw), VTX(tx));
    return(old);
}

_PR VALUE cmd_buffer_file_name(VALUE);
DEFUN("buffer-file-name", cmd_buffer_file_name, subr_buffer_file_name, (VALUE tx), V_Subr1, DOC_buffer_file_name) /*
::doc:buffer_file_name::
buffer-file-name [BUFFER]

Return the name of the file being edited in BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return(VTX(tx)->tx_FileName);
}

_PR VALUE cmd_set_buffer_file_name(VALUE, VALUE);
DEFUN("set-buffer-file-name", cmd_set_buffer_file_name, subr_set_buffer_file_name, (VALUE tx, VALUE name), V_Subr2, DOC_set_buffer_file_name) /*
::doc:set_buffer_file_name::
set-buffer-file-name BUFFER NAME

Set the name of the file being edited in BUFFER to NAME.
::end:: */
{
    DECLARE1(name, STRINGP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    make_marks_non_resident(VTX(tx));
    VTX(tx)->tx_FileName = name;
    VTX(tx)->tx_Flags |= TXFF_REFRESH_STATUS;
    make_marks_resident(VTX(tx));
    return(name);
}

_PR VALUE cmd_buffer_name(VALUE);
DEFUN("buffer-name", cmd_buffer_name, subr_buffer_name, (VALUE tx), V_Subr1, DOC_buffer_name) /*
::doc:buffer_name::
buffer-name [BUFFER]

Return the name of BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return(VTX(tx)->tx_BufferName);
}

_PR VALUE cmd_set_buffer_name(VALUE, VALUE);
DEFUN("set-buffer-name", cmd_set_buffer_name, subr_set_buffer_name, (VALUE tx, VALUE name), V_Subr2, DOC_set_buffer_name) /*
::doc:set_buffer_name::
set-buffer-name BUFFER NAME

Set the name of BUFFER to NAME.
::end:: */
{
    DECLARE1(name, STRINGP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    VTX(tx)->tx_BufferName = name;
    VTX(tx)->tx_Flags |= TXFF_REFRESH_STATUS;
    sys_reset_sleep_titles(VTX(tx));
    return(name);
}

_PR VALUE cmd_buffer_changes(VALUE);
DEFUN("buffer-changes", cmd_buffer_changes, subr_buffer_changes, (VALUE tx), V_Subr1, DOC_buffer_changes) /*
::doc:buffer_changes::
buffer-changes [BUFFER]

Return the number of modifications to BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return(MAKE_INT(VTX(tx)->tx_Changes));
}

_PR VALUE cmd_buffer_modified_p(VALUE);
DEFUN("buffer-modified-p", cmd_buffer_modified_p, subr_buffer_modified_p, (VALUE tx), V_Subr1, DOC_buffer_modified_p) /*
::doc:buffer_modified_p::
buffer-modified-p [BUFFER]

Returns t if the buffer has changed since it was last saved.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    /* TXFF_SPECIAL means this buffer's modifications aren't important. */
    if(VTX(tx)->tx_Flags & TXFF_SPECIAL)
	return(sym_nil);
    if(VTX(tx)->tx_Changes != VTX(tx)->tx_ProperSaveChanges)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_set_buffer_modified(VALUE, VALUE);
DEFUN("set-buffer-modified", cmd_set_buffer_modified, subr_set_buffer_modified, (VALUE buf, VALUE stat), V_Subr2, DOC_set_buffer_modified) /*
::doc:set_buffer_modified::
set-buffer-modified BUFFER STATUS

If STATUS is nil make it look as though buffer hasn't changed, else make
it look as though it has.
::end:: */
{
    TX *tx =BUFFERP(buf) ? VTX(buf) : curr_vw->vw_Tx;
    if(NILP(stat))
    {
	tx->tx_ProperSaveChanges = tx->tx_Changes;
	tx->tx_LastSaveChanges = tx->tx_Changes;
    }
    else
    {
	tx->tx_ProperSaveChanges = tx->tx_Changes - 1;
	tx->tx_LastSaveChanges = tx->tx_Changes - 1;
    }
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
    return VAL(tx);
}

_PR VALUE cmd_set_buffer_special(VALUE tx, VALUE specialp);
DEFUN("set-buffer-special", cmd_set_buffer_special, subr_set_buffer_special, (VALUE tx, VALUE specialp), V_Subr2, DOC_set_buffer_special) /*
::doc:set_buffer_special::
set-buffer-special BUFFER SPECIALP

When a buffer is `special' it means that it is controlled by some Lisp code,
not by the user typing into it (although this can still happen as well). This
is used for things like the `*jade*' or `*Info*' buffers (in fact most of
the buffers whose names are surrounded by asterisks are special).

What the `special' attribute actually does is make sure that the buffer is
never truely killed (`kill-buffer' removes it from each window's `buffer-list'
but doesn't detroy the actual contents) and modifications don't cause the
`+' marker to appear in the status line.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(NILP(specialp))
	VTX(tx)->tx_Flags &= ~TXFF_SPECIAL;
    else
	VTX(tx)->tx_Flags |= TXFF_SPECIAL;
    VTX(tx)->tx_Flags |= TXFF_REFRESH_STATUS;
    return(tx);
}

_PR VALUE cmd_buffer_special_p(VALUE tx);
DEFUN("buffer-special-p", cmd_buffer_special_p, subr_buffer_special_p, (VALUE tx), V_Subr1, DOC_buffer_special_p) /*
::doc:buffer_special_p::
buffer-special-p [BUFFER]

Returns t if BUFFER is ``special''. See `set-buffer-special' for the meaning of
the ``special'' attribute.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(VTX(tx)->tx_Flags & TXFF_SPECIAL)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_set_buffer_read_only(VALUE tx, VALUE stat);
DEFUN("set-buffer-read-only", cmd_set_buffer_read_only, subr_set_buffer_read_only, (VALUE tx, VALUE stat), V_Subr2, DOC_set_buffer_read_only) /*
::doc:set_buffer_read_only::
set-buffer-read-only BUFFER READ-ONLY-P

If a buffer is read-only no modification of its contents is allowed.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(NILP(stat))
	VTX(tx)->tx_Flags &= ~TXFF_RDONLY;
    else
	VTX(tx)->tx_Flags |= TXFF_RDONLY;
    VTX(tx)->tx_Flags |= TXFF_REFRESH_STATUS;
    return(tx);
}

_PR VALUE cmd_buffer_read_only_p(VALUE tx);
DEFUN("buffer-read-only-p", cmd_buffer_read_only_p, subr_buffer_read_only_p, (VALUE tx), V_Subr1, DOC_buffer_read_only_p) /*
::doc:buffer_read_only_p::
buffer-read-only-p [BUFFER]

Returns t if BUFFER is read-only. See `set-buffer-read-only'.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(VTX(tx)->tx_Flags & TXFF_RDONLY)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_buffer_length(VALUE);
DEFUN("buffer-length", cmd_buffer_length, subr_buffer_length, (VALUE tx), V_Subr1, DOC_buffer_length) /*
::doc:buffer_length::
buffer-length [BUFFER]

Returns the number of lines in BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return(MAKE_INT(VTX(tx)->tx_NumLines));
}

_PR VALUE cmd_line_length(VALUE, VALUE);
DEFUN("line-length", cmd_line_length, subr_line_length, (VALUE pos, VALUE tx), V_Subr2, DOC_line_length) /*
::doc:line_length::
line-length [LINE-POS] [BUFFER]

Returns the length (not including newline) of the specified line, or
using current cursor position if specifiers are not provided.
::end:: */
{
    if(POSP(pos))
    {
	if(!BUFFERP(tx))
	    tx = VAL(curr_vw->vw_Tx);
    }
    else
    {
	pos = curr_vw->vw_CursorPos;
	tx = VAL(curr_vw->vw_Tx);
    }
    return(MAKE_INT(VTX(tx)->tx_Lines[VROW(pos)].ln_Strlen - 1));
}

_PR VALUE cmd_with_buffer(VALUE);
DEFUN("with-buffer", cmd_with_buffer, subr_with_buffer, (VALUE args), V_SF, DOC_with_buffer) /*
::doc:with_buffer::
with-buffer BUFFER FORMS...

Temporarily switches to buffer, then executes the FORMS in it before
returning to the original buffer.
::end:: */
{
    if(CONSP(args))
    {
	GC_root gc_args;
	VALUE res;
	PUSHGC(gc_args, args);
	if((res = cmd_eval(VCAR(args))) && BUFFERP(res))
	{
	    VALUE oldtx = VAL(swap_buffers_tmp(curr_vw, VTX(res)));
	    if(oldtx)
	    {
		VALUE oldcurrvw = VAL(curr_vw);
		GC_root gc_oldtx, gc_oldcurrvw;
		PUSHGC(gc_oldtx, oldtx);
		PUSHGC(gc_oldcurrvw, oldcurrvw);
		res = cmd_progn(VCDR(args));
		POPGC; POPGC;
		if(VVIEW(oldcurrvw)->vw_Win)
		    swap_buffers_tmp(VVIEW(oldcurrvw), VTX(oldtx));
	    }
	}
	else
	    res = signal_arg_error(res, 1);
	POPGC;
	return(res);
    }
    return LISP_NULL;
}

_PR VALUE cmd_bufferp(VALUE);
DEFUN("bufferp", cmd_bufferp, subr_bufferp, (VALUE arg), V_Subr1, DOC_bufferp) /*
::doc:bufferp::
bufferp ARG

Returns t if ARG is a buffer.
::end:: */
{
    if(BUFFERP(arg))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_restrict_buffer(VALUE start, VALUE end, VALUE tx);
DEFUN_INT("restrict-buffer", cmd_restrict_buffer, subr_restrict_buffer, (VALUE start, VALUE end, VALUE tx), V_Subr3, DOC_restrict_buffer, "-m" DS_NL "M") /*
::doc:restrict_buffer::
restrict-buffer START END [BUFFER]

Limits the portion of BUFFER (or the current buffer) that may be displayed
to that between the lines specified by positions START and END.
::end:: */
{
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    cmd_unrestrict_buffer(tx);
    if(check_section(VTX(tx), &start, &end) && VROW(start) <= VROW(end))
    {
	VTX(tx)->tx_LogicalStart = VROW(start);
	VTX(tx)->tx_LogicalEnd = VROW(end) + 1;
	return sym_t;
    }
    return sym_nil;
}

_PR VALUE cmd_unrestrict_buffer(VALUE tx);
DEFUN_INT("unrestrict-buffer", cmd_unrestrict_buffer, subr_unrestrict_buffer, (VALUE tx), V_Subr1, DOC_unrestrict_buffer, "") /*
::doc:unrestrict_buffer::
unrestrict-buffer [BUFFER]

Remove any restriction on the parts of BUFFER that may be displayed.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    VTX(tx)->tx_LogicalStart = 0;
    VTX(tx)->tx_LogicalEnd = VTX(tx)->tx_NumLines;
    VTX(tx)->tx_Flags |= TXFF_REFRESH_ALL;
    return sym_t;
}
_PR VALUE cmd_restriction_start(VALUE tx);
DEFUN("restriction-start", cmd_restriction_start, subr_restriction_start, (VALUE tx), V_Subr1, DOC_restriction_start) /*
::doc:restriction_start::
restriction-start [BUFFER]

Return the position of the first character that may be displayed in BUFFER
(or the current buffer).
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return make_pos(0, VTX(tx)->tx_LogicalStart);
}

_PR VALUE cmd_restriction_end(VALUE tx);
DEFUN("restriction-end", cmd_restriction_end, subr_restriction_end, (VALUE tx), V_Subr1, DOC_restriction_end) /*
::doc:restriction_end::
restriction-end [BUFFER]

Return the position of the last character that may be displayed in BUFFER
(or the current buffer).
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return make_pos(VTX(tx)->tx_Lines[VTX(tx)->tx_LogicalEnd - 1].ln_Strlen -1,
		    VTX(tx)->tx_LogicalEnd - 1);
}

_PR VALUE cmd_in_restriction_p(VALUE pos, VALUE tx);
DEFUN("in-restriction-p", cmd_in_restriction_p, subr_in_restriction_p, (VALUE pos, VALUE tx), V_Subr2, DOC_in_restriction_p) /*
::doc:in_restriction_p::
in-restriction-p [POS] [BUFFER]

Returns t when POS (or the cursor) is inside the current display restriction
of BUFFER (or the current buffer).
::end:: */
{
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return (VROW(pos) >= VTX(tx)->tx_LogicalStart
	    && VROW(pos) < VTX(tx)->tx_LogicalEnd) ? sym_t : sym_nil;
}

_PR VALUE var_auto_save_interval(VALUE);
DEFUN("auto-save-interval", var_auto_save_interval, subr_auto_save_interval, (VALUE val), V_Var, DOC_auto_save_interval) /*
::doc:auto_save_interval::
This buffer-local variable defines the period (in seconds) between each
automatic save of the buffer. A value of zero means that this buffer is
not to be auto-saved.
::end:: */
{
    return(handle_var_int(val, &curr_vw->vw_Tx->tx_AutoSaveInterval));
}

_PR VALUE var_last_save_changes(VALUE);
DEFUN("last-save-changes", var_last_save_changes, subr_last_save_changes, (VALUE val), V_Var, DOC_last_save_changes) /*
::doc:last_save_changes::
Number of changes the last time this buffer was saved (could be auto-save).
::end:: */
{
    return(handle_var_int(val, &curr_vw->vw_Tx->tx_LastSaveChanges));
}

_PR VALUE var_last_user_save_changes(VALUE);
DEFUN("last-user-save-changes", var_last_user_save_changes, subr_last_user_save_changes, (VALUE val), V_Var, DOC_last_user_save_changes) /*
::doc:last_user_save_changes::
Number of changes the last time this buffer was saved (not from auto-save).
::end:: */
{
    return(handle_var_int(val, &curr_vw->vw_Tx->tx_ProperSaveChanges));
}

_PR VALUE var_last_save_time(VALUE);
DEFUN("last-save-time", var_last_save_time, subr_last_save_time, (VALUE val), V_Var, DOC_last_save_time) /*
::doc:last_save_time::
System time at last save of this buffer (could be from an auto-save).
::end:: */
{
    return(handle_var_long_int(val, &curr_vw->vw_Tx->tx_LastSaveTime));
}

_PR VALUE var_tab_size(VALUE);
DEFUN("tab-size", var_tab_size, subr_tab_size, (VALUE val), V_Var, DOC_tab_size) /*
::doc:tab_size::
Sets the size of tab-stops.
::end:: */
{
    return(handle_var_int(val, &curr_vw->vw_Tx->tx_TabSize));
}

_PR VALUE var_mode_name(VALUE);
DEFUN("mode-name", var_mode_name, subr_mode_name, (VALUE val), V_Var, DOC_mode_name) /*
::doc:mode_name::
This is used to display the name of the edit-mode being used in the status
line.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(val)
    {
	if(STRINGP(val))
	    tx->tx_ModeName = val;
	else
	    tx->tx_ModeName = LISP_NULL;
	return(val);
    }
    else if(tx->tx_ModeName)
	return(tx->tx_ModeName);
    VTX(tx)->tx_Flags |= TXFF_REFRESH_STATUS;
    return(sym_nil);
}

_PR VALUE var_minor_mode_names(VALUE);
DEFUN("minor-mode-names", var_minor_mode_names, subr_minor_mode_names, (VALUE val), V_Var, DOC_minor_mode_names) /*
::doc:minor_mode_names::
List of strings naming all minor-modes enabled in this buffer.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(!val)
	return(tx->tx_MinorModeNameList);
    if(!CONSP(val))
	val = sym_nil;
    tx->tx_MinorModeNameList = val;
    if(NILP(val))
	tx->tx_MinorModeNameString = null_string();
    else
    {
	int len;
	u_char *str;
	VALUE tmp = val;
	for(len = 0; CONSP(tmp) && STRINGP(VCAR(tmp)); tmp = VCDR(tmp))
	    len += STRING_LEN(VCAR(tmp)) + 1;
	tx->tx_MinorModeNameString = make_string(len + 1);
	str = VSTR(tx->tx_MinorModeNameString);
	tmp = val;
	while(CONSP(tmp) && STRINGP(VCAR(tmp)))
	{
	    *str++ = ' ';
	    str = stpcpy(str, VSTR(VCAR(tmp)));
	    tmp = VCDR(tmp);
	}
    }
    VTX(tx)->tx_Flags |= TXFF_REFRESH_STATUS;
    return(val);
}

/* chain of all non-resident marks, linked via `mk_Next' */
static Mark *non_resident_mark_chain;
/* chain of all marks, linked via `mk_NextAlloc' */
static Mark *mark_chain;

/*
 * For all non-resident marks, see if any point to NEWTX, if so link them
 * onto NEWTX's `tx_Marks' chain.
 */
void
make_marks_resident(TX *newtx)
{
    Mark *nxt, *mk = non_resident_mark_chain;
    non_resident_mark_chain = NULL;
    while(mk)
    {
	nxt = mk->mk_Next;
	if(same_files(VSTR(newtx->tx_FileName), VSTR(mk->mk_File.name)))
	{
	    mk->mk_File.tx = newtx;
	    mk->mk_Flags |= MKFF_RESIDENT;
	    mk->mk_Next = newtx->tx_MarkChain;
	    newtx->tx_MarkChain = mk;
	}
	else
	{
	    mk->mk_Next = non_resident_mark_chain;
	    non_resident_mark_chain = mk;
	}
	mk = nxt;
    }
}

void
make_marks_non_resident(TX *oldtx)
{
    Mark *nxt, *mk = oldtx->tx_MarkChain;
    oldtx->tx_MarkChain = NULL;
    while(mk)
    {
	nxt = mk->mk_Next;
	mk->mk_File.name = oldtx->tx_FileName;
	mk->mk_Flags &= ~MKFF_RESIDENT;
	mk->mk_Next = non_resident_mark_chain;
	non_resident_mark_chain = mk;
	mk = nxt;
    }
}

/*
 * Takes MK off the buffer mark chain that it's on (or the non_resident_mark_chain).
 */
static void
unchain_mark(Mark *mk)
{
    Mark **headp, *this;
    if(!(mk->mk_Flags & MKFF_RESIDENT))
	headp = &non_resident_mark_chain;
    else
	headp = &(mk->mk_File.tx->tx_MarkChain);
    this = *headp;
    *headp = NULL;
    while(this)
    {
	Mark *tmp = this->mk_Next;
	if(this != mk)
	{
	    this->mk_Next = *headp;
	    *headp = this;
	}
	this = tmp;
    }
}

void
mark_sweep(void)
{
    Mark *mk = mark_chain;
    mark_chain = NULL;
    while(mk)
    {
	Mark *nxt = mk->mk_NextAlloc;
	if(!GC_CELL_MARKEDP(VAL(mk)))
	{
	    unchain_mark(mk);
	    FREE_OBJECT(mk);
	}
	else
	{
	    GC_CLR_CELL(VAL(mk));
	    mk->mk_NextAlloc = mark_chain;
	    mark_chain = mk;
	}
	mk = nxt;
    }
}

int
mark_cmp(VALUE v1, VALUE v2)
{
    int rc = 1;
    if(VTYPE(v1) == VTYPE(v2))
    {
	u_char *name1, *name2;
	if(VMARK(v1)->mk_Flags & MKFF_RESIDENT)
	    name1 = VSTR(VMARK(v1)->mk_File.tx->tx_FileName);
	else
	    name1 = VSTR(VMARK(v1)->mk_File.name);
	if(VMARK(v2)->mk_Flags & MKFF_RESIDENT)
	    name2 = VSTR(VMARK(v2)->mk_File.tx->tx_FileName);
	else
	    name2 = VSTR(VMARK(v2)->mk_File.name);
	if(same_files(name1, name2))
	{
	    if(!(rc = VROW(VMARK(v1)->mk_Pos) - VROW(VMARK(v2)->mk_Pos)))
		rc = VCOL(VMARK(v1)->mk_Pos) - VROW(VMARK(v2)->mk_Pos);
	}
    }
    return(rc);
}

void
mark_prin(VALUE strm, VALUE obj)
{
    u_char tbuf[40];
    stream_puts(strm, "#<mark ", -1, FALSE);
    if(VMARK(obj)->mk_Flags & MKFF_RESIDENT)
	buffer_prin(strm, VAL(VMARK(obj)->mk_File.tx));
    else
    {
	stream_putc(strm, '"');
	stream_puts(strm, VPTR(VMARK(obj)->mk_File.name), -1, TRUE);
	stream_putc(strm, '"');
    }
    sprintf(tbuf, " #<pos %ld %ld>>",
	    VCOL(VMARK(obj)->mk_Pos),
	    VROW(VMARK(obj)->mk_Pos));
    stream_puts(strm, tbuf, -1, FALSE);
}

_PR VALUE cmd_make_mark(VALUE, VALUE);
DEFUN("make-mark", cmd_make_mark, subr_make_mark, (VALUE pos, VALUE buffer), V_Subr2, DOC_make_mark) /*
::doc:make_mark::
make-mark [POS] [BUFFER | FILE-NAME]

Creates a new mark pointing to position POS either in the current file
or in FILE-NAME, or BUFFER.

Note that FILE-NAME doesn't have to be a file that has been loaded, it's
stored as a string, the file it points to is only opened when needed.

Unlike position objects, the position in a file that a mark points to is
updated as the file changes -- it will always point to the same character
(for as long as that character exists, anyway).
::end:: */
{
    Mark *mk = ALLOC_OBJECT(sizeof(Mark));
    if(mk != NULL)
    {
	mk->mk_Car = V_Mark;
	mk->mk_NextAlloc = mark_chain;
	mark_chain = mk;
	data_after_gc += sizeof(Mark);
	mk->mk_Pos = POSP(pos) ? pos : curr_vw->vw_CursorPos;
	if(STRINGP(buffer))
	{
	    VALUE tx;
	    if((tx = cmd_get_file_buffer(buffer)) && BUFFERP(tx))
	    {
		mk->mk_Flags |= MKFF_RESIDENT;
		mk->mk_File.tx = VTX(tx);
		mk->mk_Next = VTX(tx)->tx_MarkChain;
		VTX(tx)->tx_MarkChain = mk;
	    }
	    else
	    {
		mk->mk_Flags |= MKFF_RESIDENT;
		mk->mk_File.name = buffer;
		mk->mk_Next = non_resident_mark_chain;
		non_resident_mark_chain = mk;
	    }
	}
	else
	{
	    if(!BUFFERP(buffer))
		buffer = VAL(curr_vw->vw_Tx);
	    mk->mk_Flags |= MKFF_RESIDENT;
	    mk->mk_File.tx = VTX(buffer);
	    mk->mk_Next = VTX(buffer)->tx_MarkChain;
	    VTX(buffer)->tx_MarkChain = mk;
	}
	return(VAL(mk));
    }
    return mem_error();
}

_PR VALUE cmd_set_mark(VALUE, VALUE, VALUE);
DEFUN("set-mark", cmd_set_mark, subr_set_mark, (VALUE mark, VALUE pos, VALUE buffer), V_Subr3, DOC_set_mark) /*
::doc:set_mark::
set-mark MARK [POS] [FILE-NAME | BUFFER]

Sets the position which MARK points to POS in FILE-NAME or BUFFER.
::end:: */
{
    DECLARE1(mark, MARKP);
    if(POSP(pos))
	VMARK(mark)->mk_Pos = pos;
    if(BUFFERP(buffer) || STRINGP(buffer))
    {
	Mark *mk, **chain;
	if(VMARK(mark)->mk_Flags & MKFF_RESIDENT)
	    chain = &(VMARK(mark)->mk_File.tx->tx_MarkChain);
	else
	    chain = &non_resident_mark_chain;
	mk = *chain;
	*chain = NULL;
	while(mk)
	{
	    Mark *nxt = mk->mk_Next;
	    if(mk != VMARK(mark))
	    {
		mk->mk_Next = *chain;
		*chain = mk;
	    }
	    else
		mk->mk_Next = NULL;
	    mk = nxt;
	}
	VMARK(mark)->mk_File.name = buffer;
	switch(VTYPE(buffer))
	{
	    VALUE tmp;
	case V_String:
	    tmp = cmd_get_file_buffer(buffer);
	    if((tmp == LISP_NULL) || NILP(tmp))
	    {
		VMARK(mark)->mk_Flags &= ~MKFF_RESIDENT;
		VMARK(mark)->mk_Next = non_resident_mark_chain;
		non_resident_mark_chain = VMARK(mark);
		break;
	    }
	    VMARK(mark)->mk_File.name = tmp;
	    /* FALL THROUGH */
	case V_Buffer:
	    VMARK(mark)->mk_Flags |= MKFF_RESIDENT;
	    VMARK(mark)->mk_Next = VMARK(mark)->mk_File.tx->tx_MarkChain;
	    VMARK(mark)->mk_File.tx->tx_MarkChain = VMARK(mark);
	    break;
	}
    }
    return(mark);
}

_PR VALUE cmd_mark_pos(VALUE);
DEFUN("mark-pos", cmd_mark_pos, subr_mark_pos, (VALUE mark), V_Subr1, DOC_mark_pos) /*
::doc:mark_pos::
mark-pos MARK

Returns the position that MARK points to. (note that this is the *same*
object that the mark stores internally -- so don't modify it unless you're
really sure you know what you're doing)
::end:: */
{
    DECLARE1(mark, MARKP);
    return VMARK(mark)->mk_Pos;
}

_PR VALUE cmd_mark_file(VALUE);
DEFUN("mark-file", cmd_mark_file, subr_mark_file, (VALUE mark), V_Subr1, DOC_mark_file) /*
::doc:mark_file::
mark-file MARK

Returns the file-name or buffer that MARK points to.
::end:: */
{
    DECLARE1(mark, MARKP);
    return(VMARK(mark)->mk_File.name);
}

_PR VALUE cmd_mark_resident_p(VALUE);
DEFUN("mark-resident-p", cmd_mark_resident_p, subr_mark_resident_p, (VALUE mark), V_Subr1, DOC_mark_resident_p) /*
::doc:mark_resident_p::
mark-resident-p MARK

Returns t if the file that MARK points to is in a buffer.
::end:: */
{
    DECLARE1(mark, MARKP);
    if(VMARK(mark)->mk_Flags & MKFF_RESIDENT)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_markp(VALUE);
DEFUN("markp", cmd_markp, subr_markp, (VALUE mark), V_Subr1, DOC_markp) /*
::doc:markp::
markp ARG

Return t if ARG is a mark.
::end:: */
{
    if(MARKP(mark))
	return(sym_t);
    return(sym_nil);
}

void
buffers_init(void)
{
    mark_static((VALUE *)&non_resident_mark_chain);
    INTERN(auto_save_function);
    ADD_SUBR(subr_make_buffer_name);
    ADD_SUBR(subr_make_buffer);
    ADD_SUBR(subr_destroy_buffer);
    ADD_SUBR(subr_get_file_buffer);
    ADD_SUBR(subr_get_buffer);
    ADD_SUBR(subr_current_buffer);
    ADD_SUBR(subr_set_current_buffer);
    ADD_SUBR(subr_buffer_file_name);
    ADD_SUBR(subr_set_buffer_file_name);
    ADD_SUBR(subr_buffer_name);
    ADD_SUBR(subr_set_buffer_name);
    ADD_SUBR(subr_buffer_changes);
    ADD_SUBR(subr_buffer_modified_p);
    ADD_SUBR(subr_set_buffer_modified);
    ADD_SUBR(subr_set_buffer_special);
    ADD_SUBR(subr_buffer_special_p);
    ADD_SUBR(subr_set_buffer_read_only);
    ADD_SUBR(subr_buffer_read_only_p);
    ADD_SUBR(subr_buffer_length);
    ADD_SUBR(subr_line_length);
    ADD_SUBR(subr_with_buffer);
    ADD_SUBR(subr_bufferp);
    ADD_SUBR_INT(subr_restrict_buffer);
    ADD_SUBR_INT(subr_unrestrict_buffer);
    ADD_SUBR(subr_restriction_start);
    ADD_SUBR(subr_restriction_end);
    ADD_SUBR(subr_in_restriction_p);
    ADD_SUBR(subr_auto_save_interval);
    ADD_SUBR(subr_last_save_changes);
    ADD_SUBR(subr_last_user_save_changes);
    ADD_SUBR(subr_last_save_time);
    ADD_SUBR(subr_tab_size);
    ADD_SUBR(subr_mode_name);
    ADD_SUBR(subr_minor_mode_names);
    ADD_SUBR(subr_make_mark);
    ADD_SUBR(subr_set_mark);
    ADD_SUBR(subr_mark_pos);
    ADD_SUBR(subr_mark_file);
    ADD_SUBR(subr_mark_resident_p);
    ADD_SUBR(subr_markp);
}

void
buffers_kill(void)
{
    TX *tx = buffer_chain;
    Mark *mk = mark_chain;
    while(tx)
    {
	TX *nxttx = tx->tx_Next;
	kill_line_list(tx);
	FREE_OBJECT(tx);
	tx = nxttx;
    }
    buffer_chain = NULL;
    while(mk)
    {
	Mark *nxtmk = mk->mk_NextAlloc;
	FREE_OBJECT(mk);
	mk = nxtmk;
    }
    mark_chain = NULL;
}
