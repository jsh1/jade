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
#include <assert.h>
#include <string.h>
#include <stdlib.h>

static void mark_sweep(void);
static void make_marks_resident(repv newtx);
static void make_marks_non_resident(TX *oldtx);

int buffer_type;

/* Chain of all allocated TXs. */
TX *buffer_chain;

DEFSYM(auto_save_function, "auto-save-function");

DEFSTRING(first_buffer_name, "*jade*");


/* Stream helper functions */

static int
pos_getc(TX *tx, repv *pos)
{
    int c = EOF;
    long row = VROW(*pos);
    long col = VCOL(*pos);
    if(row < tx->tx_LogicalEnd)
    {
	if(col >= (tx->tx_Lines[row].ln_Strlen - 1))
	{
	    if(++row == tx->tx_LogicalEnd)
		--row;
	    else
	    {
		col = 0;
		c = '\n';
	    }
	}
	else
	    c = tx->tx_Lines[row].ln_Line[col++];
    }
    *pos = make_pos(col, row);
    return c;
}

#define POS_UNGETC(p, tx)				\
    do {						\
	long row = VROW(p), col = VCOL(p);		\
	if(--col < 0)					\
	{						\
	    row--;					\
	    col = (tx)->tx_Lines[row].ln_Strlen - 1;	\
	}						\
	(p) = make_pos(col, row);			\
    } while(0)

static int
pos_putc(TX *tx, repv *pos, int c)
{
    int rc = EOF;
    if(pad_pos(tx, *pos))
    {
	u_char tmps[2];
	repv end;
	tmps[0] = (u_char)c;
	tmps[1] = 0;
	end = insert_string(tx, tmps, 1, *pos);
	if(end != rep_NULL)
	{
	    *pos = end;
	    rc = 1;
	}
    }
    return rc;
}

static int
pos_puts(TX *tx, repv *pos, u_char *buf, int bufLen)
{
    if(pad_pos(tx, *pos))
    {
	repv end = insert_string(tx, buf, bufLen, *pos);
	if(end != rep_NULL)
	{
	    *pos = end;
	    return bufLen;
	}
    }
    return EOF;
}


/* Buffers */

DEFUN("make-buffer-name", Fmake_buffer_name, Smake_buffer_name, (repv rawName), rep_Subr1) /*
::doc:Smake-buffer-name::
make-buffer-name NAME

Construct a unique buffer-name from NAME.
::end:: */
{
    int suffix = 1;
    rep_DECLARE1(rawName, rep_STRINGP);
    while(TRUE)
    {
	u_char buf[256];
	u_char *thistry;
	TX *tx = buffer_chain;
	if(suffix != 1)
	{
#ifdef HAVE_SNPRINTF
	    snprintf(buf, sizeof(buf), "%s<%d>", rep_STR(rawName), suffix);
#else
	    sprintf(buf, "%s<%d>", rep_STR(rawName), suffix);
#endif
	    thistry = buf;
	}
	else
	    thistry = rep_STR(rawName);
	while(tx)
	{
	    if(tx->tx_BufferName && !strcmp(thistry, rep_STR(tx->tx_BufferName)))
		break;
	    tx = tx->tx_Next;
	}
	if(!tx)
	{
	    if(suffix == 1)
		return(rawName);
	    return(rep_string_dup(buf));
	}
	suffix++;
    }
}

DEFUN("make-buffer", Fmake_buffer, Smake_buffer, (repv name, repv oldTx, repv litName), rep_Subr3) /*
::doc:Smake-buffer::
make-buffer NAME

Return a new buffer, it's name is the result of (make-buffer-name NAME).
::end:: */
{
    TX *tx;
    rep_DECLARE1(name, rep_STRINGP);
    if(!BUFFERP(oldTx))
    {
	if(curr_vw)
	    oldTx = rep_VAL(curr_vw->vw_Tx);
	else
	    oldTx = rep_NULL;
    }
    tx = rep_ALLOC_CELL(sizeof(TX));
    if(tx != NULL)
    {
	memset(tx, 0, sizeof(TX));
	if(clear_line_list(tx))
	{
	    tx->tx_Car = buffer_type;
	    tx->tx_BufferName = rep_NILP(litName)
	        ? Fmake_buffer_name(name) : name;
	    if(tx->tx_BufferName)
	    {
		tx->tx_Next = buffer_chain;
		buffer_chain = tx;
		rep_data_after_gc += sizeof(TX);

		tx->tx_FileName = Qnil;
		tx->tx_CanonicalFileName = Qnil;
		tx->tx_StatusId = rep_concat2("Jade: ", rep_STR(tx->tx_BufferName));
		tx->tx_SavedBlockStatus = -1;
		tx->tx_TabSize = 8;
		tx->tx_LastSaveTime = rep_time();
		tx->tx_UndoList = Qnil;
		tx->tx_ToUndoList = rep_NULL;
		tx->tx_UndoneList = Qnil;
		tx->tx_SavedCPos = make_pos(0, 0);
		tx->tx_SavedWPos = tx->tx_SavedCPos;
		make_global_extent(tx);

		return(rep_VAL(tx));
	    }
	    kill_line_list(tx);
	}
	rep_FREE_CELL(tx);
    }
    return rep_NULL;
}

static void
buffer_mark (repv val)
{
    rep_MARKVAL(VTX(val)->tx_FileName);
    rep_MARKVAL(VTX(val)->tx_CanonicalFileName);
    rep_MARKVAL(VTX(val)->tx_BufferName);
    rep_MARKVAL(VTX(val)->tx_StatusId);
    rep_MARKVAL(VTX(val)->tx_UndoList);
    rep_MARKVAL(VTX(val)->tx_ToUndoList);
    rep_MARKVAL(VTX(val)->tx_UndoneList);
    rep_MARKVAL(VTX(val)->tx_SavedCPos);
    rep_MARKVAL(VTX(val)->tx_SavedWPos);
    rep_MARKVAL(VTX(val)->tx_SavedBlockPos[0]);
    rep_MARKVAL(VTX(val)->tx_SavedBlockPos[1]);
    rep_MARKVAL(rep_VAL(VTX(val)->tx_GlobalExtent));
}

static void
buffer_sweep(void)
{
    TX *tx;

    /* First sweep the marks.. */
    mark_sweep();

    /* ..then the buffers. There is a compelling reason for this, we
       need to make any live marks in buffers that are dead
       non-resident.  But the dead marks are not important, and
       _won't_ have ensured that the buffer's tx_CanonicalFileName is
       marked (even though the buffer itself isn't. */

    tx = buffer_chain;
    buffer_chain = NULL;
    while(tx)
    {
	TX *nxt = tx->tx_Next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(tx)))
	{
	    if(tx->tx_MarkChain != NULL)
	    {
		/* rep_mark_value has ensured that tx_CanonicalFileName
		   and tx_FileName are kept even if the buffer isn't */
		make_marks_non_resident(tx);
	    }
	    kill_line_list(tx);
	    rep_FREE_CELL(tx);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(tx));
	    tx->tx_Next = buffer_chain;
	    buffer_chain = tx;
	}
	tx = nxt;
    }
}

static void
buffer_prin(repv strm, repv obj)
{
    rep_stream_puts(strm, "#<buffer ", -1, FALSE);
    rep_stream_puts(strm, rep_PTR(VTX(obj)->tx_BufferName), -1, TRUE);
    rep_stream_putc(strm, '>');
}

static int
buffer_getc (repv stream)
{
    if (BUFFERP(stream))
	return pos_getc (VTX(stream), get_tx_cursor_ptr (VTX(stream)));
    else if (rep_CONSP(stream) && POSP(rep_CDR(stream)))
	return pos_getc (VTX(rep_CAR(stream)), &rep_CDR(stream));
    else
	return -1;
}

static int
buffer_ungetc (repv stream, int c)
{
    repv *ptr;
    if (BUFFERP(stream))
    {
	ptr = get_tx_cursor_ptr (VTX(stream));
	POS_UNGETC(*ptr, VTX(stream));
    }
    else if (rep_CONSP(stream))
    {
	repv tx = rep_CAR(stream);
	ptr = &rep_CDR(stream);
	POS_UNGETC(*ptr, VTX(tx));
    }
    return 1;
}

static int
buffer_putc (repv stream, int c)
{
    if (BUFFERP(stream))
	return pos_putc (VTX(stream), get_tx_cursor_ptr(VTX(stream)), c);
    else if (rep_CONSP(stream))
    {
	if (POSP(rep_CDR(stream)))
	    return pos_putc (VTX(rep_CAR(stream)), &rep_CDR(stream), c);
	else
	{
	    repv pos = Frestriction_end (rep_CAR(stream));
	    return pos_putc (VTX(rep_CAR(stream)), &pos, c);
	}
    }
    else
	return 0;
}

static int
buffer_puts (repv stream, void *data, int len, rep_bool is_val)
{
    u_char *buf = is_val ? rep_STR(data) : data;
    if (BUFFERP(stream))
    {
	return pos_puts (VTX(stream),
			 get_tx_cursor_ptr(VTX(stream)), buf, len);
    }
    else if (rep_CONSP(stream))
    {
	if (POSP(rep_CDR(stream)))
	    return pos_puts (VTX(rep_CAR(stream)), &rep_CDR(stream), buf, len);
	else
	{
	    repv pos = Frestriction_end (rep_CAR(stream));
	    return pos_puts (VTX(rep_CAR(stream)), &pos, buf, len);
	}
    }
    else
	return 0;
}

static repv
buffer_bind (repv tx)
{
    TX *old = swap_buffers (curr_vw, VTX(tx));
    return Fcons (rep_VAL(old), rep_VAL(curr_vw));
}

static void
buffer_unbind (repv handle)
{
    if (rep_CONSP(handle))
    {
	TX *tx = VTX(rep_CAR(handle));
	VW *vw = VVIEW(rep_CDR(handle));
	swap_buffers (vw, tx);
    }
}

TX *
first_buffer(void)
{
    TX *tx = VTX(Fmake_buffer(rep_VAL(&first_buffer_name), Qnil, Qt));
    if(!curr_win)
    {
	curr_win = VWIN(Fmake_window(Qnil, Qnil, Qnil, Qnil));
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

/* Installs buffer NEW as the current buffer of VW. Returns the originally
   current buffer (may be a null pointer) */
TX *
swap_buffers(VW *vw, TX *new)
{
    TX *old = vw->vw_Tx;
    if(old != new)
    {
	if(old != NULL)
	{
	    /* Save buffer context */
	    old->tx_SavedCPos = vw->vw_CursorPos;
	    old->tx_SavedWPos = vw->vw_DisplayOrigin;
	    old->tx_SavedBlockPos[0] = vw->vw_BlockS;
	    old->tx_SavedBlockPos[1] = vw->vw_BlockE;
	    old->tx_SavedBlockStatus = vw->vw_BlockStatus;
	}
	/* Restore old context */
	vw->vw_Tx = new;
	vw->vw_CursorPos = new->tx_SavedCPos;
	vw->vw_DisplayOrigin = new->tx_SavedWPos;
	vw->vw_BlockS = new->tx_SavedBlockPos[0];
	vw->vw_BlockE = new->tx_SavedBlockPos[1];
	vw->vw_BlockStatus = new->tx_SavedBlockStatus;

	/* If we're switching buffers in the minibuffer, and there's
	   a message obscuring the minibuffer contents, remove it. */
	if((vw->vw_Flags & VWFF_MINIBUF)
	   && MINIBUFFER_ACTIVE_P(vw->vw_Win)
	   && (vw->vw_Win->w_Flags & WINFF_MESSAGE))
	{
	    (*rep_message_fun)(rep_reset_message);
	}
    }
    return old;
}

DEFUN("get-file-buffer", Fget_file_buffer, Sget_file_buffer, (repv name), rep_Subr1) /*
::doc:Sget-file-buffer::
get-file-buffer NAME

Scan all buffers for one containing the file NAME.
::end:: */
{
    repv tx;
    rep_GC_root gc_name;
    rep_DECLARE1(name, rep_STRINGP);

    rep_PUSHGC(gc_name, name);
    name = Fcanonical_file_name(name);
    rep_POPGC;
    if(!name || !rep_STRINGP(name))
	return rep_NULL;

    tx = rep_VAL(buffer_chain);
    while(VTX(tx) != 0)
    {
	if(rep_STRINGP(VTX(tx)->tx_CanonicalFileName)
	   && rep_STRING_LEN(VTX(tx)->tx_CanonicalFileName) == rep_STRING_LEN(name)
	   && memcmp(rep_STR(VTX(tx)->tx_CanonicalFileName), rep_STR(name),
		     rep_STRING_LEN(name)) == 0)
	    return tx;
	tx = rep_VAL(VTX(tx)->tx_Next);
    }
    return Qnil;
}

DEFUN("get-buffer", Fget_buffer, Sget_buffer, (repv name), rep_Subr1) /*
::doc:Sget-buffer::
get-buffer NAME

Scan all buffers for one whose name is NAME.
::end:: */
{
    TX *tx = buffer_chain;
    if(BUFFERP(name))
	return(name);
    rep_DECLARE1(name, rep_STRINGP);
    while(tx)
    {
	if(!strcmp(rep_STR(name), rep_STR(tx->tx_BufferName)))
	    return(rep_VAL(tx));
	tx = tx->tx_Next;
    }
    return(Qnil);
}

repv *
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

repv
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
	u_long time = rep_time();
	Exclusion = TRUE;
	while(tx)
	{
	    if(tx->tx_Changes
	       && tx->tx_AutoSaveInterval
	       && (tx->tx_LastSaveChanges != tx->tx_Changes)
	       && (force_save
	           || (time > (tx->tx_LastSaveTime + tx->tx_AutoSaveInterval))))
	    {
		repv val_tx = rep_VAL(tx);
		rep_GC_root gc_tx;
		rep_PUSHGC(gc_tx, val_tx);
		rep_call_lisp1(Qauto_save_function, rep_VAL(tx));
		rep_POPGC;
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

DEFUN("current-buffer", Fcurrent_buffer, Scurrent_buffer, (repv vw), rep_Subr1) /*
::doc:Scurrent-buffer::
current-buffer [VIEW]

Return the buffer that VIEW (or the current view) is displaying.
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return(rep_VAL(VVIEW(vw)->vw_Tx));
}

DEFUN("set-current-buffer", Fset_current_buffer, Sset_current_buffer, (repv tx, repv vw), rep_Subr2) /*
::doc:Sset-current-buffer::
set-current-buffer BUFFER [VIEW]

Set the buffer that VIEW (or the current view) is displaying. Returns
the buffer which was being displayed before.
::end:: */
{
    rep_DECLARE1(tx, BUFFERP);
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return rep_VAL(swap_buffers(VVIEW(vw), VTX(tx)));
}

DEFUN("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, (repv tx), rep_Subr1) /*
::doc:Sbuffer-file-name::
buffer-file-name [BUFFER]

Return the name of the file being edited in BUFFER. If the contents of BUFFER
isn't associated with a particular file, returns nil.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return VTX(tx)->tx_FileName;
}

DEFUN("set-buffer-file-name", Fset_buffer_file_name, Sset_buffer_file_name, (repv tx, repv name), rep_Subr2) /*
::doc:Sset-buffer-file-name::
set-buffer-file-name BUFFER NAME

Set the name of the file associated with the contents of BUFFER to NAME.
::end:: */
{
    repv canonical;
    rep_GC_root gc_tx, gc_name;

    if(!rep_NILP(name))
	rep_DECLARE1(name, rep_STRINGP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);

    if(rep_STRINGP(name))
    {
	rep_PUSHGC(gc_tx, tx);
	rep_PUSHGC(gc_name, name);
	canonical = Fcanonical_file_name(name);
	rep_POPGC; rep_POPGC;
	if(!canonical || !rep_STRINGP(canonical))
	    canonical = name;
    }
    else
	canonical = Qnil;

    make_marks_non_resident(VTX(tx));
    VTX(tx)->tx_FileName = name;
    VTX(tx)->tx_CanonicalFileName = canonical;
    make_marks_resident(tx);

    return name;
}

DEFUN("buffer-name", Fbuffer_name, Sbuffer_name, (repv tx), rep_Subr1) /*
::doc:Sbuffer-name::
buffer-name [BUFFER]

Return the name of BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return VTX(tx)->tx_BufferName;
}

DEFUN("set-buffer-name", Fset_buffer_name, Sset_buffer_name, (repv tx, repv name), rep_Subr2) /*
::doc:Sset-buffer-name::
set-buffer-name BUFFER NAME

Set the name of BUFFER to NAME.
::end:: */
{
    rep_DECLARE1(name, rep_STRINGP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    VTX(tx)->tx_BufferName = name;
    if(VTX(tx)->tx_StatusId == rep_NULL
       || !strncmp("Jade: ", rep_STR(VTX(tx)->tx_StatusId), 5))
    {
	/* Reset the status-id */
	VTX(tx)->tx_StatusId = rep_concat2("Jade: ", rep_STR(name));
    }
    sys_reset_sleep_titles(VTX(tx));
    return name;
}

DEFUN("buffer-changes", Fbuffer_changes, Sbuffer_changes, (repv tx), rep_Subr1) /*
::doc:Sbuffer-changes::
buffer-changes [BUFFER]

Return the number of modifications to BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return rep_MAKE_INT(VTX(tx)->tx_Changes);
}

DEFUN("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p, (repv tx), rep_Subr1) /*
::doc:Sbuffer-modified-p::
buffer-modified-p [BUFFER]

Returns t if the buffer has changed since it was last saved to disk.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return ((VTX(tx)->tx_Changes != VTX(tx)->tx_ProperSaveChanges)
	    ? Qt : Qnil);
}

DEFUN("set-buffer-modified", Fset_buffer_modified, Sset_buffer_modified, (repv buf, repv stat), rep_Subr2) /*
::doc:Sset-buffer-modified::
set-buffer-modified BUFFER STATUS

If STATUS is nil make it look as though buffer hasn't changed, else make
it look as though it has.
::end:: */
{
    TX *tx =BUFFERP(buf) ? VTX(buf) : curr_vw->vw_Tx;
    if(rep_NILP(stat))
    {
	tx->tx_ProperSaveChanges = tx->tx_Changes;
	tx->tx_LastSaveChanges = tx->tx_Changes;
	undo_record_unmodified(tx);
    }
    else
    {
	tx->tx_ProperSaveChanges = tx->tx_Changes - 1;
	tx->tx_LastSaveChanges = tx->tx_Changes - 1;
    }
    return rep_VAL(tx);
}

DEFUN("buffer-length", Fbuffer_length, Sbuffer_length, (repv tx), rep_Subr1) /*
::doc:Sbuffer-length::
buffer-length [BUFFER]

Returns the number of lines in BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return(rep_MAKE_INT(VTX(tx)->tx_NumLines));
}

DEFUN("line-length", Fline_length, Sline_length, (repv pos, repv tx), rep_Subr2) /*
::doc:Sline-length::
line-length [LINE-POS] [BUFFER]

Returns the length (not including newline) of the specified line, or
using current cursor position if specifiers are not provided.
::end:: */
{
    if(POSP(pos))
    {
	if(!BUFFERP(tx))
	    tx = rep_VAL(curr_vw->vw_Tx);
    }
    else
    {
	pos = curr_vw->vw_CursorPos;
	tx = rep_VAL(curr_vw->vw_Tx);
    }
    return(rep_MAKE_INT(VTX(tx)->tx_Lines[VROW(pos)].ln_Strlen - 1));
}

DEFUN("bufferp", Fbufferp, Sbufferp, (repv arg), rep_Subr1) /*
::doc:Sbufferp::
bufferp ARG

Returns t if ARG is a buffer.
::end:: */
{
    if(BUFFERP(arg))
	return(Qt);
    return(Qnil);
}

DEFUN_INT("restrict-buffer", Frestrict_buffer, Srestrict_buffer, (repv start, repv end, repv tx), rep_Subr3, "-m" rep_DS_NL "M") /*
::doc:Srestrict-buffer::
restrict-buffer START END [BUFFER]

Limits the portion of BUFFER (or the current buffer) that may be displayed
to that between the lines specified by positions START and END.
::end:: */
{
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    Funrestrict_buffer(tx);
    if(check_section(VTX(tx), &start, &end) && VROW(start) <= VROW(end))
    {
	VTX(tx)->tx_LogicalStart = VROW(start);
	VTX(tx)->tx_LogicalEnd = VROW(end) + 1;
	return Qt;
    }
    return Qnil;
}

DEFUN_INT("unrestrict-buffer", Funrestrict_buffer, Sunrestrict_buffer, (repv tx), rep_Subr1, "") /*
::doc:Sunrestrict-buffer::
unrestrict-buffer [BUFFER]

Remove any restriction on the parts of BUFFER that may be displayed.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    VTX(tx)->tx_LogicalStart = 0;
    VTX(tx)->tx_LogicalEnd = VTX(tx)->tx_NumLines;
    return Qt;
}
DEFUN("restriction-start", Frestriction_start, Srestriction_start, (repv tx), rep_Subr1) /*
::doc:Srestriction-start::
restriction-start [BUFFER]

Return the position of the first character that may be displayed in BUFFER
(or the current buffer).
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return make_pos(0, VTX(tx)->tx_LogicalStart);
}

DEFUN("restriction-end", Frestriction_end, Srestriction_end, (repv tx), rep_Subr1) /*
::doc:Srestriction-end::
restriction-end [BUFFER]

Return the position of the last character that may be displayed in BUFFER
(or the current buffer).
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return make_pos(VTX(tx)->tx_Lines[VTX(tx)->tx_LogicalEnd - 1].ln_Strlen -1,
		    VTX(tx)->tx_LogicalEnd - 1);
}

DEFUN("buffer-restricted-p", Fbuffer_restricted_p, Sbuffer_restricted_p, (repv tx), rep_Subr1) /*
::doc:Sbuffer-restricted-p::
buffer-restricted-p [BUFFER]

Returns t when BUFFER (or the current buffer) has been restricted to display
less than its full contents.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    return ((VTX(tx)->tx_LogicalStart > 0
	     || VTX(tx)->tx_LogicalEnd < VTX(tx)->tx_NumLines)
	    ? Qt : Qnil);
}

DEFUN("auto-save-interval", var_auto_save_interval, Sauto_save_interval, (repv val), rep_Var) /*
::doc:Vauto-save-interval::
This buffer-local variable defines the period (in seconds) between each
automatic save of the buffer. A value of zero means that this buffer is
not to be auto-saved.
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->vw_Tx->tx_AutoSaveInterval));
}

DEFUN("last-save-changes", var_last_save_changes, Slast_save_changes, (repv val), rep_Var) /*
::doc:Vlast-save-changes::
Number of changes the last time this buffer was saved (could be auto-save).
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->vw_Tx->tx_LastSaveChanges));
}

DEFUN("last-user-save-changes", var_last_user_save_changes, Slast_user_save_changes, (repv val), rep_Var) /*
::doc:Vlast-user-save-changes::
Number of changes the last time this buffer was saved (not from auto-save).
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->vw_Tx->tx_ProperSaveChanges));
}

DEFUN("last-save-time", var_last_save_time, Slast_save_time, (repv val), rep_Var) /*
::doc:Vlast-save-time::
System time at last save of this buffer (could be from an auto-save).
::end:: */
{
    if(val != rep_NULL)
    {
	if(rep_TIMEP(val))
	    curr_vw->vw_Tx->tx_LastSaveTime = rep_GET_TIME(val);
	return rep_NULL;
    }
    else
	return rep_MAKE_TIME(curr_vw->vw_Tx->tx_LastSaveTime);
}

DEFUN("tab-size", var_tab_size, Stab_size, (repv val), rep_Var) /*
::doc:Vtab-size::
Sets the size of tab-stops.
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->vw_Tx->tx_TabSize));
}

DEFUN("truncate-lines", var_truncate_lines, Struncate_lines, (repv val), rep_Var) /*
::doc:Vtruncate-lines::
When t lines that continue past the rightmost column of the screen are
truncated, not wrapped onto the next row as when this variable is nil.
The default value for all buffers is nil.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(val != rep_NULL)
    {
	if(!rep_NILP(val))
	    tx->tx_Flags |= TXFF_DONT_WRAP_LINES;
	else
	    tx->tx_Flags &= ~TXFF_DONT_WRAP_LINES;
    }
    else
	val = TX_WRAP_LINES_P(tx) ? Qnil : Qt;
    return val;
}

DEFUN("buffer-status-id", var_buffer_status_id, Sbuffer_status_id,
      (repv val), rep_Var) /*
::doc:Vbuffer-status-id::
This buffer-local string is displayed in the status line of the buffer. When
the buffer is created it is set to `Jade: BUFFER-NAME'.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(val)
    {
	tx->tx_StatusId = rep_STRINGP(val) ? val : rep_NULL;
	return val;
    }
    else if(tx->tx_StatusId)
	return tx->tx_StatusId;
    else
	return Qnil;
}

DEFUN("all-buffers", Fall_buffers, Sall_buffers, (void), rep_Subr0) /*
::doc:Sall-buffers::
all-buffers

Return a list of all allocated buffer objects.
::end:: */
{
    repv list = Qnil;
    TX *tx = buffer_chain;
    while(tx != 0)
    {
	list = Fcons(rep_VAL(tx), list);
	tx = tx->tx_Next;
    }
    return Fnreverse(list);
}

void
tx_kill_local_variables(TX *tx)
{
}


/* Marks */

int mark_type;

/* chain of all non-resident marks, linked via `next' */
static Lisp_Mark *non_resident_mark_chain;

/* chain of all allocated marks */
static Lisp_Mark *mark_chain;

DEFSTRING(non_resident, "Marks used as streams must be resident");

/* For all non-resident marks, see if any point to NEWTX, if so link them
   onto NEWTX's `tx_Marks' chain. */
static void
make_marks_resident(repv newtx)
{
    repv mk = rep_VAL(non_resident_mark_chain);
    non_resident_mark_chain = NULL;
    while(mk != rep_NULL)
    {
	repv nxt = rep_VAL(VMARK(mk)->next);
	
	if(rep_STRINGP(VTX(newtx)->tx_CanonicalFileName)
	    && strcmp(rep_STR(VTX(newtx)->tx_CanonicalFileName),
		      rep_STR(VMARK(mk)->canon_file)) == 0)
	{
	    VMARK(mk)->file = newtx;
	    VMARK(mk)->canon_file = Qnil;
	    VMARK(mk)->next = VTX(newtx)->tx_MarkChain;
	    VTX(newtx)->tx_MarkChain = VMARK(mk);
	}
	else
	{
	    VMARK(mk)->next = non_resident_mark_chain;
	    non_resident_mark_chain = VMARK(mk);
	}
	mk = nxt;
    }
}

/* Takes MK off the buffer mark chain that it's on (or the list of non-
   resident marks). */
static void
unchain_mark(Lisp_Mark *mk)
{
    Lisp_Mark **headp, *this;
    if(!MARK_RESIDENT_P(mk))
	headp = &non_resident_mark_chain;
    else
	headp = &(VTX(mk->file)->tx_MarkChain);
    this = *headp;
    *headp = NULL;
    while(this)
    {
	Lisp_Mark *tmp = this->next;
	if(this != mk)
	{
	    this->next = *headp;
	    *headp = this;
	}
	this = tmp;
    }
}

/* Put all marks pointing to buffer OLDTX onto the list of non-resident
   marks.

   **NOTE** this function is called from the buffer gc sweep function,
   and thus OLDTX may be unused; any repv fields within it used by this
   function _must_ be explicitly marked by rep_mark_value in the V_Mark case. */
static void
make_marks_non_resident(TX *oldtx)
{
    repv canon_file = (rep_STRINGP(oldtx->tx_CanonicalFileName)
			? oldtx->tx_CanonicalFileName
			: Qnil);
    repv file = (rep_STRINGP(oldtx->tx_FileName)
		  ? oldtx->tx_FileName
		  : canon_file);
    Lisp_Mark *nxt, *mk = oldtx->tx_MarkChain;
    oldtx->tx_MarkChain = NULL;
    while(mk != NULL)
    {
	nxt = mk->next;
	if (rep_NILP(canon_file) || rep_NILP(file))
	{
	    /* No file to associate with, lose the mark. */
	    unchain_mark(mk);
	}
	else
	{
	    mk->next = non_resident_mark_chain;
	    non_resident_mark_chain = mk;
	}
	mk->file = file;
	mk->canon_file = canon_file;
	mk = nxt;
    }
}

static void
mark_mark (repv val)
{
    if(!MARK_RESIDENT_P(VMARK(val)))
    {
	rep_MARKVAL(VMARK(val)->file);
	rep_MARKVAL(VMARK(val)->canon_file);
    }
    else
    {
	/* TXs don't get marked here. They should still be able to
	   be gc'd if there's marks pointing to them. The marks will
	   just get made non-resident. But to do this we'll need
	   the names of the file they point to.. */
	rep_MARKVAL(VTX(VMARK(val)->file)->tx_FileName);
	rep_MARKVAL(VTX(VMARK(val)->file)->tx_CanonicalFileName);
    }
    rep_MARKVAL(VMARK(val)->pos);
}

static void
mark_sweep(void)
{
    Lisp_Mark *mk = mark_chain;
    mark_chain = NULL;
    while(mk)
    {
	Lisp_Mark *nxt = mk->next_alloc;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(mk)))
	{
	    unchain_mark(mk);
	    rep_FREE_CELL(mk);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(mk));
	    mk->next_alloc = mark_chain;
	    mark_chain = mk;
	}
	mk = nxt;
    }
}

/* Compare two marks. */
static int
mark_cmp(repv v1, repv v2)
{
    int rc = 1;
    if(rep_TYPE(v1) == rep_TYPE(v2))
    {
	if((MARK_RESIDENT_P(VMARK(v1)) && MARK_RESIDENT_P(VMARK(v2))
	    && VMARK(v1)->file == VMARK(v2)->file)
	   || ((!MARK_RESIDENT_P(VMARK(v1)) && !MARK_RESIDENT_P(VMARK(v2)))
	       && (rep_value_cmp(VMARK(v1)->canon_file,
				 VMARK(v2)->canon_file) == 0)))
	{
	    rc = VROW(VMARK(v1)->pos) - VROW(VMARK(v2)->pos);
	    if(rc == 0)
		rc = VCOL(VMARK(v1)->pos) - VROW(VMARK(v2)->pos);
	}
    }
    return rc;
}

static void
mark_prin(repv strm, repv obj)
{
    u_char tbuf[40];
    rep_stream_puts(strm, "#<mark ", -1, FALSE);
    if(MARK_RESIDENT_P(VMARK(obj)))
	buffer_prin(strm, VMARK(obj)->file);
    else
    {
	rep_stream_putc(strm, '"');
	rep_stream_puts(strm, rep_PTR(VMARK(obj)->file), -1, TRUE);
	rep_stream_putc(strm, '"');
    }
#ifdef HAVE_SNPRINTF
    snprintf(tbuf, sizeof(tbuf),
#else
    sprintf(tbuf,
#endif
	    " #<pos %ld %ld>>",
	    VCOL(VMARK(obj)->pos),
	    VROW(VMARK(obj)->pos));
    rep_stream_puts(strm, tbuf, -1, FALSE);
}

static int
mark_getc (repv stream)
{
    if(!MARK_RESIDENT_P(VMARK(stream)))
    {
	Fsignal(Qinvalid_stream, rep_list_2(stream, rep_VAL(&non_resident)));
	return EOF;
    }
    else
	return pos_getc(VTX(VMARK(stream)->file), &VMARK(stream)->pos);
}

static int
mark_ungetc (repv stream, int c)
{
    POS_UNGETC(VMARK(stream)->pos, VTX(VMARK(stream)->file));
    return 1;
}

static int
mark_putc (repv stream, int c)
{
    if(!MARK_RESIDENT_P(VMARK(stream)))
    {
	Fsignal(Qinvalid_stream, rep_list_2(stream, rep_VAL(&non_resident)));
	return EOF;
    }
    else
	return pos_putc(VTX(VMARK(stream)->file), &VMARK(stream)->pos, c);
}

static int
mark_puts (repv stream, void *data, int len, rep_bool is_val)
{
    u_char *buf = is_val ? rep_STR(data) : data;
    if(!MARK_RESIDENT_P(VMARK(stream)))
    {
	Fsignal(Qinvalid_stream, rep_list_2(stream, rep_VAL(&non_resident)));
	return EOF;
    }
    else
	return pos_puts(VTX(VMARK(stream)->file),
			&VMARK(stream)->pos, buf, len);
}

DEFUN("make-mark", Fmake_mark, Smake_mark, (repv pos, repv buffer), rep_Subr2) /*
::doc:Smake-mark::
make-mark [POS] [FILE]

Creates a new mark pointing to position POS either in FILE or the current
buffer. FILE may be either a buffer, or a file name. If a file name, it
does not have to have been loaded into the editor, buffers are created on
demand.

Unlike position objects, the position in a file that a mark points to is
updated as the file changes -- it will always point to the same character
(for as long as that character exists, anyway).
::end:: */
{
    repv mk = rep_VAL(rep_ALLOC_CELL(sizeof(Lisp_Mark)));
    if(mk != rep_NULL)
    {
	rep_GC_root gc_mk, gc_buf;

	VMARK(mk)->car = mark_type;
	VMARK(mk)->next_alloc = mark_chain;
	mark_chain = VMARK(mk);
	rep_data_after_gc += sizeof(Lisp_Mark);
	VMARK(mk)->pos = POSP(pos) ? pos : curr_vw->vw_CursorPos;

	/* just so these are valid */
	VMARK(mk)->file = rep_VAL(curr_vw->vw_Tx);
	VMARK(mk)->canon_file = Qnil;
	VMARK(mk)->next = NULL;

	if(rep_STRINGP(buffer))
	{
	    repv tem;
	    rep_PUSHGC(gc_mk, mk);
	    rep_PUSHGC(gc_buf, buffer);
	    tem = Fget_file_buffer(buffer);
	    rep_POPGC; rep_POPGC;
	    if(tem != rep_NULL && BUFFERP(tem))
		buffer = tem;
	}
	if(rep_STRINGP(buffer))
	{
	    repv tem;
	    rep_PUSHGC(gc_mk, mk);
	    rep_PUSHGC(gc_buf, buffer);
	    tem = Fcanonical_file_name(buffer);
	    rep_POPGC; rep_POPGC;
	    VMARK(mk)->file = buffer;
	    if(tem && rep_STRINGP(tem))
		VMARK(mk)->canon_file = tem;
	    else
		VMARK(mk)->canon_file = buffer;
	    VMARK(mk)->next = non_resident_mark_chain;
	    non_resident_mark_chain = VMARK(mk);
	}
	else
	{
	    if(!BUFFERP(buffer))
		buffer = rep_VAL(curr_vw->vw_Tx);
	    VMARK(mk)->file = buffer;
	    VMARK(mk)->next = VTX(buffer)->tx_MarkChain;
	    VTX(buffer)->tx_MarkChain = VMARK(mk);
	}
	return mk;
    }
    return rep_mem_error();
}

DEFUN("set-mark-pos", Fset_mark_pos, Sset_mark_pos,
      (repv mark, repv pos), rep_Subr2) /*
::doc:Sset-mark-pos::
set-mark-pos MARK POSITION

Set the position pointed at by MARK to POSITION.
::end:: */
{
    rep_DECLARE1(mark, MARKP);
    rep_DECLARE2(pos, POSP);
    VMARK(mark)->pos = pos;
    return pos;
}

DEFUN("set-mark-file", Fset_mark_file, Sset_mark_file,
      (repv mark, repv file), rep_Subr2) /*
::doc:Sset-mark-file::
set-mark-file MARK FILE

Set the file pointed at by MARK to FILE, a buffer or a file name.
::end:: */
{
    rep_GC_root gc_mark, gc_file;
    rep_DECLARE1(mark, MARKP);
    if(rep_STRINGP(file))
    {
	repv tem;
	rep_PUSHGC(gc_mark, mark);
	rep_PUSHGC(gc_file, file);
	tem = Fget_file_buffer(file);
	rep_POPGC; rep_POPGC;
	if(tem != rep_NULL && BUFFERP(tem))
	    file = tem;
    }
    if(BUFFERP(file))
    {
	if(VMARK(mark)->file != file)
	{
	    unchain_mark(VMARK(mark));
	    VMARK(mark)->next = VTX(file)->tx_MarkChain;
	    VTX(file)->tx_MarkChain = VMARK(mark);
	}
	VMARK(mark)->file = file;
	VMARK(mark)->canon_file = Qnil;
    }
    else if(rep_STRINGP(file))
    {
	repv tem;
	rep_PUSHGC(gc_mark, mark);
	rep_PUSHGC(gc_file, file);
	tem = Fcanonical_file_name(file);
	rep_POPGC; rep_POPGC;
	if(!MARK_RESIDENT_P(VMARK(mark)))
	{
	    unchain_mark(VMARK(mark));
	    VMARK(mark)->next = non_resident_mark_chain;
	    non_resident_mark_chain = VMARK(mark);
	}
	VMARK(mark)->file = file;
	if(tem && rep_STRINGP(tem))
	    VMARK(mark)->canon_file = tem;
	else
	    VMARK(mark)->canon_file = file;
    }
    return VMARK(mark)->file;
}

DEFUN("mark-pos", Fmark_pos, Smark_pos, (repv mark), rep_Subr1) /*
::doc:Smark-pos::
mark-pos MARK

Returns the position that MARK points to. (note that this is the *same*
object that the mark stores internally -- so don't modify it unless you're
really sure you know what you're doing)
::end:: */
{
    rep_DECLARE1(mark, MARKP);
    return VMARK(mark)->pos;
}

DEFUN("mark-file", Fmark_file, Smark_file, (repv mark), rep_Subr1) /*
::doc:Smark-file::
mark-file MARK

Returns the file-name or buffer that MARK points to.
::end:: */
{
    rep_DECLARE1(mark, MARKP);
    return(VMARK(mark)->file);
}

DEFUN("mark-resident-p", Fmark_resident_p, Smark_resident_p, (repv mark), rep_Subr1) /*
::doc:Smark-resident-p::
mark-resident-p MARK

Returns t if the file that MARK points to is in a buffer.
::end:: */
{
    rep_DECLARE1(mark, MARKP);
    return MARK_RESIDENT_P(VMARK(mark)) ? Qt : Qnil;
}

DEFUN("markp", Fmarkp, Smarkp, (repv mark), rep_Subr1) /*
::doc:Smarkp::
markp ARG

Return t if ARG is a mark.
::end:: */
{
    return MARKP(mark) ? Qt : Qnil;
}


/* initialisation */

void
buffers_init(void)
{
    buffer_type = rep_register_new_type ("buffer", 0, buffer_prin,
					 buffer_prin, buffer_sweep,
					 buffer_mark, 0,
					 buffer_getc, buffer_ungetc,
					 buffer_putc, buffer_puts,
					 buffer_bind, buffer_unbind);

    mark_type = rep_register_new_type ("mark", mark_cmp, mark_prin,
				       mark_prin, 0, mark_mark,
				       0, mark_getc, mark_ungetc,
				       mark_putc, mark_puts, 0, 0);

    rep_mark_static((repv *)&non_resident_mark_chain);
    rep_INTERN(auto_save_function);
    rep_ADD_SUBR(Smake_buffer_name);
    rep_ADD_SUBR(Smake_buffer);
    rep_ADD_SUBR(Sget_file_buffer);
    rep_ADD_SUBR(Sget_buffer);
    rep_ADD_SUBR(Scurrent_buffer);
    rep_ADD_SUBR(Sset_current_buffer);
    rep_ADD_SUBR(Sbuffer_file_name);
    rep_ADD_SUBR(Sset_buffer_file_name);
    rep_ADD_SUBR(Sbuffer_name);
    rep_ADD_SUBR(Sset_buffer_name);
    rep_ADD_SUBR(Sbuffer_changes);
    rep_ADD_SUBR(Sbuffer_modified_p);
    rep_ADD_SUBR(Sset_buffer_modified);
    rep_ADD_SUBR(Sbuffer_length);
    rep_ADD_SUBR(Sline_length);
    rep_ADD_SUBR(Sbufferp);
    rep_ADD_SUBR_INT(Srestrict_buffer);
    rep_ADD_SUBR_INT(Sunrestrict_buffer);
    rep_ADD_SUBR(Srestriction_start);
    rep_ADD_SUBR(Srestriction_end);
    rep_ADD_SUBR(Sbuffer_restricted_p);
    rep_ADD_SUBR(Sauto_save_interval);
    rep_ADD_SUBR(Slast_save_changes);
    rep_ADD_SUBR(Slast_user_save_changes);
    rep_ADD_SUBR(Slast_save_time);
    rep_ADD_SUBR(Stab_size);
    rep_ADD_SUBR(Struncate_lines);
    rep_ADD_SUBR(Sbuffer_status_id);
    rep_ADD_SUBR(Sall_buffers);
    rep_ADD_SUBR(Smake_mark);
    rep_ADD_SUBR(Sset_mark_pos);
    rep_ADD_SUBR(Sset_mark_file);
    rep_ADD_SUBR(Smark_pos);
    rep_ADD_SUBR(Smark_file);
    rep_ADD_SUBR(Smark_resident_p);
    rep_ADD_SUBR(Smarkp);
}

void
buffers_kill(void)
{
    TX *tx = buffer_chain;
    Lisp_Mark *mk = mark_chain;
    while(tx)
    {
	TX *nxttx = tx->tx_Next;
	kill_line_list(tx);
	rep_FREE_CELL(tx);
	tx = nxttx;
    }
    buffer_chain = NULL;
    while(mk)
    {
	Lisp_Mark *nxtmk = mk->next_alloc;
	rep_FREE_CELL(mk);
	mk = nxtmk;
    }
    mark_chain = NULL;
}
