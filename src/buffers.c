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
#include <inttypes.h>

static void mark_sweep(void);
static void make_marks_resident(repv newtx);
static void make_marks_non_resident(Lisp_Buffer *oldtx);

int buffer_type;

/* Chain of all allocated TXs. */
Lisp_Buffer *buffer_chain;

DEFSYM(auto_save_function, "auto-save-function");

DEFSTRING(first_buffer_name, "*jade*");


/* Stream helper functions */

static int
pos_getc(Lisp_Buffer *tx, repv *pos)
{
    int c = EOF;
    intptr_t row = VROW(*pos);
    intptr_t col = VCOL(*pos);
    if(row < tx->logical_end)
    {
	if(col >= (tx->lines[row].ln_Strlen - 1))
	{
	    if(++row == tx->logical_end)
		--row;
	    else
	    {
		col = 0;
		c = '\n';
	    }
	}
	else
	    c = tx->lines[row].ln_Line[col++];
    }
    *pos = make_pos(col, row);
    return c;
}

#define POS_UNGETC(p, tx)				\
    do {						\
	intptr_t row = VROW(p), col = VCOL(p);	\
	if(--col < 0)					\
	{						\
	    row--;					\
	    col = (tx)->lines[row].ln_Strlen - 1;	\
	}						\
	(p) = make_pos(col, row);			\
    } while(0)

static int
pos_putc(Lisp_Buffer *tx, repv *pos, int c)
{
    int rc = EOF;
    if(pad_pos(tx, *pos))
    {
	char tmps[2];
	repv end;
	tmps[0] = c;
	tmps[1] = 0;
	end = insert_string(tx, tmps, 1, *pos);
	if(end != 0)
	{
	    *pos = end;
	    rc = 1;
	}
    }
    return rc;
}

static intptr_t
pos_puts(Lisp_Buffer *tx, repv *pos, const char *buf, intptr_t bufLen)
{
    if(pad_pos(tx, *pos))
    {
	repv end = insert_string(tx, buf, bufLen, *pos);
	if(end != 0)
	{
	    *pos = end;
	    return bufLen;
	}
    }
    return EOF;
}


/* Buffers */

DEFUN("make-buffer-name", Fmake_buffer_name, Smake_buffer_name, (repv rawName), rep_Subr1) /*
::doc:make-buffer-name::
make-buffer-name NAME

Construct a unique buffer-name from NAME.
::end:: */
{
    int suffix = 1;
    rep_DECLARE1(rawName, rep_STRINGP);
    while(true)
    {
	char buf[256];
	const char *thistry;
	Lisp_Buffer *tx = buffer_chain;
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
	    if(tx->buffer_name && !strcmp(thistry, rep_STR(tx->buffer_name)))
		break;
	    tx = tx->next;
	}
	if(!tx)
	{
	    if(suffix == 1)
		return(rawName);
	    return(rep_string_copy(buf));
	}
	suffix++;
    }
}

DEFUN("make-buffer", Fmake_buffer, Smake_buffer, (repv name, repv oldTx, repv litName), rep_Subr3) /*
::doc:make-buffer::
make-buffer NAME

Return a new buffer, it's name is the result of (make-buffer-name NAME).
::end:: */
{
    Lisp_Buffer *tx;
    rep_DECLARE1(name, rep_STRINGP);
    if(!BUFFERP(oldTx))
    {
	if(curr_vw)
	    oldTx = rep_VAL(curr_vw->tx);
	else
	    oldTx = 0;
    }
    tx = rep_alloc(sizeof(Lisp_Buffer));
    if(tx != NULL)
    {
	memset(tx, 0, sizeof(Lisp_Buffer));
	if(clear_line_list(tx))
	{
	    tx->car = buffer_type;
	    tx->buffer_name = rep_NILP(litName)
	        ? Fmake_buffer_name(name) : name;
	    if(tx->buffer_name)
	    {
		tx->next = buffer_chain;
		buffer_chain = tx;
		rep_data_after_gc += sizeof(Lisp_Buffer);

		tx->file_name = Qnil;
		tx->canonical_file_name = Qnil;
		tx->status_string = rep_string_concat2("Jade: ", rep_STR(tx->buffer_name));
		tx->saved_block_state = -1;
		tx->tab_size = 8;
		tx->last_saved_time = rep_time();
		tx->undo_list = Qnil;
		tx->pending_undo_list = 0;
		tx->did_undo_list = Qnil;
		tx->saved_cursor_pos = make_pos(0, 0);
		tx->saved_display_origin = tx->saved_cursor_pos;
		make_global_extent(tx);

		return(rep_VAL(tx));
	    }
	    kill_line_list(tx);
	}
	rep_free(tx);
    }
    return 0;
}

static void
buffer_mark (repv val)
{
    rep_MARKVAL(VBUFFER(val)->file_name);
    rep_MARKVAL(VBUFFER(val)->canonical_file_name);
    rep_MARKVAL(VBUFFER(val)->buffer_name);
    rep_MARKVAL(VBUFFER(val)->status_string);
    rep_MARKVAL(VBUFFER(val)->undo_list);
    rep_MARKVAL(VBUFFER(val)->pending_undo_list);
    rep_MARKVAL(VBUFFER(val)->did_undo_list);
    rep_MARKVAL(VBUFFER(val)->saved_cursor_pos);
    rep_MARKVAL(VBUFFER(val)->saved_display_origin);
    rep_MARKVAL(VBUFFER(val)->saved_block[0]);
    rep_MARKVAL(VBUFFER(val)->saved_block[1]);
    rep_MARKVAL(rep_VAL(VBUFFER(val)->global_extent));
}

static void
buffer_sweep(void)
{
    Lisp_Buffer *tx;

    /* First sweep the marks.. */
    mark_sweep();

    /* ..then the buffers. There is a compelling reason for this, we
       need to make any live marks in buffers that are dead
       non-resident.  But the dead marks are not important, and
       _won't_ have ensured that the buffer's canonical_file_name is
       marked (even though the buffer itself isn't. */

    tx = buffer_chain;
    buffer_chain = NULL;
    while(tx)
    {
	Lisp_Buffer *nxt = tx->next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(tx)))
	{
	    if(tx->mark_chain != NULL)
	    {
		/* rep_mark_value has ensured that canonical_file_name
		   and file_name are kept even if the buffer isn't */
		make_marks_non_resident(tx);
	    }
	    kill_line_list(tx);
	    rep_free(tx);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(tx));
	    tx->next = buffer_chain;
	    buffer_chain = tx;
	}
	tx = nxt;
    }
}

static void
buffer_prin(repv strm, repv obj)
{
    rep_stream_puts(strm, "#<buffer ", -1, false);
    rep_stream_puts(strm, rep_PTR(VBUFFER(obj)->buffer_name), -1, true);
    rep_stream_putc(strm, '>');
}

static int
buffer_getc (repv stream)
{
    if (BUFFERP(stream))
	return pos_getc (VBUFFER(stream), get_buffer_cursor_ptr (VBUFFER(stream)));
    else if (rep_CONSP(stream) && POSP(rep_CDR(stream)))
	return pos_getc (VBUFFER(rep_CAR(stream)), &rep_CDR(stream));
    else
	return -1;
}

static int
buffer_ungetc (repv stream, int c)
{
    repv *ptr;
    if (BUFFERP(stream))
    {
	ptr = get_buffer_cursor_ptr (VBUFFER(stream));
	POS_UNGETC(*ptr, VBUFFER(stream));
    }
    else if (rep_CONSP(stream))
    {
	repv tx = rep_CAR(stream);
	ptr = &rep_CDR(stream);
	POS_UNGETC(*ptr, VBUFFER(tx));
    }
    return 1;
}

static int
buffer_putc (repv stream, int c)
{
    if (BUFFERP(stream))
	return pos_putc (VBUFFER(stream), get_buffer_cursor_ptr(VBUFFER(stream)), c);
    else if (rep_CONSP(stream))
    {
	if (POSP(rep_CDR(stream)))
	    return pos_putc (VBUFFER(rep_CAR(stream)), &rep_CDR(stream), c);
	else
	{
	    repv pos = Frestriction_end (rep_CAR(stream));
	    return pos_putc (VBUFFER(rep_CAR(stream)), &pos, c);
	}
    }
    else
	return 0;
}

static intptr_t
buffer_puts (repv stream, const void *data, intptr_t len, bool is_val)
{
    const char *buf = is_val ? rep_STR(data) : data;
    if (BUFFERP(stream))
    {
	return pos_puts (VBUFFER(stream),
			 get_buffer_cursor_ptr(VBUFFER(stream)), buf, len);
    }
    else if (rep_CONSP(stream))
    {
	if (POSP(rep_CDR(stream)))
	    return pos_puts (VBUFFER(rep_CAR(stream)), &rep_CDR(stream), buf, len);
	else
	{
	    repv pos = Frestriction_end (rep_CAR(stream));
	    return pos_puts (VBUFFER(rep_CAR(stream)), &pos, buf, len);
	}
    }
    else
	return 0;
}

static repv
buffer_bind (repv tx)
{
    Lisp_Buffer *old = swap_buffers (curr_vw, VBUFFER(tx));
    return Fcons (rep_VAL(old), rep_VAL(curr_vw));
}

static void
buffer_unbind (repv handle)
{
    if (rep_CONSP(handle))
    {
	Lisp_Buffer *tx = VBUFFER(rep_CAR(handle));
	Lisp_View *vw = VVIEW(rep_CDR(handle));
	swap_buffers (vw, tx);
    }
}

Lisp_Buffer *
first_buffer(void)
{
    Lisp_Buffer *tx = VBUFFER(Fmake_buffer(rep_VAL(&first_buffer_name), Qnil, Qt));
    if(!curr_win)
    {
	curr_win = VWINDOW(Fmake_window(Qnil));
	if(!curr_win)
	    return(NULL);
    }
    if(tx)
    {
	swap_buffers(curr_vw, tx);
	return(tx);
    }
    return(NULL);
}

/* Installs buffer NEW as the current buffer of VW. Returns the originally
   current buffer (may be a null pointer) */
Lisp_Buffer *
swap_buffers(Lisp_View *vw, Lisp_Buffer *new)
{
    Lisp_Buffer *old = vw->tx;
    if(old != new)
    {
	if(old != NULL)
	{
	    /* Save buffer context */
	    old->saved_cursor_pos = vw->cursor_pos;
	    old->saved_display_origin = vw->display_origin;
	    old->saved_block[0] = vw->block_start;
	    old->saved_block[1] = vw->block_end;
	    old->saved_block_state = vw->block_state;
	}
	/* Restore old context */
	vw->tx = new;
	vw->cursor_pos = new->saved_cursor_pos;
	vw->display_origin = new->saved_display_origin;
	vw->block_start = new->saved_block[0];
	vw->block_end = new->saved_block[1];
	vw->block_state = new->saved_block_state;

	/* If we're switching buffers in the minibuffer, and there's
	   a message obscuring the minibuffer contents, remove it. */
	if((vw->car & VWFF_MINIBUF)
	   && MINIBUFFER_ACTIVE_P(vw->window)
	   && (vw->window->car & WINFF_MESSAGE))
	{
	    (*rep_message_fun)(rep_reset_message);
	}
    }
    return old;
}

DEFUN("get-file-buffer", Fget_file_buffer, Sget_file_buffer, (repv name), rep_Subr1) /*
::doc:get-file-buffer::
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
	return 0;

    tx = rep_VAL(buffer_chain);
    while(VBUFFER(tx) != 0)
    {
	if(rep_STRINGP(VBUFFER(tx)->canonical_file_name)
	   && rep_STRING_LEN(VBUFFER(tx)->canonical_file_name) == rep_STRING_LEN(name)
	   && memcmp(rep_STR(VBUFFER(tx)->canonical_file_name), rep_STR(name),
		     rep_STRING_LEN(name)) == 0)
	    return tx;
	tx = rep_VAL(VBUFFER(tx)->next);
    }
    return Qnil;
}

DEFUN("get-buffer", Fget_buffer, Sget_buffer, (repv name), rep_Subr1) /*
::doc:get-buffer::
get-buffer NAME

Scan all buffers for one whose name is NAME.
::end:: */
{
    Lisp_Buffer *tx = buffer_chain;
    if(BUFFERP(name))
	return(name);
    rep_DECLARE1(name, rep_STRINGP);
    while(tx)
    {
	if(!strcmp(rep_STR(name), rep_STR(tx->buffer_name)))
	    return(rep_VAL(tx));
	tx = tx->next;
    }
    return(Qnil);
}

repv *
get_buffer_cursor_ptr(Lisp_Buffer *tx)
{
    Lisp_View *vw;

    /* Check active view first */
    if(curr_vw->tx == tx)
	return(&curr_vw->cursor_pos);

    /* Then other views in the same window. */
    for(vw = curr_win->view_list; vw != 0; vw = vw->next_view)
    {
	if(vw->window && vw->window->w_Window && (vw->tx == tx))
	    return(&vw->cursor_pos);
    }

    /* Finally all other windows */
    for(vw = view_chain; vw != 0; vw = vw->next)
    {
	if(vw->window && vw->window->w_Window && (vw->tx == tx))
	    return(&vw->cursor_pos);
    }

    return(&tx->saved_cursor_pos);
}    

repv
get_buffer_cursor(Lisp_Buffer *tx)
{
    return *get_buffer_cursor_ptr(tx);
}

/* Returns true if a buffer was saved, in which case calling the
   function again may save another buffer. If force_save is true, don't
   worry about the time between saves, just save the next buffer */

bool
auto_save_buffers(bool force_save)
{
    /*
     * Stops me entering here more than once at the same time. This
     * can happen when tracing the `auto-save-function'
     */
    static bool Exclusion;
    if(!Exclusion)
    {
	Lisp_Buffer *tx = buffer_chain;
	uintptr_t time = rep_time();
	Exclusion = true;
	while(tx)
	{
	    if(tx->change_count
	       && tx->auto_save_interval
	       && (tx->last_saved_change_count != tx->change_count)
	       && (force_save
	           || (time > (tx->last_saved_time + tx->auto_save_interval))))
	    {
		repv val_tx = rep_VAL(tx);
		rep_GC_root gc_tx;
		rep_PUSHGC(gc_tx, val_tx);
		rep_call_lisp1(Fsymbol_value(Qauto_save_function, Qt),
			       rep_VAL(tx));
		rep_POPGC;
		tx->last_saved_time = time;
		tx->last_saved_change_count = tx->change_count;
		Exclusion = false;
		return true;
	    }
	    tx = tx->next;
	}
	Exclusion = false;
    }
    return false;
}

DEFUN("current-buffer", Fcurrent_buffer, Scurrent_buffer, (repv vw), rep_Subr1) /*
::doc:current-buffer::
current-buffer [VIEW]

Return the buffer that VIEW (or the current view) is displaying.
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return(rep_VAL(VVIEW(vw)->tx));
}

DEFUN("set-current-buffer", Fset_current_buffer, Sset_current_buffer, (repv tx, repv vw), rep_Subr2) /*
::doc:set-current-buffer::
set-current-buffer BUFFER [VIEW]

Set the buffer that VIEW (or the current view) is displaying. Returns
the buffer which was being displayed before.
::end:: */
{
    rep_DECLARE1(tx, BUFFERP);
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return rep_VAL(swap_buffers(VVIEW(vw), VBUFFER(tx)));
}

DEFUN("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, (repv tx), rep_Subr1) /*
::doc:buffer-file-name::
buffer-file-name [BUFFER]

Return the name of the file being edited in BUFFER. If the contents of BUFFER
isn't associated with a particular file, returns nil.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return VBUFFER(tx)->file_name;
}

DEFUN("set-buffer-file-name", Fset_buffer_file_name, Sset_buffer_file_name, (repv tx, repv name), rep_Subr2) /*
::doc:set-buffer-file-name::
set-buffer-file-name BUFFER NAME

Set the name of the file associated with the contents of BUFFER to NAME.
::end:: */
{
    repv canonical;
    rep_GC_root gc_tx, gc_name;

    if(!rep_NILP(name))
	rep_DECLARE1(name, rep_STRINGP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);

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

    make_marks_non_resident(VBUFFER(tx));
    VBUFFER(tx)->file_name = name;
    VBUFFER(tx)->canonical_file_name = canonical;
    make_marks_resident(tx);

    return name;
}

DEFUN("buffer-name", Fbuffer_name, Sbuffer_name, (repv tx), rep_Subr1) /*
::doc:buffer-name::
buffer-name [BUFFER]

Return the name of BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return VBUFFER(tx)->buffer_name;
}

DEFUN("set-buffer-name", Fset_buffer_name, Sset_buffer_name, (repv tx, repv name), rep_Subr2) /*
::doc:set-buffer-name::
set-buffer-name BUFFER NAME

Set the name of BUFFER to NAME.
::end:: */
{
    rep_DECLARE1(name, rep_STRINGP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    VBUFFER(tx)->buffer_name = name;
    if(VBUFFER(tx)->status_string == 0
       || !strncmp("Jade: ", rep_STR(VBUFFER(tx)->status_string), 5))
    {
	/* Reset the status-id */
	VBUFFER(tx)->status_string = rep_string_concat2("Jade: ", rep_STR(name));
    }
    return name;
}

DEFUN("buffer-changes", Fbuffer_changes, Sbuffer_changes, (repv tx), rep_Subr1) /*
::doc:buffer-changes::
buffer-changes [BUFFER]

Return the number of modifications to BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return rep_MAKE_INT(VBUFFER(tx)->change_count);
}

DEFUN("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p, (repv tx), rep_Subr1) /*
::doc:buffer-modified-p::
buffer-modified-p [BUFFER]

Returns t if the buffer has changed since it was last saved to disk.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return ((VBUFFER(tx)->change_count != VBUFFER(tx)->proper_saved_changed_count)
	    ? Qt : Qnil);
}

DEFUN("set-buffer-modified", Fset_buffer_modified, Sset_buffer_modified, (repv buf, repv stat), rep_Subr2) /*
::doc:set-buffer-modified::
set-buffer-modified BUFFER STATUS

If STATUS is nil make it look as though buffer hasn't changed, else make
it look as though it has.
::end:: */
{
    Lisp_Buffer *tx =BUFFERP(buf) ? VBUFFER(buf) : curr_vw->tx;
    if(rep_NILP(stat))
    {
	tx->proper_saved_changed_count = tx->change_count;
	tx->last_saved_change_count = tx->change_count;
	undo_record_unmodified(tx);
    }
    else
    {
	tx->proper_saved_changed_count = tx->change_count - 1;
	tx->last_saved_change_count = tx->change_count - 1;
    }
    return rep_VAL(tx);
}

DEFUN("buffer-length", Fbuffer_length, Sbuffer_length, (repv tx), rep_Subr1) /*
::doc:buffer-length::
buffer-length [BUFFER]

Returns the number of lines in BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return(rep_MAKE_INT(VBUFFER(tx)->line_count));
}

DEFUN("line-length", Fline_length, Sline_length, (repv pos, repv tx), rep_Subr2) /*
::doc:line-length::
line-length [LINE-POS] [BUFFER]

Returns the length (not including newline) of the specified line, or
using current cursor position if specifiers are not provided.
::end:: */
{
    if(POSP(pos))
    {
	if(!BUFFERP(tx))
	    tx = rep_VAL(curr_vw->tx);
    }
    else
    {
	pos = curr_vw->cursor_pos;
	tx = rep_VAL(curr_vw->tx);
    }
    return(rep_MAKE_INT(VBUFFER(tx)->lines[VROW(pos)].ln_Strlen - 1));
}

DEFUN("bufferp", Fbufferp, Sbufferp, (repv arg), rep_Subr1) /*
::doc:bufferp::
bufferp ARG

Returns t if ARG is a buffer.
::end:: */
{
    if(BUFFERP(arg))
	return(Qt);
    return(Qnil);
}

DEFUN_INT("restrict-buffer", Frestrict_buffer, Srestrict_buffer, (repv start, repv end, repv tx), rep_Subr3, "-m\nM") /*
::doc:restrict-buffer::
restrict-buffer START END [BUFFER]

Limits the portion of BUFFER (or the current buffer) that may be displayed
to that between the lines specified by positions START and END.
::end:: */
{
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    Funrestrict_buffer(tx);
    if(check_section(VBUFFER(tx), &start, &end) && VROW(start) <= VROW(end))
    {
	VBUFFER(tx)->logical_start = VROW(start);
	VBUFFER(tx)->logical_end = VROW(end) + 1;
	return Qt;
    }
    return Qnil;
}

DEFUN_INT("unrestrict-buffer", Funrestrict_buffer, Sunrestrict_buffer, (repv tx), rep_Subr1, "") /*
::doc:unrestrict-buffer::
unrestrict-buffer [BUFFER]

Remove any restriction on the parts of BUFFER that may be displayed.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    VBUFFER(tx)->logical_start = 0;
    VBUFFER(tx)->logical_end = VBUFFER(tx)->line_count;
    return Qt;
}
DEFUN("restriction-start", Frestriction_start, Srestriction_start, (repv tx), rep_Subr1) /*
::doc:restriction-start::
restriction-start [BUFFER]

Return the position of the first character that may be displayed in BUFFER
(or the current buffer).
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return make_pos(0, VBUFFER(tx)->logical_start);
}

DEFUN("restriction-end", Frestriction_end, Srestriction_end, (repv tx), rep_Subr1) /*
::doc:restriction-end::
restriction-end [BUFFER]

Return the position of the last character that may be displayed in BUFFER
(or the current buffer).
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return make_pos(VBUFFER(tx)->lines[VBUFFER(tx)->logical_end - 1].ln_Strlen -1,
		    VBUFFER(tx)->logical_end - 1);
}

DEFUN("buffer-restricted-p", Fbuffer_restricted_p, Sbuffer_restricted_p, (repv tx), rep_Subr1) /*
::doc:buffer-restricted-p::
buffer-restricted-p [BUFFER]

Returns t when BUFFER (or the current buffer) has been restricted to display
less than its full contents.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return ((VBUFFER(tx)->logical_start > 0
	     || VBUFFER(tx)->logical_end < VBUFFER(tx)->line_count)
	    ? Qt : Qnil);
}

DEFUN("auto-save-interval", Fauto_save_interval, Sauto_save_interval, (repv val), rep_Subr1) /*
::doc:auto-save-interval::
auto-save-interval [NEW-VALUE]

This buffer-local variable defines the period (in seconds) between each
automatic save of the buffer. A value of zero means that this buffer is
not to be auto-saved.
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->tx->auto_save_interval));
}

DEFUN("last-save-changes", Flast_save_changes, Slast_save_changes, (repv val), rep_Subr1) /*
::doc:last-save-changes::
last-save-changes [NEW-VALUE]

Number of changes the last time this buffer was saved (could be auto-save).
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->tx->last_saved_change_count));
}

DEFUN("last-user-save-changes", Flast_user_save_changes, Slast_user_save_changes, (repv val), rep_Subr1) /*
::doc:last-user-save-changes::
last-user-save-changes [NEW-VALUE]

Number of changes the last time this buffer was saved (not from auto-save).
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->tx->proper_saved_changed_count));
}

DEFUN("last-save-time", Flast_save_time, Slast_save_time, (repv val), rep_Subr1) /*
::doc:last-save-time::
last-save-time [NEW-VALUE]

System time at last save of this buffer (could be from an auto-save).
::end:: */
{
    intptr_t old = curr_vw->tx->last_saved_time;
    if(val != rep_nil)
	curr_vw->tx->last_saved_time = rep_get_long_uint(val);
    return rep_make_long_uint(old);
}

DEFUN("tab-size", Ftab_size, Stab_size, (repv val), rep_Subr1) /*
::doc:tab-size::
tab-size [NEW-VALUE]

Sets the size of tab-stops.
::end:: */
{
    return(rep_handle_var_int(val, &curr_vw->tx->tab_size));
}

DEFUN("truncate-lines", Ftruncate_lines, Struncate_lines, (repv val), rep_Subr1) /*
::doc:truncate-lines::
truncate-lines VALUE

When t lines that continue past the rightmost column of the screen are
truncated, not wrapped onto the next row as when this variable is nil.
The default value for all buffers is nil.
::end:: */
{
    Lisp_Buffer *tx = curr_vw->tx;
    repv old = TX_WRAP_LINES_P(tx) ? Qnil : Qt;
    if(!rep_NILP(val))
	tx->car |= TXFF_DONT_WRAP_LINES;
    else
	tx->car &= ~TXFF_DONT_WRAP_LINES;
    return old;
}

DEFUN("buffer-status-id", Fbuffer_status_id, Sbuffer_status_id,
      (repv val), rep_Subr1) /*
::doc:buffer-status-id::
buffer-status-line VALUE

This buffer-local string is displayed in the status line of the buffer. When
the buffer is created it is set to `Jade: BUFFER-NAME'.
::end:: */
{
    Lisp_Buffer *tx = curr_vw->tx;
    repv old = tx->status_string ? tx->status_string : Qnil;
    tx->status_string = rep_STRINGP(val) ? val : 0;
    return old;
}

DEFUN("all-buffers", Fall_buffers, Sall_buffers, (void), rep_Subr0) /*
::doc:all-buffers::
all-buffers

Return a list of all allocated buffer objects.
::end:: */
{
    repv list = Qnil;
    Lisp_Buffer *tx = buffer_chain;
    while(tx != 0)
    {
	list = Fcons(rep_VAL(tx), list);
	tx = tx->next;
    }
    return Fnreverse(list);
}

void
kill_buffer_local_variables(Lisp_Buffer *tx)
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
    while(mk != 0)
    {
	repv nxt = rep_VAL(VMARK(mk)->next);
	
	if(rep_STRINGP(VBUFFER(newtx)->canonical_file_name)
	    && strcmp(rep_STR(VBUFFER(newtx)->canonical_file_name),
		      rep_STR(VMARK(mk)->canon_file)) == 0)
	{
	    VMARK(mk)->file = newtx;
	    VMARK(mk)->canon_file = Qnil;
	    VMARK(mk)->next = VBUFFER(newtx)->mark_chain;
	    VBUFFER(newtx)->mark_chain = VMARK(mk);
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
	headp = &(VBUFFER(mk->file)->mark_chain);
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
make_marks_non_resident(Lisp_Buffer *oldtx)
{
    repv canon_file = (rep_STRINGP(oldtx->canonical_file_name)
			? oldtx->canonical_file_name
			: Qnil);
    repv file = (rep_STRINGP(oldtx->file_name)
		  ? oldtx->file_name
		  : canon_file);
    Lisp_Mark *nxt, *mk = oldtx->mark_chain;
    oldtx->mark_chain = NULL;
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
	rep_MARKVAL(VBUFFER(VMARK(val)->file)->file_name);
	rep_MARKVAL(VBUFFER(VMARK(val)->file)->canonical_file_name);
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
	    rep_free(mk);
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
    char tbuf[40];
    rep_stream_puts(strm, "#<mark ", -1, false);
    if(MARK_RESIDENT_P(VMARK(obj)))
	buffer_prin(strm, VMARK(obj)->file);
    else
    {
	rep_stream_putc(strm, '"');
	rep_stream_puts(strm, rep_PTR(VMARK(obj)->file), -1, true);
	rep_stream_putc(strm, '"');
    }
#ifdef HAVE_SNPRINTF
    snprintf(tbuf, sizeof(tbuf),
#else
    sprintf(tbuf,
#endif
	    " #<pos %" PRIdPTR " %" PRIdPTR ">>",
	    VCOL(VMARK(obj)->pos),
	    VROW(VMARK(obj)->pos));
    rep_stream_puts(strm, tbuf, -1, false);
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
	return pos_getc(VBUFFER(VMARK(stream)->file), &VMARK(stream)->pos);
}

static int
mark_ungetc (repv stream, int c)
{
    POS_UNGETC(VMARK(stream)->pos, VBUFFER(VMARK(stream)->file));
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
	return pos_putc(VBUFFER(VMARK(stream)->file), &VMARK(stream)->pos, c);
}

static intptr_t
mark_puts (repv stream, const void *data, intptr_t len, bool is_val)
{
    const char *buf = is_val ? rep_STR(data) : data;
    if(!MARK_RESIDENT_P(VMARK(stream)))
    {
	Fsignal(Qinvalid_stream, rep_list_2(stream, rep_VAL(&non_resident)));
	return EOF;
    }
    else
	return pos_puts(VBUFFER(VMARK(stream)->file),
			&VMARK(stream)->pos, buf, len);
}

DEFUN("make-mark", Fmake_mark, Smake_mark, (repv pos, repv buffer), rep_Subr2) /*
::doc:make-mark::
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
    repv mk = rep_VAL(rep_alloc(sizeof(Lisp_Mark)));
    if(mk != 0)
    {
	rep_GC_root gc_mk, gc_buf;

	VMARK(mk)->car = mark_type;
	VMARK(mk)->next_alloc = mark_chain;
	mark_chain = VMARK(mk);
	rep_data_after_gc += sizeof(Lisp_Mark);
	VMARK(mk)->pos = POSP(pos) ? pos : curr_vw->cursor_pos;

	/* just so these are valid */
	VMARK(mk)->file = rep_VAL(curr_vw->tx);
	VMARK(mk)->canon_file = Qnil;
	VMARK(mk)->next = NULL;

	if(rep_STRINGP(buffer))
	{
	    repv tem;
	    rep_PUSHGC(gc_mk, mk);
	    rep_PUSHGC(gc_buf, buffer);
	    tem = Fget_file_buffer(buffer);
	    rep_POPGC; rep_POPGC;
	    if(tem != 0 && BUFFERP(tem))
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
		buffer = rep_VAL(curr_vw->tx);
	    VMARK(mk)->file = buffer;
	    VMARK(mk)->next = VBUFFER(buffer)->mark_chain;
	    VBUFFER(buffer)->mark_chain = VMARK(mk);
	}
	return mk;
    }
    return rep_mem_error();
}

DEFUN("set-mark-pos", Fset_mark_pos, Sset_mark_pos,
      (repv mark, repv pos), rep_Subr2) /*
::doc:set-mark-pos::
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
::doc:set-mark-file::
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
	if(tem != 0 && BUFFERP(tem))
	    file = tem;
    }
    if(BUFFERP(file))
    {
	if(VMARK(mark)->file != file)
	{
	    unchain_mark(VMARK(mark));
	    VMARK(mark)->next = VBUFFER(file)->mark_chain;
	    VBUFFER(file)->mark_chain = VMARK(mark);
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
::doc:mark-pos::
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
::doc:mark-file::
mark-file MARK

Returns the file-name or buffer that MARK points to.
::end:: */
{
    rep_DECLARE1(mark, MARKP);
    return(VMARK(mark)->file);
}

DEFUN("mark-resident-p", Fmark_resident_p, Smark_resident_p, (repv mark), rep_Subr1) /*
::doc:mark-resident-p::
mark-resident-p MARK

Returns t if the file that MARK points to is in a buffer.
::end:: */
{
    rep_DECLARE1(mark, MARKP);
    return MARK_RESIDENT_P(VMARK(mark)) ? Qt : Qnil;
}

DEFUN("markp", Fmarkp, Smarkp, (repv mark), rep_Subr1) /*
::doc:markp::
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
    static rep_type buffer = {
	.name = "buffer",
	.print = buffer_prin,
	.sweep = buffer_sweep,
	.mark = buffer_mark,
	.getc = buffer_getc,
	.ungetc = buffer_ungetc,
	.putc = buffer_putc,
	.puts = buffer_puts,
	.bind = buffer_bind,
	.unbind = buffer_unbind,
    };

    static rep_type mark = {
	.name = "mark",
	.compare = mark_cmp,
	.print = mark_prin,
	.mark = mark_mark,
	.getc = mark_getc,
	.ungetc = mark_ungetc,
	.putc = mark_putc,
	.puts = mark_puts,
    };

    buffer_type = rep_define_type(&buffer);
    mark_type = rep_define_type(&mark);

    rep_mark_static((void *)&non_resident_mark_chain);
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
    Lisp_Buffer *tx = buffer_chain;
    Lisp_Mark *mk = mark_chain;
    while(tx)
    {
	Lisp_Buffer *nxttx = tx->next;
	kill_line_list(tx);
	rep_free(tx);
	tx = nxttx;
    }
    buffer_chain = NULL;
    while(mk)
    {
	Lisp_Mark *nxtmk = mk->next_alloc;
	rep_free(mk);
	mk = nxtmk;
    }
    mark_chain = NULL;
}
