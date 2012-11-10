/* undo.c -- Recording and use of undo information
   Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING. If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <string.h>


/* Maximum number of bytes that *each buffer* may devote to undo
   information.  */
static int max_undo_size = 10000;

/* Lets us use the string which undo_record_deletion() creates for
   other uses.	*/
static repv pending_deletion_string;
static repv pending_deletion_start, pending_deletion_end;
static Lisp_Buffer *pending_deletion_tx;

/* While we're in Fundo() this is set.  This is also tested by
   undo_distinct(); if false it will call coalesce_undo() if necessary.
   undo_distinct() always sets this to false. */
static bool in_undo;
static Lisp_Buffer *last_undid_tx;

DEFSYM(undo, "undo");

/* If not in an undo, this will re-combine the waiting_undo and
   undo_list. */
static void
coalesce_undo(Lisp_Buffer *tx)
{
    if(!in_undo
       && (tx->pending_undo_list != 0))
    {
	repv tmp = Fnreverse(tx->did_undo_list);
	if(tmp)
	{
	    if (rep_CONSP(tmp) && rep_CAR(tmp) != Qnil)
		tmp = Fcons (Qnil, tmp);
	    if (rep_CONSP(tx->pending_undo_list)
		&& rep_CAR(tx->pending_undo_list) != Qnil)
	    {
		tx->pending_undo_list = Fcons (Qnil, tx->pending_undo_list);
	    }

	    tx->undo_list = Fnconc(rep_list_3(tx->undo_list,
					       tmp,
					       tx->pending_undo_list));
	}
	tx->did_undo_list = Qnil;
	tx->pending_undo_list = 0;
	last_undid_tx = NULL;
    }
}

/* Called *after* recording an undo command, checks if this is the
   first change to the buffer. Should be called before the operation. */
static inline void
check_first_mod(Lisp_Buffer *tx)
{
    if((tx->change_count == tx->proper_saved_changed_count)
       && ((tx->car & TXFF_NO_UNDO) == 0))
    {
	/* First modification, record this. */
	tx->undo_list = Fcons(Qt, tx->undo_list);
    }
}

/* This should be called whenever the buffer is saved, and thus set as
   being unmodified. Any previous "unmodified" marker in the buffer's
   undo list is deleted, so that undoing back past this old marker won't
   errnoneously set the buffer as being unmodified (i.e. the same as the
   copy on disk). */
void
undo_record_unmodified(Lisp_Buffer *tx)
{
    if((tx->change_count == tx->proper_saved_changed_count)
       && ((tx->car & TXFF_NO_UNDO) == 0))
    {
	repv *ptr = &tx->undo_list;
	coalesce_undo(tx);
	while(rep_CONSP(*ptr))
	{
	    if(rep_CAR(*ptr) == Qt)
	    {
		/* found it */
		*ptr = rep_CDR(*ptr);
		break;
	    }
	    else
		ptr = &rep_CDR(*ptr);
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		break;
	}
    }
}

/* Grabs the string between START and END in buffer TX and adds it to
   the buffer's undo-list.  This has to be done *before* the text is
   actually deleted from the buffer (for obvious reasons).  */
void
undo_record_deletion(Lisp_Buffer *tx, repv start, repv end)
{
    if((tx->car & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	repv string;
	if((pending_deletion_string != 0)
	   && (pending_deletion_tx = tx)
	   && (POS_EQUAL_P(pending_deletion_start, start))
	   && (POS_EQUAL_P(pending_deletion_end, end)))
	{
	    /* A saved deletion; use it. */
	    string = pending_deletion_string;
	}
	else
	{
	    size_t len = section_length(tx, start, end);
	    if(len == 1)
	    {
		/* A deletion of 1 character is recorded as a character. */
		string = Fget_char(start, rep_VAL(tx));
		if(!string || !rep_INTP(string))
		    return;
	    }
	    else
	    {
		string = rep_make_string(len + 1);
		copy_section(tx, start, end, rep_STR(string));
		rep_STR(string)[len] = 0;
	    }
	}
	coalesce_undo(tx);
	check_first_mod(tx);
	tx->undo_list = Fcons(Fcons(start, string),
				   tx->undo_list);
    }
    pending_deletion_string = 0;
}

/* Lets the saved deletion be used for more than the undo list. Call
   this *before* doing anything else. It will be copy the string and
   return it. The next call to undo_record_deletion() will use the
   *same* copy (unless the parameters don't match).  */
repv
undo_push_deletion(Lisp_Buffer *tx, repv start, repv end)
{
    size_t len = section_length(tx, start, end);
    if(len > 0)
    {
	repv string = rep_make_string(len + 1);
	copy_section(tx, start, end, rep_STR(string));
	rep_STR(string)[len] = 0;
	pending_deletion_string = string;
	pending_deletion_start = start;
	pending_deletion_end = end;
	pending_deletion_tx = tx;
	return(string);
    }
    else
	return rep_null_string();
}

/* Adds an insertion between START and END to the TX buffer's undo-list.
   Doesn't copy anything, just records START and END.  */
void
undo_record_insertion(Lisp_Buffer *tx, repv start, repv end)
{
    if((tx->car & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	repv item;
	coalesce_undo(tx);
	check_first_mod(tx);
	item = tx->undo_list;
	if(rep_CONSP(item) && rep_CONSP(rep_CAR(item)))
	{
	    item = rep_CAR(item);
	    if(POSP(rep_CDR(item)) && POS_EQUAL_P(start, rep_CDR(item)))
	    {
		/* This insertion is directly after the end of the
		   previous insertion; extend the previous one to cover
		   this one.  */
		rep_CDR(item) = end;
		return;
	    }
	}
	tx->undo_list = Fcons(Fcons(start, end), tx->undo_list);
    }
}

/* Record that the text between START and END has been modified.  This
   must be done *before* the modification is actually done.  */
void
undo_record_modification(Lisp_Buffer *tx, repv start, repv end)
{
    if((tx->car & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	undo_record_deletion(tx, start, end);
	undo_record_insertion(tx, start, end);
    }
}

/* Signal the end of this command. This includes adding a group-separator
   to all buffer's undo lists (that need one). */
void
undo_end_of_command(void)
{
    Lisp_Buffer *tx;
    repv last = Fsymbol_value (Qlast_command, Qt);
    if((!rep_NILP(last)) && (last != Qundo)
       && last_undid_tx && last_undid_tx->pending_undo_list)
    {
	coalesce_undo(last_undid_tx);
    }
    in_undo = false;

    tx = buffer_chain;
    while(tx != NULL)
    {
	if(!rep_NILP(tx->undo_list)
	   && ((tx->car & TXFF_NO_UNDO) == 0)
	   && !(rep_CONSP(tx->undo_list) && rep_NILP(rep_CAR(tx->undo_list))))
	    tx->undo_list = Fcons(Qnil, tx->undo_list);
	tx = tx->next;
    }
}

DEFSTRING(nothing_to_undo, "Nothing to undo!");

DEFUN_INT("undo", Fundo, Sundo, (repv tx, repv arg), rep_Subr2, rep_DS_NL "p") /*
::doc:undo::
undo [BUFFER] [ARG]

In the buffer BUFFER, undo everything back to the start of the previous
command. Consecutive undo commands work backwards through the BUFFER's
history.

ARG is the number of commands to undo, when called interactively this is
taken from the prefix argument.
::end:: */
{
    intptr_t count = rep_INTP(arg) ? rep_INT(arg) : 1;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(VBUFFER(tx)->pending_undo_list == 0)
    {
	/* First call. */
	VBUFFER(tx)->pending_undo_list = VBUFFER(tx)->undo_list;
	VBUFFER(tx)->undo_list = Qnil;
	if(rep_CONSP(VBUFFER(tx)->pending_undo_list) && rep_NILP(rep_CAR(VBUFFER(tx)->pending_undo_list)))
	    /* Ignore the initial group separator */
	    count++;
    }
    if(rep_NILP(VBUFFER(tx)->pending_undo_list))
    {
	return(Fsignal(Qerror, rep_LIST_1(rep_VAL(&nothing_to_undo))));
    }
    in_undo = true;
    last_undid_tx = VBUFFER(tx);
    while(rep_CONSP(VBUFFER(tx)->pending_undo_list))
    {
	repv item = rep_CAR(VBUFFER(tx)->pending_undo_list);
	VBUFFER(tx)->pending_undo_list = rep_CDR(VBUFFER(tx)->pending_undo_list);
	VBUFFER(tx)->did_undo_list = Fcons(item, VBUFFER(tx)->did_undo_list);
	if(rep_NILP(item))
	{
	    /* Group separator; break the loop if ARG commands undone. */
	    if(--count <= 0)
		break;
	}
	else if(rep_CONSP(item) && rep_CONSP(rep_CAR(item)))
	{
	    if(rep_STRINGP(rep_CDR(item)))
	    {
		/* A deleted string */
		repv new = Finsert(rep_CDR(item), rep_CAR(item), tx);
		if(new && !rep_NILP(new))
		    Fgoto(new);
	    }
	    else if(rep_INTP(rep_CDR(item)))
	    {
		/* A deleted character */
		repv tmp = rep_make_string(2);
		uint8_t c = rep_INT(rep_CDR(item));
		rep_STR(tmp)[0] = c;
		rep_STR(tmp)[1] = 0;
		tmp = Finsert(tmp, rep_CAR(item), tx);
		if(tmp && !rep_NILP(tmp))
		    Fgoto(tmp);
	    }
	    else if(rep_CONSP(rep_CDR(item)))
	    {
		/* An insertion */
		Fdelete_area(rep_CAR(item), rep_CDR(item), tx);
		Fgoto(rep_CAR(item));
	    }
	    else if(POSP(item))
		Fgoto(item);
	}
	else if(item == Qt)
	{
	    /* clear modification flag. */
	    VBUFFER(tx)->proper_saved_changed_count = VBUFFER(tx)->change_count;
	    VBUFFER(tx)->last_saved_change_count = VBUFFER(tx)->change_count;
	}
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    break;
    }
    Fset (Qthis_command, Qundo);
    return(Qt);
}

DEFUN("max-undo-size", Fmax_undo_size, Smax_undo_size, (repv val), rep_Subr1) /*
::doc:max-undo-size::
max-undo-size [NEW-VALUE]

The maximum amount of storage that a single buffer may devote to recording
undo information.
::end:: */
{
    return rep_handle_var_int (val, &max_undo_size);
}

DEFUN("buffer-record-undo", Fbuffer_record_undo, Sbuffer_record_undo, (void), rep_Subr0) /*
::doc:buffer-record-undo::
buffer-record-undo

When nil no undo information is kept in this buffer.
::end:: */
{
    Lisp_Buffer *tx = curr_vw->tx;
    return (tx->car & TXFF_NO_UNDO) ? Qnil : Qt;
}

DEFUN("set-buffer-record-undo", Fset_buffer_record_undo, Sset_buffer_record_undo, (repv val), rep_Subr1) /*
::doc:set-buffer-record-undo::
set-buffer-record-undo VALUE

When nil no undo information is kept in this buffer.
::end:: */
{
    Lisp_Buffer *tx = curr_vw->tx;
    if(rep_NILP(val))
	tx->car |= TXFF_NO_UNDO;
    else
	tx->car &= ~TXFF_NO_UNDO;
    return val;
}

DEFUN("buffer-undo-list", Fbuffer_undo_list, Sbuffer_undo_list, (void), rep_Subr0) /*
::doc:buffer-undo-list::
buffer-undo-list

This buffer's list of undo information.
::end:: */
{
    Lisp_Buffer *tx = curr_vw->tx;
    return tx->undo_list;
}

DEFUN("set-buffer-undo-list", Fset_buffer_undo_list, Sset_buffer_undo_list, (repv val), rep_Subr1) /*
::doc:set-buffer-undo-list::
set-buffer-undo-list VALUE

This buffer's list of undo information.
::end:: */
{
    Lisp_Buffer *tx = curr_vw->tx;
    tx->undo_list = val;
    return val;
}

/* Called by gc, this makes each undo-lists use less memory than
   max-undo-size. But it always leaves upto the first boundary
   intact.  Doesn't handle the case when the undo list is split
   into three bits while in the middle of a sequence of undo's.  */
void
undo_trim(void)
{
    Lisp_Buffer *tx;

    if (in_undo)
	return;

    tx = buffer_chain;
    while(tx)
    {
	repv *undo_list;
	size_t size_count = 0;
	if(tx->pending_undo_list && !rep_NILP(tx->pending_undo_list))
	    undo_list = &tx->pending_undo_list;
	else if(!rep_NILP(tx->did_undo_list))
	    undo_list = &tx->did_undo_list;
	else
	    undo_list = &tx->undo_list;
	while(rep_CONSP(*undo_list))
	{
	    repv item = rep_CAR(*undo_list);
	    size_count += sizeof(rep_cons);
	    if(rep_CONSP(item))
	    {
		size_count += sizeof(rep_cons);
		if(rep_CONSP(rep_CDR(item)))
		    size_count += sizeof(rep_cons) * 2;
		else if(rep_STRINGP(rep_CDR(item)))
		    size_count += rep_STRING_LEN(rep_CDR(item));
		if(size_count > (size_t) max_undo_size)
		{
		    /* Truncate the list at the end of this group. */
		    while(rep_CONSP(*undo_list) && !rep_NILP(rep_CAR(*undo_list)))
			undo_list = &rep_CDR(*undo_list);
		    *undo_list = Qnil;
		    break;
		}
	    }
	    undo_list = &rep_CDR(*undo_list);
	}
	tx = tx->next;
    }
}

void
undo_init(void)
{
    rep_mark_static(&pending_deletion_string);
    rep_mark_static(&pending_deletion_start);
    rep_mark_static(&pending_deletion_end);
    rep_INTERN(undo);
    rep_ADD_SUBR_INT(Sundo);
    rep_ADD_SUBR(Smax_undo_size);
    rep_ADD_SUBR(Sset_buffer_record_undo);
    rep_ADD_SUBR(Sbuffer_record_undo);
    rep_ADD_SUBR(Sbuffer_undo_list);
    rep_ADD_SUBR(Sset_buffer_undo_list);
}
