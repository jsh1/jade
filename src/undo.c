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
#include "jade_protos.h"

#include <string.h>

_PR void  undo_record_unmodified(TX *tx);
_PR void  undo_record_deletion(TX *, VALUE, VALUE);
_PR VALUE undo_push_deletion(TX *, VALUE, VALUE);
_PR void  undo_record_insertion(TX *, VALUE, VALUE);
_PR void  undo_record_modification(TX *, VALUE, VALUE);
_PR void  undo_end_of_command(void);
_PR void  undo_trim(void);
_PR void  undo_init(void);

/* Maximum number of bytes that *each buffer* may devote to undo
   information.  */
static long max_undo_size = 10000;

/* Lets us use the string which undo_record_deletion() creates for
   other uses.	*/
static VALUE pending_deletion_string;
static VALUE pending_deletion_start, pending_deletion_end;
static TX *pending_deletion_tx;

/* While we're in cmd_undo() this is set.  This is also tested by
   undo_distinct(); if FALSE it will call coalesce_undo() if necessary.
   undo_distinct() always sets this to FALSE. */
static bool in_undo;
static TX *last_undid_tx;

_PR VALUE sym_undo;
DEFSYM(undo, "undo");

/* If not in an undo, this will re-combine the waiting_undo and
   tx_UndoList. */
static void
coalesce_undo(TX *tx)
{
    if(!in_undo
       && (tx->tx_ToUndoList != LISP_NULL))
    {
	VALUE tmp = cmd_nreverse(tx->tx_UndoneList);
	if(tmp)
	{
	    tx->tx_UndoList = cmd_nconc(list_3(tx->tx_UndoList,
					       tmp,
					       tx->tx_ToUndoList));
	}
	tx->tx_UndoneList = sym_nil;
	tx->tx_ToUndoList = LISP_NULL;
	last_undid_tx = NULL;
    }
}

/* Called *after* recording an undo command, checks if this is the
   first change to the buffer. Should be called before the operation. */
static inline void
check_first_mod(TX *tx)
{
    if((tx->tx_Changes == tx->tx_ProperSaveChanges)
       && ((tx->tx_Flags & TXFF_NO_UNDO) == 0))
    {
	/* First modification, record this. */
	tx->tx_UndoList = cmd_cons(sym_t, tx->tx_UndoList);
    }
}

/* This should be called whenever the buffer is saved, and thus set as
   being unmodified. Any previous "unmodified" marker in the buffer's
   undo list is deleted, so that undoing back past this old marker won't
   errnoneously set the buffer as being unmodified (i.e. the same as the
   copy on disk). */
void
undo_record_unmodified(TX *tx)
{
    if((tx->tx_Changes == tx->tx_ProperSaveChanges)
       && ((tx->tx_Flags & TXFF_NO_UNDO) == 0))
    {
	VALUE *ptr = &tx->tx_UndoList;
	coalesce_undo(tx);
	while(CONSP(*ptr))
	{
	    if(VCAR(*ptr) == sym_t)
	    {
		/* found it */
		*ptr = VCDR(*ptr);
		break;
	    }
	    else
		ptr = &VCDR(*ptr);
	    TEST_INT;
	    if(INT_P)
		break;
	}
    }
}

/* Grabs the string between START and END in buffer TX and adds it to
   the buffer's undo-list.  This has to be done *before* the text is
   actually deleted from the buffer (for obvious reasons).  */
void
undo_record_deletion(TX *tx, VALUE start, VALUE end)
{
    if((tx->tx_Flags & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	VALUE string;
	if((pending_deletion_string != LISP_NULL)
	   && (pending_deletion_tx = tx)
	   && (POS_EQUAL_P(pending_deletion_start, start))
	   && (POS_EQUAL_P(pending_deletion_end, end)))
	{
	    /* A saved deletion; use it. */
	    string = pending_deletion_string;
	}
	else
	{
	    long len = section_length(tx, start, end);
	    if(len == 1)
	    {
		/* A deletion of 1 character is recorded as a character. */
		string = cmd_get_char(start, VAL(tx));
		if(!string || !INTP(string))
		    return;
	    }
	    else
	    {
		string = make_string(len + 1);
		copy_section(tx, start, end, VSTR(string));
		VSTR(string)[len] = 0;
	    }
	}
	coalesce_undo(tx);
	check_first_mod(tx);
	tx->tx_UndoList = cmd_cons(cmd_cons(start, string),
				   tx->tx_UndoList);
    }
    pending_deletion_string = LISP_NULL;
}

/* Lets the saved deletion be used for more than the undo list. Call
   this *before* doing anything else. It will be copy the string and
   return it. The next call to undo_record_deletion() will use the
   *same* copy (unless the parameters don't match).  */
VALUE
undo_push_deletion(TX *tx, VALUE start, VALUE end)
{
    long len = section_length(tx, start, end);
    if(len > 0)
    {
	VALUE string = make_string(len + 1);
	copy_section(tx, start, end, VSTR(string));
	VSTR(string)[len] = 0;
	pending_deletion_string = string;
	pending_deletion_start = start;
	pending_deletion_end = end;
	pending_deletion_tx = tx;
	return(string);
    }
    else
	return null_string();
}

/* Adds an insertion between START and END to the TX buffer's undo-list.
   Doesn't copy anything, just records START and END.  */
void
undo_record_insertion(TX *tx, VALUE start, VALUE end)
{
    if((tx->tx_Flags & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	VALUE item;
	coalesce_undo(tx);
	check_first_mod(tx);
	item = tx->tx_UndoList;
	if(CONSP(item) && CONSP(VCAR(item)))
	{
	    item = VCAR(item);
	    if(POSP(VCDR(item)) && POS_EQUAL_P(start, VCDR(item)))
	    {
		/* This insertion is directly after the end of the
		   previous insertion; extend the previous one to cover
		   this one.  */
		VCDR(item) = end;
		return;
	    }
	}
	tx->tx_UndoList = cmd_cons(cmd_cons(start, end), tx->tx_UndoList);
    }
}

/* Record that the text between START and END has been modified.  This
   must be done *before* the modification is actually done.  */
void
undo_record_modification(TX *tx, VALUE start, VALUE end)
{
    if((tx->tx_Flags & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
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
    TX *tx;
    if((!last_command || !NILP(last_command))
       && (last_command != sym_undo)
       && last_undid_tx
       && last_undid_tx->tx_ToUndoList)
    {
	coalesce_undo(last_undid_tx);
    }
    in_undo = FALSE;

    tx = buffer_chain;
    while(tx != NULL)
    {
	if(!NILP(tx->tx_UndoList)
	   && ((tx->tx_Flags & TXFF_NO_UNDO) == 0)
	   && !(CONSP(tx->tx_UndoList) && NILP(VCAR(tx->tx_UndoList))))
	    tx->tx_UndoList = cmd_cons(sym_nil, tx->tx_UndoList);
	tx = tx->tx_Next;
    }
}

DEFSTRING(nothing_to_undo, "Nothing to undo!");

_PR VALUE cmd_undo(VALUE tx, VALUE arg);
DEFUN_INT("undo", cmd_undo, subr_undo, (VALUE tx, VALUE arg), V_Subr2, DOC_undo, DS_NL "p") /*
::doc:undo::
undo [BUFFER] [ARG]

In the buffer BUFFER, undo everything back to the start of the previous
command. Consecutive undo commands work backwards through the BUFFER's
history.

ARG is the number of commands to undo, when called interactively this is
taken from the prefix argument.
::end:: */
{
    long count = INTP(arg) ? VINT(arg) : 1;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(VTX(tx)->tx_ToUndoList == LISP_NULL)
    {
	/* First call. */
	VTX(tx)->tx_ToUndoList = VTX(tx)->tx_UndoList;
	VTX(tx)->tx_UndoList = sym_nil;
	if(CONSP(VTX(tx)->tx_ToUndoList) && NILP(VCAR(VTX(tx)->tx_ToUndoList)))
	    /* Ignore the initial group separator */
	    count++;
    }
    if(NILP(VTX(tx)->tx_ToUndoList))
    {
	return(cmd_signal(sym_error, LIST_1(VAL(&nothing_to_undo))));
    }
    in_undo = TRUE;
    last_undid_tx = VTX(tx);
    while(CONSP(VTX(tx)->tx_ToUndoList))
    {
	VALUE item = VCAR(VTX(tx)->tx_ToUndoList);
	VTX(tx)->tx_ToUndoList = VCDR(VTX(tx)->tx_ToUndoList);
	VTX(tx)->tx_UndoneList = cmd_cons(item, VTX(tx)->tx_UndoneList);
	if(NILP(item))
	{
	    /* Group separator; break the loop if ARG commands undone. */
	    if(--count <= 0)
		break;
	}
	else if(CONSP(item) && CONSP(VCAR(item)))
	{
	    if(STRINGP(VCDR(item)))
	    {
		/* A deleted string */
		VALUE new = cmd_insert(VCDR(item), VCAR(item), tx);
		if(new && !NILP(new))
		    cmd_goto(new);
	    }
	    else if(INTP(VCDR(item)))
	    {
		/* A deleted character */
		VALUE tmp = make_string(2);
		VSTR(tmp)[0] = (u_char)VINT(VCDR(item));
		VSTR(tmp)[1] = 0;
		tmp = cmd_insert(tmp, VCAR(item), tx);
		if(tmp && !NILP(tmp))
		    cmd_goto(tmp);
	    }
	    else if(CONSP(VCDR(item)))
	    {
		/* An insertion */
		cmd_delete_area(VCAR(item), VCDR(item), tx);
		cmd_goto(VCAR(item));
	    }
	    else if(POSP(item))
		cmd_goto(item);
	}
	else if(item == sym_t)
	{
	    /* clear modification flag. */
	    VTX(tx)->tx_ProperSaveChanges = VTX(tx)->tx_Changes;
	}
	TEST_INT;
	if(INT_P)
	    break;
    }
    this_command = sym_undo;
    return(sym_t);
}

_PR VALUE var_max_undo_size(VALUE val);
DEFUN("max-undo-size", var_max_undo_size, subr_max_undo_size, (VALUE val), V_Var, DOC_max_undo_size) /*
::doc:max_undo_size::
The maximum amount of storage that a single buffer may devote to recording
undo information.
::end:: */
{
    if(val)
    {
	if(INTP(val))
	    max_undo_size = VINT(val);
	return LISP_NULL;
    }
    else
	return(MAKE_INT(max_undo_size));
}

_PR VALUE var_buffer_record_undo(VALUE val);
DEFUN("buffer-record-undo", var_buffer_record_undo, subr_buffer_record_undo, (VALUE val), V_Var, DOC_buffer_record_undo) /*
::doc:buffer_record_undo::
When nil no undo information is kept in this buffer.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(val)
    {
	if(NILP(val))
	    tx->tx_Flags |= TXFF_NO_UNDO;
	else
	    tx->tx_Flags &= ~TXFF_NO_UNDO;
	return LISP_NULL;
    }
    else
	return((tx->tx_Flags & TXFF_NO_UNDO) ? sym_nil : sym_t);
}

VALUE var_buffer_undo_list(VALUE val);
DEFUN("buffer-undo-list", var_buffer_undo_list, subr_buffer_undo_list, (VALUE val), V_Var, DOC_buffer_undo_list) /*
::doc:buffer_undo_list::
This buffer's list of undo information.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(val)
	tx->tx_UndoList = val;
    return(tx->tx_UndoList);
}

/* Called by gc, this makes each undo-lists use less memory than
   max-undo-size. But it always leaves upto the first boundary
   intact.  Doesn't handle the case when the undo list is split
   into three bits while in the middle of a sequence of undo's.  */
void
undo_trim(void)
{
    TX *tx = buffer_chain;
    while(tx)
    {
	VALUE *undo_list;
	long size_count = 0;
	if(tx->tx_ToUndoList && !NILP(tx->tx_ToUndoList))
	    undo_list = &tx->tx_ToUndoList;
	else if(!NILP(tx->tx_UndoneList))
	    undo_list = &tx->tx_UndoneList;
	else
	    undo_list = &tx->tx_UndoList;
	while(CONSP(*undo_list))
	{
	    VALUE item = VCAR(*undo_list);
	    size_count += sizeof(Lisp_Cons);
	    if(CONSP(item))
	    {
		size_count += sizeof(Lisp_Cons);
		if(CONSP(VCDR(item)))
		    size_count += sizeof(Lisp_Cons) * 2;
		else if(STRINGP(VCDR(item)))
		    size_count += STRING_LEN(VCDR(item));
		if(size_count > max_undo_size)
		{
		    /* Truncate the list at the end of this group. */
		    while(CONSP(*undo_list) && !NILP(VCAR(*undo_list)))
			undo_list = &VCDR(*undo_list);
		    *undo_list = sym_nil;
		    break;
		}
	    }
	    undo_list = &VCDR(*undo_list);
	}
	tx = tx->tx_Next;
    }
}

void
undo_init(void)
{
    mark_static(&pending_deletion_string);
    mark_static(&pending_deletion_start);
    mark_static(&pending_deletion_end);
    INTERN(undo);
    ADD_SUBR_INT(subr_undo);
    ADD_SUBR(subr_max_undo_size);
    ADD_SUBR(subr_buffer_record_undo);
    ADD_SUBR(subr_buffer_undo_list);
}
