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
static long max_undo_size = 10000;

/* Lets us use the string which undo_record_deletion() creates for
   other uses.	*/
static repv pending_deletion_string;
static repv pending_deletion_start, pending_deletion_end;
static TX *pending_deletion_tx;

/* While we're in Fundo() this is set.  This is also tested by
   undo_distinct(); if FALSE it will call coalesce_undo() if necessary.
   undo_distinct() always sets this to FALSE. */
static bool in_undo;
static TX *last_undid_tx;

DEFSYM(undo, "undo");

/* If not in an undo, this will re-combine the waiting_undo and
   tx_UndoList. */
static void
coalesce_undo(TX *tx)
{
    if(!in_undo
       && (tx->tx_ToUndoList != rep_NULL))
    {
	repv tmp = Fnreverse(tx->tx_UndoneList);
	if(tmp)
	{
	    if (rep_CONSP(tmp) && rep_CAR(tmp) != Qnil)
		tmp = Fcons (Qnil, tmp);
	    if (rep_CONSP(tx->tx_ToUndoList)
		&& rep_CAR(tx->tx_ToUndoList) != Qnil)
	    {
		tx->tx_ToUndoList = Fcons (Qnil, tx->tx_ToUndoList);
	    }

	    tx->tx_UndoList = Fnconc(rep_list_3(tx->tx_UndoList,
					       tmp,
					       tx->tx_ToUndoList));
	}
	tx->tx_UndoneList = Qnil;
	tx->tx_ToUndoList = rep_NULL;
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
	tx->tx_UndoList = Fcons(Qt, tx->tx_UndoList);
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
	repv *ptr = &tx->tx_UndoList;
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
undo_record_deletion(TX *tx, repv start, repv end)
{
    if((tx->tx_Flags & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	repv string;
	if((pending_deletion_string != rep_NULL)
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
	tx->tx_UndoList = Fcons(Fcons(start, string),
				   tx->tx_UndoList);
    }
    pending_deletion_string = rep_NULL;
}

/* Lets the saved deletion be used for more than the undo list. Call
   this *before* doing anything else. It will be copy the string and
   return it. The next call to undo_record_deletion() will use the
   *same* copy (unless the parameters don't match).  */
repv
undo_push_deletion(TX *tx, repv start, repv end)
{
    long len = section_length(tx, start, end);
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
undo_record_insertion(TX *tx, repv start, repv end)
{
    if((tx->tx_Flags & TXFF_NO_UNDO) == 0 && !POS_EQUAL_P(start, end))
    {
	repv item;
	coalesce_undo(tx);
	check_first_mod(tx);
	item = tx->tx_UndoList;
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
	tx->tx_UndoList = Fcons(Fcons(start, end), tx->tx_UndoList);
    }
}

/* Record that the text between START and END has been modified.  This
   must be done *before* the modification is actually done.  */
void
undo_record_modification(TX *tx, repv start, repv end)
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
    if((!last_command || !rep_NILP(last_command))
       && (last_command != Qundo)
       && last_undid_tx
       && last_undid_tx->tx_ToUndoList)
    {
	coalesce_undo(last_undid_tx);
    }
    in_undo = FALSE;

    tx = buffer_chain;
    while(tx != NULL)
    {
	if(!rep_NILP(tx->tx_UndoList)
	   && ((tx->tx_Flags & TXFF_NO_UNDO) == 0)
	   && !(rep_CONSP(tx->tx_UndoList) && rep_NILP(rep_CAR(tx->tx_UndoList))))
	    tx->tx_UndoList = Fcons(Qnil, tx->tx_UndoList);
	tx = tx->tx_Next;
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
    long count = rep_INTP(arg) ? rep_INT(arg) : 1;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(VTX(tx)->tx_ToUndoList == rep_NULL)
    {
	/* First call. */
	VTX(tx)->tx_ToUndoList = VTX(tx)->tx_UndoList;
	VTX(tx)->tx_UndoList = Qnil;
	if(rep_CONSP(VTX(tx)->tx_ToUndoList) && rep_NILP(rep_CAR(VTX(tx)->tx_ToUndoList)))
	    /* Ignore the initial group separator */
	    count++;
    }
    if(rep_NILP(VTX(tx)->tx_ToUndoList))
    {
	return(Fsignal(Qerror, rep_LIST_1(rep_VAL(&nothing_to_undo))));
    }
    in_undo = TRUE;
    last_undid_tx = VTX(tx);
    while(rep_CONSP(VTX(tx)->tx_ToUndoList))
    {
	repv item = rep_CAR(VTX(tx)->tx_ToUndoList);
	VTX(tx)->tx_ToUndoList = rep_CDR(VTX(tx)->tx_ToUndoList);
	VTX(tx)->tx_UndoneList = Fcons(item, VTX(tx)->tx_UndoneList);
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
		rep_STR(tmp)[0] = (u_char)rep_INT(rep_CDR(item));
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
	    VTX(tx)->tx_ProperSaveChanges = VTX(tx)->tx_Changes;
	    VTX(tx)->tx_LastSaveChanges = VTX(tx)->tx_Changes;
	}
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    break;
    }
    this_command = Qundo;
    return(Qt);
}

DEFUN("max-undo-size", var_max_undo_size, Smax_undo_size, (repv val), rep_Var) /*
::doc:max-undo-size::
The maximum amount of storage that a single buffer may devote to recording
undo information.
::end:: */
{
    if(val)
    {
	if(rep_INTP(val))
	    max_undo_size = rep_INT(val);
	return rep_NULL;
    }
    else
	return(rep_MAKE_INT(max_undo_size));
}

DEFUN("buffer-record-undo", var_buffer_record_undo, Sbuffer_record_undo, (repv val), rep_Var) /*
::doc:buffer-record-undo::
When nil no undo information is kept in this buffer.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    if(val)
    {
	if(rep_NILP(val))
	    tx->tx_Flags |= TXFF_NO_UNDO;
	else
	    tx->tx_Flags &= ~TXFF_NO_UNDO;
	return rep_NULL;
    }
    else
	return((tx->tx_Flags & TXFF_NO_UNDO) ? Qnil : Qt);
}

repv var_buffer_undo_list(repv val);
DEFUN("buffer-undo-list", var_buffer_undo_list, Sbuffer_undo_list, (repv val), rep_Var) /*
::doc:buffer-undo-list::
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
    TX *tx;

    if (in_undo)
	return;

    tx = buffer_chain;
    while(tx)
    {
	repv *undo_list;
	long size_count = 0;
	if(tx->tx_ToUndoList && !rep_NILP(tx->tx_ToUndoList))
	    undo_list = &tx->tx_ToUndoList;
	else if(!rep_NILP(tx->tx_UndoneList))
	    undo_list = &tx->tx_UndoneList;
	else
	    undo_list = &tx->tx_UndoList;
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
		if(size_count > max_undo_size)
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
	tx = tx->tx_Next;
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
    rep_ADD_SUBR(Sbuffer_record_undo);
    rep_ADD_SUBR(Sbuffer_undo_list);
}
