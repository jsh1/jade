/* keys.c -- Key binding and evaluating (this should be called events.c)
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

/* Function to make a hash key from an event (c == code, m == modifiers)  */
#define KEYTAB_HASH_FUN(c,m) (((c) * 33) + (((m) & EV_MOD_MASK)))

#define KEYTAB_SIZE 127

_PR VALUE usekey(void *, u_long, u_long, bool);
_PR bool print_event_prefix(void);
_PR void keys_init(void);

/* current_event holds the event we're processing (or 0s), last_event
   contains the previously processed event.  */
_PR u_long current_event[2], last_event[2];
u_long current_event[2], last_event[2];

/* Pointer to the window system's representation of the current_event,
   used for cooking events into strings.  */
static void *current_os_event;

/* print_prefix means echo all events upto the end of the key-sequence.
   printed_this_prefix says the last event has been echoed. */
static bool print_prefix, printed_this_prefix;

/* Buffer holding the events making this key-sequence. */
#define EVENT_BUFSIZ 20
static u_long event_buf[EVENT_BUFSIZ]; /* one event = (code,mods) */
static int event_index;

static DEFSYM(keymap_path, "keymap-path");
static DEFSYM(unbound_key_hook, "unbound-key-hook");
static DEFSYM(esc_means_meta, "esc-means-meta");
static DEFSYM(keymap, "keymap");

static VALUE next_keymap_path;

/* TRUE when the Meta qualifier should be added to the next event. */
static bool pending_meta;

/* Modifier mask for the meta modifier. */
_PR u_long ev_mod_meta;
u_long ev_mod_meta;

/* This doesn't belong here but I couldn't find anywhere else :-( */
_PR VALUE sym_idle_hook;
DEFSYM(idle_hook, "idle-hook");

/* Some doc strings
::doc:keymap_path::
A list of keymaps (ie, keylists and/or keytables). When an event occurs
each keymap in the list is searched for an event binding which matches
it. These bindings are installed in a keymap by the function `bind-keys'.
See also `next-keymap-path'.
::end::
::doc:unbound_key_hook::
When no event binding can be found for an event this hook is evaluated in
the standard manner (see the function `eval-hook' for details).
::end::
::doc:esc_means_meta::
When this variable is non-nil the `ESC' key means that the next event
is qualified by the `Meta' modifier.
This feature is included mainly for compatibility with GNU Emacs.
::end::
::doc:idle_hook::
This hook gets evaluated every second while the editor is idle. Don't depend
on how regularly this gets called, any events from the window-system will
delay it. Also, auto-saving files and garbage-collection take precedence
when there's idle time available. Use this hook sparingly, or for short
periods only!
::end::
*/


/* Search the keymap KM for a binding of CODE&MODS.  */
static VALUE
findkey(VALUE km, u_long code, u_long mods)
{
    switch(VTYPE(km))
    {
    case V_Vector:
	if(VVECT(km)->size != KEYTAB_SIZE)
	    return LISP_NULL;
	km = VVECTI(km, KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE);
	break;
    case V_Cons:
	km = VCDR(km);
	break;
    }
    while(CONSP(km))
    {
	VALUE this = VCAR(km);
	if((VINT(VVECTI(this, KEY_MODS)) == mods)
	   && (VINT(VVECTI(this, KEY_CODE)) == code))
	    return(this);
	km = VCDR(km);
    }
    return LISP_NULL;
}

/* Search for a binding of CODE&MODS.  */
static VALUE
lookup_binding(u_long code, u_long mods)
{
    VALUE kp;
    if(!NILP(next_keymap_path))
    {
	kp = next_keymap_path;
	next_keymap_path = sym_nil;
    }
    else
    {
	kp = cmd_symbol_value(sym_keymap_path, sym_t);
	if(VOIDP(kp))
	    return LISP_NULL;
    }
    while(CONSP(kp))
    {
	VALUE thispath = VCAR(kp);
	VALUE k;
	if(SYMBOLP(thispath))
	    thispath = cmd_symbol_value(thispath, sym_t);
	if(!VOIDP(thispath))
	{
	    if(!NILP(cmd_keymapp(thispath)))
	    {
		k = findkey(thispath, code, mods);
		if(k && VECTORP(k))
		    return(VVECTI(k, KEY_COMMAND));
	    }
	}
	kp = VCDR(kp);
    }
    return LISP_NULL;
}

/* Process the event CODE+MODS, CURS-STATE is TRUE if the cursor is drawn.
   OS-INPUT-MSG is the raw input event from the window-system, this is
   only used to cook a string from.  */
VALUE
usekey(void *OSInputMsg, u_long code, u_long mods, bool cursState)
{
    VALUE result = sym_nil;
    event_buf[event_index++] = code;
    event_buf[event_index++] = mods;
    if(event_index == EVENT_BUFSIZ)
	event_index = 0;
    printed_this_prefix = FALSE;
    if(!NILP(VSYM(sym_esc_means_meta)->value)
       && !pending_meta
       && (code == esc_code) && (mods == esc_mods))
    {
	/* Treat this ESC as a Meta-prefix. */
	pending_meta = TRUE;
    }
    else
    {
	VALUE cmd;
	VW *vw = curr_vw;
	bool inmulti = !(NILP(next_keymap_path)
			 || (next_keymap_path == sym_t));
	if(pending_meta)
	{
	    mods |= ev_mod_meta;
	    pending_meta = FALSE;
	}
	current_event[0] = code;
	current_event[1] = mods;
	current_os_event = OSInputMsg;
#if 0
	reset_message(curr_win);
#endif
	cmd = lookup_binding(code, mods);
	if(cmd)
	{
	    if(cursState)
	    {
		cursor(vw, CURS_OFF);
		cursState = FALSE;
	    }
	    result = cmd_call_command(cmd, sym_nil);
	}
	else if(inmulti)
	    beep(vw);
	else
	{
	    if(cursState)
	    {
		cursor(vw, CURS_OFF);
		cursState = FALSE;
	    }
	    result = cmd_call_hook(sym_unbound_key_hook, sym_nil, sym_nil);
	    if(result && !NILP(result))
		;
	    else if(result && (mods & EV_TYPE_KEYBD) && OSInputMsg)
	    {
		u_char buff[256];
		int len;
		if((len = cook_key(OSInputMsg, buff, 256 - 1)) >= 0)
		{
		    buff[len] = 0;
		    if(len > 0)
		    {
			if(!read_only(vw->vw_Tx))
			{
			    VALUE old_undo_head = LISP_NULL;
			    cmd_call_hook(sym_pre_command_hook,
					  sym_nil, sym_nil);
			    if(last_command == sym_t
			       && CONSP(vw->vw_Tx->tx_UndoList)
			       && NILP(VCAR(vw->vw_Tx->tx_UndoList)))
			    {
				/* Last command was also an insertion,
				   fix it so that the undo information
				   is merged. */
				old_undo_head = vw->vw_Tx->tx_UndoList;
				vw->vw_Tx->tx_UndoList
				    = VCDR(vw->vw_Tx->tx_UndoList);
			    }
			    if(pad_cursor(vw))
				insert_string(vw->vw_Tx, buff,
					      len, vw->vw_CursorPos);
			    if(old_undo_head != LISP_NULL)
			    {
				VCDR(old_undo_head) = vw->vw_Tx->tx_UndoList;
				vw->vw_Tx->tx_UndoList = old_undo_head;
			    }
			    cmd_call_hook(sym_post_command_hook,
					  sym_nil, sym_nil);
			    last_command = sym_t;
			    result = sym_t;
			}
			else
			    result = LISP_NULL;
		    }
		}
		else
		    message("error: key translation screwup");
	    }
	}
	last_event[0] = current_event[0];
	last_event[1] = current_event[1];
	current_event[0] = current_event[1] = 0;
	current_os_event = NULL;
    }
    if(curr_vw)
    {
	if(print_prefix)
	{
	    print_event_prefix();
	    if(NILP(next_keymap_path)
	       && !pending_meta)
	    {
		print_prefix = FALSE;
	    }
	}
	curr_vw->vw_Flags |= VWFF_REFRESH_STATUS;
	refresh_world();
	if(!cursState)
	    cursor(curr_vw, CURS_ON);
    }
    if(NILP(next_keymap_path) && !pending_meta)
	event_index = 0;
    return(result);
}

_PR VALUE cmd_make_keytab(void);
DEFUN("make-keytab", cmd_make_keytab, subr_make_keytab, (void), V_Subr0, DOC_make_keytab) /*
::doc:make_keytab::
make-keytab

Return a new key-table suitable for storing bindings in. This is a 127
element vector, each element is an empty list of bindings.
::end:: */
{
    return(cmd_make_vector(MAKE_INT(KEYTAB_SIZE), sym_nil));
}

_PR VALUE cmd_make_keylist(void);
DEFUN("make-keylist", cmd_make_keylist, subr_make_keylist, (void), V_Subr0, DOC_make_keylist) /*
::doc:make_keylist::
make-keylist

Return a new key-list suitable for storing bindings in. This is a cons
cell looking like `(keymap . LIST-OF-BINDINGS)', LIST-OF-BINDINGS starts
off empty.
::end:: */
{
    return(cmd_cons(sym_keymap, sym_nil));
}

_PR VALUE cmd_bind_keys(VALUE args);
DEFUN("bind-keys", cmd_bind_keys, subr_bind_keys, (VALUE args), V_SubrN, DOC_bind_keys) /*
::doc:bind_keys::
bind-keys KEY-MAP { EVENT-DESCRIPTION COMMAND }...
::end:: */
{
    bool rc = TRUE;
    VALUE km, arg1, res = LISP_NULL;
    if(!CONSP(args))
	return LISP_NULL;
    km = VCAR(args);
    args = VCDR(args);
    while(rc && CONSP(args) && CONSP(VCDR(args)))
    {
	u_long code, mods;
	VALUE key;
	arg1 = VCAR(args);
	args = VCDR(args);
	if(STRINGP(arg1))
	{
	    if(!lookup_event(&code, &mods, VSTR(arg1)))
		goto end;
	}
	else if(!NILP(cmd_eventp(arg1)))
	{
	    code = VINT(VCAR(arg1));
	    mods = VINT(VCDR(arg1));
	}
	else
	{
	    cmd_signal(sym_bad_event_desc, LIST_1(arg1));
	    goto end;
	}
	rc = FALSE;
	key = make_vector(3);
	if(key)
	{
	    VVECTI(key, KEY_CODE) = MAKE_INT(code);
	    VVECTI(key, KEY_MODS) = MAKE_INT(mods);
	    VVECTI(key, KEY_COMMAND) = VCAR(args);
	    if(VECTORP(km))
	    {
		u_long hash = KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE;
		VALUE old = VVECTI(km, hash);
		VVECTI(km, hash) = cmd_cons(key, old);
	    }
	    else
		VCDR(km) = cmd_cons(key, VCDR(km));
	    args = VCDR(args);
	    rc = TRUE;
	}
	else
	    goto end;
    }
    if(rc)
	res = sym_t;
end:
    return(res);
}

_PR VALUE cmd_unbind_keys(VALUE args);
DEFUN("unbind-keys", cmd_unbind_keys, subr_unbind_keys, (VALUE args), V_SubrN, DOC_unbind_keys) /*
::doc:unbind_keys::
unbind-keys KEY-MAP EVENT-DESCRIPTION...
::end:: */
{
    bool rc = TRUE;
    VALUE km, arg1, res = LISP_NULL;
    if(!CONSP(args))
	return LISP_NULL;
    km = VCAR(args);
    if(!((VECTORP(km) && VVECT(km)->size == KEYTAB_SIZE)
       || CONSP(km)))
	return(signal_arg_error(km, 1));
    args = VCDR(args);
    while(rc && CONSP(args))
    {
	u_long code, mods;
	VALUE *keyp;
	arg1 = VCAR(args);
	if(STRINGP(arg1))
	{
	    if(!lookup_event(&code, &mods, VSTR(arg1)))
		goto end;
	}
	else if(!NILP(cmd_eventp(arg1)))
	{
	    code = VINT(VCAR(arg1));
	    mods = VINT(VCDR(arg1));
	}
	else
	{
	    cmd_signal(sym_bad_event_desc, LIST_1(arg1));
	    goto end;
	}
	rc = FALSE;
	if(VECTORP(km))
	    keyp = &VVECTI(km, KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE);
	else
	    keyp = &VCDR(km);
	while(CONSP(*keyp))
	{
	    /* This code is borrowed from cmd_delq */
	    if((VINT(VVECTI(VCAR(*keyp), KEY_MODS)) == mods)
	       && (VINT(VVECTI(VCAR(*keyp), KEY_CODE)) == code))
	    {
		*keyp = VCDR(*keyp);
		/* Keybindings are supposed to nest so only delete the
		   first entry for this event  */
		break;
	    }
	    else
		keyp = &VCDR(*keyp);
	    TEST_INT;
	    if(INT_P)
		return LISP_NULL;
	}
	rc = TRUE;
	args = VCDR(args);
    }
    if(rc)
	res = sym_t;
end:
    return(res);
}

_PR VALUE var_next_keymap_path(VALUE val);
DEFUN("next-keymap-path", var_next_keymap_path, subr_next_keymap_path, (VALUE val), V_Var, DOC_next_keymap_path) /*
::doc:next_keymap_path::
The value of `keymap-path' to be used for the *next* keypress. This is
usually used to chain together multi-key bindings.
::end:: */
{
    if(val)
	next_keymap_path = val;
    /* This isn't a true command */
    this_command = sym_nil;
    /* Pass the prefix-arg along */
    var_prefix_arg(var_current_prefix_arg(LISP_NULL));
    return(next_keymap_path);
}

_PR VALUE cmd_current_event_string(void);
DEFUN("current-event-string", cmd_current_event_string, subr_current_event_string, (void), V_Subr0, DOC_current_event_string) /*
::doc:current_event_string::
current-event-string

Returns the string which would have been inserted by the current event if
a Lisp function hadn't been called instead.
::end:: */
{
    u_char buff[256];
    int len;
    if(!current_os_event)
    {
	static DEFSTRING(str, "Not in event handler");
	return(cmd_signal(sym_error, LIST_1(VAL(str))));
    }
    len = cook_key(current_os_event, buff, 256 - 1);
    if(len > 0)
	return(string_dupn(buff, len));
    return(VAL(null_string));
}

_PR VALUE cmd_current_event(void);
DEFUN("current-event", cmd_current_event, subr_current_event, (void), V_Subr0, DOC_current_event) /*
::doc:current_event::
current-event

Return the event which caused the current command to be invoked.
::end:: */
{
    if(current_event[1])
	return(cmd_cons(MAKE_INT(current_event[0]),
			MAKE_INT(current_event[1])));
    else
	return(sym_nil);
}

_PR VALUE cmd_last_event(void);
DEFUN("last-event", cmd_last_event, subr_last_event, (void), V_Subr0, DOC_last_event) /*
::doc:last_event::
last-event

Return the previous event which occurred.
::end:: */
{
    if(last_event[1])
	return(cmd_cons(MAKE_INT(last_event[0]),
			MAKE_INT(last_event[1])));
    else
	return(sym_nil);
}

_PR VALUE cmd_event_name(VALUE ev);
DEFUN("event-name", cmd_event_name, subr_event_name, (VALUE ev), V_Subr1, DOC_event_name) /*
::doc:event_name::
event-name EVENT

Returns a string naming the event EVENT.
::end:: */
{
    u_char buf[256];
    if(NILP(cmd_eventp(ev)))
	return(signal_arg_error(ev, 1));
    if(lookup_event_name(buf, VINT(VCAR(ev)), VINT(VCDR(ev))))
	return(string_dup(buf));
    return(sym_nil);
}

_PR VALUE cmd_lookup_event(VALUE name);
DEFUN("lookup-event", cmd_lookup_event, subr_lookup_event, (VALUE name), V_Subr1, DOC_lookup_event) /*
::doc:lookup_event::
lookup-event EVENT-NAME

Return the event whose name is EVENT-NAME.
::end:: */
{
    u_long code, mods;
    DECLARE1(name, STRINGP);
    if(lookup_event(&code, &mods, VSTR(name)))
	return(cmd_cons(MAKE_INT(code), MAKE_INT(mods)));
    else
	return(sym_nil);
}

_PR VALUE cmd_lookup_event_binding(VALUE ev, VALUE reset);
DEFUN("lookup-event-binding", cmd_lookup_event_binding, subr_lookup_event_binding, (VALUE ev, VALUE reset), V_Subr2, DOC_lookup_event_binding) /*
::doc:lookup_event_binding::
lookup-event-binding EVENT [RESET-PATH]

Return the command currently associated with the event EVENT.
If RESET-PATH is non-nil the value of `next-keymap-path' will be cleared
after being used.
::end:: */
{
    VALUE old_next_km_path = next_keymap_path;
    VALUE res;
    if(NILP(cmd_eventp(ev)))
	return(signal_arg_error(ev, 1));
    res = lookup_binding(VINT(VCAR(ev)), VINT(VCDR(ev)));
    if(NILP(reset))
    {
	/* We don't want next-keymap-path to be reset */
	next_keymap_path = old_next_km_path;
    }
    return(res ? res : sym_nil);
}

_PR VALUE cmd_keymapp(VALUE arg);
DEFUN("keymapp", cmd_keymapp, subr_keymapp, (VALUE arg), V_Subr1, DOC_keymapp) /*
::doc:keymapp::
keymapp ARG

Returns t if ARG can be used as a keymap.
::end:: */
{
    if((VECTORP(arg) && VVECT(arg)->size == KEYTAB_SIZE)
       || (CONSP(arg) && VCAR(arg) == sym_keymap))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_eventp(VALUE arg);
DEFUN("eventp", cmd_eventp, subr_eventp, (VALUE arg), V_Subr1, DOC_eventp) /*
::doc:eventp::
eventp ARG

Returns t if the ARG is an input event.
::end:: */
{
    if(CONSP(arg) && INTP(VCAR(arg)) && INTP(VCDR(arg)))
	return(sym_t);
    else
	return(sym_nil);
}

/* If necessary, print the name of the current event prefix and return
   TRUE, else return FALSE.  */
bool
print_event_prefix(void)
{
    int i;
    u_char buf[256];
    u_char *bufp = buf;
    if((NILP(next_keymap_path) && !pending_meta)
       && (!print_prefix || printed_this_prefix))
    {
	print_prefix = FALSE;
	return(FALSE);
    }
    if(!print_prefix)
	print_prefix = TRUE;
    for(i = 0; i < event_index; i += 2)
    {
	if(lookup_event_name(bufp, event_buf[i], event_buf[i+1]))
	{
	    bufp += strlen(bufp);
	    *bufp++ = ' ';
	}
    }
    if(!NILP(next_keymap_path) || pending_meta)
    {
	if(bufp > buf)
	    bufp--;			/* erase the last space */
	*bufp++ = '.';
	*bufp++ = '.';
	*bufp++ = '.';
    }
    messagen(buf, bufp - buf);
    printed_this_prefix = TRUE;
    return(TRUE);
}

void
keys_init(void)
{
    ev_mod_meta = sys_find_meta();

    INTERN(keymap_path); DOC(keymap_path);
    INTERN(unbound_key_hook); DOC(unbound_key_hook);
    INTERN(esc_means_meta); DOC(esc_means_meta);
    VSYM(sym_esc_means_meta)->value = sym_t;
    INTERN(idle_hook); DOC(idle_hook);
    INTERN(keymap);
    next_keymap_path = sym_nil;
    mark_static(&next_keymap_path);

    ADD_SUBR(subr_make_keytab);
    ADD_SUBR(subr_make_keylist);
    ADD_SUBR(subr_bind_keys);
    ADD_SUBR(subr_unbind_keys);
    ADD_SUBR(subr_next_keymap_path);
    ADD_SUBR(subr_current_event_string);
    ADD_SUBR(subr_current_event);
    ADD_SUBR(subr_last_event);
    ADD_SUBR(subr_event_name);
    ADD_SUBR(subr_lookup_event);
    ADD_SUBR(subr_lookup_event_binding);
    ADD_SUBR(subr_keymapp);
    ADD_SUBR(subr_eventp);
}
