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
#include <lib/jade_protos.h>

#include <string.h>
#include <stdlib.h>

/* Function to make a hash key from an event (c == code, m == modifiers)  */
#define KEYTAB_HASH_FUN(c,m) (((c) * 33) + (((m) & EV_MOD_MASK)))

#define KEYTAB_SIZE 127

_PR VALUE eval_input_event(void *, u_long, u_long);
_PR bool lookup_event(u_long *, u_long *, u_char *);
_PR bool lookup_event_name(u_char *, u_long, u_long);
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

_PR VALUE sym_global_keymap, sym_local_keymap, sym_unbound_key_hook;
_PR VALUE sym_esc_means_meta, sym_keymap, sym_overriding_local_keymap;
_PR VALUE sym_minor_mode_keymap_alist, sym_autoload_keymap;
DEFSYM(global_keymap, "global-keymap");
DEFSYM(local_keymap, "local-keymap");
DEFSYM(overriding_local_keymap, "overriding-local-keymap");
DEFSYM(unbound_key_hook, "unbound-key-hook");
DEFSYM(esc_means_meta, "esc-means-meta");
DEFSYM(keymap, "keymap");
DEFSYM(minor_mode_keymap_alist, "minor-mode-keymap-alist");
DEFSYM(autoload_keymap, "autoload-keymap");

static VALUE next_keymap_path;

/* TRUE when the Meta qualifier should be added to the next event. */
static bool pending_meta;

/* This doesn't belong here but I couldn't find anywhere else :-( */
_PR VALUE sym_idle_hook;
DEFSYM(idle_hook, "idle-hook");

/* Some doc strings
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


/* Keymap searching */

/* Search the keymap KM for a binding of CODE&MODS.
   If CALLBACK is non-nil it's a function to call for the binding found.
   If this function returns true, then this binding is acceptable and
   is returned from the function. */
static VALUE
search_keymap(VALUE km, u_long code, u_long mods, bool (*callback)(VALUE key))
{
    /* If it's a symbol, dereference it. */
    while(SYMBOLP(km) && !NILP(km) && !INT_P)
    {
	VALUE tem = cmd_symbol_value(km, sym_t);
	if(tem == km)
	    break;
	km = tem;
	TEST_INT;
    }

    /* Find the list of bindings to scan. */
    if(VECTORP(km))
    {
	if(VVECT_LEN(km) != KEYTAB_SIZE)
	    return LISP_NULL;
	km = VVECTI(km, KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE);
    }
    else if CONSP(km)
	km = VCDR(km);
    else
	return LISP_NULL;

    /* Scan them for a match.. */
    while(CONSP(km))
    {
	TEST_INT; if(INT_P) break;
	if(CONSP(VCAR(km)))
	{
	    VALUE ev = KEY_EVENT(VCAR(km));
	    if((VINT(EVENT_MODS(ev)) == mods) && VINT(EVENT_CODE(ev)) == code)
	    {
		VALUE key = VCAR(km);
		if(callback == 0 || callback(key))
		    return key;
	    }
	    km = VCDR(km);
	}
	else
	    /* An inherited sub-keymap. Start scanning it */
	    km = VCDR(km);
    }
    return LISP_NULL;
}

/* Search for a binding of CODE&MODS.  */
static VALUE
lookup_binding(u_long code, u_long mods, bool (*callback)(VALUE key))
{
    VALUE k = LISP_NULL, nkp = next_keymap_path;
    next_keymap_path = LISP_NULL;
    if(nkp == LISP_NULL || nkp == sym_global_keymap)
    {
	VALUE tem;

	tem = cmd_symbol_value(sym_overriding_local_keymap, sym_t);
	if(tem && !VOIDP(tem) && !NILP(tem))
	{
	    /* overriding-local-keymap replaces all but global-keymap
	       if non-nil. */
	    k = search_keymap(tem, code, mods, callback);
	}
	else
	{
	    /* First search all current extents.. */
	    tem = cmd_get_extent(sym_nil, sym_nil);
	    while(!k && EXTENTP(tem))
	    {
		k = search_keymap(cmd_extent_get(tem, sym_keymap),
				  code, mods, callback);
		tem = cmd_extent_parent(tem);
	    }

	    /* ..then the minor modes.. */
	    tem = cmd_symbol_value(sym_minor_mode_keymap_alist, sym_t);
	    while(!k && CONSP(tem) && CONSP(VCAR(tem)) && !INT_P)
	    {
		VALUE cell = VCAR(tem);
		if(SYMBOLP(VCAR(cell)))
		{
		    VALUE cond;
		    GC_root gc_tem;
		    PUSHGC(gc_tem, tem);
		    cond = cmd_symbol_value(VCAR(cell), sym_t);
		    if(!NILP(cond) && !VOIDP(cond))
			k = search_keymap(VCDR(cell), code, mods, callback);
		    POPGC;
		}
		tem = VCDR(tem);
		TEST_INT;
	    }
	    /* ..then the local map */
	    if(!k)
		k = search_keymap(sym_local_keymap, code, mods, callback);
	}

	/* Finally, scan the global map */
	if(!k)
	    k = search_keymap(sym_global_keymap, code, mods, callback);
    }
    else
    {
	GC_root gc_nkp;
	PUSHGC(gc_nkp, nkp);
	while(!k && CONSP(nkp))
	{
	    k = search_keymap(VCAR(nkp), code, mods, callback);
	    nkp = VCDR(nkp);
	}
	POPGC;
    }
    return (k != LISP_NULL && KEYP(k)) ? KEY_COMMAND(k) : LISP_NULL;
}

static bool
eval_input_callback(VALUE key)
{
    VALUE cmd = KEY_COMMAND(key);
    if(SYMBOLP(cmd))
    {
	VALUE fun = VSYM(cmd)->function;
	if(CONSP(fun) && VCAR(fun) == sym_autoload_keymap)
	{
	    GC_root gc_key;
	    PUSHGC(gc_key, key);
	    fun = load_autoload(cmd, fun, FALSE);
	    POPGC;
	    if(fun == LISP_NULL)
		return FALSE;
	}
	if(fun == sym_keymap)
	{
	    /* A prefix key, add its list to the next-keymap-path. */
	    next_keymap_path = cmd_cons(cmd, next_keymap_path
					? next_keymap_path : sym_nil);
	    /* Look for more prefix keys */
	    return FALSE;
	}
    }
    next_keymap_path = LISP_NULL;
    return TRUE;
}

/* Process the event CODE+MODS. OS-INPUT-MSG is the raw input event
   from the window-system, this is only used to cook a string from.  */
VALUE
eval_input_event(void *OSInputMsg, u_long code, u_long mods)
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
	VW *vw = curr_vw;
	VALUE cmd, orig_next_keymap_path = next_keymap_path;
	if(pending_meta)
	{
	    mods |= EV_MOD_META;
	    pending_meta = FALSE;
	}
	current_event[0] = code;
	current_event[1] = mods;
	current_os_event = OSInputMsg;
	cmd = lookup_binding(code, mods, eval_input_callback);
	if(cmd != LISP_NULL)
	{
	    /* Found a binding for this event; evaluate it. */
	    result = cmd_call_command(cmd, sym_nil);
	}
	else if(next_keymap_path != LISP_NULL)
	{
	    /* We already handled some prefixes. */
	    this_command = sym_keymap;
	    result = sym_nil;
	}
	else if(orig_next_keymap_path != LISP_NULL
		&& !NILP(orig_next_keymap_path)
		&& orig_next_keymap_path != sym_global_keymap)
	{
	    /* A multi-key binding, but no final step; clear the prefix
	       argument for the next command and beep. */
	    var_prefix_arg(sym_nil);
	    beep(vw);
	}
	else
	{
	    /* An unbound key with no prefix keys. */
	    result = cmd_call_hook(sym_unbound_key_hook, sym_nil, sym_or);
	    if(result != LISP_NULL && NILP(result)
	       && (mods & EV_TYPE_KEYBD) && OSInputMsg)
	    {
		/* Try to self-insert */
		u_char buff[256];
		int len;
		if((len = cook_key(OSInputMsg, buff, 256 - 1)) >= 0)
		{
		    buff[len] = 0;
		    if(len > 0)
		    {
			if(!read_only_pos(vw->vw_Tx, vw->vw_CursorPos))
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
			    {
				VALUE arg = cmd_prefix_numeric_argument(var_prefix_arg(LISP_NULL));
				if(!INTP(arg) || VINT(arg) < 1)
				    beep(vw);
				else if(VINT(arg) == 1)
				    insert_string(vw->vw_Tx, buff,
						  len, vw->vw_CursorPos);
				else if(len == 1
					&& VINT(arg) < sizeof(buff) - 1)
				{
				    /* Inserting a single char, more than
				       once, build a string of them. */
				    memset(buff, buff[0], VINT(arg));
				    insert_string(vw->vw_Tx, buff,
						  VINT(arg), vw->vw_CursorPos);
				}
				else
				{
				    /* Do a looping insertion */
				    int i = VINT(arg);
				    while(i-- > 0)
					insert_string(vw->vw_Tx, buff,
						      len, vw->vw_CursorPos);
				}
			    }
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
		/* Remove prefix arg. */
		var_prefix_arg(sym_nil);
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
	    if(next_keymap_path == LISP_NULL && !pending_meta)
		print_prefix = FALSE;
	}
    }
    if(next_keymap_path == LISP_NULL && !pending_meta)
	event_index = 0;
    return(result);
}


/* Translate text->event and vice versa */

struct key_def {
    const char *name;
    u_long mods, code;
};

static struct key_def default_mods[] = {
    { "S",	  EV_MOD_SHIFT },
    { "Shift",    EV_MOD_SHIFT },
    { "SFT",      EV_MOD_SHIFT },
    { "C",        EV_MOD_CTRL },
    { "Ctrl",     EV_MOD_CTRL },
    { "Control",  EV_MOD_CTRL },
    { "CTL",      EV_MOD_CTRL },
    { "M",        EV_MOD_META },
    { "Meta",     EV_MOD_META },
    { "Mod1",     EV_MOD_MOD1 },
    { "Mod2",     EV_MOD_MOD2 },
    { "Mod3",     EV_MOD_MOD3 },
    { "Mod4",     EV_MOD_MOD4 },
    { "Button1",  EV_MOD_BUTTON1 },
    { "Button2",  EV_MOD_BUTTON2 },
    { "Button3",  EV_MOD_BUTTON3 },
    { "Button4",  EV_MOD_BUTTON4 },
    { "Button5",  EV_MOD_BUTTON5 },
    { 0, 0 }
};

static struct key_def default_codes[] = {
    { "Click1",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Off",      EV_TYPE_MOUSE, EV_CODE_MOUSE_UP },
    { "Move",     EV_TYPE_MOUSE, EV_CODE_MOUSE_MOVE },
    { 0, 0 }
};

/* Puts the integers defining the event described in DESC into CODE
   and MODS. */
bool
lookup_event(u_long *code, u_long *mods, u_char *desc)
{
    char *tem;
    char buf[100];
    *code = *mods = 0;

    /* First handle all modifiers */
    while(*desc && (tem = strchr(desc + 1, '-')) != 0)
    {
	struct key_def *x = default_mods;

	memcpy(buf, desc, tem - (char *)desc);
	buf[tem - (char *)desc] = 0;

	while(x->name != 0)
	{
	    if(strcasecmp(buf, x->name) == 0)
	    {
		*mods |= x->mods;
		break;
	    }
	    x++;
	}
	if(x->name == 0 && !sys_lookup_mod(buf, mods))
	    goto error;

	desc = tem + 1;
    }

    /* Then go for the code itself */
    {
	struct key_def *x = default_codes;
	while(x->name != 0)
	{
	    if(strcasecmp(desc, x->name) == 0)
	    {
		*mods |= x->mods;
		*code = x->code;
		return TRUE;
	    }
	    x++;
	}
	if(sys_lookup_code(desc, code, mods))
	    return TRUE;
	else
	    goto error;
    }

error:
    cmd_signal(sym_bad_event_desc, LIST_1(string_dup(desc)));
    return FALSE;
}

/* Constructs the name of the event defined by CODE and MODS in BUF.  */
bool
lookup_event_name(u_char *buf, u_long code, u_long mods)
{
    int i;
    struct key_def *x;
    u_long type = mods & EV_TYPE_MASK;

    char *end = buf;
    *buf = 0;

    if(mods & WINDOW_META(curr_win))
	mods = (mods & ~WINDOW_META(curr_win)) | EV_MOD_META;
    mods &= ~EV_TYPE_MASK;

    for(i = 2; i < 32 && mods != 0; i++)	/* magic numbers!? */
    {
	u_long mask = 1 << i;
	if(mods & mask)
	{
	    mods &= ~mask;
	    x = default_mods;
	    while(x->name != 0)
	    {
		if(x->mods == mask)
		{
		    end = stpcpy(end, x->name);
		    break;
		}
		x++;
	    }
	    if(x->name == 0)
		end = sys_lookup_mod_name(end, mask);
	    end = stpcpy(end, "-");
	}
    }

    x = default_codes;
    while(x->name != 0)
    {
	if(type == x->mods && code == x->code)
	{
	    strcpy(end, x->name);
	    return TRUE;
	}
	x++;
    }
    return sys_lookup_code_name(end, code, type);
}


/* Lisp functions */
_PR VALUE cmd_make_keymap(void);
DEFUN("make-keymap", cmd_make_keymap, subr_make_keymap, (void), V_Subr0,
      DOC_make_keymap) /*
::doc:make_keymap::
make-keymap

Return a new keymap suitable for storing bindings in. This is a 127
element vector, each element is an empty list of bindings for that hash
bucket. Compare with the make-sparse-keymap function.
::end:: */
{
    return cmd_make_vector(MAKE_INT(KEYTAB_SIZE), sym_nil);
}

_PR VALUE cmd_make_sparse_keymap(VALUE base);
DEFUN("make-sparse-keymap", cmd_make_sparse_keymap, subr_make_sparse_keymap,
      (VALUE base), V_Subr1, DOC_make_sparse_keymap) /*
::doc:make_sparse_keymap::
make-sparse-keymap [BASE-KEYMAP]

Return a new keymap suitable for storing bindings in. This is a cons cell
looking like `(keymap . LIST-OF-BINDINGS)', LIST-OF-BINDINGS is initially
nil.

If BASE-KEYMAP is non-nil, it should be an existing sparse keymap whose
bindings are to be inherited by the new keymap.
::end:: */
{
    if(!NILP(base) && (!CONSP(base) || VCAR(base) != sym_keymap))
	return signal_arg_error(base, 1);

    return cmd_cons(sym_keymap, base);
}

_PR VALUE cmd_bind_keys(VALUE args);
DEFUN("bind-keys", cmd_bind_keys, subr_bind_keys, (VALUE args), V_SubrN, DOC_bind_keys) /*
::doc:bind_keys::
bind-keys KEYMAP { EVENT-DESCRIPTION COMMAND }...

Adds key bindings to KEYMAP. Each EVENT-DESCRIPTION is a string naming an
event to bind to the corresponding COMMAND.

Returns KEYMAP when successful.
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
	key = MAKE_KEY(MAKE_EVENT(MAKE_INT(code), MAKE_INT(mods)), VCAR(args));
	if(key != LISP_NULL)
	{
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
	res = km;
end:
    return res;
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
    if(!((VECTORP(km) && VVECT_LEN(km) == KEYTAB_SIZE)
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
	    VALUE cell = VCAR(*keyp);
	    if(CONSP(cell))
	    {
		if((VINT(EVENT_MODS(KEY_EVENT(cell))) == mods)
		   && (VINT(EVENT_CODE(KEY_EVENT(cell))) == code))
		{
		    *keyp = VCDR(*keyp);
		    /* Keybindings are supposed to nest so only delete the
		    first entry for this event  */
		    break;
		}
		else
		    keyp = &VCDR(*keyp);
	    }
	    else
		/* An inherited keymap. Only delete bindings from
		   the initial keymap. */
		break;

	    TEST_INT; if(INT_P) return LISP_NULL;
	}
	rc = TRUE;
	args = VCDR(args);
    }
    if(rc)
	res = sym_t;
end:
    return(res);
}

_PR VALUE cmd_next_keymap_path(VALUE path);
DEFUN("next-keymap-path", cmd_next_keymap_path, subr_next_keymap_path,
      (VALUE path), V_Subr1, DOC_next_keymap_path) /*
::doc:next_keymap_path::
next-keymap-path [PATH]

Install a temporary list of keymaps, PATH, to be used when the next input
event is received. This function sets this-command to nil, and prefix-arg
to current-prefix-arg (i.e. it preserves the context for the next command).

As a special case, if PATH is the symbol t, the keymap to be used
for the next input event is returned, without changing anything.
Alternatively, if PATH is the symbol `global-keymap', use the currently
active root keymaps.
::end:: */
{
    if(path != sym_t)
    {
	next_keymap_path = path;
	/* This isn't a true command */
	this_command = sym_nil;
	/* Pass the prefix-arg along */
	var_prefix_arg(var_current_prefix_arg(LISP_NULL));
    }
    return next_keymap_path ? next_keymap_path : sym_nil;
}

DEFSTRING(not_in_handler, "Not in event handler");
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
	return(cmd_signal(sym_error, LIST_1(VAL(&not_in_handler))));
    len = cook_key(current_os_event, buff, 256 - 1);
    if(len > 0)
	return(string_dupn(buff, len));
    return(null_string());
}

_PR VALUE cmd_current_event(void);
DEFUN("current-event", cmd_current_event, subr_current_event, (void), V_Subr0, DOC_current_event) /*
::doc:current_event::
current-event

Return the event which caused the current command to be invoked.
::end:: */
{
    if(current_event[1])
	return MAKE_EVENT(MAKE_INT(current_event[0]),
			  MAKE_INT(current_event[1]));
    else
	return sym_nil;
}

_PR VALUE cmd_last_event(void);
DEFUN("last-event", cmd_last_event, subr_last_event, (void), V_Subr0, DOC_last_event) /*
::doc:last_event::
last-event

Return the previous event which occurred.
::end:: */
{
    if(last_event[1])
	return MAKE_EVENT(MAKE_INT(last_event[0]),
			  MAKE_INT(last_event[1]));
    else
	return sym_nil;
}

_PR VALUE cmd_event_name(VALUE ev);
DEFUN("event-name", cmd_event_name, subr_event_name, (VALUE ev), V_Subr1, DOC_event_name) /*
::doc:event_name::
event-name EVENT

Returns a string naming the event EVENT.
::end:: */
{
    u_char buf[256];
    if(!EVENTP(ev))
	return signal_arg_error(ev, 1);

    if(lookup_event_name(buf, VINT(EVENT_CODE(ev)), VINT(EVENT_MODS(ev))))
	return string_dup(buf);
    else
	return sym_nil;
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
	return MAKE_EVENT(MAKE_INT(code), MAKE_INT(mods));
    else
	return sym_nil;
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
    if(!EVENTP(ev))
	return(signal_arg_error(ev, 1));

    res = lookup_binding(VINT(EVENT_CODE(ev)), VINT(EVENT_MODS(ev)), 0);
    if(NILP(reset))
    {
	/* We don't want next-keymap-path to be reset */
	next_keymap_path = old_next_km_path;
    }
    return res ? res : sym_nil;
}

_PR VALUE cmd_search_keymap(VALUE ev, VALUE km);
DEFUN("search-keymap", cmd_search_keymap, subr_search_keymap,
      (VALUE ev, VALUE km), V_Subr2, DOC_search_keymap) /*
::doc:search_keymap::
search-keymap EVENT KEYMAP

Return the (COMMAND . EVENT) binding of EVENT in KEYMAP, or nil.
::end:: */
{
    VALUE res;
    DECLARE1(ev, EVENTP);
    res = search_keymap(km, VINT(EVENT_CODE(ev)), VINT(EVENT_MODS(ev)), 0);
    return res ? res : sym_nil;
}

_PR VALUE cmd_keymapp(VALUE arg);
DEFUN("keymapp", cmd_keymapp, subr_keymapp, (VALUE arg), V_Subr1, DOC_keymapp) /*
::doc:keymapp::
keymapp ARG

Returns t if ARG can be used as a keymap.
::end:: */
{
    if((VECTORP(arg) && VVECT_LEN(arg) == KEYTAB_SIZE)
       || (CONSP(arg) && VCAR(arg) == sym_keymap))
	return sym_t;
    else
	return sym_nil;
}

_PR VALUE cmd_eventp(VALUE arg);
DEFUN("eventp", cmd_eventp, subr_eventp, (VALUE arg), V_Subr1, DOC_eventp) /*
::doc:eventp::
eventp ARG

Returns t if the ARG is an input event.
::end:: */
{
    return EVENTP(arg) ? sym_t : sym_nil;
}

/* If necessary, print the name of the current event prefix and return
   TRUE, else return FALSE.  */
bool
print_event_prefix(void)
{
    int i;
    u_char buf[256];
    u_char *bufp = buf;
    if((next_keymap_path == LISP_NULL && !pending_meta)
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
    if(next_keymap_path != LISP_NULL || pending_meta)
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
    INTERN(global_keymap);
    INTERN(local_keymap);
    INTERN(overriding_local_keymap);
    INTERN(unbound_key_hook);
    INTERN(esc_means_meta); DOC(esc_means_meta);
    VSYM(sym_esc_means_meta)->value = sym_t;
    INTERN(idle_hook); DOC(idle_hook);
    INTERN(keymap);
    INTERN(minor_mode_keymap_alist);
    INTERN(autoload_keymap);
    next_keymap_path = LISP_NULL;
    mark_static(&next_keymap_path);

    ADD_SUBR(subr_make_keymap);
    ADD_SUBR(subr_make_sparse_keymap);
    ADD_SUBR(subr_bind_keys);
    ADD_SUBR(subr_unbind_keys);
    ADD_SUBR(subr_next_keymap_path);
    ADD_SUBR(subr_current_event_string);
    ADD_SUBR(subr_current_event);
    ADD_SUBR(subr_last_event);
    ADD_SUBR(subr_event_name);
    ADD_SUBR(subr_lookup_event);
    ADD_SUBR(subr_lookup_event_binding);
    ADD_SUBR(subr_search_keymap);
    ADD_SUBR(subr_keymapp);
    ADD_SUBR(subr_eventp);
}
