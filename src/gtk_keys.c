/* gtk_keys.c -- Event translation for GTK
   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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
#include <gdk/gdkkeysyms.h>
#include <gdk/gdkprivate.h>
#include <string.h>

#ifdef HAVE_X11
/* XXX These X11 headers are required for the ButtonX defines and the
   XXX gtk_find_meta function, and also the IsModifierKey macro.
   XXX Also this code relies on the equivalence of GDK and X11
   XXX keysyms and modifiers. */
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/keysym.h>
#endif

unsigned long esc_code = GDK_Escape, esc_mods = EV_TYPE_KEYBD;

static unsigned long num_lock_mod, scroll_lock_mod, all_lock_mask;

static unsigned long
translate_mods(unsigned long mods, unsigned int state, bool subst_meta)
{
    if(state & GDK_SHIFT_MASK)
	mods |= EV_MOD_SHIFT;
    if(state & GDK_LOCK_MASK)
	mods ^= EV_MOD_SHIFT;
    if(state & GDK_CONTROL_MASK)
	mods |= EV_MOD_CTRL;
    if(state & GDK_MOD1_MASK)
	mods |= EV_MOD_MOD1;
    if(state & GDK_MOD2_MASK)
	mods |= EV_MOD_MOD2;
    if(state & GDK_MOD3_MASK)
	mods |= EV_MOD_MOD3;
    if(state & GDK_MOD4_MASK)
	mods |= EV_MOD_MOD4;
    if(state & GDK_MOD5_MASK)
	mods |= EV_MOD_MOD5;
    if(state & GDK_BUTTON1_MASK)
	mods |= EV_MOD_BUTTON1;
    if(state & GDK_BUTTON2_MASK)
	mods |= EV_MOD_BUTTON2;
    if(state & GDK_BUTTON3_MASK)
	mods |= EV_MOD_BUTTON3;
    if(state & GDK_BUTTON4_MASK)
	mods |= EV_MOD_BUTTON4;
    if(state & GDK_BUTTON5_MASK)
	mods |= EV_MOD_BUTTON5;

    if(subst_meta && (mods & WINDOW_META(curr_win)) != 0)
	mods = (mods & ~WINDOW_META(curr_win)) | EV_MOD_META;

    return(mods);
}

void
translate_event(unsigned long *code, unsigned long *mods, GdkEvent *ev)
{
    switch((int) ev->type)
    {
    case GDK_KEY_PRESS:
	if (IsModifierKey (ev->key.keyval))
	    break;
	*mods = translate_mods(*mods, ev->key.state & ~all_lock_mask, true);
	if(*mods & EV_MOD_SHIFT)
	{
	    /* Some keys don't have keysym at index 1, if not treat it as
	       normal keysym shifted.

	       XXX It seems that GDK gives no method for handling this.
	       XXX There's no way to tell if a keysym originates from a
	       XXX a ``shiftable'' keycode, so revert to Xlib.. */
#ifdef HAVE_X11
	    KeyCode keycode = XKeysymToKeycode (gdk_display, ev->key.keyval);
	    if (keycode != 0)
	    {
		*code = XKeycodeToKeysym(gdk_display, keycode, 1);
		if(*code == NoSymbol)
		    *code = XKeycodeToKeysym(gdk_display, keycode, 0);
		else
		    *mods &= ~EV_MOD_SHIFT;
	    }
#else
	    /* Substandard GDK version */
	    if (gdk_keyval_is_upper (ev->key.keyval))
		*mods &= ~EV_MOD_SHIFT;
	    *code = ev->key.keyval;
#endif
	}
	else
	    *code = ev->key.keyval;
	if(*code != 0)
	    *mods |= EV_TYPE_KEYBD;
	break;

    case GDK_BUTTON_PRESS:
	*code = EV_CODE_MOUSE_CLICK1;
	goto button;

    case GDK_2BUTTON_PRESS:
	*code = EV_CODE_MOUSE_CLICK2;
	goto button;

    case GDK_BUTTON_RELEASE:
	*code = EV_CODE_MOUSE_UP;
    button:
	*mods = (EV_TYPE_MOUSE | translate_mods(*mods, ev->button.state & ~all_lock_mask, true));
	switch(ev->button.button)
	{
	case Button1:
	    *mods |= EV_MOD_BUTTON1;
	    break;
	case Button2:
	    *mods |= EV_MOD_BUTTON2;
	    break;
	case Button3:
	    *mods |= EV_MOD_BUTTON3;
	    break;
	case Button4:
	    *mods |= EV_MOD_BUTTON4;
	    break;
	case Button5:
	    *mods |= EV_MOD_BUTTON5;
	    break;
	}
	break;

    case GDK_MOTION_NOTIFY:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = (EV_TYPE_MOUSE | translate_mods(*mods, ev->motion.state & ~all_lock_mask, true));
	break;
    }
}

size_t
cook_key(void *event, char *buf, size_t buflen)
{
    GdkEvent *ev = event;
    if (ev->type == GDK_KEY_PRESS)
    {
	size_t actual_length = MIN (buflen - 1, ev->key.length);
	memcpy (buf, ev->key.string, actual_length);
	buf[actual_length] = 0;
	return actual_length;
    }
    else
	return 0;
}

/*
 * Stuff to translate textual key descriptions into key codes
 */

struct key_def {
    const char *name;
    unsigned long mods, code;
};

static struct key_def x11_mods[] = {
    { "LMB",      EV_MOD_LMB, 0 },
    { "MMB",      EV_MOD_MMB, 0 },
    { "RMB",      EV_MOD_RMB, 0 },
    { 0 }
};

static struct key_def x11_codes[] = {
    { "SPC",      EV_TYPE_KEYBD, GDK_space },
    { "Space",    EV_TYPE_KEYBD, GDK_space },
    { "Spacebar", EV_TYPE_KEYBD, GDK_space },
    { "TAB",      EV_TYPE_KEYBD, GDK_Tab },
    { "RET",      EV_TYPE_KEYBD, GDK_Return },
    { "Return",   EV_TYPE_KEYBD, GDK_Return },
    { "ESC",      EV_TYPE_KEYBD, GDK_Escape },
    { "Escape",   EV_TYPE_KEYBD, GDK_Escape },
    { "BS",       EV_TYPE_KEYBD, GDK_BackSpace },
    { "Backspace", EV_TYPE_KEYBD, GDK_BackSpace },
    { "DEL",      EV_TYPE_KEYBD, GDK_Delete },
    { "Delete",   EV_TYPE_KEYBD, GDK_Delete },
    { "Help",     EV_TYPE_KEYBD, GDK_Help },
    { "Up",       EV_TYPE_KEYBD, GDK_Up },
    { "Down",     EV_TYPE_KEYBD, GDK_Down },
    { "Right",    EV_TYPE_KEYBD, GDK_Right },
    { "Left",     EV_TYPE_KEYBD, GDK_Left },

    /* X defines lots of long names for these simple keys...  */
    { " ",        EV_TYPE_KEYBD, GDK_space },
    { "!",        EV_TYPE_KEYBD, GDK_exclam },
    { "\"",       EV_TYPE_KEYBD, GDK_quotedbl },
    { "#",        EV_TYPE_KEYBD, GDK_numbersign },
    { "$",        EV_TYPE_KEYBD, GDK_dollar },
    { "%",        EV_TYPE_KEYBD, GDK_percent },
    { "&",        EV_TYPE_KEYBD, GDK_ampersand },
    { "'",        EV_TYPE_KEYBD, GDK_quoteright },
    { "(",        EV_TYPE_KEYBD, GDK_parenleft },
    { ")",        EV_TYPE_KEYBD, GDK_parenright },
    { "*",        EV_TYPE_KEYBD, GDK_asterisk },
    { "+",        EV_TYPE_KEYBD, GDK_plus },
    { ",",        EV_TYPE_KEYBD, GDK_comma },
    { "-",        EV_TYPE_KEYBD, GDK_minus },
    { ".",        EV_TYPE_KEYBD, GDK_period },
    { "/",        EV_TYPE_KEYBD, GDK_slash },
    { ":",        EV_TYPE_KEYBD, GDK_colon },
    { ";",        EV_TYPE_KEYBD, GDK_semicolon },
    { "<",        EV_TYPE_KEYBD, GDK_less },
    { "=",        EV_TYPE_KEYBD, GDK_equal },
    { ">",        EV_TYPE_KEYBD, GDK_greater },
    { "?",        EV_TYPE_KEYBD, GDK_question },
    { "@",        EV_TYPE_KEYBD, GDK_at },
    { "[",        EV_TYPE_KEYBD, GDK_bracketleft },
    { "\\",       EV_TYPE_KEYBD, GDK_backslash },
    { "]",        EV_TYPE_KEYBD, GDK_bracketright },
    { "^",        EV_TYPE_KEYBD, GDK_asciicircum },
    { "_",        EV_TYPE_KEYBD, GDK_underscore },
    { "`",        EV_TYPE_KEYBD, GDK_quoteleft },
    { "{",        EV_TYPE_KEYBD, GDK_braceleft },
    { "|",        EV_TYPE_KEYBD, GDK_bar },
    { "}",        EV_TYPE_KEYBD, GDK_braceright },
    { "~",        EV_TYPE_KEYBD, GDK_asciitilde },

    { 0 }
};

bool
sys_lookup_mod(const char *name, unsigned long *mods)
{
    struct key_def *x = x11_mods;
    while(x->name != 0)
    {
	if(strcasecmp(name, x->name) == 0)
	{
	    *mods |= x->mods;
	    return true;
	}
	x++;
    }
    return false;
}

bool
sys_lookup_code(const char *name, unsigned long *code, unsigned long *mods)
{
    struct key_def *x = x11_codes;
    while(x->name != 0)
    {
	if(strcasecmp(name, x->name) == 0)
	{
	    *mods |= x->mods;
	    *code = x->code;
	    return true;
	}
	x++;
    }

    {
	unsigned int ks = gdk_keyval_from_name (name);
	if(ks != 0)
	{
	    *mods |= EV_TYPE_KEYBD;
	    *code = ks;
	    return true;
	}
    }

    return false;
}

char *
sys_lookup_mod_name(char *buf, unsigned long mod)
{
    struct key_def *x = x11_mods;
    while(x->name != 0)
    {
	if(x->mods & mod)
	    return stpcpy(buf, x->name);
	x++;
    }
    return buf;
}

bool
sys_lookup_code_name(char *buf, unsigned long code, unsigned long type)
{
    struct key_def *x = x11_codes;
    char *tem;
    while(x->name != 0)
    {
	if(x->mods == type && x->code == code)
	{
	    strcpy(buf, x->name);
	    return true;
	}
	x++;
    }

    tem = gdk_keyval_name (code);
    if(tem != 0)
    {
	strcpy(buf, tem);
	return true;
    }

    return false;
}

/* Return the jade modifier mask used as the meta key. This code
   shamelessly stolen from Emacs 19. :-)

   XXX how can I do this in GDK? */
unsigned long
gtk_find_meta(void)
{
#ifdef HAVE_X11
    unsigned long meta_mod = 0, alt_mod = 0;

    int min_code, max_code;
    KeySym *syms;
    int syms_per_code;
    XModifierKeymap *mods;

#if XlibSpecificationRelease >= 4
    XDisplayKeycodes(gdk_display, &min_code, &max_code);
#else
    min_code = gdk_display->min_keycode;
    max_code = gdk_display->max_keycode;
#endif

    syms = XGetKeyboardMapping(gdk_display, min_code, max_code - min_code + 1,
			       &syms_per_code);
    mods = XGetModifierMapping(gdk_display);

    {
	int row, col;

	for(row = 3; row < 8; row++)
	{
	    for(col = 0; col < mods->max_keypermod; col++)
	    {
		KeyCode code = mods->modifiermap[(row * mods->max_keypermod)
						+ col];
		int code_col;
		if(code == 0)
		    continue;
		for(code_col = 0; code_col < syms_per_code; code_col++)
		{
		    int sym = syms[((code - min_code) * syms_per_code)
				  + code_col];
		    switch(sym)
		    {
		    case XK_Meta_L: case XK_Meta_R:
			meta_mod = translate_mods(0, 1 << row, false);
			break;

		    case XK_Alt_L: case XK_Alt_R:
			alt_mod = translate_mods(0, 1 << row, false);
			break;

		    case XK_Num_Lock:
			num_lock_mod = 1 << row;
			break;

		    case XK_Scroll_Lock:
			scroll_lock_mod = 1 << row;
			break;
		    }
		}
	    }
	}
    }

    all_lock_mask = LockMask | num_lock_mod | scroll_lock_mod;

    if(meta_mod == 0)
	meta_mod = alt_mod;

    XFree((char *)syms);
    XFreeModifiermap(mods);

    return meta_mod;
#else /* !HAVE_X11 */
    return GDK_MOD1_MASK;
#endif
}
