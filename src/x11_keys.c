/* x11_keys.c -- Event translation for X11
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
#include <string.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

u_long esc_code = XK_Escape, esc_mods = EV_TYPE_KEYBD;

static u_long
translate_mods(u_long mods, unsigned int state, bool subst_meta)
{
    if(state & ShiftMask)
	mods |= EV_MOD_SHIFT;
    if(state & LockMask)
	mods ^= EV_MOD_SHIFT;
    if(state & ControlMask)
	mods |= EV_MOD_CTRL;
    if(state & Mod1Mask)
	mods |= EV_MOD_MOD1;
    if(state & Mod2Mask)
	mods |= EV_MOD_MOD2;
    if(state & Mod3Mask)
	mods |= EV_MOD_MOD3;
    if(state & Mod4Mask)
	mods |= EV_MOD_MOD4;
    if(state & Mod5Mask)
	mods |= EV_MOD_MOD5;
    if(state & Button1Mask)
	mods |= EV_MOD_BUTTON1;
    if(state & Button2Mask)
	mods |= EV_MOD_BUTTON2;
    if(state & Button3Mask)
	mods |= EV_MOD_BUTTON3;
    if(state & Button4Mask)
	mods |= EV_MOD_BUTTON4;
    if(state & Button5Mask)
	mods |= EV_MOD_BUTTON5;

    if(subst_meta && (mods & WINDOW_META(curr_win)) != 0)
	mods = (mods & ~WINDOW_META(curr_win)) | EV_MOD_META;

    return(mods);
}

void
translate_event(u_long *code, u_long *mods,
		XEvent *xev, struct x11_display *dpy)
{
    switch(xev->type)
    {
    case KeyPress:
	*mods = translate_mods(*mods, xev->xkey.state, TRUE);
	if(*mods & EV_MOD_SHIFT)
	{
	    /* Some keys don't have keysym at index 1, if not treat it as
	       normal keysym shifted.  */
	    *code = XKeycodeToKeysym(xev->xany.display, xev->xkey.keycode, 1);
	    if(*code == NoSymbol)
		*code = XKeycodeToKeysym(xev->xany.display,
					 xev->xkey.keycode, 0);
	    else
		*mods &= ~EV_MOD_SHIFT;
	}
	else
	    *code = XKeycodeToKeysym(xev->xany.display, xev->xkey.keycode, 0);
	if((*code != NoSymbol) && !IsModifierKey(*code))
	    *mods |= EV_TYPE_KEYBD;
	break;

    case ButtonPress:
	if(xev->xbutton.button == dpy->last_click_button
	   && xev->xbutton.time < (dpy->last_click + DOUBLE_CLICK_TIME))
	{
	    *code = EV_CODE_MOUSE_CLICK2;
	}
	else
	    *code = EV_CODE_MOUSE_CLICK1;
	dpy->last_click = xev->xbutton.time;
	dpy->last_click_button = xev->xbutton.button;
	goto button;

    case ButtonRelease:
	*code = EV_CODE_MOUSE_UP;
    button:
	*mods = (EV_TYPE_MOUSE
		 | translate_mods(*mods, xev->xbutton.state, TRUE));
	switch(xev->xbutton.button)
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

    case MotionNotify:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = (EV_TYPE_MOUSE
		 | translate_mods(*mods, xev->xmotion.state, TRUE));
	break;
    }
}

int
cook_key(void *event, u_char *buf, int buflen)
{
    XKeyEvent *xk = event;
    KeySym ks;
    return(XLookupString(xk, buf, buflen, &ks, NULL));
}

/*
 * Stuff to translate textual key descriptions into key codes
 */

struct key_def {
    const char *name;
    u_long mods, code;
};

static struct key_def x11_mods[] = {
    { "LMB",      EV_MOD_LMB, 0 },
    { "MMB",      EV_MOD_MMB, 0 },
    { "RMB",      EV_MOD_RMB, 0 },
    { 0 }
};

static struct key_def x11_codes[] = {
    { "SPC",      EV_TYPE_KEYBD, XK_space },
    { "Space",    EV_TYPE_KEYBD, XK_space },
    { "Spacebar", EV_TYPE_KEYBD, XK_space },
    { "TAB",      EV_TYPE_KEYBD, XK_Tab },
    { "RET",      EV_TYPE_KEYBD, XK_Return },
    { "Return",   EV_TYPE_KEYBD, XK_Return },
    { "ESC",      EV_TYPE_KEYBD, XK_Escape },
    { "Escape",   EV_TYPE_KEYBD, XK_Escape },
    { "BS",       EV_TYPE_KEYBD, XK_BackSpace },
    { "Backspace", EV_TYPE_KEYBD, XK_BackSpace },
    { "DEL",      EV_TYPE_KEYBD, XK_Delete },
    { "Delete",   EV_TYPE_KEYBD, XK_Delete },
    { "Help",     EV_TYPE_KEYBD, XK_Help },
    { "Up",       EV_TYPE_KEYBD, XK_Up },
    { "Down",     EV_TYPE_KEYBD, XK_Down },
    { "Right",    EV_TYPE_KEYBD, XK_Right },
    { "Left",     EV_TYPE_KEYBD, XK_Left },

    /* X defines lots of long names for these simple keys...  */
    { " ",        EV_TYPE_KEYBD, XK_space },
    { "!",        EV_TYPE_KEYBD, XK_exclam },
    { "\"",       EV_TYPE_KEYBD, XK_quotedbl },
    { "#",        EV_TYPE_KEYBD, XK_numbersign },
    { "$",        EV_TYPE_KEYBD, XK_dollar },
    { "%",        EV_TYPE_KEYBD, XK_percent },
    { "&",        EV_TYPE_KEYBD, XK_ampersand },
    { "'",        EV_TYPE_KEYBD, XK_quoteright },
    { "(",        EV_TYPE_KEYBD, XK_parenleft },
    { ")",        EV_TYPE_KEYBD, XK_parenright },
    { "*",        EV_TYPE_KEYBD, XK_asterisk },
    { "+",        EV_TYPE_KEYBD, XK_plus },
    { ",",        EV_TYPE_KEYBD, XK_comma },
    { "-",        EV_TYPE_KEYBD, XK_minus },
    { ".",        EV_TYPE_KEYBD, XK_period },
    { "/",        EV_TYPE_KEYBD, XK_slash },
    { ":",        EV_TYPE_KEYBD, XK_colon },
    { ";",        EV_TYPE_KEYBD, XK_semicolon },
    { "<",        EV_TYPE_KEYBD, XK_less },
    { "=",        EV_TYPE_KEYBD, XK_equal },
    { ">",        EV_TYPE_KEYBD, XK_greater },
    { "?",        EV_TYPE_KEYBD, XK_question },
    { "@",        EV_TYPE_KEYBD, XK_at },
    { "[",        EV_TYPE_KEYBD, XK_bracketleft },
    { "\\",       EV_TYPE_KEYBD, XK_backslash },
    { "]",        EV_TYPE_KEYBD, XK_bracketright },
    { "^",        EV_TYPE_KEYBD, XK_asciicircum },
    { "_",        EV_TYPE_KEYBD, XK_underscore },
    { "`",        EV_TYPE_KEYBD, XK_quoteleft },
    { "{",        EV_TYPE_KEYBD, XK_braceleft },
    { "|",        EV_TYPE_KEYBD, XK_bar },
    { "}",        EV_TYPE_KEYBD, XK_braceright },
    { "~",        EV_TYPE_KEYBD, XK_asciitilde },

    { 0 }
};

bool
sys_lookup_mod(const char *name, u_long *mods)
{
    struct key_def *x = x11_mods;
    while(x->name != 0)
    {
	if(strcasecmp(name, x->name) == 0)
	{
	    *mods |= x->mods;
	    return TRUE;
	}
	x++;
    }
    return FALSE;
}

bool
sys_lookup_code(const char *name, u_long *code, u_long *mods)
{
    struct key_def *x = x11_codes;
    while(x->name != 0)
    {
	if(strcasecmp(name, x->name) == 0)
	{
	    *mods |= x->mods;
	    *code = x->code;
	    return TRUE;
	}
	x++;
    }

    {
	unsigned int ks = XStringToKeysym(name);
	if(ks != NoSymbol)
	{
	    *mods |= EV_TYPE_KEYBD;
	    *code = ks;
	    return TRUE;
	}
    }

    return FALSE;
}

char *
sys_lookup_mod_name(char *buf, u_long mod)
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
sys_lookup_code_name(char *buf, u_long code, u_long type)
{
    struct key_def *x = x11_codes;
    char *tem;
    while(x->name != 0)
    {
	if(x->mods == type && x->code == code)
	{
	    strcpy(buf, x->name);
	    return TRUE;
	}
	x++;
    }

    tem = XKeysymToString((KeySym)code);
    if(tem != 0)
    {
	strcpy(buf, tem);
	return TRUE;
    }

    return FALSE;
}

/* Return the jade modifier mask used as the meta key. This code
   shamelessly stolen from Emacs 19. :-) */
u_long
x11_find_meta(struct x11_display *xd)
{
    u_long meta_mod = 0, alt_mod = 0;

    int min_code, max_code;
    KeySym *syms;
    int syms_per_code;
    XModifierKeymap *mods;

#if XlibSpecificationRelease >= 4
    XDisplayKeycodes(xd->display, &min_code, &max_code);
#else
    min_code = xd->display->min_keycode;
    max_code = xd->display->max_keycode;
#endif

    syms = XGetKeyboardMapping(xd->display, min_code, max_code - min_code + 1,
			       &syms_per_code);
    mods = XGetModifierMapping(xd->display);

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
			meta_mod = translate_mods(0, 1 << row, FALSE);
			break;

		    case XK_Alt_L: case XK_Alt_R:
			alt_mod = translate_mods(0, 1 << row, FALSE);
			break;
		    }
		}
	    }
	}
    }

    if(meta_mod == 0)
	meta_mod = alt_mod;

    XFree((char *)syms);
    XFreeModifiermap(mods);

    return meta_mod;
}
