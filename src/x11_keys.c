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
#include "jade_protos.h"

#include <string.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

#ifdef SWAP_DELETE_KEYS
# define XK_DELETE XK_BackSpace
# define XK_BACKSPACE XK_Delete
#else
# define XK_DELETE XK_Delete
# define XK_BACKSPACE XK_BackSpace
#endif

_PR void translate_event(u_long *, u_long *, XEvent *);
_PR int cook_key(void *, u_char *, int);
_PR bool lookup_event(u_long *, u_long *, u_char *);
_PR bool lookup_event_name(u_char *, u_long, u_long);
_PR u_long sys_find_meta(void);

_PR u_long esc_code, esc_mods;
u_long esc_code = XK_Escape, esc_mods = EV_TYPE_KEYBD;

static u_long
translate_mods(u_long mods, unsigned int state)
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
    return(mods);
}

void
translate_event(u_long *code, u_long *mods, XEvent *xev)
{
    static Time LastClick;
    switch(xev->type)
    {
    case KeyPress:
	*mods = translate_mods(*mods, xev->xkey.state);
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
	if(xev->xbutton.time < (LastClick + DOUBLE_CLICK_TIME))
	    *code = EV_CODE_MOUSE_CLICK2;
	else
	    *code = EV_CODE_MOUSE_CLICK1;
	LastClick = xev->xbutton.time;
	goto button;
	break;

    case ButtonRelease:
	*code = EV_CODE_MOUSE_UP;
    button:
	*mods = EV_TYPE_MOUSE
	    | translate_mods(*mods, xev->xbutton.state);
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
	*mods = EV_TYPE_MOUSE | translate_mods(*mods, xev->xmotion.state);
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

typedef struct
{
    u_char	 *kd_Name;
    u_long	  kd_Mods;
    u_long	  kd_Code;
} KeyDesc;

static const KeyDesc KeyDescr[] =
{
    { "Shift",    EV_MOD_SHIFT, 0 },
    { "SFT",      EV_MOD_SHIFT, 0 },
    { "Ctrl",     EV_MOD_CTRL, 0 },
    { "Control",  EV_MOD_CTRL, 0 },
    { "CTL",      EV_MOD_CTRL, 0 },
    { "Meta",     EV_MOD_FAKE_META, 0 },
    { "Mod1",     EV_MOD_MOD1, 0 },
    { "Mod2",     EV_MOD_MOD2, 0 },
    { "Amiga",    EV_MOD_MOD2, 0 },
    { "Mod3",     EV_MOD_MOD3, 0 },
    { "Mod4",     EV_MOD_MOD4, 0 },
    { "LMB",      EV_MOD_LMB, 0 },
    { "Button1",  EV_MOD_BUTTON1, 0 },
    { "MMB",      EV_MOD_MMB, 0 },
    { "Button2",  EV_MOD_BUTTON2, 0 },
    { "RMB",      EV_MOD_RMB, 0 },
    { "Button3",  EV_MOD_BUTTON3, 0 },
    { "Button4",  EV_MOD_BUTTON4, 0 },
    { "Button5",  EV_MOD_BUTTON5, 0 },

    { "SPC",      EV_TYPE_KEYBD, XK_space },
    { "Space",    EV_TYPE_KEYBD, XK_space },
    { "Spacebar", EV_TYPE_KEYBD, XK_space },
    { "TAB",      EV_TYPE_KEYBD, XK_Tab },
    { "RET",      EV_TYPE_KEYBD, XK_Return },
    { "Return",   EV_TYPE_KEYBD, XK_Return },
    { "ESC",      EV_TYPE_KEYBD, XK_Escape },
    { "Escape",   EV_TYPE_KEYBD, XK_Escape },
    { "BS",       EV_TYPE_KEYBD, XK_BACKSPACE },
    { "Backspace", EV_TYPE_KEYBD, XK_BACKSPACE },
    { "DEL",      EV_TYPE_KEYBD, XK_DELETE },
    { "Delete",   EV_TYPE_KEYBD, XK_DELETE },
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
#if 0
    /* It's insane but for some reason solaris seems to think
       that strcasecmp("m", "\243") is true (comparing 'm' and the
       pounds sterling sign). This makes any binding to "X-m" come
       out as "X-\243"! */
    { "£",        EV_TYPE_KEYBD, XK_sterling },
#endif

    /* Mouse events */
    { "Click1",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Off",      EV_TYPE_MOUSE, EV_CODE_MOUSE_UP },
    { "Move",     EV_TYPE_MOUSE, EV_CODE_MOUSE_MOVE },

    { NULL, 0, 0 }
};

/* Puts the integers defining the event described in DESC into CODE and
   MODS. This needs some work, should really separate mods and codes
   more and check for trailing unused characters at the end. */
bool
lookup_event(u_long *code, u_long *mods, u_char *desc)
{
    u_char *str = desc;
    bool rc = TRUE;
    *code = *mods = 0;
    for(;;)
    {
	u_char buff[100];
	u_char *tmp = buff;
	u_char c = *str++;
	const KeyDesc *kd = KeyDescr;
	/* Get this one token.  The first character is read separately to
	   allow minus characters to be used to represent itself as well as
	   to terminate a token. */
	if(c != 0)
	{
	    *tmp++ = c;
	    while((c = *str) && (c != '-'))
	    {
		*tmp++ = c;
		str++;
	    }
	}
	else
	    goto error;
	*tmp = 0;
	if(*str)
	    str++;
	while(kd->kd_Name)
	{
	    if(!strcasecmp(kd->kd_Name, buff))
	    {
		if(kd->kd_Mods & EV_MOD_FAKE_META)
		    *mods |= (kd->kd_Mods & ~EV_MOD_FAKE_META) | ev_mod_meta;
		else
		    *mods |= kd->kd_Mods;
		*code = kd->kd_Code;
		if(*mods & EV_TYPE_MASK)
		    goto end;
		break;
	    }
	    kd++;
	}
	if(!kd->kd_Name)
	{
	    unsigned int ks;
	    if((ks = XStringToKeysym(buff)) != NoSymbol)
	    {
		*mods |= EV_TYPE_KEYBD;
		*code = ks;
		goto end;
	    }
	    else
	    {
	    error:
		cmd_signal(sym_bad_event_desc, LIST_1(string_dup(desc)));
		rc = FALSE;
		goto end;
	    }
	}
    }
end:
    return(rc);
}

/* Constructs the name of the event defined by CODE and MODS in BUF.  */
bool
lookup_event_name(u_char *buf, u_long code, u_long mods)
{
    /* First resolve all modifiers */
    u_long tmp_mods;
    const KeyDesc *kd = KeyDescr;
    u_char *name;
    if(mods & ev_mod_meta)
	mods = (mods & ~ev_mod_meta) | EV_MOD_FAKE_META;
    tmp_mods = mods & EV_MOD_MASK;
    while(kd->kd_Name && (tmp_mods != 0))
    {
	if((tmp_mods & kd->kd_Mods) != 0)
	{
	    tmp_mods &= ~kd->kd_Mods;
	    buf = stpcpy(buf, kd->kd_Name);
	    *buf++ = '-';
	}
	kd++;
    }
    if(tmp_mods != 0)
	return(FALSE);
    /* Now try to find the code in our lookup table */
    tmp_mods = mods & EV_TYPE_MASK;
#if 0
    /* Since all modifiers are at the start of the table this is
       unnecessary :-) */
    kd = KeyDescr;
#endif
    while(kd->kd_Name)
    {
	if((kd->kd_Mods == tmp_mods) && (kd->kd_Code == code))
	{
	    strcpy(buf, kd->kd_Name);
	    return(TRUE);
	}
	kd++;
    }
    /* Couldn't find it here; have to go to the window-system. */
    name = XKeysymToString((KeySym)code);
    if(name)
    {
	strcpy(buf, name);
	return(TRUE);
    }
    return(FALSE);
}

/* Return the jade modifier mask used as the meta key. This code
   shamelessly stolen from Emacs 19. :-)

   TODO: Should really maintain a separate Meta for each open display */
u_long
sys_find_meta(void)
{
    u_long meta_mod = 0, alt_mod = 0;

    int min_code, max_code;
    KeySym *syms;
    int syms_per_code;
    XModifierKeymap *mods;

#if XlibSpecificationRelease >= 4
    XDisplayKeycodes(x11_display_list->display, &min_code, &max_code);
#else
    min_code = x11_display_list->display->min_keycode;
    max_code = x11_display_list->display->max_keycode;
#endif

    syms = XGetKeyboardMapping(x11_display_list->display,
			       min_code, max_code - min_code + 1,
			       &syms_per_code);
    mods = XGetModifierMapping(x11_display_list->display);

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
			meta_mod = translate_mods(0, 1 << row);
			break;

		    case XK_Alt_L: case XK_Alt_R:
			alt_mod = translate_mods(0, 1 << row);
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
