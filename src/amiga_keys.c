/* amiga_keys.c -- Event translation for AmigaDOS
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

#define INTUI_V36_NAMES_ONLY
#include <clib/intuition_protos.h>
#include <clib/keymap_protos.h>
#include <string.h>

#define SHIFTQUALS (IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT)
#define ALTQUALS   (IEQUALIFIER_LALT | IEQUALIFIER_RALT)
#define CMDQUALS   (IEQUALIFIER_LCOMMAND | IEQUALIFIER_RCOMMAND)

_PR void translate_event(u_long *, u_long *, struct IntuiMessage *);
_PR int  cook_key(struct IntuiMessage *, u_char *, int);
_PR bool lookup_event(u_long *, u_long *, u_char *);
_PR bool lookup_event_name(u_char *, u_long, u_long);
_PR u_long sys_find_meta(void);

_PR u_long esc_code, esc_mods;
u_long esc_code = 0x45, esc_mods = EV_TYPE_KEYBD;

static u_long
translate_quals(u_long mods, u_short qual)
{
    if(qual & SHIFTQUALS)
	mods |= EV_MOD_SHIFT;
    if(qual & ALTQUALS)
	mods |= EV_MOD_META;
    if(qual & CMDQUALS)
	mods |= EV_MOD_MOD2; /* Amiga keys */
    if(qual & IEQUALIFIER_CONTROL)
	mods |= EV_MOD_CTRL;
    if(qual & IEQUALIFIER_CAPSLOCK)
	mods ^= EV_MOD_SHIFT;
    if(qual & IEQUALIFIER_LEFTBUTTON)
	mods |= EV_MOD_LMB;
    if(qual & IEQUALIFIER_MIDBUTTON)
	mods |= EV_MOD_MMB;
    if(qual & IEQUALIFIER_RBUTTON)
	mods |= EV_MOD_RMB;
    return(mods);
}

static u_long
translate_mods(u_long mods)
{
    u_long quals = 0;
    if(mods & EV_MOD_SHIFT)
	quals |= IEQUALIFIER_LSHIFT;
    if(mods & EV_MOD_CTRL)
	quals |= IEQUALIFIER_CONTROL;
    if(mods & EV_MOD_META)
	quals |= IEQUALIFIER_LALT;
    if(mods & EV_MOD_MOD2)
	quals |= IEQUALIFIER_LCOMMAND;
    if(mods & EV_MOD_LMB)
	quals |= IEQUALIFIER_LEFTBUTTON;
    if(mods & EV_MOD_MMB)
	quals |= IEQUALIFIER_MIDBUTTON;
    if(mods & EV_MOD_RMB)
	quals |= IEQUALIFIER_RBUTTON;
    return(mods);
}

/* Takes an IntuiMsg and converts it's contents to an EVENT. */
void
translate_event(u_long *code, u_long *mods, struct IntuiMessage *im)
{
    static u_long ClickMics, ClickSecs;
    *mods = translate_quals(*mods, im->Qualifier);
    switch(im->Class)
    {
    case IDCMP_RAWKEY:
	/* This magic means that we disreguard all upcodes and qualifiers.  */
	if((im->Code > 0x7f) || ((im->Code >= 0x60) && (im->Code <= 0x67)))
	    return;
	*code = (u_long)im->Code;
	*mods |= EV_TYPE_KEYBD;
	break;
    case IDCMP_MOUSEBUTTONS:
	*mods |= EV_TYPE_MOUSE;
	if(im->Code & IECODE_UP_PREFIX)
	{
	    *code = EV_CODE_MOUSE_UP;
	    switch(im->Code & ~IECODE_UP_PREFIX)
	    {
	    case IECODE_LBUTTON:
		*mods |= EV_MOD_LMB;
		break;
	    case IECODE_MBUTTON:
		*mods |= EV_MOD_MMB;
		break;
	    case IECODE_RBUTTON:
		*mods |= EV_MOD_RMB;
		break;
	    }
	}
	else
	{
	    if(DoubleClick(ClickSecs, ClickMics, im->Seconds, im->Micros))
		*code = EV_CODE_MOUSE_CLICK2;
	    else
		*code = EV_CODE_MOUSE_CLICK1;
	    ClickSecs = im->Seconds;
	    ClickMics = im->Micros;
	}
	break;
    case IDCMP_MOUSEMOVE:
	if(im->Qualifier & (IEQUALIFIER_MIDBUTTON | IEQUALIFIER_RBUTTON
			    | IEQUALIFIER_LEFTBUTTON))
	{
	    *mods |= EV_TYPE_MOUSE;
	    *code = EV_CODE_MOUSE_MOVE;
	}
	break;
    }
}

int
cook_key(struct IntuiMessage *im, u_char *buf, int buflen)
{
    struct InputEvent ie;
    ie.ie_Class = IECLASS_RAWKEY;
    ie.ie_SubClass = 0;
    ie.ie_Code = im->Code;
    ie.ie_Qualifier = im->Qualifier;
    ie.ie_EventAddress = *((APTR *)im->IAddress);
    return(MapRawKey(&ie, buf, buflen, NULL));
}

/*
 * Stuff to translate textual key descriptions into key codes
 */

typedef struct KeyDesc
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

    { "SPC",       EV_TYPE_KEYBD, 0x40 },
    { "Space",     EV_TYPE_KEYBD, 0x40 },
    { "Spacebar",  EV_TYPE_KEYBD, 0x40 },
    { "BS",        EV_TYPE_KEYBD, 0x41 },
    { "Backspace", EV_TYPE_KEYBD, 0x41 },
    { "TAB",       EV_TYPE_KEYBD, 0x42 },
    { "RET",       EV_TYPE_KEYBD, 0x44 },
    { "Return",    EV_TYPE_KEYBD, 0x44 },
    { "ESC",       EV_TYPE_KEYBD, 0x45 },
    { "Escape",    EV_TYPE_KEYBD, 0x45 },
    { "DEL",       EV_TYPE_KEYBD, 0x46 },
    { "Delete",    EV_TYPE_KEYBD, 0x46 },
    { "Help",      EV_TYPE_KEYBD, 0x5f },
    { "Up",        EV_TYPE_KEYBD, 0x4c },
    { "Down",      EV_TYPE_KEYBD, 0x4d },
    { "Right",     EV_TYPE_KEYBD, 0x4e },
    { "Left",      EV_TYPE_KEYBD, 0x4f },
    { "F1",        EV_TYPE_KEYBD, 0x50 },
    { "F2",        EV_TYPE_KEYBD, 0x51 },
    { "F3",        EV_TYPE_KEYBD, 0x52 },
    { "F4",        EV_TYPE_KEYBD, 0x53 },
    { "F5",        EV_TYPE_KEYBD, 0x54 },
    { "F6",        EV_TYPE_KEYBD, 0x55 },
    { "F7",        EV_TYPE_KEYBD, 0x56 },
    { "F8",        EV_TYPE_KEYBD, 0x57 },
    { "F9",        EV_TYPE_KEYBD, 0x58 },
    { "F10",       EV_TYPE_KEYBD, 0x59 },

    /* Numeric keypad */
    { "KP_Enter",   EV_TYPE_KEYBD, 0x43 },
    { "KP_Multiply",EV_TYPE_KEYBD, 0x5d },
    { "KP_Divide",  EV_TYPE_KEYBD, 0x5c },
    { "KP_Minus",   EV_TYPE_KEYBD, 0x4a },
    { "KP_Add",     EV_TYPE_KEYBD, 0x5e },
    { "KP_Decimal", EV_TYPE_KEYBD, 0x39 },
    { "KP_0",       EV_TYPE_KEYBD, 0x0a },
    { "KP_1",       EV_TYPE_KEYBD, 0x1d },
    { "KP_2",       EV_TYPE_KEYBD, 0x1e },
    { "KP_3",       EV_TYPE_KEYBD, 0x1f },
    { "KP_4",       EV_TYPE_KEYBD, 0x2d },
    { "KP_5",       EV_TYPE_KEYBD, 0x2e },
    { "KP_6",       EV_TYPE_KEYBD, 0x2f },
    { "KP_7",       EV_TYPE_KEYBD, 0x3d },
    { "KP_8",       EV_TYPE_KEYBD, 0x3e },
    { "KP_9",       EV_TYPE_KEYBD, 0x3f },

    /* Mouse events */
    { "Click1",     EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",     EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Off",        EV_TYPE_MOUSE, EV_CODE_MOUSE_UP },
    { "Move",       EV_TYPE_MOUSE, EV_CODE_MOUSE_MOVE },

    { NULL, 0, 0 }
};

/* Puts the integers defining the event described in DESC into CODE and
   MODS.  */
bool
lookup_event(u_long *code, u_long *mods, u_char *desc)
{
    bool rc = TRUE;
    u_char *str = desc;
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
	    if(!stricmp(kd->kd_Name, buff))
	    {
		if(kd->kd_Mods & EV_MOD_FAKE_META)
		    *mods |= (kd->kd_Mods & ~EV_MOD_FAKE_META) | ev_mod_meta;
		else
		    *mods |= kd->kd_Mods;
		*code |= kd->kd_Code;
		if(*mods & EV_TYPE_MASK)
		    goto end;
		break;
	    }
	    kd++;
	}
	if(!kd->kd_Name)
	{
	    u_char coded[2];
	    if(MapANSI(buff, strlen(buff), coded, 1, NULL) == 1)
	    {
		*mods |= EV_TYPE_KEYBD;
		*mods = translate_quals(*mods, (UWORD)coded[1]);
		*code = (u_long)coded[0];
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
    struct InputEvent ie;
    short actual;
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

    /* Couldn't find it here; have to go to the keymap.library */
    ie.ie_Class = IECLASS_RAWKEY;
    ie.ie_SubClass = 0;
    ie.ie_Code = code;
    ie.ie_Qualifier = 0;
    ie.ie_EventAddress = NULL;	/* is this safe? */
    actual = MapRawKey(&ie, buf, 10, NULL);
    if(actual == -1)
	return(FALSE);
    buf[actual] = 0;
    return(TRUE);
}

u_long
sys_find_meta(void)
{
    return EV_MOD_ALT;
}
