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
#include "mac_internal.h"
#include <string.h>

unsigned long esc_code = 8+53, esc_mods = EV_TYPE_KEYBD;

static unsigned long
translate_mods(unsigned long mods, unsigned int state, bool subst_meta)
{
    if(state & NSEventModifierFlagShift)
	mods |= EV_MOD_SHIFT;
    if(state & NSEventModifierFlagCapsLock)
	mods ^= EV_MOD_SHIFT;
    if(state & NSEventModifierFlagControl)
	mods |= EV_MOD_CTRL;
    if(state & NSEventModifierFlagOption)
	mods |= EV_MOD_MOD1;
    if(state & NSEventModifierFlagCommand)
	mods |= EV_MOD_MOD2;

    if(subst_meta && (mods & WINDOW_META(curr_win)) != 0)
	mods = (mods & ~WINDOW_META(curr_win)) | EV_MOD_META;

    return mods;
}

static unsigned long
button_mods (int number)
{
    if (number == 0)
	return EV_MOD_BUTTON1;
    else if (number == 1)
	return EV_MOD_BUTTON2;
    else if (number == 2)
	return EV_MOD_BUTTON3;
    else if (number == 3)
	 return EV_MOD_BUTTON4;
    else if (number == 4)
	return EV_MOD_BUTTON5;
    return 0;
}

void
sys_translate_event(unsigned long *code, unsigned long *mods, void *ev_)
{
    NSEvent *ev = ev_;

    switch((int) [ev type])
    {
    case NSEventTypeKeyDown:
	*mods = translate_mods(*mods, [ev modifierFlags], TRUE);
	*code = 8 + [ev keyCode];
	if(*code != 0)
	    *mods |= EV_TYPE_KEYBD;
	break;

    case NSEventTypeLeftMouseDown:
    case NSEventTypeRightMouseDown:
    case NSEventTypeOtherMouseDown:
	if ([ev clickCount] == 1)
	    *code = EV_CODE_MOUSE_CLICK1;
	else
	    *code = EV_CODE_MOUSE_CLICK2;
	goto button;

    case NSEventTypeLeftMouseUp:
    case NSEventTypeRightMouseUp:
    case NSEventTypeOtherMouseUp:
	*code = EV_CODE_MOUSE_UP;
    button:
	*mods = EV_TYPE_MOUSE;
	*mods |= translate_mods(*mods, [ev modifierFlags], TRUE);
	*mods |= button_mods ([ev buttonNumber]);
	break;

    case NSEventTypeMouseMoved:
    case NSEventTypeLeftMouseDragged:
    case NSEventTypeRightMouseDragged:
    case NSEventTypeOtherMouseDragged:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = EV_TYPE_MOUSE;
	*mods |= translate_mods(*mods, [ev modifierFlags], TRUE);
	*mods |= button_mods ([ev buttonNumber]);
	break;
    }
}

size_t
sys_cook_key(void *event, char *buf, size_t buflen)
{
    NSEvent *ev = event;
    NSAutoreleasePool *pool;
    const char *str;
    size_t actual_length;

    if ([ev type] != NSEventTypeKeyDown)
	return 0;

    pool = [[NSAutoreleasePool alloc] init];

    str = [[ev characters] UTF8String];

    if (str != NULL)
    {
	actual_length = MIN (buflen - 1, strlen (str));
	memcpy (buf, str, actual_length);
	buf[actual_length] = 0;
    }
    else
	actual_length = 0;

    [pool drain];

    return actual_length;
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
    { "SPC",      EV_TYPE_KEYBD, 49 },
    { "Space",    EV_TYPE_KEYBD, 49 },
    { "Spacebar", EV_TYPE_KEYBD, 49 },
    { "TAB",      EV_TYPE_KEYBD, 48 },
    { "RET",      EV_TYPE_KEYBD, 36 },
    { "Return",   EV_TYPE_KEYBD, 36 },
    { "ESC",      EV_TYPE_KEYBD, 53 },
    { "Escape",   EV_TYPE_KEYBD, 53 },
    { "BS",       EV_TYPE_KEYBD, 51 },
    { "Backspace", EV_TYPE_KEYBD, 51 },
    { "DEL",      EV_TYPE_KEYBD, 117 },
    { "Delete",   EV_TYPE_KEYBD, 117 },
    { "Up",       EV_TYPE_KEYBD, 126 },
    { "Down",     EV_TYPE_KEYBD, 125 },
    { "Right",    EV_TYPE_KEYBD, 124 },
    { "Left",     EV_TYPE_KEYBD, 123 },

    { 0 }
};

struct keymap_entry {
    const char *a, *b, *c, *d;
};

static const struct keymap_entry mac_keys[256] = {
/* this table is from `xmodmap -pke'. */
[8] = {"a", "A", "aring", "Aring"},
[9] = {"s", "S", "ssharp", "Iacute"},
[10] = {"d", "D", "partialderivative", "Icircumflex"},
[11] = {"f", "F", "function", "Idiaeresis"},
[12] = {"h", "H", "abovedot", "Oacute"},
[13] = {"g", "G", "copyright", "doubleacute"},
[14] = {"z", "Z", "Greek_OMEGA", "cedilla"},
[15] = {"x", "X", "approxeq", "ogonek"},
[16] = {"c", "C", "ccedilla", "Ccedilla"},
[17] = {"v", "V", "radical", "U25CA"},
[18] = {"section", "plusminus"},
[19] = {"b", "B", "integral", "idotless"},
[20] = {"q", "Q", "oe", "OE"},
[21] = {"w", "W", "U2211", "doublelowquotemark"},
[22] = {"e", "E", "dead_acute", "acute"},
[23] = {"r", "R", "registered", "U2030"},
[24] = {"y", "Y", "yen", "Aacute"},
[25] = {"t", "T", "dagger", "caron"},
[26] = {"1", "!", "exclamdown", "U2044"},
[27] = {"2", "@", "trademark", "EuroSign"},
[28] = {"3", "#", "sterling", "U2039"},
[29] = {"4", "$", "cent", "U203A"},
[30] = {"6", "^", "section", "UFB02"},
[31] = {"5", "%", "infinity", "UFB01"},
[32] = {"=", "+", "notequal", "plusminus"},
[33] = {"9", "(", "ordfeminine", "periodcentered"},
[34] = {"7", "&", "paragraph", "doubledagger"},
[35] = {"-", "_", "endash", "emdash"},
[36] = {"8", "*", "enfilledcircbullet", "degree"},
[37] = {"0", ")", "masculine", "singlelowquotemark"},
[38] = {"]", "}", "leftsinglequotemark", "rightsinglequotemark"},
[39] = {"o", "O", "oslash", "Oslash"},
[40] = {"u", "U", "dead_diaeresis", "diaeresis"},
[41] = {"[", "{", "leftdoublequotemark", "rightdoublequotemark"},
[42] = {"i", "I", "dead_circumflex", "U02C6"},
[43] = {"p", "P", "Greek_pi", "U220F"},
[44] = {"Return"},
[45] = {"l", "L", "notsign", "Ograve"},
[46] = {"j", "J", "U2206", "Ocircumflex"},
[47] = {"'", "\"", "ae", "AE"},
[48] = {"k", "K", "U02DA", "UF8FF"},
[49] = {";", ":", "ellipsis", "Uacute"},
[50] = {"\\", "|", "guillemotleft", "guillemotright"},
[51] = {",", "<", "lessthanequal", "macron"},
[52] = {"/", "?", "division", "questiondown"},
[53] = {"n", "N", "dead_tilde", "U02DC"},
[54] = {"m", "M", "mu", "Acircumflex"},
[55] = {".", ">", "greaterthanequal", "breve"},
[56] = {"Tab"},
[57] = {" ", "NoSymbol", "nobreakspace"},
[58] = {"`", "~", "dead_grave", "grave"},
[59] = {"BackSpace"},
[61] = {"Escape"},
[73] = {"KP_Decimal"},
[74] = {"Right", "*"},
[75] = {"KP_Multiply"},
[77] = {"KP_Add"},
[78] = {"Left", "+"},
[79] = {"Escape"},
[80] = {"Down", "="},
[84] = {"KP_Enter"},
[85] = {"Up", "/"},
[86] = {"KP_Subtract"},
[89] = {"KP_Equal"},
[90] = {"KP_0"},
[91] = {"KP_1"},
[92] = {"KP_2"},
[93] = {"KP_3"},
[94] = {"KP_4"},
[95] = {"KP_5"},
[96] = {"KP_6"},
[97] = {"KP_7"},
[99] = {"KP_8"},
[100] = {"KP_9"},
[104] = {"F5"},
[105] = {"F6"},
[106] = {"F7"},
[107] = {"F3"},
[108] = {"F8"},
[109] = {"F9"},
[111] = {"F11"},
[113] = {"F13"},
[115] = {"F14"},
[117] = {"F10"},
[119] = {"F12"},
[121] = {"F15"},
[122] = {"Help"},
[123] = {"Home"},
[124] = {"Prior"},
[125] = {"Delete"},
[126] = {"F4"},
[127] = {"End"},
[128] = {"F2"},
[129] = {"Next"},
[130] = {"F1"},
[131] = {"Left"},
[132] = {"Right"},
[133] = {"Down"},
[134] = {"Up"},
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
	    return TRUE;
	}
	x++;
    }
    return FALSE;
}

bool
sys_lookup_code(const char *name, unsigned long *code, unsigned long *mods)
{
    struct key_def *x = x11_codes;
    int i;

    while(x->name != 0)
    {
	if(strcasecmp(name, x->name) == 0)
	{
	    *mods |= x->mods;
	    *code = 8 + x->code;
	    return TRUE;
	}
	x++;
    }

    for (i = 0; i < 256; i++)
    {
	if (mac_keys[i].a != NULL && strcmp (name, mac_keys[i].a) == 0)
	{
	    *mods |= EV_TYPE_KEYBD;
	    *code = i;
	    return TRUE;
	}
	if (mac_keys[i].b != NULL && strcmp (name, mac_keys[i].b) == 0)
	{
	    *mods |= EV_TYPE_KEYBD | EV_MOD_SHIFT;
	    *code = i;
	    return TRUE;
	}
	/* FIXME: mode switched keys. */
    }

    return FALSE;
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

    while(x->name != 0)
    {
	if(x->mods == type && 8 + x->code == code)
	{
	    strcpy(buf, x->name);
	    return TRUE;
	}
	x++;
    }

    if (type == EV_TYPE_KEYBD && mac_keys[code].a != NULL)
    {
	strcpy (buf, mac_keys[code].a);
	return TRUE;
    }

    return FALSE;
}

unsigned long
mac_find_meta(void)
{
#if 0
    /* Use Option as Meta, to match Terminal.app. */

    return EV_MOD_MOD1;
#else
    /* fuck that, too hard to get used to.. */

    return EV_MOD_MOD2;
#endif
}
