/* amiga_menus.c -- Pull-down menu handling for AmigaDOS
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
#include <clib/gadtools_protos.h>
#include <string.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#define MAX_MENUS 500
#define NMSIZE (MAX_MENUS * sizeof(struct NewMenu))

_PR void ami_clear_menu(struct Window *);
_PR void ami_set_menu(struct Window *);
_PR VALUE evalmenu(WORD, WORD);

_PR void ami_mark_menus(void);
_PR void ami_menus_init(void);
_PR void ami_menus_kill(void);

static struct Menu *menu_strip;
static struct NewMenu *new_menu;
static APTR vis_info;

#ifdef AMIGA_NEED_VARARGS_STUBS
struct Menu *
CreateMenus(struct NewMenu *newMenu, Tag firstTag, ...)
{
    return(CreateMenusA(newMenu, (struct TagItem *)&firstTag));
}
APTR
GetVisualInfo(struct Screen *screen, Tag firstTag, ...)
{
    return(GetVisualInfoA(screen, (struct TagItem *)&firstTag));
}
BOOL
LayoutMenus(struct Menu *menu, APTR vi, Tag firstTag, ...)
{
    return(LayoutMenusA(menu, vi, (struct TagItem *)&firstTag));
}
#endif /* AMIGA_NEEDS_VARARGS_STUBS */

void
ami_clear_menu(struct Window *wd)
{
    ClearMenuStrip(wd);
    wd->Flags |= WFLG_RMBTRAP;
}

static void
clear_all_menus(void)
{
    VW *thisvw;
    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Window)
	    ami_clear_menu(thisvw->vw_Window);
    }
}

void
ami_set_menu(struct Window *wd)
{
    if(menu_strip)
    {
	SetMenuStrip(wd, menu_strip);
	wd->Flags &= ~WFLG_RMBTRAP;
    }
}

static void
set_all_menus(void)
{
    VW *thisvw;
    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Window)
	    ami_set_menu(thisvw->vw_Window);
    }
}

_PR VALUE cmd_set_menu_enabled(VALUE stat, VALUE vw);
DEFUN("set-menu-enabled", cmd_set_menu_enabled, subr_set_menu_enabled, (VALUE stat, VALUE vw), V_Subr2, DOC_set_menu_enabled) /*
::doc:set_menu_enabled::
set-menu-enabled ENABLED-P [WINDOW]
<AMIGA-ONLY>

If ENABLED-P is non-nil the menu will be active in WINDOW, if WINDOW is not
specified, menus will be enabled in *all* windows. When a menu is not enabled
in a window right mouse button events will be detectable.

Note that for a menu to be displayed the `set-menu' command must be used first
to create a menu strip.
::end:: */
{
    if(NILP(vw))
    {
	if(!NILP(stat))
	    set_all_menus();
	else
	    clear_all_menus();
    }
    else
    {
	if(!NILP(stat))
	    ami_set_menu(VWIN(vw)->vw_Window);
	else
	    ami_clear_menu(VWIN(vw)->vw_Window);
    }
    return(sym_t);
}

_PR VALUE cmd_menu_enabled_p(VALUE vw);
DEFUN("menu-enabled-p", cmd_menu_enabled_p, subr_menu_enabled_p, (VALUE vw), V_Subr1, DOC_menu_enabled_p) /*
::doc:menu_enabled_p::
menu-enabled-p [WINDOW]
<AMIGA-ONLY>

Returns t if a menu is being displayed in WINDOW (or the current window).
::end:: */
{
    if(!WINDOWP(vw))
	vw = curr_vw;
    return(VWIN(vw)->vw_Window->MenuStrip ? sym_t : sym_nil);
}

_PR VALUE cmd_set_menu(VALUE args);
DEFUN("set-menu", cmd_set_menu, subr_set_menu, (VALUE args), V_SubrN, DOC_set_menu) /*
::doc:set_menu::
set-menu MENUS...
<AMIGA-ONLY>

Creates a new menustrip, each window then has these menus.
Each MENU defines a single menu block, it has this format,

    (MENU-NAME MENU-ITEM...)

MENU-NAME is the name of the block, each MENU-ITEM defines a single menuitem,
they are either nil (empty menu item) or look like,

    (ITEM-NAME [SHORTCUT] COMMAND)

ITEM-NAME is the item name, SHORTCUT is an optional keyboard shortcut (a one
character long string) and COMMAND is the command to be called when the
menu is selected (see the function `call-command').

The shortcuts may be upper or lower case. Menu shortcuts are only
considered to be case-significant when two shortcuts of the same letter (but
different case) are defined.
::end:: */
{
    if(!CONSP(args))
    {
	signal_arg_error(args, 0);
	return(NULL);
    }
    struct NewMenu *nm = mycalloc(NMSIZE);
    if(nm)
    {
	struct NewMenu *cur = nm;
	ami_menus_kill();
	while(CONSP(args) && CONSP(VCAR(args)))
	{
	    VALUE menu = VCAR(args);
	    cur->nm_Type = NM_TITLE;
	    cur->nm_Label = VSTR(STRINGP(VCAR(menu)) ? VCAR(menu) : null_string);
	    cur++;
	    menu = VCDR(menu);
	    while(CONSP(menu))
	    {
		VALUE item = VCAR(menu);
		cur->nm_Type = NM_ITEM;
		if(CONSP(item) && STRINGP(VCAR(item)))
		{
		    cur->nm_Label = VSTR(VCAR(item));
		    item = VCDR(item);
		    if(CONSP(item) && STRINGP(VCAR(item)))
		    {
			cur->nm_CommKey = VSTR(VCAR(item));
			item = VCDR(item);
		    }
		    if(CONSP(item))
			cur->nm_UserData = VCAR(item);
		}
		else
		    cur->nm_Label = NM_BARLABEL;
		cur++;
		menu = VCDR(menu);
	    }
	    args = VCDR(args);
	}
	cur->nm_Type = NM_END;
	cur++;
	new_menu = mymalloc((cur - nm) * sizeof(struct NewMenu));
	if(new_menu)
	{
	    memcpy(new_menu, nm, (cur - nm) * sizeof(struct NewMenu));
	    myfree(nm);
	}
	else
	    new_menu = nm;

	menu_strip = CreateMenus(new_menu, TAG_END);
	if(menu_strip)
	{
	    vis_info = GetVisualInfo(curr_vw->vw_Window->WScreen, TAG_END);
	    if(vis_info)
	    {
		if(LayoutMenus(menu_strip, vis_info,
#if AMIGA_INCLUDE_VER >= 39
			       GTMN_NewLookMenus, TRUE,
#endif
			       TAG_END))
		{
		    set_all_menus();
		    return(sym_t);
		}
		else
		    cmd_signal(sym_error, LIST_1(MKSTR("Can't layout menus")));
		FreeVisualInfo(vis_info);
		vis_info = NULL;
	    }
	    else
		cmd_signal(sym_error, LIST_1(MKSTR("Can't get visual info")));
	    FreeMenus(menu_strip);
	    menu_strip = NULL;
	}
	else
	    cmd_signal(sym_error, LIST_1(MKSTR("Can't create menus")));
	myfree(new_menu);
	new_menu = NULL;
    }
    return(NULL);
}

/*
 * This routine is used so we can make command key shortcuts case
 * sensitive - is there an easier way than this???
 */
static struct MenuItem *
findcommmenu(u_char commKey)
{
    struct Menu *menu;
    for(menu = menu_strip; menu; menu = menu->NextMenu)
    {
	struct MenuItem *mi;
	for(mi = menu->FirstItem; mi; mi = mi->NextItem)
	{
	    struct MenuItem *si;
	    for(si = mi->SubItem; si; si = si->NextItem)
	    {
		if(si->Command == commKey)
		    return(si);
	    }
	    if(mi->Command == commKey)
		return(mi);
	}
    }
    return(FALSE);
}

/*
 * called from IDCMP event loop
 */
VALUE
evalmenu(WORD code, WORD qual)
{
    VALUE res = sym_nil;
    if(menu_strip)
    {
	struct MenuItem *mi;
	while(mi = ItemAddress(menu_strip, code))
	{
	    VALUE command;
	    if(mi->Command && (qual & IEQUALIFIER_RCOMMAND))
	    {
		struct MenuItem *actual;
		if(qual & (IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT))
		{
		    if(actual = findcommmenu(toupper(mi->Command)))
			mi = actual;
		}
		else
		{
		    if(actual = findcommmenu(tolower(mi->Command)))
			mi = actual;
		}
		code = MENUNULL;
	    }
	    else
		code = mi->NextSelect;
	    command = GTMENUITEM_USERDATA(mi);
	    if(command)
		res = cmd_call_command(command, sym_nil);
	    refresh_world();
	    cursor(curr_vw, CURS_ON);
	}
    }
    return(res);
}

void
ami_mark_menus(void)
{
    if(new_menu)
    {
	struct NewMenu *nm = new_menu;
	while(nm->nm_Type != NM_END)
	{
	    if(nm->nm_Label != NM_BARLABEL)
	    {
		MARKVAL(VAL(STRING_HDR(nm->nm_Label)));
		if(nm->nm_CommKey)
		    MARKVAL(VAL(STRING_HDR(nm->nm_CommKey)));
		MARKVAL(VAL(nm->nm_UserData));
	    }
	    nm++;
	}
    }
}

void
ami_menus_init(void)
{
    ADD_SUBR(subr_set_menu_enabled);
    ADD_SUBR(subr_menu_enabled_p);
    ADD_SUBR(subr_set_menu);
}

void
ami_menus_kill(void)
{
    if(menu_strip)
    {
	clear_all_menus();
	FreeMenus(menu_strip);
	menu_strip = NULL;
    }
    if(vis_info)
    {
	FreeVisualInfo(vis_info);
	vis_info = NULL;
    }
    if(new_menu)
    {
	myfree(new_menu);
	new_menu = NULL;
    }
}
