/* amiga_windows.c -- Window handling for AmigaDOS
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
#include "revision.h"

/* The SET_WRITE_MASK() things needs all this.	*/
#ifdef _DCC
# define  GfxBase_DECLARED
#endif
#include <clib/graphics_protos.h>
#include <graphics/gfxbase.h>
#include <graphics/gfxmacros.h>
extern struct GfxBase *GfxBase;

#define INTUI_V36_NAMES_ONLY
#include <clib/intuition_protos.h>
#include <intuition/gadgetclass.h>
#include <clib/diskfont_protos.h>
#include <string.h>
#include <stdlib.h>

_PR int sys_sleep_vw(VW *);
_PR int sys_unsleep_vw(VW *);
_PR void sys_new_vw(VW *);
_PR void sys_kill_vw(VW *);
_PR void sys_update_dimensions(VW *);
_PR void sys_update_scroller(VW *);
_PR struct Window *sys_new_window(VW *, VW *, bool);
_PR void sys_kill_window(VW *);
static void close_shared_window(struct Window *);
static void strip_intui_messages(struct MsgPort *, struct Window *);
_PR struct IntuiMessage *ami_get_win_imsg(struct Window *);
_PR void sys_activate_win(VW *);
_PR void sys_set_vw_pos(VW *, long, long, long, long);
_PR int ami_ezreq(u_char *, u_char *, long, ...);
_PR int sys_set_font(VW *);
_PR void sys_unset_font(VW *);
_PR void sys_reset_sleep_titles(TX *);
_PR bool sys_get_mouse_pos(POS *, VW *);
_PR void sys_windows_init(void);

#define IDCMP (IDCMP_NEWSIZE | IDCMP_MOUSEBUTTONS | IDCMP_MOUSEMOVE \
	| IDCMP_MENUPICK | IDCMP_CLOSEWINDOW | IDCMP_RAWKEY \
	| IDCMP_ACTIVEWINDOW | IDCMP_GADGETUP | IDCMP_GADGETUP \
	| IDCMP_INTUITICKS)

#define SIDCMP (IDCMP_MOUSEBUTTONS | IDCMP_RAWKEY | IDCMP_INTUITICKS)

#define WFLAGS (WFLG_DRAGBAR | WFLG_DEPTHGADGET | WFLG_CLOSEGADGET \
	| WFLG_SIZEGADGET | WFLG_REPORTMOUSE | WFLG_ACTIVATE | WFLG_RMBTRAP \
	| WFLG_SMART_REFRESH | WFLG_NOCAREREFRESH)

#define SWFLAGS (WFLG_DRAGBAR | WFLG_ACTIVATE \
	| WFLG_SMART_REFRESH | WFLG_NOCAREREFRESH | WFLG_RMBTRAP)

static const u_char *window_title = VERSSTRING;
static struct MsgPort *shared_window_port;

#ifdef AMIGA_NEED_VARARGS_STUBS
ULONG
SetGadgetAttrs(struct Gadget *gadget, struct Window *window, struct Requester *req, unsigned long tag1, ...)
{
    return(SetGadgetAttrsA(gadget, window, req, (struct TagItem *)&tag1));
}
APTR
NewObject(struct IClass *class, UBYTE *classID, unsigned long tag1, ...)
{
    return(NewObjectA(class, classID, (struct TagItem *)&tag1));
}
#endif /* AMIGA_NEEDS_VARARGS_STUBS */

/*
 * Have I screwed this up??
 */
int
sys_sleep_vw(VW *vw)
{
    int rc = FALSE;
    struct Window *swin;
    if(swin = OpenWindowTags(NULL,
			     WA_Left, vw->vw_Window->LeftEdge,
			     WA_Top, vw->vw_Window->TopEdge,
			     WA_Width, 150,
			     WA_Height, vw->vw_Window->BorderTop,
			     WA_IDCMP, 0,
			     WA_Flags, SWFLAGS,
			     WA_Title, VSTR(vw->vw_Tx->tx_BufferName),
			     WA_AutoAdjust, TRUE,
			     WA_PubScreenName, VSTR(vw->vw_WindowSys.ws_ScreenName),
			     WA_PubScreenFallBack, TRUE,
			     TAG_END))
    {
	swin->UserPort = shared_window_port;
	ModifyIDCMP(swin, SIDCMP);

	vw->vw_WindowSys.ws_OldDimensions[0] = vw->vw_Window->LeftEdge;
	vw->vw_WindowSys.ws_OldDimensions[1] = vw->vw_Window->TopEdge;
	vw->vw_WindowSys.ws_OldDimensions[2] = vw->vw_Window->Width;
	vw->vw_WindowSys.ws_OldDimensions[3] = vw->vw_Window->Height;

	ami_clear_menu(vw->vw_Window);
	Forbid();
	strip_intui_messages(vw->vw_Window->UserPort, vw->vw_Window);
	vw->vw_Window->UserPort = NULL;
	ModifyIDCMP(vw->vw_Window, 0L);
	Permit();
	CloseWindow(vw->vw_Window);
#ifndef NOSCRLBAR
	if(((vw->vw_Flags & VWFF_SLEEPING) == 0) && vw->vw_SBar.gad)
	{
	    DisposeObject(vw->vw_SBar.gad);
	    vw->vw_SBar.gad = NULL;
	}
#endif
	vw->vw_Window = swin;
	vw->vw_WindowSys.ws_Rp = swin->RPort;
	vw->vw_Flags |= VWFF_SLEEPING;
	swin->UserData = (BYTE *)vw;
	rc = TRUE;
    }
    else
	cmd_signal(sym_error, LIST_1(MKSTR("Can't open window")));
    return(rc);
}

int
sys_unsleep_vw(VW *vw)
{
    int rc = TRUE;
    if(vw->vw_Flags & VWFF_SLEEPING)
    {
	struct Window *oldwin = vw->vw_Window;
	if(sys_new_window(vw, vw, FALSE))
	{
	    Forbid();
	    strip_intui_messages(oldwin->UserPort, oldwin);
	    oldwin->UserPort = NULL;
	    ModifyIDCMP(oldwin, 0L);
	    Permit();
	    CloseWindow(oldwin);
	    vw->vw_Flags &= ~VWFF_MESSAGE;
	    sys_update_dimensions(vw);
#ifndef NOSCRLBAR
	    sys_update_scroller(vw);
#endif
	    vw->vw_Flags |= VWFF_FORCE_REFRESH;
	}
	else
	    rc = FALSE;
    }
    return(rc);
}

void
sys_new_vw(VW *vw)
{
}

void
sys_kill_vw(VW *vw)
{
}

void
sys_update_dimensions(VW *vw)
{
    if(vw->vw_Window && ((vw->vw_Flags & VWFF_SLEEPING) == 0))
    {
	struct Window *wd = vw->vw_Window;

	vw->vw_MessageLineY = wd->Height - wd->BorderBottom
	    - vw->vw_FontY - 2;
	vw->vw_MessageFontY = vw->vw_MessageLineY + 2 + FONT_ASCENT(vw);

	vw->vw_XStartPix = (UWORD)wd->BorderLeft;
	vw->vw_YStartPix = (UWORD)wd->BorderTop;
	vw->vw_XEndPix = wd->Width - (UWORD)wd->BorderRight - 1;
	vw->vw_YEndPix = (vw->vw_MessageLineY - 1);
	vw->vw_XWidthPix = vw->vw_XEndPix - vw->vw_XStartPix;
	vw->vw_YHeightPix = vw->vw_YEndPix - vw->vw_YStartPix;

	vw->vw_FontStart = vw->vw_YStartPix + vw->vw_Font->tf_Baseline;

	vw->vw_MaxX = (vw->vw_XWidthPix / vw->vw_FontX);
	vw->vw_MaxY = (vw->vw_YHeightPix / vw->vw_FontY);
	if((vw->vw_XStepRatio <= 0)
	   || ((vw->vw_XStep = vw->vw_MaxX / vw->vw_XStepRatio) <= 0))
	    vw->vw_XStep = 1;
	if((vw->vw_YStepRatio <= 0)
	   || ((vw->vw_YStep = vw->vw_MaxY / vw->vw_YStepRatio) <= 0))
	    vw->vw_YStep = 1;
    }
}

#ifndef NOSCRLBAR
void
sys_update_scroller(VW *vw)
{
    if(((vw->vw_Flags & VWFF_SLEEPING) == 0) && vw->vw_SBar.gad)
    {
	vw->vw_SBar.top = vw->vw_StartLine;
	vw->vw_SBar.total = vw->vw_Tx->tx_NumLines;
	SetGadgetAttrs(vw->vw_SBar.gad, vw->vw_Window, NULL,
		       PGA_Visible, vw->vw_MaxY,
		       PGA_Total, vw->vw_SBar.total,
		       PGA_Top, vw->vw_SBar.top,
		       TAG_END);
    }
}
#endif

/*
 * Opens an editor window. Sets up the shared IDCMP
 */
struct Window *
sys_new_window(VW *oldVW, VW *vw, bool useDefDims)
{
    u_short *dimensions;
    struct Screen *scr;
    struct Window *res = NULL;
    bool truedims = FALSE;
    if(!useDefDims && oldVW)
    {
	if(oldVW->vw_Flags & VWFF_SLEEPING)
	    dimensions = oldVW->vw_WindowSys.ws_OldDimensions;
	else
	    dimensions = &oldVW->vw_Window->LeftEdge;
	truedims = TRUE;
    }
    else
	dimensions = def_dims;
    if(scr = LockPubScreen(*VSTR(vw->vw_WindowSys.ws_ScreenName)
			   ? VSTR(vw->vw_WindowSys.ws_ScreenName)
			   : NULL))
    {
#ifndef NOSCRLBAR
	if(vw->vw_SBar.gad = NewObject(NULL, "propgclass",
				       GA_RelRight, -13,
				       GA_Top, 1 + scr->WBorTop + scr->Font->ta_YSize + 1,
				       GA_Width, 10,
				       GA_RelHeight, -12 - (scr->WBorTop + scr->Font->ta_YSize + 1),
				       GA_RelVerify, TRUE,
				       GA_Immediate, TRUE,
				       GA_FollowMouse, TRUE,
				       GA_RightBorder, TRUE,
				       PGA_Borderless, TRUE,
				       PGA_Freedom, FREEVERT,
				       PGA_Total, 1,
				       PGA_Visible, 1,
				       PGA_Top, 0,
				       PGA_NewLook, TRUE,
				       TAG_END))
	{
#endif
	    if(res = OpenWindowTags(NULL,
				    WA_Left, dimensions[0],
				    WA_Top, dimensions[1],
				    WA_InnerWidth, truedims ? dimensions[2] : (dimensions[2] * vw->vw_FontX + 1),
				    WA_InnerHeight, truedims ? dimensions[3] : (dimensions[3] + 1) * vw->vw_FontY + 3,
				    WA_IDCMP, shared_window_port ? 0 : IDCMP,
				    WA_MinWidth, 80,
				    WA_MinHeight, 40,
				    WA_MaxWidth, ~0,
				    WA_MaxHeight, ~0,
#if AMIGA_INCLUDE_VER >= 39
				    WA_Flags, WFLAGS | WFLG_NEWLOOKMENUS,
#else
				    WA_Flags, WFLAGS,
#endif
#ifndef NOSCRLBAR
				    WA_Gadgets, vw->vw_SBar.gad,
#endif
				    WA_Title, window_title,
				    WA_AutoAdjust, TRUE,
				    WA_PubScreen, scr,
				    WA_RptQueue, 2,
				    TAG_END))
	    {
		if(shared_window_port)
		{
		    res->UserPort = shared_window_port;
		    ModifyIDCMP(res, IDCMP);
		}
		else
		    shared_window_port = res->UserPort;
		vw->vw_Window = res;
		ami_set_menu(res);
		vw->vw_WindowSys.ws_Rp = res->RPort;
		res->UserData = (BYTE *)vw;
		SetFont(vw->vw_WindowSys.ws_Rp, vw->vw_Font);
		SET_WRITE_MASK(vw->vw_WindowSys.ws_Rp, 1);
	    }
#ifndef NOSCRLBAR
	    else
		DisposeObject(vw->vw_SBar.gad);
	}
#endif
	UnlockPubScreen(NULL, scr);
    }
    return(res);
}
void
sys_kill_window(VW *vw)
{
    ami_clear_menu(vw->vw_Window);
    close_shared_window(vw->vw_Window);
#ifndef NOSCRLBAR
    if(((vw->vw_Flags & VWFF_SLEEPING) == 0) && vw->vw_SBar.gad)
    {
	DisposeObject(vw->vw_SBar.gad);
	vw->vw_SBar.gad = NULL;
    }
#endif
}

/*
 * Code to close a window who's sharing a UserPort -- taken from
 * CloseWindow() autodoc.
 */
static void
close_shared_window(struct Window *win)
{
    ami_clear_menu(win);
    if(window_count > 1)
    {
	Forbid();
	strip_intui_messages(win->UserPort, win);
	win->UserPort = NULL;
	ModifyIDCMP(win, 0L);
	Permit();
    }
    else
	shared_window_port = NULL;
    CloseWindow(win);
}

static void
strip_intui_messages(struct MsgPort *mp, struct Window *win)
{
    struct IntuiMessage *msg;
    struct Node *succ;

    msg = (struct IntuiMessage *)mp->mp_MsgList.lh_Head;
    while(succ = msg->ExecMessage.mn_Node.ln_Succ)
    {
	if(msg->IDCMPWindow == win)
	{
	    Remove((struct Node *)msg);
	    ReplyMsg((struct Message *)msg);
	}
	msg = (struct IntuiMessage *)succ;
    }
}

/*
 * Gets the next Imsg for the specified window -- this is used for event
 * loops local to a single window
 */
struct IntuiMessage *
ami_get_win_imsg(struct Window *win)
{
    struct IntuiMessage *msg;
    struct Node *succ;

    Forbid();
    msg = (struct IntuiMessage *)win->UserPort->mp_MsgList.lh_Head;
    while(succ = msg->ExecMessage.mn_Node.ln_Succ)
    {
	if(msg->IDCMPWindow == win)
	{
	    Remove((struct Node *)msg);
	    Permit();
	    return(msg);
	}
	msg = (struct IntuiMessage *)succ;
    }
    Permit();
    return(FALSE);
}

void
sys_activate_win(VW *vw)
{
    WindowToFront(vw->vw_Window);
    ActivateWindow(vw->vw_Window);
}

void
sys_set_vw_pos(VW *vw, long x, long y, long w, long h)
{
    if(vw->vw_Flags & VWFF_SLEEPING)
    {
	vw->vw_WindowSys.ws_OldDimensions[0] = x;
	vw->vw_WindowSys.ws_OldDimensions[1] = y;
	vw->vw_WindowSys.ws_OldDimensions[2] = w;
	vw->vw_WindowSys.ws_OldDimensions[3] = h;
    }
    else
    {
	ChangeWindowBox(vw->vw_Window, x, y, w, h);
	vw->vw_DeferRefresh = 1;
    }
}

static struct EasyStruct ezstruct =
{
    sizeof(struct EasyStruct),
    0,
    "Jade request...",
    NULL,
    NULL,
};

int
ami_ezreq(u_char *bodyFmt, u_char *gadFmt, long arg1, ...)
{
    ezstruct.es_TextFormat = bodyFmt;
    ezstruct.es_GadgetFormat = gadFmt;
    return(EasyRequestArgs(curr_vw->vw_Window, &ezstruct, NULL, &arg1));
}

_PR VALUE cmd_req(VALUE body, VALUE gads);
DEFUN("req", cmd_req, subr_req, (VALUE body, VALUE gads), V_Subr2, DOC_req) /*
::doc:req::
req BODY-STRING GADGETS-STRING
<AMIGA-ONLY>

Function to do a request.
Result is number of gadget pressed (starting at 1 for leftmost gadget and
incrementing by one, rightmost gadget is 0.
::end:: */
{
    DECLARE1(body, STRINGP);
    DECLARE2(gads, STRINGP);
    return(make_number(ami_ezreq(VSTR(body), VSTR(gads), 0)));
}

int
sys_set_font(VW *vw)
{
    struct TextAttr ta;
    struct TextFont *tf;

    ta.ta_Name = VSTR(vw->vw_FontName);
    ta.ta_YSize = vw->vw_WindowSys.ws_FontSize;
    ta.ta_Style = FS_NORMAL;
    ta.ta_Flags = 0;

    if(tf = OpenDiskFont(&ta))
    {
	if(tf->tf_Flags & FPF_PROPORTIONAL)
	{
	    CloseFont(tf);
	    return(FALSE);
	}
	if(vw->vw_Font)
	    CloseFont(vw->vw_Font);
	vw->vw_Font = tf;
	vw->vw_FontX = vw->vw_Font->tf_XSize;
	vw->vw_FontY = vw->vw_Font->tf_YSize;
	if(vw->vw_Window)
	{
	    SetFont(vw->vw_WindowSys.ws_Rp, tf);
	    sys_update_dimensions(vw);
	}
	return(TRUE);
    }
    return(FALSE);
}

void
sys_unset_font(VW *vw)
{
    if(vw->vw_Font)
    {
	CloseFont(vw->vw_Font);
	vw->vw_Font = NULL;
    }
}

_PR VALUE cmd_set_font(VALUE font, VALUE vw);
DEFUN_INT("set-font", cmd_set_font, subr_set_font, (VALUE font, VALUE vw), V_Subr2, DOC_set_font, "sFont name (`FONT_NAME-SIZE'): ")
{
    VALUE oldfont, newfont;
    LONG oldsize, newsize;
    u_char *tmp;
    DECLARE1(font, STRINGP);
    if(!WINDOWP(vw))
	vw = VAL(curr_vw);
    oldfont = VWIN(vw)->vw_FontName;
    oldsize = VWIN(vw)->vw_WindowSys.ws_FontSize;
    tmp = strrchr(VSTR(font), '-');
    if(!tmp)
	return(cmd_signal(sym_error, list_2(MKSTR("No font-size specified"),
					    font)));
    newsize = atol(tmp+1);
    newfont = string_dupn(VSTR(font), tmp - VSTR(font));
    VWIN(vw)->vw_WindowSys.ws_FontSize = newsize;
    VWIN(vw)->vw_FontName = newfont;
    if(sys_set_font(VWIN(vw)))
    {
	VWIN(vw)->vw_Flags |= VWFF_FORCE_REFRESH;
	return(sym_t);
    }
    else
    {
	cmd_signal(sym_error, list_2(MKSTR("Can't open font"), font));
	VWIN(vw)->vw_FontName = oldfont;
	VWIN(vw)->vw_WindowSys.ws_FontSize = oldsize;
	return(NULL);
    }
}

_PR VALUE var_pub_screen(VALUE val);
DEFUN("pub-screen", var_pub_screen, subr_pub_screen, (VALUE val), V_Var, DOC_pub_screen) /*
::doc:pub_screen::
This variable controls the name of the public-screen which the window
opens onto. It is only available when running on an Amiga.
::end:: */
{
    VW *vw = curr_vw;
    if(val)
    {
	if(STRINGP(val))
	    vw->vw_WindowSys.ws_ScreenName = val;
	return(val);
    }
    else
	return(vw->vw_WindowSys.ws_ScreenName);
}

/*
 * Used when the name of a TX is changed. All sleeping views have their title
 * display updated.
 */
void
sys_reset_sleep_titles(TX *tx)
{
    VW *vw = view_chain;
    while(vw)
    {
	if(vw->vw_Window && (vw->vw_Tx == tx)
	   && (vw->vw_Flags & VWFF_SLEEPING))
	{
	    SetWindowTitles(vw->vw_Window, VSTR(tx->tx_BufferName),
			    (UBYTE *)~0);
	}
	vw = vw->vw_Next;
    }
}

_PR VALUE cmd_screen_width(void);
DEFUN("screen-width", cmd_screen_width, subr_screen_width, (void), V_Subr0, DOC_screen_width)
{
    return(make_number(curr_vw->vw_Window->WScreen->Width));
}

_PR VALUE cmd_screen_height(void);
DEFUN("screen-height", cmd_screen_height, subr_screen_height, (void), V_Subr0, DOC_screen_height)
{
    return(make_number(curr_vw->vw_Window->WScreen->Height));
}

_PR VALUE cmd_window_left_edge(void);
DEFUN("window-left-edge", cmd_window_left_edge, subr_window_left_edge, (void), V_Subr0, DOC_window_left_edge)
{
    return(make_number(curr_vw->vw_Window->LeftEdge));
}

_PR VALUE cmd_window_top_edge(void);
DEFUN("window-top-edge", cmd_window_top_edge, subr_window_top_edge, (void), V_Subr0, DOC_window_top_edge)
{
    return(make_number(curr_vw->vw_Window->TopEdge));
}

_PR VALUE cmd_window_width(void);
DEFUN("window-width", cmd_window_width, subr_window_width, (void), V_Subr0, DOC_window_width)
{
    return(make_number(curr_vw->vw_Window->Width));
}

_PR VALUE cmd_window_height(void);
DEFUN("window-height", cmd_window_height, subr_window_height, (void), V_Subr0, DOC_window_height)
{
    return(make_number(curr_vw->vw_Window->Height));
}

_PR VALUE cmd_window_bar_height(void);
DEFUN("window-bar-heigth", cmd_window_bar_height, subr_window_bar_height, (void), V_Subr0, DOC_window_bar_height)
{
    return(make_number((LONG)curr_vw->vw_Window->BorderTop));
}

bool
sys_get_mouse_pos(POS *res, VW *vw)
{
    struct Window *wd = vw->vw_Window;
    WORD x = wd->MouseX;
    WORD y = wd->MouseY;
    if(((x < wd->Width) && (x > (wd->Width - wd->BorderRight)))
    || ((y > 0) && (y < wd->BorderTop)))
    {
	*res = vw->vw_CursorPos;
	return(TRUE);
    }
    if(x >= wd->Width)
	res->pos_Col = vw->vw_StartCol + vw->vw_MaxX + 1;
    else if(x <= 0)
	res->pos_Col = vw->vw_StartCol - 1;
    else if(x < (wd->Width - wd->BorderRight))
	res->pos_Col = vw->vw_StartCol + ((x - (WORD)vw->vw_XStartPix) / (WORD)vw->vw_FontX);

    if(y >= wd->Height)
	res->pos_Line = vw->vw_StartLine + vw->vw_MaxY;
    else if(y <= 0)
	res->pos_Line = vw->vw_StartLine - 1;
    else if(y > wd->BorderTop)
	res->pos_Line = vw->vw_StartLine + ((y - (WORD)vw->vw_YStartPix) / (WORD)vw->vw_FontY);

    if(res->pos_Col < 0)
	res->pos_Col = 0;
    if(res->pos_Line < 0)
	res->pos_Line = 0;
    if(res->pos_Line >= vw->vw_Tx->tx_NumLines)
	res->pos_Line = vw->vw_Tx->tx_NumLines - 1;
    res->pos_Col = char_col(vw->vw_Tx, res->pos_Col, res->pos_Line);
    return(TRUE);
}

void
sys_windows_init(void)
{
    ADD_SUBR(subr_req);
    ADD_SUBR(subr_set_font);
    ADD_SUBR(subr_pub_screen);
    ADD_SUBR(subr_screen_width);
    ADD_SUBR(subr_screen_height);
    ADD_SUBR(subr_window_left_edge);
    ADD_SUBR(subr_window_top_edge);
    ADD_SUBR(subr_window_width);
    ADD_SUBR(subr_window_height);
    ADD_SUBR(subr_window_bar_height);
}
