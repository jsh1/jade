/* x11_windows.c -- Window handling for X11
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

#include <string.h>
#include <X11/Xutil.h>

_PR int sys_sleep_win(WIN *);
_PR int sys_unsleep_win(WIN *);
_PR void sys_new_vw(VW *);
_PR void sys_kill_vw(VW *);
_PR void sys_update_dimensions(WIN *);
_PR void x11_update_dimensions(WIN *, int, int);
_PR Window sys_new_window(WIN *, WIN *, bool);
_PR void sys_kill_window(WIN *);
_PR void sys_activate_win(WIN *);
_PR void sys_set_win_pos(WIN *, long, long, long, long);
_PR WIN *x11_find_window(Window);
_PR int sys_set_font(WIN *);
_PR void sys_unset_font(WIN *);
_PR void sys_reset_sleep_titles(TX *);
_PR VALUE sys_get_mouse_pos(WIN *);
_PR void sys_windows_init(void);

#define INPUT_EVENTS ButtonPressMask | ButtonReleaseMask | KeyPressMask \
		     | ExposureMask | StructureNotifyMask | FocusChangeMask \
		     | ButtonMotionMask | PointerMotionHintMask

static XSizeHints size_hints;
static XClassHint class_hints = { "jade", "Editor" };
static XWMHints wm_hints;

/* If non-null when opening a window, it's opened on this display */
static struct x11_display *pending_display;

/* Let the window-manager handle all iconifying... */
int
sys_sleep_win(WIN *w)
{
    if((w->w_Flags & WINFF_SLEEPING) == 0)
    {
	XIconifyWindow(WINDOW_XDPY(w)->display, w->w_Window,
		       WINDOW_XDPY(w)->screen);
	w->w_Flags |= WINFF_SLEEPING;
    }
    return(TRUE);
}

int
sys_unsleep_win(WIN *w)
{
    if(w->w_Flags & WINFF_SLEEPING)
    {
	/* Does this work?? */
	wm_hints.flags |= StateHint;
	wm_hints.initial_state = IconicState;
	XSetWMHints(WINDOW_XDPY(w)->display, w->w_Window, &wm_hints);
	XMapWindow(WINDOW_XDPY(w)->display, w->w_Window);
	w->w_Flags &= ~WINFF_SLEEPING;
    }
    return(TRUE);
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
sys_update_dimensions(WIN *w)
{
    XWindowAttributes xwa;
    XGetWindowAttributes(WINDOW_XDPY(w)->display, w->w_Window, &xwa);
    x11_update_dimensions(w, xwa.width, xwa.height);
}

void
x11_update_dimensions(WIN *w, int width, int height)
{
    if(w->w_Window && ((w->w_Flags & WINFF_SLEEPING) == 0))
    {
	w->w_LeftPix = 0;
	w->w_TopPix = 0;
	w->w_RightPix = width;
	w->w_BottomPix = height;
	w->w_WidthPix = w->w_RightPix - w->w_LeftPix;
	w->w_HeightPix = w->w_BottomPix - w->w_TopPix;
    }
}

/* The only thing necessary in `vw' is the font stuff (I think) */
Window
sys_new_window(WIN *oldW, WIN *w, bool useDefDims)
{
    unsigned int x, y, width, height;
    Window win;
    struct x11_display *dpy;

    if(pending_display != 0)
	dpy = pending_display;
    else if(oldW != 0)
	dpy = WINDOW_XDPY(oldW);
    else
	dpy = x11_display_list;

    size_hints.flags = 0;
    if(!useDefDims && oldW)
    {
	x = y = 0;
	width = w->w_FontX * oldW->w_MaxX;
	height = w->w_FontY * oldW->w_MaxY;
	size_hints.flags |= PPosition | PSize;
    }
    else
    {
	if(def_dims[0] != -1)
	{
	    x = def_dims[0];
	    size_hints.flags |= USPosition;
	}
	else
	    x = 0;
	if(def_dims[1] != -1)
	{
	    y = def_dims[1];
	    size_hints.flags |= USPosition;
	}
	else
	    y = 0;
	if(def_dims[2] != -1)
	{
	    width = def_dims[2];
	    size_hints.flags |= USSize;
	}
	else
	    width = 80;
	if(def_dims[3] != -1)
	{
	    height = def_dims[3];
	    size_hints.flags |= USSize;
	}
	else
	    height = 24;
	width = w->w_FontX * width;
	height = (w->w_FontY * (height + 2));
    }
    win = XCreateSimpleWindow(dpy->display,
			      DefaultRootWindow(dpy->display),
			      x, y, width, height,
			      1, dpy->fore_pixel, dpy->back_pixel);
    if(win)
    {
	XGCValues xgcv;
	w->w_Window = win;
	WINDOW_XDPY(w) = dpy;
	dpy->window_count++;

	xgcv.foreground = dpy->fore_pixel;
	xgcv.background = dpy->back_pixel;
	xgcv.line_width = 1;
	xgcv.font = w->w_WindowSys.ws_Font->fid;
	w->w_WindowSys.ws_GC_array[P_TEXT] = XCreateGC(dpy->display,
						       w->w_Window,
						       GCForeground
						       | GCBackground
						       | GCLineWidth
						       | GCFont,
						       &xgcv);
	xgcv.foreground = dpy->back_pixel;
	xgcv.background = dpy->fore_pixel;
	w->w_WindowSys.ws_GC_array[P_TEXT_RV] = XCreateGC(dpy->display,
							  w->w_Window,
							  GCForeground
							  | GCBackground
							  | GCLineWidth
							  | GCFont,
							  &xgcv);
	xgcv.foreground = dpy->fore_pixel;
	xgcv.background = dpy->high_pixel;
	w->w_WindowSys.ws_GC_array[P_BLOCK] = XCreateGC(dpy->display,
							w->w_Window,
							GCForeground
							| GCBackground
							| GCLineWidth
							| GCFont,
							&xgcv);
	xgcv.foreground = dpy->high_pixel;
	xgcv.background = dpy->fore_pixel;
	w->w_WindowSys.ws_GC_array[P_BLOCK_RV] = XCreateGC(dpy->display,
							   w->w_Window,
							   GCForeground
							   | GCBackground
							   | GCLineWidth
							   | GCFont,
							   &xgcv);
	size_hints.x = x,
	size_hints.y = y,
	size_hints.width = width,
	size_hints.height = height,
	size_hints.base_width = 0;
	size_hints.base_height = w->w_FontY + 3;
	size_hints.width_inc = w->w_FontX;
	size_hints.height_inc = w->w_FontY;
	size_hints.min_width = size_hints.base_width + size_hints.width_inc;
	size_hints.min_height = size_hints.base_height + size_hints.height_inc;
	size_hints.flags |= PMinSize | PResizeInc | PBaseSize;
	wm_hints.flags = InputHint | StateHint;
	wm_hints.input = True;
	wm_hints.initial_state = NormalState;
	XSetWMProperties(dpy->display, win, NULL, NULL,
			 x11_argv, x11_argc, &size_hints, &wm_hints,
			 &class_hints);
	XStoreName(dpy->display, win, "Jade");
	XSetWMProtocols(dpy->display, win, &dpy->wm_delete_window, 1);
	XSelectInput(dpy->display, win, INPUT_EVENTS);
	XMapWindow(dpy->display, win);
	XDefineCursor(dpy->display, win, dpy->text_cursor);
	return win;
    }
    return FALSE;
}

void
sys_kill_window(WIN *w)
{
    int i;
    x11_window_lose_selections(w);
    for(i = 0; i < P_MAX; i++)
	XFreeGC(WINDOW_XDPY(w)->display, w->w_WindowSys.ws_GC_array[i]);
    XDestroyWindow(WINDOW_XDPY(w)->display, w->w_Window);
    if(--(WINDOW_XDPY(w)->window_count) == 0)
	x11_close_display(WINDOW_XDPY(w));
    WINDOW_XDPY(w) = 0;
}

void
sys_activate_win(WIN *w)
{
    /* Not sure about all this??  */
    XRaiseWindow(WINDOW_XDPY(w)->display, w->w_Window);
    XWarpPointer(WINDOW_XDPY(w)->display, None, w->w_Window, 0, 0, 0, 0, 1, 1);
}

void
sys_set_win_pos(WIN *win, long x, long y, long w, long h)
{
    XMoveResizeWindow(WINDOW_XDPY(win)->display, win->w_Window,
		      (unsigned int)x, (unsigned int)y,
		      (unsigned int)w, (unsigned int)h);
}

WIN *
x11_find_window(Window win)
{
    WIN *w = win_chain;
    while(w)
    {
	if(w->w_Window == win)
	    break;
	w = w->w_Next;
    }
    return(w);
}

int
sys_set_font(WIN *w)
{
    XFontStruct *font;

    struct x11_display *dpy;
    if(WINDOW_XDPY(w) != 0)
	dpy = WINDOW_XDPY(w);
    else if(pending_display != 0)
	dpy = pending_display;
    else
	dpy = x11_display_list;

    if((font = XLoadQueryFont(dpy->display, VSTR(w->w_FontName)))
       || (font = XLoadQueryFont(dpy->display, DEFAULT_FONT)))
    {
	if(w->w_WindowSys.ws_Font)
	    XFreeFont(dpy->display, w->w_WindowSys.ws_Font);
	w->w_WindowSys.ws_Font = font;
	w->w_FontX = XTextWidth(font, "M", 1);
	w->w_FontY = font->ascent + font->descent;
	if(w->w_Window)
	{
	    int i;
	    int width, height;
	    width = w->w_MaxX * w->w_FontX;
	    height = w->w_MaxY * w->w_FontY;
	    for(i = 0; i < P_MAX; i++)
		XSetFont(dpy->display,
			 w->w_WindowSys.ws_GC_array[i], font->fid);
	    sys_update_dimensions(w);
	    size_hints.width = width;
	    size_hints.height = height;
	    size_hints.base_width = 0;
	    size_hints.base_height = w->w_FontY;
	    size_hints.width_inc = w->w_FontX;
	    size_hints.height_inc = w->w_FontY;
	    size_hints.min_width = size_hints.base_width
				   + size_hints.width_inc;
	    size_hints.min_height = size_hints.base_height
				    + size_hints.height_inc;
	    size_hints.flags = PResizeInc | PMinSize | PBaseSize;
	    XSetWMNormalHints(dpy->display, w->w_Window, &size_hints);
	    XResizeWindow(dpy->display, w->w_Window, width, height);
	}
	return TRUE;
    }
    return FALSE;
}

void
sys_unset_font(WIN *w)
{
    if(w->w_WindowSys.ws_Font && WINDOW_XDPY(w) != 0)
    {
	XFreeFont(WINDOW_XDPY(w)->display, w->w_WindowSys.ws_Font);
	w->w_WindowSys.ws_Font = NULL;
    }
}

DEFSTRING(no_font, "Can't open font");

_PR VALUE cmd_set_font(VALUE fontname, VALUE win);
DEFUN_INT("set-font", cmd_set_font, subr_set_font, (VALUE fontname, VALUE win), V_Subr2, DOC_set_font, "sFont name: ") /*
::doc:set_font::
set-font FONT-NAME [WINDOW]

FONT-NAME specifies the font to use in WINDOW (or the active one).
Under X11 FONT-NAME is a standard font description, under AmigaDOS it is the
name of the font followed by a dash and then the point size to use (for
example "topaz.font-8" to get an 8-point topaz font).
::end:: */
{
    VALUE oldfont;
    DECLARE1(fontname, STRINGP);
    if(!WINDOWP(win))
	win = VAL(curr_win);
    oldfont = VWIN(win)->w_FontName;
    VWIN(win)->w_FontName = fontname;
    if(sys_set_font(VWIN(win)))
    {
	VWIN(win)->w_Flags |= WINFF_FORCE_REFRESH;
#if 0
	VWIN(win)->w_DeferRefresh++;
#endif
	return(sym_t);
    }
    else
    {
	cmd_signal(sym_error, list_2(VAL(&no_font), fontname));
	VWIN(win)->w_FontName = oldfont;
	return LISP_NULL;
    }
}

void
sys_reset_sleep_titles(TX *tx)
{
}

/* Now this returns the glyph position in the window of the cursor;
   it doesn't worry about the views in the window. */
VALUE
sys_get_mouse_pos(WIN *w)
{
    if(w != x11_current_event_win)
    {
	Window tmpw;
	int tmp;
	int x, y;
	if(XQueryPointer(WINDOW_XDPY(w)->display, w->w_Window,
			 &tmpw, &tmpw, &tmp, &tmp,
			 &x, &y, &tmp))
	{
	    return make_pos((x - w->w_LeftPix) / w->w_FontX,
			    (y - w->w_TopPix) / w->w_FontY);
	}
	else
	    return LISP_NULL;
    }
    else
	return make_pos(x11_current_mouse_x, x11_current_mouse_y);
}

_PR VALUE cmd_flush_output(void);
DEFUN("flush-output", cmd_flush_output, subr_flush_output, (void), V_Subr0, DOC_flush_output) /*
::doc:flush_output::
flush-output

Forces any cached window output to be drawn. This is usually unnecessary.
::end:: */
{
    struct x11_display *dpy = x11_display_list;
    while(dpy != 0)
    {
	XFlush(dpy->display);
	dpy = dpy->next;
    }
    return sym_t;
}

DEFSTRING(no_display, "Can't open display");
_PR VALUE cmd_make_window_on_display(VALUE display);
DEFUN("make-window-on-display", cmd_make_window_on_display,
      subr_make_window_on_display, (VALUE display), V_Subr1,
      DOC_make_window_on_display) /*
::doc:make_window_on_display::
make-window-on-display DISPLAY-NAME

Create a new window, as with make-window, but opened on the X11 display
called DISPLAY-NAME.
::end:: */
{
    struct x11_display *xdisplay;
    DECLARE1(display, STRINGP);
    xdisplay = x11_open_display(VSTR(display));
    if(xdisplay != 0)
    {
	VALUE win;
	pending_display = xdisplay;
	win = cmd_make_window(sym_nil, sym_nil, sym_nil, sym_nil);
	pending_display = 0;
	return win;
    }
    else
	return cmd_signal(sym_window_error, LIST_2(VAL(&no_display), display));
}

void
sys_windows_init(void)
{
    ADD_SUBR_INT(subr_set_font);
    ADD_SUBR(subr_flush_output);
    ADD_SUBR(subr_make_window_on_display);

    x11_misc_init();
}
