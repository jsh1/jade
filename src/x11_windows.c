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
#include <string.h>
#include <X11/Xutil.h>
#include <assert.h>


#define INPUT_EVENTS ButtonPressMask | ButtonReleaseMask | KeyPressMask \
		     | ExposureMask | StructureNotifyMask | FocusChangeMask \
		     | PointerMotionMask | PointerMotionHintMask

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
sys_new_window(WIN *oldW, WIN *w, short *dims)
{
    unsigned int x, y, width, height;
    Window win;
    struct x11_display *dpy;
    repv face;
    struct x11_color *bg, *fg;

    if(pending_display != 0)
	dpy = pending_display;
    else if(oldW != 0)
	dpy = WINDOW_XDPY(oldW);
    else
	dpy = x11_display_list;

    size_hints.flags = 0;
    if(dims[0] >= 0)
    {
	x = dims[0];
	size_hints.flags |= USPosition;
    }
    else
	x = 0;
    if(dims[1] >= 0)
    {
	y = dims[1];
	size_hints.flags |= USPosition;
    }
    else
	y = 0;
    if(dims[2] > 0)
    {
	width = dims[2];
	size_hints.flags |= USSize;
    }
    else
	width = 80;
    if(dims[3] > 0)
    {
	height = dims[3];
	size_hints.flags |= USSize;
    }
    else
	height = 24;

    width = w->w_FontX * width;
    height = (w->w_FontY * (height + 2));

    face = Fsymbol_value(Qdefault_face, Qt);
    if(FACEP(face))
    {
	fg = x11_get_color_dpy(VCOLOR(VFACE(face)->foreground), dpy);
	bg = x11_get_color_dpy(VCOLOR(VFACE(face)->background), dpy);
    }

    {
	XSetWindowAttributes wa;
	wa.background_pixel = (x11_opt_reverse_video
			       ? fg->color.pixel : bg->color.pixel);
	wa.border_pixel = (x11_opt_reverse_video
			   ? bg->color.pixel : fg->color.pixel);
	wa.colormap = dpy->colormap;
	wa.cursor = dpy->text_cursor;

	win = XCreateWindow(dpy->display, DefaultRootWindow(dpy->display),
			    x, y, width, height, 1, dpy->depth, InputOutput,
			    dpy->visual,
			    CWBackPixel | CWBorderPixel
			    | CWColormap | CWCursor, &wa);
    }

    if(win)
    {
	w->w_Window = win;
	WINDOW_XDPY(w) = dpy;
	dpy->window_count++;

	w->w_WindowSys.ws_GC_values.line_width = 0;
	w->w_WindowSys.ws_GC_values.foreground = fg->color.pixel;
	w->w_WindowSys.ws_GC_values.background = bg->color.pixel;
	w->w_WindowSys.ws_GC_values.font = w->w_WindowSys.ws_Font->fid;
	w->w_WindowSys.ws_GC = XCreateGC(dpy->display, w->w_Window,
					 GCForeground | GCBackground
					 | GCLineWidth | GCFont,
					 &w->w_WindowSys.ws_GC_values);
	size_hints.x = x,
	size_hints.y = y,
	size_hints.width = width,
	size_hints.height = height,
	size_hints.base_width = 0;
	size_hints.base_height = 0;
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
	XSetIconName(dpy->display, win, "jade");
	XSetWMProtocols(dpy->display, win, &dpy->wm_delete_window, 1);
	if(!batch_mode_p ())
	{
	    XSelectInput(dpy->display, win, INPUT_EVENTS);
	    XMapWindow(dpy->display, win);
	}
	return win;
    }
    return FALSE;
}

void
sys_kill_window(WIN *w)
{
    x11_window_lose_selections(w);
    XFreeGC(WINDOW_XDPY(w)->display, w->w_WindowSys.ws_GC);
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

void
sys_set_win_name(WIN *win, char *name)
{
    XStoreName(WINDOW_XDPY(win)->display, win->w_Window, name);
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

static inline void
face_to_gc(WIN *w, Merged_Face *f, bool invert)
{
    unsigned long mask = 0;
    struct x11_color *c;
    Font fid;

    if(f->car & FACEFF_INVERT)
	invert = !invert;

    c = x11_get_color_dpy(VCOLOR(invert ? f->background : f->foreground),
			  WINDOW_XDPY(w));
    if(c != 0)
    {
	if(w->w_WindowSys.ws_GC_values.foreground != c->color.pixel)
	    w->w_WindowSys.ws_GC_values.foreground = c->color.pixel;
	mask |= GCForeground;
    }

    c = x11_get_color_dpy(VCOLOR(invert ? f->foreground : f->background),
			  WINDOW_XDPY(w));
    if(c != 0)
    {
	if(w->w_WindowSys.ws_GC_values.background != c->color.pixel)
	    w->w_WindowSys.ws_GC_values.background = c->color.pixel;
	mask |= GCBackground;
    }

    if((f->car & FACEFF_BOLD) && w->w_WindowSys.ws_BoldFont)
	fid = w->w_WindowSys.ws_BoldFont->fid;
    else
	fid = w->w_WindowSys.ws_Font->fid;
    if(w->w_WindowSys.ws_GC_values.font != fid)
    {
	w->w_WindowSys.ws_GC_values.font = fid;
	mask |= GCFont;
    }

    /* FIXME: italic?! */

    if(mask != 0)
	XChangeGC(WINDOW_XDPY(w)->display, w->w_WindowSys.ws_GC,
		  mask, &w->w_WindowSys.ws_GC_values);
}

void
sys_draw_glyphs(WIN *w, int col, int row, glyph_attr attr, char *str,
		int len, bool all_spaces)
{
    bool invert = FALSE;
    Merged_Face *f;
    int x, y;

    assert(attr <= GA_LastFace);

    f = &w->w_MergedFaces[attr];
    if(!f->valid)
	return;
    
    x = w->w_LeftPix + w->w_FontX * col;
    y = w->w_TopPix + w->w_FontY * row;
    if(!all_spaces)
    {
	face_to_gc(w, f, invert);
	XDrawImageString(WINDOW_XDPY(w)->display, w->w_Window,
			 w->w_WindowSys.ws_GC,
			 x, y + w->w_WindowSys.ws_Font->ascent, str, len);
    }
    else
    {
	face_to_gc(w, f, !invert);
	XFillRectangle(WINDOW_XDPY(w)->display, w->w_Window,
		       w->w_WindowSys.ws_GC, x, y,
		       len * w->w_FontX, w->w_FontY);
    }

    if(f->car & FACEFF_UNDERLINE)
    {
	if(all_spaces)
	    face_to_gc(w, f, invert);
	XDrawLine(WINDOW_XDPY(w)->display, w->w_Window,
		  w->w_WindowSys.ws_GC,
		  x, y + w->w_WindowSys.ws_Font->ascent + 1,
		  x + len * w->w_FontX - 1,
		  y + w->w_WindowSys.ws_Font->ascent + 1);
    }

    if(f->car & FACEFF_BOXED)
    {
	int i;
	if(all_spaces)
	    face_to_gc(w, f, invert);
	for(i = 0; i < len; i++)
	{
	    XDrawRectangle(WINDOW_XDPY(w)->display, w->w_Window,
			   w->w_WindowSys.ws_GC, x, y,
			   w->w_FontX - 1, w->w_FontY - 1);
	    x += w->w_FontX;
	}
    }
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

    if((font = XLoadQueryFont(dpy->display, rep_STR(w->w_FontName)))
       || (font = XLoadQueryFont(dpy->display, DEFAULT_FONT)))
    {
	if(w->w_WindowSys.ws_Font)
	    XFreeFont(dpy->display, w->w_WindowSys.ws_Font);
	if(w->w_WindowSys.ws_BoldFont)
	{
	    XFreeFont(dpy->display, w->w_WindowSys.ws_BoldFont);
	    w->w_WindowSys.ws_BoldFont = 0;
	}
	w->w_WindowSys.ws_Font = font;
	w->w_FontX = XTextWidth(font, "M", 1);
	w->w_FontY = font->ascent + font->descent;
	if(w->w_Window)
	{
	    int width, height;
	    width = w->w_MaxX * w->w_FontX;
	    height = w->w_MaxY * w->w_FontY;
	    XSetFont(dpy->display, w->w_WindowSys.ws_GC, font->fid);
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

	/* Now try to find the bold version. ho ho. */
	{
	    unsigned long value;
	    if(XGetFontProperty(font, XA_FONT, &value))
	    {
		char *name = (char *)XGetAtomName(dpy->display, (Atom)value);
		char *tem = name;
		int dashes = 0;
		while(*tem && dashes != 3)
		{
		    if(*tem++ == '-')
			dashes++;
		}
		if(dashes == 3)
		{
		    /* So the next part of the string should be the weight. */
		    char buf[256];
		    memcpy(buf, name, tem - name);
		    strcpy(buf + (tem - name), "bold");
		    while(*tem)
		    {
			if(*tem++ == '-')
			    break;
		    }
		    strcat(buf, tem - 1);
		    w->w_WindowSys.ws_BoldFont
			= XLoadQueryFont(dpy->display, buf);
		}
		XFree(name);
	    }
	}
	return TRUE;
    }
    return FALSE;
}

void
sys_unset_font(WIN *w)
{
    if(WINDOW_XDPY(w) != 0)
    {
	if(w->w_WindowSys.ws_Font)
	{
	    XFreeFont(WINDOW_XDPY(w)->display, w->w_WindowSys.ws_Font);
	    w->w_WindowSys.ws_Font = NULL;
	}
	if(w->w_WindowSys.ws_BoldFont)
	{
	    XFreeFont(WINDOW_XDPY(w)->display, w->w_WindowSys.ws_BoldFont);
	    w->w_WindowSys.ws_BoldFont = NULL;
	}
    }
}

bool
sys_deleting_window_would_exit (WIN *w)
{
    return (x11_display_list->next == 0
	    && x11_display_list->window_count == 1);
}

/* Now this returns the glyph position in the window of the cursor;
   it doesn't worry about the views in the window. */
repv
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
	    return rep_NULL;
    }
    else
	return make_pos(x11_current_mouse_x, x11_current_mouse_y);
}

DEFUN("flush-output", Fflush_output, Sflush_output, (void), rep_Subr0) /*
::doc:flush-output::
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
    return Qt;
}

DEFSTRING(no_display, "Can't open display");
DEFUN_INT("make-window-on-display", Fmake_window_on_display,
	  Smake_window_on_display, (repv display), rep_Subr1,
	  "sDisplay to open window on:") /*
::doc:make-window-on-display::
make-window-on-display DISPLAY-NAME

Create a new window, as with make-window, but opened on the X11 display
called DISPLAY-NAME.

When called interactively, DISPLAY-NAME is prompted for.
::end:: */
{
    struct x11_display *xdisplay;
    rep_DECLARE1(display, rep_STRINGP);
    xdisplay = x11_open_display(rep_STR(display));
    if(xdisplay != 0)
    {
	repv win;
	pending_display = xdisplay;
	win = Fmake_window(Qnil);
	pending_display = 0;
	return win;
    }
    else
	return Fsignal(Qwindow_error, rep_LIST_2(rep_VAL(&no_display), display));
}

void
sys_windows_init(void)
{
    rep_ADD_SUBR(Sflush_output);
    rep_ADD_SUBR_INT(Smake_window_on_display);

    rep_test_int_fun = x11_handle_async_input;

    x11_misc_init();
}
