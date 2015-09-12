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
		     | VisibilityChangeMask | PointerMotionMask \
		     | PointerMotionHintMask

static XSizeHints size_hints;
static XClassHint class_hints = { "jade", "Editor" };
static XWMHints wm_hints;

/* If non-null when opening a window, it's opened on this display */
static struct x11_display *pending_display;

/* Let the window-manager handle all iconifying... */
bool
sys_sleep_win(Lisp_Window *w)
{
    if((w->car & WINFF_SLEEPING) == 0)
    {
	XIconifyWindow(WINDOW_XDPY(w)->display, w->w_Window,
		       WINDOW_XDPY(w)->screen);
	w->car |= WINFF_SLEEPING;
    }
    return(true);
}

bool
sys_unsleep_win(Lisp_Window *w)
{
    if(w->car & WINFF_SLEEPING)
    {
	/* Does this work?? */
	wm_hints.flags |= StateHint;
	wm_hints.initial_state = IconicState;
	XSetWMHints(WINDOW_XDPY(w)->display, w->w_Window, &wm_hints);
	XMapWindow(WINDOW_XDPY(w)->display, w->w_Window);
	w->car &= ~WINFF_SLEEPING;
    }
    return(true);
}

void
sys_update_dimensions(Lisp_Window *w)
{
    XWindowAttributes xwa;
    XGetWindowAttributes(WINDOW_XDPY(w)->display, w->w_Window, &xwa);
    x11_update_dimensions(w, xwa.width, xwa.height);
}

void
x11_update_dimensions(Lisp_Window *w, int width, int height)
{
    if(w->w_Window && ((w->car & WINFF_SLEEPING) == 0))
    {
	w->pixel_left = 0;
	w->pixel_top = 0;
	w->pixel_right = width;
	w->pixel_bottom = height;
	w->pixel_width = w->pixel_right - w->pixel_left;
	w->pixel_height = w->pixel_bottom - w->pixel_top;
    }
}

#ifdef HAVE_X11_XFT_XFT_H
static void
xcolor_to_xftcolor (XColor *xc, XftColor *xfc)
{
    xfc->pixel = xc->pixel;
    xfc->color.red = xc->red;
    xfc->color.green = xc->green;
    xfc->color.blue = xc->blue;
    xfc->color.alpha = 65535;
}
#endif

/* The only thing necessary in `vw' is the font stuff (I think) */
Window
sys_new_window(Lisp_Window *oldW, Lisp_Window *w, int *dims)
{
    unsigned int x, y, width, height;
    Window win;
    struct x11_display *dpy;
    repv face;
    struct x11_color *bg = 0, *fg = 0;

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

    width = w->font_width * width;
    height = (w->font_height * (height + 2));

    face = Fsymbol_value(Qdefault_face, Qt);
    if(FACEP(face))
    {
	fg = x11_get_color_dpy(VCOLOR(VFACE(face)->foreground), dpy);
	bg = x11_get_color_dpy(VCOLOR(VFACE(face)->background), dpy);
    }

    {
	XSetWindowAttributes wa;

	if (x11_opt_reverse_video)
	{
	    if (fg != 0)
		wa.background_pixel = fg->color.pixel;
	    if (bg != 0)
		wa.border_pixel = bg->color.pixel;
	}
	else
	{
	    if (bg != 0)
		wa.background_pixel = bg->color.pixel;
	    if (fg != 0)
		wa.border_pixel = fg->color.pixel;
	}

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
	long gcmask = 0;

	w->w_Window = win;
	WINDOW_XDPY(w) = dpy;
	dpy->window_count++;

	w->window_system.ws_GC_values.line_width = 0;
	w->window_system.ws_GC_values.foreground = fg->color.pixel;
	w->window_system.ws_GC_values.background = bg->color.pixel;
	gcmask = GCForeground | GCBackground | GCLineWidth;
#ifndef HAVE_X11_XFT_XFT_H
	w->window_system.ws_GC_values.font = w->window_system.ws_Font->fid;
	gcmask |= GCFont;
#else
	w->window_system.ws_XftDraw = XftDrawCreate (dpy->display, w->w_Window,
						   dpy->visual, dpy->colormap);
#endif
	w->window_system.ws_GC = XCreateGC(dpy->display, w->w_Window,
					 gcmask, &w->window_system.ws_GC_values);
#ifdef HAVE_X11_XFT_XFT_H
	xcolor_to_xftcolor (&fg->color, &w->window_system.ws_XftColor);
#endif
	size_hints.x = x,
	size_hints.y = y,
	size_hints.width = width,
	size_hints.height = height,
	size_hints.base_width = 0;
	size_hints.base_height = 0;
	size_hints.width_inc = w->font_width;
	size_hints.height_inc = w->font_height;
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
    return false;
}

void
sys_kill_window(Lisp_Window *w)
{
    x11_window_lose_selections(w);
    XFreeGC(WINDOW_XDPY(w)->display, w->window_system.ws_GC);
#ifdef HAVE_X11_XFT_XFT_H
    XftDrawDestroy (w->window_system.ws_XftDraw);
#endif
    XDestroyWindow(WINDOW_XDPY(w)->display, w->w_Window);
    if(--(WINDOW_XDPY(w)->window_count) == 0)
	x11_close_display(WINDOW_XDPY(w));
    WINDOW_XDPY(w) = 0;
}

void
sys_activate_win(Lisp_Window *w)
{
    /* Not sure about all this??  */
    XRaiseWindow(WINDOW_XDPY(w)->display, w->w_Window);
    XWarpPointer(WINDOW_XDPY(w)->display, None, w->w_Window, 0, 0, 0, 0, 1, 1);
}

void
sys_set_win_pos(Lisp_Window *win, long x, long y, long w, long h)
{
    XMoveResizeWindow(WINDOW_XDPY(win)->display, win->w_Window,
		      (unsigned int)x, (unsigned int)y,
		      (unsigned int)w, (unsigned int)h);
}

void
sys_set_win_name(Lisp_Window *win, const char *name)
{
    XStoreName(WINDOW_XDPY(win)->display, win->w_Window, name);
}

Lisp_Window *
x11_find_window(Window win)
{
    Lisp_Window *w = win_chain;
    while(w)
    {
	if(w->w_Window == win)
	    break;
	w = w->next;
    }
    return(w);
}

static inline void
face_to_gc(Lisp_Window *w, Merged_Face *f, bool invert)
{
    unsigned long mask = 0;
    struct x11_color *c;
#ifndef HAVE_X11_XFT_XFT_H
    Font fid;
#endif

    if(f->car & FACEFF_INVERT)
	invert = !invert;

    c = x11_get_color_dpy(VCOLOR(invert ? f->background : f->foreground),
			  WINDOW_XDPY(w));
    if(c != 0)
    {
	if(w->window_system.ws_GC_values.foreground != c->color.pixel)
	    w->window_system.ws_GC_values.foreground = c->color.pixel;
	mask |= GCForeground;
#ifdef HAVE_X11_XFT_XFT_H
	xcolor_to_xftcolor (&c->color, &w->window_system.ws_XftColor);
#endif
    }

    c = x11_get_color_dpy(VCOLOR(invert ? f->foreground : f->background),
			  WINDOW_XDPY(w));
    if(c != 0)
    {
	if(w->window_system.ws_GC_values.background != c->color.pixel)
	    w->window_system.ws_GC_values.background = c->color.pixel;
	mask |= GCBackground;
    }

#ifndef HAVE_X11_XFT_XFT_H
    if((f->car & FACEFF_BOLD) && w->window_system.ws_BoldFont)
	fid = w->window_system.ws_BoldFont->fid;
    else
	fid = w->window_system.ws_Font->fid;
    if(w->window_system.ws_GC_values.font != fid)
    {
	w->window_system.ws_GC_values.font = fid;
	mask |= GCFont;
    }
#else
    if((f->car & FACEFF_BOLD) && w->window_system.ws_BoldFont)
	w->window_system.ws_XftFont = w->window_system.ws_BoldFont;
    else
	w->window_system.ws_XftFont = w->window_system.ws_Font;
#endif

    /* FIXME: italic?! */

    if(mask != 0)
    {
	XChangeGC(WINDOW_XDPY(w)->display, w->window_system.ws_GC,
		  mask, &w->window_system.ws_GC_values);
    }
}

void
sys_draw_glyphs(Lisp_Window *w, int col, int row, uint8_t attr, char *str,
		int len, bool all_spaces)
{
    bool invert = false;
    Merged_Face *f;
    int x, y;

    assert(attr <= GA_LastFace);

    f = &w->merged_faces[attr];
    if(!f->valid)
	return;
    
    x = w->pixel_left + w->font_width * col;
    y = w->pixel_top + w->font_height * row;
    if(!all_spaces)
    {
#ifndef HAVE_X11_XFT_XFT_H
	face_to_gc(w, f, invert);
	XDrawImageString(WINDOW_XDPY(w)->display, w->w_Window,
			 w->window_system.ws_GC,
			 x, y + w->window_system.ws_Font->ascent, str, len);
#else
	face_to_gc(w, f, !invert);
	XFillRectangle (WINDOW_XDPY (w)->display, w->w_Window,
			w->window_system.ws_GC, x, y,
			w->font_width * len, w->font_height);
	face_to_gc(w, f, invert);
	XftDrawString8 (w->window_system.ws_XftDraw,
			&w->window_system.ws_XftColor,
			w->window_system.ws_XftFont,
			x, y + w->window_system.ws_Font->ascent,
			(XftChar8 *) str, len);
#endif
    }
    else
    {
	face_to_gc(w, f, !invert);
	XFillRectangle(WINDOW_XDPY(w)->display, w->w_Window,
		       w->window_system.ws_GC, x, y,
		       len * w->font_width, w->font_height);
    }

    if(f->car & FACEFF_UNDERLINE)
    {
	if(all_spaces)
	    face_to_gc(w, f, invert);
	XDrawLine(WINDOW_XDPY(w)->display, w->w_Window,
		  w->window_system.ws_GC,
		  x, y + w->window_system.ws_Font->ascent + 1,
		  x + len * w->font_width - 1,
		  y + w->window_system.ws_Font->ascent + 1);
    }

    if(f->car & FACEFF_BOXED)
    {
	int i;
	if(all_spaces)
	    face_to_gc(w, f, invert);
	for(i = 0; i < len; i++)
	{
	    XDrawRectangle(WINDOW_XDPY(w)->display, w->w_Window,
			   w->window_system.ws_GC, x, y,
			   w->font_width - 1, w->font_height - 1);
	    x += w->font_width;
	}
    }
}

bool
sys_set_font(Lisp_Window *w)
{
#ifndef HAVE_X11_XFT_XFT_H
    XFontStruct *font;
#else
    XftFont *font;
#endif

    struct x11_display *dpy;
    if(WINDOW_XDPY(w) != 0)
	dpy = WINDOW_XDPY(w);
    else if(pending_display != 0)
	dpy = pending_display;
    else
	dpy = x11_display_list;

#ifndef HAVE_X11_XFT_XFT_H
    if((font = XLoadQueryFont(dpy->display, rep_STR(w->font_name)))
       || (font = XLoadQueryFont(dpy->display, DEFAULT_FONT)))
#else
    if((font = XftFontOpenName(dpy->display, 0, rep_STR(w->font_name)))
       || (font = XftFontOpenName(dpy->display, 0, DEFAULT_FONT)))
#endif
    {
	if(w->window_system.ws_Font)
	{
#ifndef HAVE_X11_XFT_XFT_H
	    XFreeFont(dpy->display, w->window_system.ws_Font);
#else
	    XftFontClose (dpy->display, w->window_system.ws_Font);
#endif
	}
	if(w->window_system.ws_BoldFont)
	{
#ifndef HAVE_X11_XFT_XFT_H
	    XFreeFont(dpy->display, w->window_system.ws_BoldFont);
#else
	    XftFontClose (dpy->display, w->window_system.ws_BoldFont);
#endif
	    w->window_system.ws_BoldFont = 0;
	}
	w->window_system.ws_Font = font;

#ifndef HAVE_X11_XFT_XFT_H
	w->font_width = XTextWidth(font, "M", 1);
#else
	{ XGlyphInfo info;
	  XftTextExtents8 (dpy->display, font, (XftChar8 *) "M", 1, &info);
	  w->font_width = info.xOff; }
#endif
	w->font_height = font->ascent + font->descent;
	if(w->w_Window)
	{
	    int width, height;
	    width = w->column_count * w->font_width;
	    height = w->row_count * w->font_height;
#ifndef HAVE_X11_XFT_XFT_H
	    XSetFont(dpy->display, w->window_system.ws_GC, font->fid);
#else
	    w->window_system.ws_XftFont = font;
#endif
	    sys_update_dimensions(w);
	    size_hints.width = width;
	    size_hints.height = height;
	    size_hints.base_width = 0;
	    size_hints.base_height = w->font_height;
	    size_hints.width_inc = w->font_width;
	    size_hints.height_inc = w->font_height;
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
#ifndef HAVE_X11_XFT_XFT_H
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
		    w->window_system.ws_BoldFont
			= XLoadQueryFont(dpy->display, buf);
		}
		XFree(name);
	    }
#else
	    char buf[1024];
	    sprintf (buf, "%s:bold", rep_STR (w->font_name));
	    w->window_system.ws_BoldFont = XftFontOpenName(dpy->display, 0, buf);
#endif
	}
	return true;
    }
    return false;
}

void
sys_unset_font(Lisp_Window *w)
{
    if(WINDOW_XDPY(w) != 0)
    {
	if(w->window_system.ws_Font)
	{
#ifndef HAVE_X11_XFT_XFT_H
	    XFreeFont(WINDOW_XDPY(w)->display, w->window_system.ws_Font);
#else
	    XftFontClose (WINDOW_XDPY(w)->display, w->window_system.ws_Font);
#endif
	    w->window_system.ws_Font = NULL;
	}
	if(w->window_system.ws_BoldFont)
	{
#ifndef HAVE_X11_XFT_XFT_H
	    XFreeFont(WINDOW_XDPY(w)->display, w->window_system.ws_BoldFont);
#else
	    XftFontClose (WINDOW_XDPY(w)->display, w->window_system.ws_BoldFont);
#endif
	    w->window_system.ws_BoldFont = NULL;
	}
    }
}

bool
sys_deleting_window_would_exit (Lisp_Window *w)
{
    return (x11_display_list->next == 0
	    && x11_display_list->window_count == 1);
}

/* Now this returns the glyph position in the window of the cursor;
   it doesn't worry about the views in the window. */
repv
sys_get_mouse_pos(Lisp_Window *w)
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
	    return make_pos((x - w->pixel_left) / w->font_width,
			    (y - w->pixel_top) / w->font_height);
	}
	else
	    return 0;
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
