/* x11_windowsys.h -- X11 window-system data and macros
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

#ifndef _X11_WINDOWSYS_H
#define _X11_WINDOWSYS_H

/* Per display data */
struct x11_display {
    struct x11_display *next;

    Display *display;
    int screen;

    /* Number of windows open */
    int window_count;

    /* Interned atoms */
    Atom wm_delete_window, jade_selection;

    /* Mouse cursor for the window */
    Cursor text_cursor;

    /* Modifier code of the Meta key in this display */
    u_long meta_mod;

    /* Data for testing double-clicks */
    Time last_click;
    u_long last_click_button;
};

typedef struct {
    struct x11_display *ws_Display;
    Window		ws_Window;
    XFontStruct	       *ws_Font;
    XFontStruct	       *ws_BoldFont;	/* or null */
    GC			ws_GC;
    XGCValues		ws_GC_values;
    int			ws_Width, ws_Height;
    int			ws_HasFocus;
} W_WindowSys;

#define w_Window	w_WindowSys.ws_Window
#define WINDOW_NIL	(0)

#define WINDOW_XDPY(w)	((w)->w_WindowSys.ws_Display)
#define WINDOW_META(w)  (WINDOW_XDPY(w)->meta_mod)
#define WINDOW_HAS_FOCUS(w) ((w)->w_WindowSys.ws_HasFocus)

struct x11_color {
    struct x11_color *next;
    struct x11_display *dpy;
    XColor color;
};

#define SYS_COLOR_TYPE struct x11_color *

#if 0
typedef struct {
    /* ... */
} ScrollBar;
#endif

/* Macros for drawing operations. These are used in redisplay.c for
   system-independent rendering. */

#define SYS_DRAW_GLYPHS sys_draw_glyphs

/* Copy WxH glyphs from (X1,Y1) to (X2,Y2)  */
#define COPY_GLYPHS(win, x1, y1, w, h, x2, y2)				\
    do {								\
	int x1pix = (win)->w_LeftPix + (win)->w_FontX * (x1);		\
	int y1pix = (win)->w_TopPix + (win)->w_FontY * (y1);		\
	int x2pix = (win)->w_LeftPix + (win)->w_FontX * (x2);		\
	int y2pix = (win)->w_TopPix + (win)->w_FontY * (y2);		\
	int width = (w) * (win)->w_FontX;				\
	int height = (h) * (win)->w_FontY;				\
	XCopyArea(WINDOW_XDPY(win)->display,				\
		  (win)->w_Window, (win)->w_Window,			\
		  (win)->w_WindowSys.ws_GC,				\
		  x1pix, y1pix, width, height, x2pix, y2pix);		\
    } while(0)


#endif /* _X11_WINDOWSYS_H */
