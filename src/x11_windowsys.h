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

/* Pen colours. */
enum {
    P_TEXT = 0,				/* foreground-on-background */
    P_TEXT_RV,				/* bg-on-fg */
    P_BLOCK,				/* fg-on-highlight */
    P_BLOCK_RV,				/* hl-on-fg */
    P_MAX
};

/* Per display data */
struct x11_display {
    struct x11_display *next;

    Display *display;
    int screen;

    /* Number of windows open */
    int window_count;

    /* Allocated colours */
    u_long fore_pixel, back_pixel, high_pixel;

    /* Interned atoms */
    Atom wm_delete_window, jade_selection;

    /* Mouse cursor for the window */
    Cursor text_cursor;
};

typedef struct {
    struct x11_display *ws_Display;
    Window		ws_Window;
    GC			ws_GC_array[P_MAX];
    int			ws_PenX, ws_PenY;
    XFontStruct	       *ws_Font;
    int			ws_Width, ws_Height;
} W_WindowSys;

#define w_Window	w_WindowSys.ws_Window
#define WINDOW_NIL	(0)

#define WINDOW_XDPY(w)	((w)->w_WindowSys.ws_Display)

#if 0
typedef struct {
    /* ... */
} ScrollBar;
#endif

/* Macros for drawing operations. These are mainly used in render.c for
   system-independant rendering. */

/* Draw LEN bytes of the string STR with pen PEN at glyph position (X,Y). */
#define DRAW_GLYPHS(win, x, y, pen, str, len)				\
    do {								\
	int xpix = (win)->w_LeftPix + (win)->w_FontX * (x);		\
	int ypix = ((win)->w_TopPix + (win)->w_FontY * (y)		\
		    + (win)->w_WindowSys.ws_Font->ascent);		\
	XDrawImageString(WINDOW_XDPY(win)->display, (win)->w_Window,	\
			 (win)->w_WindowSys.ws_GC_array[pen],		\
			 xpix, ypix, str, len);				\
    } while(0)

/* Fill LEN glyphs from (X,Y) with pen PEN. */
#define FILL_GLYPHS(win, x, y, pen, len)				\
    do {								\
	int xpix = (win)->w_LeftPix + (win)->w_FontX * (x);		\
	int ypix = (win)->w_TopPix + (win)->w_FontY * (y);		\
	XFillRectangle(WINDOW_XDPY(win)->display, (win)->w_Window,	\
		       (win)->w_WindowSys.ws_GC_array[pen],		\
		       xpix, ypix,					\
		       (len) * (win)->w_FontX, (win)->w_FontY);		\
    } while(0)

/* Clear LEN glyphs from (X,Y). */
#define CLEAR_GLYPHS(win, x, y, len)				\
    do {							\
	int xpix = (win)->w_LeftPix + (win)->w_FontX * (x);	\
	int ypix = (win)->w_TopPix + (win)->w_FontY * (y);	\
	XClearArea(WINDOW_XDPY(win)->display, (win)->w_Window,	\
		       xpix, ypix,				\
		       (len) * (win)->w_FontX, (win)->w_FontY,	\
		       False);					\
    } while(0)

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
		  (win)->w_WindowSys.ws_GC_array[P_TEXT],		\
		  x1pix, y1pix, width, height, x2pix, y2pix);		\
    } while(0)

#endif /* _X11_WINDOWSYS_H */
