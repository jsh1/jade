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

typedef struct {
    Window		ws_Window;
    GC			ws_GC_array[P_MAX];
    int			ws_PenX, ws_PenY;
    XFontStruct	       *ws_Font;
    int			ws_Width, ws_Height;
} W_WindowSys;

#define w_Window	w_WindowSys.ws_Window
#define w_Font		w_WindowSys.ws_Font
#define WINDOW_NIL	(0)

#if 0
typedef struct {
    /* ... */
} ScrollBar;
#endif

/*
 * Macros for drawing operations. These are mainly used in render.c for
 * system-independant rendering.
 */

/* Move to position (X,Y). The next DRAW() will happen at this position. */
#define MOVE(vw,x,y)							\
    do {								\
	(vw)->vw_Win->w_WindowSys.ws_PenX = (vw)->vw_LeftPix + (x);	\
	(vw)->vw_Win->w_WindowSys.ws_PenY = (vw)->vw_TopPix + (y);	\
    } while(0)

#define PEN_X(vw) ((vw)->vw_Win->w_WindowSys.ws_PenX - (vw)->vw_LeftPix)
#define PEN_Y(vw) ((vw)->vw_Win->w_WindowSys.ws_PenY - (vw)->vw_TopPix)

/* Draw LEN bytes of the string STR with colour PEN (P_TEXT or P_BLOCK)
   at the position set by the MOVE() macro.  */
#define TEXT(vw,pen,str,len)						    \
    do {								    \
	XDrawImageString(x11_display, (vw)->vw_Win->w_Window,		    \
			 (vw)->vw_Win->w_WindowSys.ws_GC_array[pen],	    \
			 (vw)->vw_Win->w_WindowSys.ws_PenX,		    \
			 (vw)->vw_Win->w_WindowSys.ws_PenY,		    \
			 (str), (len));					    \
	(vw)->vw_Win->w_WindowSys.ws_PenX += (len) * (vw)->vw_Win->w_FontX; \
    } while(0)

/* Clear a rectangle of size WxH with top-left corner (X,Y).  */
#define CLR_AREA(vw,x,y,w,h)						    \
    XClearArea(x11_display, (vw)->vw_Win->w_Window, (vw)->vw_LeftPix + (x), \
	       (vw)->vw_TopPix + (y), (w), (h), False)

/* Clear a rectangle from (X1,Y1) to (X2-1,Y2-1) inclusively. */
#define CLR_RECT(vw,x1,y1,x2,y2)					     \
    XClearArea(x11_display, (vw)->vw_Win->w_Window, (vw)->vw_LeftPix + (x1), \
	       (vw)->vw_TopPix + (y1), (x2) - (x1), (y2) - (y1), False)

/* Fill a rectangle WxH at (X,Y) with colour PEN.  */
#define SET_AREA(vw,pen,x,y,w,h)						    \
    XFillRectangle(x11_display, (vw)->vw_Win->w_Window,			    \
		   (vw)->vw_Win->w_WindowSys.ws_GC_array[pen],		    \
		   (vw)->vw_LeftPix + (x), (vw)->vw_TopPix + (y), (w), (h))

/* Fill a rectangle from (X1,Y1) to (X2-1,Y2-1) inclusively with PEN. */
#define SET_RECT(vw,pen,x1,y1,x2,y2)					\
    XFillRectangle(x11_display, (vw)->vw_Win->w_Window,			\
		   (vw)->vw_Win->w_WindowSys.ws_GC_array[pen],		\
		   (vw)->vw_LeftPix + (x1), (vw)->vw_TopPix + (y1),	\
		   (x2) - (x1), (y2) - (y1))

/* Copy WxH pixels from (X1,Y1) to (X2,Y2)  */
#define COPY_AREA(vw,x1,y1,w,h,x2,y2)					     \
    do {								     \
	XCopyArea(x11_display, (vw)->vw_Win->w_Window,			     \
		  (vw)->vw_Win->w_Window,				     \
		  (vw)->vw_Win->w_WindowSys.ws_GC_array[P_TEXT],	     \
		  (vw)->vw_LeftPix + (x1), (vw)->vw_TopPix + (y1), (w), (h), \
		  (vw)->vw_LeftPix + (x2), (vw)->vw_TopPix + (y2));	     \
	x11_handle_gexposures((vw)->vw_Win);				     \
    } while(0)

/* Number of pixels from top of font to its baseline.  */
#define FONT_ASCENT(vw) ((vw)->vw_Win->w_Font->ascent)

/* Draw a line from (x1,y1) to (x2,y2)  */
#define DRAW_LINE(vw,x1,y1,x2,y2)				\
    XDrawLine(x11_display, vw->vw_Win->w_Window,		\
	      vw->vw_Win->w_WindowSys.ws_GC_array[P_TEXT],	\
	      (vw)->vw_LeftPix + (x1), (vw)->vw_TopPix + (y1),	\
	      (vw)->vw_LeftPix + (x2), (vw)->vw_TopPix + (y2))

#endif /* _X11_WINDOWSYS_H */
