/* amiga_windowsys.h -- Amiga window-system data and macros
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

#ifndef _AMIGA_WINDOWSYS_H
#define _AMIGA_WINDOWSYS_H

typedef struct {
    struct Window      *ws_Window;
    struct RastPort    *ws_Rp;
    short		ws_PenX, ws_PenY;
    struct TextFont    *ws_Font;
    UWORD		ws_OldDimensions[4];	/* l,t,w,h */
    short		ws_FontSize;
    short		ws_Pad; /* keep on long word alignment. */
    VALUE		ws_ScreenName;
} VW_WindowSys;

#define vw_Window	vw_WindowSys.ws_Window
#define vw_Font 	vw_WindowSys.ws_Font
#define WINDOW_NIL	NULL

typedef struct {
    struct Gadget      *gad;
    long		top;
    long		total;
} ScrollBar;

/* Pen colours. */
#define P_TEXT	0
#define P_BLOCK 1

/*
 * Macros for drawing operations. These are mainly used in render.c for
 * system-independant rendering.
 */

/* Move to position (X,Y). The next DRAW() will happen at this position. */
#define MOVE(vw,x,y) \
    do { \
	(vw)->vw_WindowSys.ws_PenX = x; \
	(vw)->vw_WindowSys.ws_PenY = y; \
    } while(0)

#define PEN_X(vw) ((vw)->vw_WindowSys.ws_PenX)
#define PEN_Y(vw) ((vw)->vw_WindowSys.ws_PenY)

/* Draw LEN bytes of the string STR with colour COL (P_TEXT or P_BLOCK)
   at the position set by the MOVE() macro.  */
#define TEXT(vw,col,str,len) \
    do { \
	Move((vw)->vw_WindowSys.ws_Rp, \
	     (vw)->vw_WindowSys.ws_PenX, \
	     (vw)->vw_WindowSys.ws_PenY); \
	SetAPen((vw)->vw_WindowSys.ws_Rp, 1); \
	SetDrMd((vw)->vw_WindowSys.ws_Rp, JAM2 | (((col) == P_TEXT) ? 0 : INVERSVID)); \
	Text((vw)->vw_WindowSys.ws_Rp, (str), (len)); \
	(vw)->vw_WindowSys.ws_PenX += (len) * (vw)->vw_FontX; \
    } while(0)

/* Clear a rectangle from (X,Y) to (X+W,Y+H).  */
#define CLR_AREA(vw,x,y,w,h) \
    do { \
	SetAPen((vw)->vw_WindowSys.ws_Rp, 0); \
	SetDrMd((vw)->vw_WindowSys.ws_Rp, JAM2); \
	RectFill((vw)->vw_WindowSys.ws_Rp, \
		 (x), (y), ((x) + (w)) - 1, ((y) + (h)) - 1); \
    } while(0)

#define CLR_RECT(vw,x1,y1,x2,y2) \
    do { \
	SetAPen((vw)->vw_WindowSys.ws_Rp, 0); \
	SetDrMd((vw)->vw_WindowSys.ws_Rp, JAM2); \
	RectFill((vw)->vw_WindowSys.ws_Rp, (x1), (y1), (x2) - 1, (y2) - 1); \
    } while(0)

/* Fill a rectangle from (X,Y) to (X+W,Y+H).  */
#define SET_AREA(vw,x,y,w,h) \
    do { \
	SetAPen((vw)->vw_WindowSys.ws_Rp, 1); \
	SetDrMd((vw)->vw_WindowSys.ws_Rp, JAM2); \
	RectFill((vw)->vw_WindowSys.ws_Rp, \
		 (x), (y), ((x) + (w)) - 1, ((y) + (h)) - 1); \
    } while(0)

#define SET_RECT(vw,x1,y1,x2,y2) \
    do { \
	SetAPen((vw)->vw_WindowSys.ws_Rp, 1); \
	SetDrMd((vw)->vw_WindowSys.ws_Rp, JAM2); \
	RectFill((vw)->vw_WindowSys.ws_Rp, (x1), (y1), (x2) - 1, (y2) - 1); \
    } while(0)

/* Copy pixels from (X1,Y1),(X1+W,Y1+H) to (X2,Y2)  */
#define COPY_AREA(vw,x1,y1,w,h,x2,y2) \
    ClipBlit((vw)->vw_WindowSys.ws_Rp, (x1), (y1), \
	     (vw)->vw_WindowSys.ws_Rp, (x2), (y2), \
	     (w), (h), 0xc0)

/* Number of pixels from top of font to its baseline.  */
#define FONT_ASCENT(vw) ((vw)->vw_Font->tf_Baseline)

/* Draw a line from (x1,y1) to (x2,y2)	*/
#define DRAW_LINE(vw,x1,y1,x2,y2) \
    do { \
	SetAPen((vw)->vw_WindowSys.ws_Rp, 1); \
	SetDrMd((vw)->vw_WindowSys.ws_Rp, JAM2); \
	Move((vw)->vw_WindowSys.ws_Rp, x1, y1); \
	Draw((vw)->vw_WindowSys.ws_Rp, x2, y2); \
    } while(0)

#endif /* _AMIGA_WINDOWSYS_H */
