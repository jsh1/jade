/* x11_render.c -- Rendering for X11
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

_PR void x11_handle_gexposures(WIN *);
_PR void x11_handle_expose(WIN *, XExposeEvent *);

static void
redraw_exposed_area(WIN *w, u_int x1, u_int y1, u_int x2, u_int y2)
{
    VW *vw;
    for(vw = w->w_ViewList; vw != 0; vw = vw->vw_NextView)
    {
	Pos start, end;
	u_int first_row, last_row;
	if(vw->vw_TopPix > y2)
	    return;
	if(vw->vw_BottomPix > y1)
	{
	    if(vw->vw_Flags & VWFF_MINIBUF
	       && w->w_Flags & WINFF_MESSAGE)
	    {
		redraw_message(w);
		break;
	    }

	    first_row = ((vw->vw_TopPix > y1)
			 ? 0
			 : (y1 - vw->vw_TopPix) / w->w_FontY);
	    last_row = ((vw->vw_BottomPix > y2)
			? (y2 - vw->vw_TopPix) / w->w_FontY
			: vw->vw_MaxY-1);

	    PCOL(&start) = (((x1 - vw->vw_LeftPix) / w->w_FontX)
			    + VCOL(vw->vw_DisplayOrigin));
	    PROW(&start) = first_row + VROW(vw->vw_DisplayOrigin);
	    PCOL(&end) = ((((x2 - 1)- vw->vw_LeftPix) / w->w_FontX)
			  + VCOL(vw->vw_DisplayOrigin));
	    PROW(&end) = last_row + VROW(vw->vw_DisplayOrigin);

	    redraw_rect(vw, VAL(&start), VAL(&end), FALSE);
	}
	if(!(vw->vw_Flags & VWFF_MINIBUF)
	   && vw->vw_BottomPix < y2)
	    redraw_status_buffer(vw);
    }
}
	
static Bool
gex_pred(Display *dpy, XEvent *ev, XPointer arg)
{
    return((ev->type == NoExpose) || (ev->type == GraphicsExpose));
}
void
x11_handle_gexposures(WIN *w)
{
    XEvent ev;
    do {
	unsigned int x1, y1, x2, y2;
	XIfEvent(x11_display, &ev, gex_pred, (XPointer)NULL);
	if(ev.type == NoExpose)
	    return;
	x1 = ev.xgraphicsexpose.x;
	y1 = ev.xgraphicsexpose.y;
	x2 = x1 + ev.xgraphicsexpose.width;
	y2 = y1 + ev.xgraphicsexpose.height;
	redraw_exposed_area(w, x1, y1, x2, y2);
    } while(ev.xgraphicsexpose.count > 0);
}

void
x11_handle_expose(WIN *w, XExposeEvent *ev)
{
    unsigned int x1, y1, x2, y2;
    x1 = ev->x;
    y1 = ev->y;
    x2 = x1 + ev->width;
    y2 = y1 + ev->height;
    redraw_exposed_area(w, x1, y1, x2, y2);
}
