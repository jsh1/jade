/* refresh.c -- Working out what to redraw in a window
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

_PR void refresh_init(void);
_PR void refresh_view(VW *);
_PR void flag_insertion(TX *, POS *, POS *);
_PR void flag_deletion(TX *, POS *, POS *);
_PR void flag_modification(TX *, POS *, POS *);
_PR void refresh_window(WIN *w);
_PR void refresh_world(void);
_PR void refresh_world_curs(void);
_PR void refresh_message(WIN *w);

_PR VALUE cmd_cursor(VALUE status);
DEFUN("cursor", cmd_cursor, subr_cursor, (VALUE status), V_Subr1, DOC_cursor) /*
::doc:cursor::
cursor STATUS

Turns cursor on or off, normally cursor is always off when an event is being
evaluated.
::end:: */
{
    cursor(curr_vw, NILP(status) ? CURS_OFF : CURS_ON);
    return(sym_t);
}

_PR VALUE cmd_refresh_all(void);
DEFUN_INT("refresh-all", cmd_refresh_all, subr_refresh_all, (void), V_Subr0, DOC_refresh_all, "") /*
::doc:refresh_all::
refresh-all

Redraw anything that has been changed since the last refresh.
::end:: */
{
    refresh_world();
    return(sym_t);
}

void
refresh_init(void)
{
    ADD_SUBR(subr_cursor);
    ADD_SUBR(subr_refresh_all);
}

/*
 * If no changes have been made to the buffer, call this to do any vertical
 * scrolling. Returns 0 if nothing happened, 1 if it scrolled a bit, 2
 * if it redrew the whole view.
 */
static int
vert_scroll(VW *vw)
{
    long y = vw->vw_StartLine - vw->vw_LastDisplayOrigin.pos_Line;
    if(y < 0)
    {
	if(-y >= vw->vw_MaxY || -y > vw->vw_MaxScroll)
	{
	    redraw_all(vw);
	    return(2);
	}
	else
	{
	    scroll_vw(vw, y);
	    redraw_lines(vw, vw->vw_StartLine,
			 vw->vw_LastDisplayOrigin.pos_Line);
	    return(1);
	}
    }
    else if(y > 0)
    {
	if(y >= vw->vw_MaxY || y > vw->vw_MaxScroll)
	{
	    redraw_all(vw);
	    return(2);
	}
	else
	{
	    scroll_vw(vw, y);
	    redraw_lines(vw, vw->vw_StartLine + vw->vw_MaxY - y,
			 vw->vw_StartLine + vw->vw_MaxY);
	    return(1);
	}
    }
    return(0);
}

/*
 * Refreshes one view.
 */
void
refresh_view(VW *vw)
{
    TX *tx = vw->vw_Tx;
    if(vw && vw->vw_Win->w_Window && (!vw->vw_DeferRefresh))
    {
	if((vw->vw_Win->w_Flags & WINFF_SLEEPING) == 0)
	{
	    resync_xy(vw);
	    if(((vw->vw_Flags & VWFF_MINIBUF) == 0)
	       && ((vw->vw_Flags & VWFF_REFRESH_STATUS)
	           || (vw->vw_Flags & VWFF_FORCE_REFRESH)
		   || (vw->vw_Tx->tx_Flags & TXFF_REFRESH_STATUS)
		   || (vw->vw_LastRefTx != vw->vw_Tx)))
	    {
		update_status_buffer(vw);
		redraw_status_buffer(vw);
		vw->vw_Flags &= ~VWFF_REFRESH_STATUS;
	    }
	    if((vw->vw_LastRefTx != vw->vw_Tx)
	       || (vw->vw_Flags & VWFF_FORCE_REFRESH)
	       || (tx->tx_Flags & TXFF_REFRESH_ALL))
	    {
		if(vw->vw_Flags & VWFF_MINIBUF
		   && vw->vw_Win->w_Flags & WINFF_MESSAGE)
		{
		    refresh_message(vw->vw_Win);
		}
		else
		{
		    redraw_all(vw);
		    tx->tx_Flags &= ~TXFF_REFRESH_ALL;
		}
		vw->vw_Flags &= ~(VWFF_FORCE_REFRESH | VWFF_REFRESH_BLOCK);
	    }
	    else if(tx->tx_Changes == tx->tx_LastChanges)
	    {
		vert_scroll(vw);
		if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
		{
		    redraw_lines_clr(vw, vw->vw_BlockS.pos_Line,
				     vw->vw_BlockE.pos_Line);
		    vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
		}
	    }
	    else
	    {
		long endline = vw->vw_StartLine + vw->vw_MaxY;
		int vscrl;
		/* check if modified region hits window */
		if((vw->vw_StartLine > tx->tx_ModEnd.pos_Line)
		   || (endline <= tx->tx_ModStart.pos_Line))
		{
		    /* nope. just do any easy scrolling. */
		    vert_scroll(vw);
		    if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
		    {
			redraw_lines_clr(vw, vw->vw_BlockS.pos_Line,
					 vw->vw_BlockE.pos_Line);
			vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
		    }
		}
		else if((vscrl = vert_scroll(vw)) != 2)
		{
		    /* is modified region just one line? */
		    if((tx->tx_ModStart.pos_Line == tx->tx_ModEnd.pos_Line)
		       && (tx->tx_ModDelta == 0))
		    {
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			{
			    redraw_lines_clr(vw, vw->vw_BlockS.pos_Line,
					     vw->vw_BlockE.pos_Line);
			    if((tx->tx_ModStart.pos_Line < vw->vw_BlockS.pos_Line)
			       || (tx->tx_ModStart.pos_Line > vw->vw_BlockE.pos_Line))
				redraw_line_from(vw, tx->tx_ModStart.pos_Col,
						 tx->tx_ModStart.pos_Line);
			    vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
			}
			else
			    redraw_line_from(vw, tx->tx_ModStart.pos_Col,
						 tx->tx_ModStart.pos_Line);
		    }
		    else if(tx->tx_ModDelta == 0)
		    {
			/* not able to do any pasting */
			redraw_region(vw, &tx->tx_ModStart, &tx->tx_ModEnd);
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			{
			    if(!(POS_LESS_P(&vw->vw_BlockE, &tx->tx_ModEnd)
				 && POS_GREATER_P(&vw->vw_BlockS, &tx->tx_ModStart)))
				redraw_lines_clr(vw, vw->vw_BlockS.pos_Line,
						 vw->vw_BlockE.pos_Line);
			    vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
			}
		    }
		    else if(tx->tx_ModDelta > 0)
		    {
			/* lines have been added: move down the lines they
			   displaced. */
			if(vscrl == 0)
			    cut_paste_lines(vw, tx->tx_ModStart.pos_Line + 1,
					    tx->tx_ModStart.pos_Line + tx->tx_ModDelta + 1);
			redraw_region(vw, &tx->tx_ModStart, &tx->tx_ModEnd);
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			{
			    if(!(POS_LESS_P(&vw->vw_BlockE, &tx->tx_ModEnd)
				 && POS_GREATER_P(&vw->vw_BlockS, &tx->tx_ModStart)))
				redraw_lines_clr(vw, vw->vw_BlockS.pos_Line,
						 vw->vw_BlockE.pos_Line);
			    vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
			}
		    }
		    else if(tx->tx_ModDelta < 0)
		    {
			/* lines deleted. */
			POS line_end;
			if(vscrl == 0)
			{
#if 1
			    if(tx->tx_ModStart.pos_Col == 0)
				cut_paste_lines(vw, tx->tx_ModEnd.pos_Line - tx->tx_ModDelta,
						tx->tx_ModEnd.pos_Line);
			    else
				cut_paste_lines(vw, tx->tx_ModEnd.pos_Line - tx->tx_ModDelta + 1,
						tx->tx_ModEnd.pos_Line + 1);
#else
			    if(tx->tx_ModStart.pos_Col == 0)
				cut_paste_lines(vw, tx->tx_ModEnd.pos_Line,
						tx->tx_ModEnd.pos_Line + tx->tx_ModDelta);
			    else
				cut_paste_lines(vw, tx->tx_ModEnd.pos_Line + 1,
						tx->tx_ModEnd.pos_Line + tx->tx_ModDelta + 1);
#endif
			}
			line_end.pos_Line = tx->tx_ModStart.pos_Line;
			line_end.pos_Col = tx->tx_Lines[line_end.pos_Line].ln_Strlen - 1;
			redraw_region(vw, &tx->tx_ModStart,
				      POS_LESS_P(&tx->tx_ModEnd, &line_end)
				      ? &line_end
				      : &tx->tx_ModEnd);
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			{
			    if(!(POS_LESS_P(&vw->vw_BlockE, &tx->tx_ModEnd)
				 && POS_GREATER_P(&vw->vw_BlockS, &tx->tx_ModStart)))
				redraw_lines_clr(vw, vw->vw_BlockS.pos_Line,
						 vw->vw_BlockE.pos_Line);
			    vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
			}
		    }
		}
	    }
	    vw->vw_LastRefTx = tx;
	    vw->vw_LastDisplayOrigin = vw->vw_DisplayOrigin;
	}
	else
	    vw->vw_Flags |= VWFF_FORCE_REFRESH;
    }
    else if(vw)
    {
	if(!(--vw->vw_DeferRefresh))
	    vw->vw_Flags |= VWFF_FORCE_REFRESH;
    }
}

/*
 * Notes that buffer TX has had text added between START and END.
 */
void
flag_insertion(TX *tx, POS *start, POS *end)
{
    if(tx->tx_LastChanges == tx->tx_Changes)
    {
	/* first insertion */
	tx->tx_ModStart = *start;
	tx->tx_ModEnd = *end;
	tx->tx_ModDelta = end->pos_Line - start->pos_Line;
    }
    else
    {
	if(POS_LESS_P(start, &tx->tx_ModStart))
	    tx->tx_ModStart = *start;
	if(POS_GREATER_P(end, &tx->tx_ModEnd))
	    tx->tx_ModEnd = *end;
	tx->tx_ModDelta += end->pos_Line - start->pos_Line;
    }
    tx->tx_Changes++;
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
}

/*
 * Same for deleted areas.
 */
void
flag_deletion(TX *tx, POS *start, POS *end)
{
    if(tx->tx_LastChanges == tx->tx_Changes)
    {
	/* first */
	tx->tx_ModStart = *start;
#if 0
	tx->tx_ModEnd = *start;
#else
	tx->tx_ModEnd = *end;
#endif
	tx->tx_ModDelta = -(end->pos_Line - start->pos_Line);
    }
    else
    {
	if(POS_LESS_P(start, &tx->tx_ModStart))
	    tx->tx_ModStart = *start;
#if 0
	if(POS_GREATER_P(start, &tx->tx_ModEnd))
	    tx->tx_ModEnd = *start;
#else
	if(POS_GREATER_P(end, &tx->tx_ModEnd))
	    tx->tx_ModEnd = *end;
#endif
	tx->tx_ModDelta -= end->pos_Line - start->pos_Line;
    }
    tx->tx_Changes++;
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
}

/*
 * Means that there is still the same layout of text between START and END,
 * but some of the character values may have been modified.
 */
void
flag_modification(TX *tx, POS *start, POS *end)
{
    if(tx->tx_LastChanges == tx->tx_Changes)
    {
	/* first */
	tx->tx_ModStart = *start;
	tx->tx_ModEnd = *end;
	tx->tx_ModDelta = 0;
    }
    else
    {
	if(POS_LESS_P(start, &tx->tx_ModStart))
	    tx->tx_ModStart = *start;
	if(POS_GREATER_P(end, &tx->tx_ModEnd))
	    tx->tx_ModEnd = *end;
    }
    tx->tx_Changes++;
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
}

/*
 * Refreshes a whole window
 */
void
refresh_window(WIN *w)
{
    if(w->w_Window)
    {
	VW *vw;
	for(vw = w->w_ViewList; vw; vw = vw->vw_NextView)
	{
	    if(w->w_Flags & WINFF_FORCE_REFRESH)
		vw->vw_Flags |= VWFF_FORCE_REFRESH;
	    refresh_view(vw);
	}
	w->w_Flags &= ~WINFF_FORCE_REFRESH;
    }
}

/*
 * Refeshes everything that should be.
 */
void
refresh_world(void)
{
    WIN *w;
    TX *tx;
    for(w = win_chain; w != 0; w = w->w_Next)
    {
	if(w->w_Window)
	    refresh_window(w);
    }
    tx = buffer_chain;
    while(tx)
    {
	tx->tx_LastChanges = tx->tx_Changes;
	tx->tx_Flags &= ~TXFF_REFRESH_STATUS;
	tx = tx->tx_Next;
    }
}

/*
 * Same as the above but assumes that the cursor is currently drawn.
 */
void
refresh_world_curs(void)
{
    WIN *w;
    TX *tx;
    for(w = win_chain; w != 0; w = w->w_Next)
    {
	if(w->w_Window)
	{
	    VW *vw;
	    for(vw = w->w_ViewList; vw != 0; vw = vw->vw_NextView)
	    {
		if(w->w_Flags & WINFF_FORCE_REFRESH)
		    vw->vw_Flags |= VWFF_FORCE_REFRESH;
		if(vw == curr_vw)
		    cursor(vw, CURS_OFF);
		refresh_view(vw);
		if(vw == curr_vw)
		    cursor(vw, CURS_ON);
	    }
	    w->w_Flags &= ~WINFF_FORCE_REFRESH;
	}
    }
    tx = buffer_chain;
    while(tx)
    {
	tx->tx_LastChanges = tx->tx_Changes;
	tx->tx_Flags &= ~TXFF_REFRESH_STATUS;
	tx = tx->tx_Next;
    }
}

void
refresh_message(WIN *w)
{
    if(w->w_Flags & WINFF_MESSAGE)
    {
	redraw_message(w);
	w->w_MiniBuf->vw_Tx->tx_Flags |= TXFF_REFRESH_ALL;
    }
}
