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
#include <assert.h>

_PR void refresh_init(void);
_PR void refresh_view(VW *);
_PR void flag_insertion(TX *, VALUE, VALUE);
_PR void flag_deletion(TX *, VALUE, VALUE);
_PR void flag_modification(TX *, VALUE, VALUE);
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
    ADD_SUBR_INT(subr_refresh_all);
}

/*
 * If no changes have been made to the buffer, call this to do any vertical
 * scrolling. Returns 0 if nothing happened, 1 if it scrolled a bit, 2
 * if it redrew the whole view.
 */
static int
vert_scroll(VW *vw)
{
    long y = VROW(vw->vw_DisplayOrigin) - VROW(vw->vw_LastDisplayOrigin);
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
	    redraw_lines(vw, VROW(vw->vw_DisplayOrigin),
			 VROW(vw->vw_LastDisplayOrigin));
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
	    redraw_lines(vw, VROW(vw->vw_DisplayOrigin) + vw->vw_MaxY - y,
			 VROW(vw->vw_DisplayOrigin) + vw->vw_MaxY);
	    return(1);
	}
    }
    return(0);
}

/* Fills in the four element array DEST-ARRAY with start,end pairs
   defining the region found when subtracting the intersection betwen
   (START1,END1) and (START2,END2) from the union of these two regions.
   Returns the number of pairs of coords put into DEST-ARRAY, either
   0, 1, or 2. */
static int
sub_intersect_regions(Pos *start1, Pos *end1, Pos *start2, Pos *end2,
		      Pos **dest_array)
{
    if(PPOS_GREATER_P(start1, start2))
    {
	Pos *tmp;
	tmp = start1; start1 = start2; start2 = tmp;
	tmp = end1;   end1 = end2;     end2 = tmp;
    }
    if(PPOS_EQUAL_P(start1, start2))
    {
	if(PPOS_EQUAL_P(end1, end2))
	    return 0;
	else if(PPOS_LESS_P(end1, end2))
	{
	    dest_array[0] = end1;
	    dest_array[1] = end2;
	    return 1;
	}
	else
	{
	    dest_array[0] = end2;
	    dest_array[1] = end1;
	    return 1;
	}
    }
    else if(PPOS_LESS_P(end1, end2))
    {
	if(PPOS_LESS_P(end1, start2))
	{
	    dest_array[0] = start1;
	    dest_array[1] = end1;
	    dest_array[2] = start2;
	    dest_array[3] = end2;
	    return 2;
	}
	else
	{
	    dest_array[0] = start1;
	    dest_array[1] = start2;
	    dest_array[2] = end1;
	    dest_array[3] = end2;
	    return 2;
	}
    }
    else
    {
	if(PPOS_LESS_P(end2, start1))
	{
	    dest_array[0] = start1;
	    dest_array[1] = end2;
	    dest_array[2] = start2;
	    dest_array[3] = end1;
	    return 2;
	}
	else
	{
	    dest_array[0] = start1;
	    dest_array[1] = start2;
	    dest_array[2] = end2;
	    dest_array[3] = end1;
	    return 2;
	}
    }
}

/* Similar to above but subtracts region (START2,END2) from (START1,END1) */
static int
sub_regions(Pos *start1, Pos *end1, Pos *start2, Pos *end2,
	    Pos **dest_array)
{
    if(PPOS_EQUAL_P(start1, start2))
    {
	if(PPOS_GREATER_EQUAL_P(end2, end1))
	    return 0;
	dest_array[0] = start1;
	dest_array[1] = end1;
	return 1;
    }
    else if(PPOS_LESS_P(start1, start2))
    {
	if(PPOS_GREATER_EQUAL_P(end2, end1))
	{
	    dest_array[0] = start1;
	    dest_array[1] = end1;
	    return 1;
	}
	else
	{
	    dest_array[0] = start1;
	    dest_array[1] = start2;
	    dest_array[2] = end2;
	    dest_array[3] = end1;
	    return 2;
	}
    }
    else
    {
	if(PPOS_GREATER_EQUAL_P(end2, end1))
	    return 0;
	dest_array[0] = end2;
	dest_array[1] = end1;
	return 1;
    }
}
	    
/* Refresh the block in VW. If START and END are non-null they specify
   a region of the buffer that has _already_ been redrawn this refresh;
   any parts of the block inside this region that need redrawing can
   be assumed to be correct already.

   Need to fix rectangular block redrawing (horizontal updates don't
   work properly) */
static void
refresh_block(VW *vw, VALUE start, VALUE end)
{
    /* Make copies so they can be reversed for rect-blocks sometimes */
    Pos block_start, block_end;
    Pos last_block_start, last_block_end;
    Pos tstart, tend;

    if(vw->vw_BlockStatus == 0)
    {
	COPY_VPOS(&block_start, vw->vw_BlockS);
	COPY_VPOS(&block_end, vw->vw_BlockE);
    }
    if(vw->vw_LastBlockStatus == 0)
    {
	COPY_VPOS(&last_block_start, vw->vw_LastBlockS);
	COPY_VPOS(&last_block_end, vw->vw_LastBlockE);
    }

    if(vw->vw_Flags & VWFF_RECTBLOCKS)
    {
	if(vw->vw_BlockStatus == 0
	   && PCOL(&block_start) > PCOL(&block_end))
	{
	    long tmp = PCOL(&block_start);
	    PCOL(&block_start) = PCOL(&block_end);
	    PCOL(&block_end) = tmp;
	}
	if(vw->vw_LastBlockStatus == 0
	   && PCOL(&last_block_start) > PCOL(&last_block_end))
	{
	    long tmp = PCOL(&last_block_start);
	    PCOL(&last_block_start) = PCOL(&last_block_end);
	    PCOL(&last_block_end) = tmp;
	}
    }

    if(vw->vw_BlockStatus == 0 && vw->vw_LastBlockStatus == 0)
    {
	/* A block was drawn last time as well, try to economise. */
	Pos *regions[4];
	int count = sub_intersect_regions(&block_start, &block_end,
					  &last_block_start,
					  &last_block_end, regions);
	int i;
	for(i = 0; i < count; i++)
	{
	    if(!start || !end)
		redraw_region(vw, regions[i*2], regions[i*2+1]);
	    else
	    {
		Pos *dr_regions[4];
		int dr_count, j;
		dr_count = sub_regions(regions[i*2], regions[i*2+1],
				       &tstart, &tend, dr_regions);
		COPY_VPOS(&tstart, start); COPY_VPOS(&tend, end);
		for(j = 0; j < dr_count; j++)
		    redraw_region(vw, dr_regions[j*2], dr_regions[j*2+1]);
	    }
	}
    }
    else
    {
	Pos *start_draw, *end_draw;
	if(vw->vw_BlockStatus == 0)
	{
	    /* Draw the newly marked block */
	    start_draw = &block_start;
	    end_draw = &block_end;
	}
	else
	{
	    /* Wipeout the old block */
	    start_draw = &last_block_start;
	    end_draw = &last_block_end;
	}
	if(!start || !end)
	    redraw_region(vw, start_draw, end_draw);
	else
	{
	    Pos *regions[4];
	    int count, i;
	    COPY_VPOS(&tstart, start); COPY_VPOS(&tend, end);
	    count = sub_regions(start_draw, end_draw,
				&tstart, &tend, regions);
	    for(i = 0; i < count; i++)
		redraw_region(vw, regions[i*2], regions[i*2+1]);
	}
    }
    vw->vw_Flags &= ~VWFF_REFRESH_BLOCK;
}

/* Refreshes one view. */
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
		   || (vw->vw_LastRefTx != vw->vw_Tx)
		   || (!POS_EQUAL_P(vw->vw_DisplayOrigin,
				    vw->vw_LastDisplayOrigin))))
	    {
		update_status_buffer(vw);
		redraw_status_buffer(vw);
		vw->vw_Flags &= ~VWFF_REFRESH_STATUS;
	    }
	    if((vw->vw_LastRefTx != vw->vw_Tx)
	       || (vw->vw_Flags & VWFF_FORCE_REFRESH)
	       || (tx->tx_Flags & TXFF_REFRESH_ALL)
	       || (tx->tx_LogicalStart != tx->tx_LastLogicalStart)
	       || (tx->tx_LogicalEnd != tx->tx_LastLogicalEnd))
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
		    refresh_block(vw, LISP_NULL, LISP_NULL);
	    }
	    else
	    {
		long endline = VROW(vw->vw_DisplayOrigin) + vw->vw_MaxY;
		int vscrl;
		/* check if modified region hits window */
		if((VROW(vw->vw_DisplayOrigin) > VROW(tx->tx_ModEnd))
		   || (endline <= VROW(tx->tx_ModStart)))
		{
		    /* nope. just do any easy scrolling. */
		    vert_scroll(vw);
		    if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			refresh_block(vw, LISP_NULL, LISP_NULL);
		}
		else if((vscrl = vert_scroll(vw)) != 2)
		{
		    /* is modified region just one line? */
		    if((VROW(tx->tx_ModStart) == VROW(tx->tx_ModEnd))
		       && (tx->tx_ModDelta == 0))
		    {
			redraw_line_from(vw, VCOL(tx->tx_ModStart),
					 VROW(tx->tx_ModStart));
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			{
			    VALUE end = cmd_end_of_line(tx->tx_ModStart,
							VAL(tx), sym_nil);
			    refresh_block(vw, tx->tx_ModStart, end);
			}
		    }
		    else if(tx->tx_ModDelta == 0)
		    {
			/* not able to do any pasting */
			redraw_vregion(vw, tx->tx_ModStart, tx->tx_ModEnd);
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			    refresh_block(vw, tx->tx_ModStart, tx->tx_ModEnd);
		    }
		    else if(tx->tx_ModDelta > 0)
		    {
			/* lines have been added: move down the lines they
			   displaced. */
			if(vscrl == 0)
			    cut_paste_lines(vw, VROW(tx->tx_ModStart) + 1,
					    VROW(tx->tx_ModStart)
					    + tx->tx_ModDelta + 1);
			redraw_vregion(vw, tx->tx_ModStart, tx->tx_ModEnd);
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			    refresh_block(vw, tx->tx_ModStart, tx->tx_ModEnd);
		    }
		    else if(tx->tx_ModDelta < 0)
		    {
			/* lines deleted. */
			VALUE line_end;
			VALUE end;
			if(vscrl == 0)
			{
			    if(VCOL(tx->tx_ModStart) == 0)
				cut_paste_lines(vw, VROW(tx->tx_ModEnd)
						- tx->tx_ModDelta,
						VROW(tx->tx_ModEnd));
			    else
				cut_paste_lines(vw, VROW(tx->tx_ModEnd)
						- tx->tx_ModDelta + 1,
						VROW(tx->tx_ModEnd) + 1);
			}
			line_end = cmd_end_of_line(tx->tx_ModStart,
						   VAL(tx), sym_nil);
			if(POS_LESS_P(tx->tx_ModEnd, line_end))
			    end = line_end;
			else
			    end = tx->tx_ModEnd;
			redraw_vregion(vw, tx->tx_ModStart, end);
			if(vw->vw_Flags & VWFF_REFRESH_BLOCK)
			    refresh_block(vw, tx->tx_ModStart, end);
		    }
		}
	    }
	    vw->vw_LastRefTx = tx;
	    vw->vw_LastDisplayOrigin = vw->vw_DisplayOrigin;
	    vw->vw_LastBlockS = vw->vw_BlockS;
	    vw->vw_LastBlockE = vw->vw_BlockE;
	    vw->vw_LastBlockStatus = vw->vw_BlockStatus;
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

/* Notes that buffer TX has had text added between START and END. */
void
flag_insertion(TX *tx, VALUE start, VALUE end)
{
    if((tx->tx_Flags & TXFF_REFRESH_ALL) == 0)
    {
	if(tx->tx_LastChanges == tx->tx_Changes)
	{
	    /* first insertion */
	    tx->tx_ModStart = start;
	    tx->tx_ModEnd = end;
	    tx->tx_ModDelta = VROW(end) - VROW(start);
	}
	else
	{
	    if(POS_LESS_P(start, tx->tx_ModStart))
		tx->tx_ModStart = start;
	    if(POS_GREATER_P(end, tx->tx_ModEnd))
		tx->tx_ModEnd = end;
	    tx->tx_ModDelta += VROW(end) - VROW(start);
	}
    }
    tx->tx_Changes++;
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
}

/* Same for deleted areas. */
void
flag_deletion(TX *tx, VALUE start, VALUE end)
{
    if((tx->tx_Flags & TXFF_REFRESH_ALL) == 0)
    {
	if(tx->tx_LastChanges == tx->tx_Changes)
	{
	    /* first */
	    tx->tx_ModStart = start;
	    tx->tx_ModEnd = end;
	    tx->tx_ModDelta = -(VROW(end) - VROW(start));
	}
	else
	{
	    if(POS_LESS_P(start, tx->tx_ModStart))
		tx->tx_ModStart = start;
	    if(POS_GREATER_P(end, tx->tx_ModEnd))
		tx->tx_ModEnd = end;
	    tx->tx_ModDelta -= VROW(end) - VROW(start);
	}
    }
    tx->tx_Changes++;
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
}

/* Means that there is still the same layout of text between START and END,
   but some of the character values may have been modified. */
void
flag_modification(TX *tx, VALUE start, VALUE end)
{
    if((tx->tx_Flags & TXFF_REFRESH_ALL) == 0)
    {
	if(tx->tx_LastChanges == tx->tx_Changes)
	{
	    /* first */
	    tx->tx_ModStart = start;
	    tx->tx_ModEnd = end;
	    tx->tx_ModDelta = 0;
	}
	else
	{
	    if(POS_LESS_P(start, tx->tx_ModStart))
		tx->tx_ModStart = start;
	    if(POS_GREATER_P(end, tx->tx_ModEnd))
		tx->tx_ModEnd = end;
	}
    }
    tx->tx_Changes++;
    tx->tx_Flags |= TXFF_REFRESH_STATUS;
}

/* Refreshes a whole window */
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

/* Refeshes everything that should be. */
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
	tx->tx_LastLogicalStart = tx->tx_LogicalStart;
	tx->tx_LastLogicalEnd = tx->tx_LogicalEnd;
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
	tx->tx_LastLogicalStart = tx->tx_LogicalStart;
	tx->tx_LastLogicalEnd = tx->tx_LogicalEnd;
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
