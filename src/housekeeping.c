/* housekeeping.c -- Generally editor fiddly stuff
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

_PR void adjust_marks_add_x(TX *, long, long, long);
_PR void adjust_marks_sub_x(TX *, long, long, long);
_PR void adjust_marks_add_y(TX *, long, long);
_PR void adjust_marks_sub_y(TX *, long, long);
_PR void adjust_marks_split_y(TX *, long, long);
_PR void adjust_marks_join_y(TX *, long, long);

_PR void reset_all_views(TX *);


/* The next few routines deal with updating the various references to
   coordinates throughout the views after chunks have been deleted and
   inserted.  */

void
adjust_marks_add_x(TX *tx, long addx, long xpos, long ypos)
{
    VW *thisvw;
    Lisp_Mark *thismark;

#define UPD(p)						\
    do {						\
	if(p && VROW(p) == ypos && VCOL(p) >= xpos)	\
	    (p) = make_pos(VCOL(p) + addx, VROW(p));	\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos);
            if(!(thisvw->vw_Flags & VWFF_RECTBLOCKS))
	    {
		UPD(thisvw->vw_BlockS);
		UPD(thisvw->vw_BlockE);
	    }
	}
    }

    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->next)
	UPD(thismark->pos);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#undef UPD
}

void
adjust_marks_sub_x(TX *tx, long subx, long xpos, long ypos)
{
    VW *thisvw;
    Lisp_Mark *thismark;

#define UPD(p)						\
    do {						\
	if(p && VROW(p) == ypos && VCOL(p) >= xpos)	\
	{						\
	    register long col = VCOL(p) - subx;		\
	    (p) = make_pos(MAX(col, xpos), VROW(p));	\
	}						\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos);
            if(!(thisvw->vw_Flags & VWFF_RECTBLOCKS))
	    {
		UPD(thisvw->vw_BlockS);
		UPD(thisvw->vw_BlockE);
	    }
	}
    }

    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->next)
	UPD(thismark->pos);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#undef UPD
}

/*
 * Whole lines only please
 */
void
adjust_marks_add_y(TX *tx, long addy, long ypos)
{
    VW *thisvw;
    Lisp_Mark *thismark;

#define UPD(p)						\
    do {						\
	if(p && VROW(p) >= ypos)			\
	    (p) = make_pos(VCOL(p), VROW(p) + addy);	\
    } while(0)

#define UPD_Y(y)				\
    do {					\
	if(y >= ypos)				\
	    y += addy;				\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos);
	    UPD(thisvw->vw_BlockS);
	    UPD(thisvw->vw_BlockE);
	    if(thisvw != curr_vw)
		UPD(thisvw->vw_DisplayOrigin);
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->next)
	UPD(thismark->pos);


    if(tx->tx_LogicalStart > ypos)
	tx->tx_LogicalStart += addy;
    UPD_Y(tx->tx_LogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#undef UPD
#undef UPD_Y
}

/*
 * Whole lines only please
 */
void
adjust_marks_sub_y(TX *tx, long suby, long ypos)
{
    VW *thisvw;
    Lisp_Mark *thismark;

#define UPD_Y(y)				\
    do {					\
	if(y > ypos)				\
	{					\
	    if((y -= suby) < ypos)		\
		y = ypos;			\
	}					\
    } while(0)

#define UPD(p)					\
    if(p && VROW(p) >= ypos)			\
    {						\
	register long row = VROW(p) - suby;	\
	if(row < ypos)				\
	    (p) = make_pos(0, ypos);		\
	else					\
	    (p) = make_pos(VCOL(p), row);	\
    } while(0)

#define UPD1(p)						\
    if(p && VROW(p) >= ypos)				\
    {							\
	register long row = VROW(p) - suby;		\
	(p) = make_pos(VCOL(p), MAX(row, ypos));	\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos);
            if(thisvw->vw_Flags & VWFF_RECTBLOCKS)
	    {
		UPD1(thisvw->vw_BlockS);
		UPD1(thisvw->vw_BlockE);
	    }
            else
	    {
                UPD(thisvw->vw_BlockS);
                UPD(thisvw->vw_BlockE);
	    }
	    if(thisvw != curr_vw)
		UPD1(thisvw->vw_DisplayOrigin);
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->next)
	UPD(thismark->pos);

    UPD_Y(tx->tx_LogicalStart);
    UPD_Y(tx->tx_LogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#undef UPD_Y
#undef UPD
#undef UPD1
}

/*
 * Use when splitting a line into 2, cursor should be at position of split
 */
void
adjust_marks_split_y(TX *tx, long xpos, long ypos)
{
    VW *thisvw;
    Lisp_Mark *thismark;

#define UPD_Y(y)				\
    do {					\
	if(y > ypos)				\
	    y++;				\
    } while(0)

#define UPD(p)							\
    do {							\
	if(p && VROW(p) >= ypos && VCOL(p) >= xpos)		\
	{							\
	    (p) = make_pos((VROW(p) == ypos && VCOL(p) >= xpos)	\
			   ? VCOL(p) - xpos : VCOL(p),		\
			   VROW(p) + 1);			\
	}							\
    } while(0)

#define UPD1(p)						\
    do {						\
	if(p && VROW(p) > ypos)				\
	    (p) = make_pos(VCOL(p), VROW(p) + 1);	\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos);
            if(thisvw->vw_Flags & VWFF_RECTBLOCKS)
	    {
		UPD1(thisvw->vw_BlockS);
		UPD1(thisvw->vw_BlockE);
	    }
            else
	    {
                UPD(thisvw->vw_BlockS);
                UPD(thisvw->vw_BlockE);
	    }
	    if(thisvw != curr_vw)
		UPD1(thisvw->vw_DisplayOrigin);
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->next)
	UPD(thismark->pos);

    UPD_Y(tx->tx_LogicalStart);
    UPD_Y(tx->tx_LogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#undef UPD_Y
#undef UPD
#undef UPD1
}

/*
 * Use when compacting 2 adjacent lines into one
 */
void
adjust_marks_join_y(TX *tx, long xpos, long ypos)
{
    VW *thisvw;
    Lisp_Mark *thismark;

#define UPD_Y(y)				\
    do {					\
	if(y > ypos)				\
	    y--;				\
    } while(0)

#define UPD(p)								\
    do {								\
	if(p && VROW(p) > ypos)						\
	    (p) = make_pos(VCOL(p) + ((VROW(p) == ypos + 1) ? xpos : 0),\
			   VROW(p) - 1);				\
    } while(0)

#define UPD1(p)						\
    do {						\
	if(p && VROW(p) > ypos)				\
	    (p) = make_pos(VCOL(p), VROW(p) - 1);	\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos);
            if(thisvw->vw_Flags & VWFF_RECTBLOCKS)
	    {
		UPD1(thisvw->vw_BlockS);
		UPD1(thisvw->vw_BlockE);
	    }
            else
	    {
                UPD(thisvw->vw_BlockS);
                UPD(thisvw->vw_BlockE);
	    }
	    if(thisvw != curr_vw)
		UPD1(thisvw->vw_DisplayOrigin);
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->next)
	UPD(thismark->pos);

    UPD_Y(tx->tx_LogicalStart);
    UPD_Y(tx->tx_LogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#undef UPD
#undef UPD2
}


/* Miscellaneous */

/* This makes all views of this file have their cursor at the top of the
   file, it also refreshes each view. */
void
reset_all_views(TX *tx)
{
    VW *thisvw;
    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    thisvw->vw_CursorPos = cmd_start_of_buffer(VAL(tx), sym_nil);
	    thisvw->vw_DisplayOrigin = thisvw->vw_CursorPos;
	    thisvw->vw_BlockStatus = -1;
	}
	tx->tx_SavedCPos = make_pos(0, 0);
	tx->tx_SavedWPos = tx->tx_SavedCPos;
	tx->tx_SavedBlockStatus = -1;
    }
}
