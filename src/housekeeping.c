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
_PR void resync_xy(VW *);
_PR void set_start_col(VW *, long);
_PR void set_start_line(VW *, long);
_PR void reset_all_views(TX *);


/* The next few routines deal with updating the various references to
   coordinates throughout the views after chunks have been deleted and
   inserted.  */

void
adjust_marks_add_x(TX *tx, long addx, long xpos, long ypos)
{
    VW *thisvw;
    Mark *thismark;

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
		UPD(thisvw->vw_LastBlockS);
		UPD(thisvw->vw_LastBlockE);
	    }
	}
    }

    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
	UPD(thismark->mk_Pos);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);
    UPD(tx->tx_ModEnd);
    if((tx->tx_ModStart
	&& VROW(tx->tx_ModStart) == ypos) && (VCOL(tx->tx_ModStart) > xpos))
	tx->tx_ModStart = make_pos(VCOL(tx->tx_ModStart) + addx,
				   VROW(tx->tx_ModStart));

#undef UPD
}

void
adjust_marks_sub_x(TX *tx, long subx, long xpos, long ypos)
{
    VW *thisvw;
    Mark *thismark;

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
		UPD(thisvw->vw_LastBlockS);
		UPD(thisvw->vw_LastBlockE);
	    }
	}
    }

    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
	UPD(thismark->mk_Pos);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

    UPD(tx->tx_ModStart);
    if(tx->tx_ModEnd
       && VROW(tx->tx_ModEnd) == ypos && VCOL(tx->tx_ModEnd) > xpos)
    {
	register long col = VCOL(tx->tx_ModEnd) - subx;
	tx->tx_ModEnd = make_pos(MAX(col, xpos), VROW(tx->tx_ModEnd));
    }

#undef UPD
}

/*
 * Whole lines only please
 */
void
adjust_marks_add_y(TX *tx, long addy, long ypos)
{
    VW *thisvw;
    Mark *thismark;

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
	    UPD(thisvw->vw_LastBlockS);
	    UPD(thisvw->vw_LastBlockE);
	    if(thisvw != curr_vw)
	    {
		UPD(thisvw->vw_DisplayOrigin);
		UPD(thisvw->vw_LastDisplayOrigin);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
	UPD(thismark->mk_Pos);


    if(tx->tx_LogicalStart > ypos)
	tx->tx_LogicalStart += addy;
    if(tx->tx_LastLogicalStart > ypos)
	tx->tx_LastLogicalStart += addy;
    UPD_Y(tx->tx_LogicalEnd);
    UPD_Y(tx->tx_LastLogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

#if 0
#if 1
    if(tx->tx_ModStart && VROW(tx->tx_ModStart) > ypos)
	tx->tx_ModStart = make_pos(VCOL(tx->tx_ModStart),
				   VROW(tx->tx_ModStart) + addy);
#else
    UPD(tx->tx_ModStart);
#endif
    UPD(tx->tx_ModEnd);
#endif

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
    Mark *thismark;

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
		UPD1(thisvw->vw_LastBlockS);
		UPD1(thisvw->vw_LastBlockE);
	    }
            else
	    {
                UPD(thisvw->vw_BlockS);
                UPD(thisvw->vw_BlockE);
                UPD(thisvw->vw_LastBlockS);
                UPD(thisvw->vw_LastBlockE);
	    }
	    if(thisvw != curr_vw)
	    {
		UPD1(thisvw->vw_DisplayOrigin);
		UPD1(thisvw->vw_LastDisplayOrigin);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
	UPD(thismark->mk_Pos);

    UPD_Y(tx->tx_LogicalStart);
    UPD_Y(tx->tx_LogicalEnd);
    UPD_Y(tx->tx_LastLogicalStart);
    UPD_Y(tx->tx_LastLogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

    UPD(tx->tx_ModStart);
    if(tx->tx_ModEnd && VROW(tx->tx_ModEnd) > ypos)
    {
	register long row = VROW(tx->tx_ModEnd) - suby;
	if(row < ypos)
	    tx->tx_ModEnd = make_pos(0, ypos);
	else
	    tx->tx_ModEnd = make_pos(VCOL(tx->tx_ModEnd), row);
    }

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
    Mark *thismark;

#define UPD_Y(y)				\
    do {					\
	if(y > ypos)				\
	    y++;				\
    } while(0)

#define UPD(p)							\
    do {							\
	if(p && VROW(p) >= ypos)				\
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
		UPD1(thisvw->vw_LastBlockS);
		UPD1(thisvw->vw_LastBlockE);
	    }
            else
	    {
                UPD(thisvw->vw_BlockS);
                UPD(thisvw->vw_BlockE);
                UPD(thisvw->vw_LastBlockS);
                UPD(thisvw->vw_LastBlockE);
	    }
	    if(thisvw != curr_vw)
	    {
		UPD1(thisvw->vw_DisplayOrigin);
		UPD1(thisvw->vw_LastDisplayOrigin);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
	UPD(thismark->mk_Pos);

    UPD_Y(tx->tx_LogicalStart);
    UPD_Y(tx->tx_LogicalEnd);
    UPD_Y(tx->tx_LastLogicalStart);
    UPD_Y(tx->tx_LastLogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

    UPD(tx->tx_ModEnd);
    if(tx->tx_ModStart)
    {
	if((VROW(tx->tx_ModStart) == ypos) && (VCOL(tx->tx_ModStart) > xpos))
	    tx->tx_ModStart = make_pos(VCOL(tx->tx_ModStart) - xpos,
				       VROW(tx->tx_ModStart) + 1);
	else if(VROW(tx->tx_ModStart) > ypos)
	    tx->tx_ModStart = make_pos(VCOL(tx->tx_ModStart),
				       VROW(tx->tx_ModStart) + 1);
     }

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
    Mark *thismark;

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
		UPD1(thisvw->vw_LastBlockS);
		UPD1(thisvw->vw_LastBlockE);
	    }
            else
	    {
                UPD(thisvw->vw_BlockS);
                UPD(thisvw->vw_BlockE);
                UPD(thisvw->vw_LastBlockS);
                UPD(thisvw->vw_LastBlockE);
	    }
	    if(thisvw != curr_vw)
	    {
		UPD1(thisvw->vw_DisplayOrigin);
		UPD1(thisvw->vw_LastDisplayOrigin);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
	UPD(thismark->mk_Pos);

    UPD_Y(tx->tx_LogicalStart);
    UPD_Y(tx->tx_LogicalEnd);
    UPD_Y(tx->tx_LastLogicalStart);
    UPD_Y(tx->tx_LastLogicalEnd);

    UPD(tx->tx_SavedCPos);
    UPD(tx->tx_SavedWPos);
    UPD(tx->tx_SavedBlockPos[0]);
    UPD(tx->tx_SavedBlockPos[1]);

    UPD(tx->tx_ModStart);
    UPD(tx->tx_ModEnd);

#undef UPD
#undef UPD2
}


/* These routines are called to recalculate the cursor's position on the
   screen...  */

static void
resync_x(VW *vw)
{
    long offset, start_col = VCOL(vw->vw_DisplayOrigin);
    calc_cursor_offset(vw);
    offset = vw->vw_LastCursorOffset;
    while((offset - start_col) >= vw->vw_MaxX)
    {
	start_col += vw->vw_XStep;
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
    }
    while(offset < start_col)
    {
	start_col -= vw->vw_XStep;
	if(start_col < 0)
	    start_col = 0;
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
    }
    if(VCOL(vw->vw_DisplayOrigin) != start_col)
	vw->vw_DisplayOrigin = make_pos(start_col, VROW(vw->vw_DisplayOrigin));
}

static void
resync_y(VW *vw)
{
    TX *tx = vw->vw_Tx;
    long y, start_row = VROW(vw->vw_DisplayOrigin);

    /* First check that the cursor is within the current
       restriction, if not move the cursor until it is. */
    if(VROW(vw->vw_CursorPos) < tx->tx_LogicalStart)
	vw->vw_CursorPos = cmd_restriction_start(VAL(tx));
    if(VROW(vw->vw_CursorPos) >= tx->tx_LogicalEnd)
	vw->vw_CursorPos = cmd_restriction_end(VAL(tx));

    /* Next check that the cursor is within the visible portion
       of the buffer. If not change the visible region. */
    y = VROW(vw->vw_CursorPos) - start_row;
    if(y < 0)
    {
	if(-y > vw->vw_YStep)
	    start_row = VROW(vw->vw_CursorPos) - (vw->vw_MaxY / 2);
	else
	    start_row -= vw->vw_YStep;
    }
    else if(y >= vw->vw_MaxY)
    {
	if((vw->vw_MaxY + vw->vw_YStep) <= y)
	    start_row = VROW(vw->vw_CursorPos) - (vw->vw_MaxY / 2);
	else
	    start_row += vw->vw_YStep;
    }

    /* Finally do some sanity checks: ensure that nothing
       outside the restriction is visible, and that there's
       no wasted space when displaying the bottom of the
       restriction. */
    if(start_row < tx->tx_LogicalStart)
	start_row = tx->tx_LogicalStart;
    else if(start_row >= tx->tx_LogicalEnd
	    /* Check for a `gap' at the bottom of the display */
	    || (start_row != VROW(vw->vw_LastDisplayOrigin)
	        && (tx->tx_LogicalEnd - start_row) < vw->vw_MaxY))
    {
	start_row = MAX(tx->tx_LogicalEnd - vw->vw_MaxY,
			tx->tx_LogicalStart);
    }

    if(VROW(vw->vw_DisplayOrigin) != start_row)
	vw->vw_DisplayOrigin = make_pos(VCOL(vw->vw_DisplayOrigin), start_row);
}

void
resync_xy(VW *vw)
{
    /* kludge: remind me, why is this necessary? */
    if(vw->vw_Tx != vw->vw_LastRefTx)
	vw->vw_LastDisplayOrigin = vw->vw_DisplayOrigin;

    resync_x(vw);
    resync_y(vw);
}

void
set_start_col(VW *vw, long col)
{
    if(VCOL(vw->vw_DisplayOrigin) != col)
    {
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
	vw->vw_DisplayOrigin = make_pos(col, VROW(vw->vw_DisplayOrigin));
    }
}

void
set_start_line(VW *vw, long line)
{
    if(line != VROW(vw->vw_DisplayOrigin))
    {
	long row;
	vw->vw_DisplayOrigin = make_pos(VCOL(vw->vw_DisplayOrigin), line);

	row = line + VROW(vw->vw_CursorPos) - VROW(vw->vw_DisplayOrigin);
	if(row >= vw->vw_Tx->tx_LogicalEnd)
	    row = vw->vw_Tx->tx_LogicalEnd - 1;
	vw->vw_CursorPos = make_pos(VCOL(vw->vw_CursorPos), row);
    }
}

/*
 * This makes all views of this file have their cursor at the top of the
 * file, it also refreshes each view.
 */
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
	    thisvw->vw_Flags |= VWFF_FORCE_REFRESH;
	}
	tx->tx_SavedCPos = make_pos(0, 0);
	tx->tx_SavedWPos = tx->tx_SavedCPos;
	tx->tx_SavedBlockStatus = -1;
    }
}
