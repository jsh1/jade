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

#define UPD(x,y)				\
    do {					\
	if((y == ypos) && (x >= xpos))		\
	    x += addx;				\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos.pos_Col, thisvw->vw_CursorPos.pos_Line);
            if(!(thisvw->vw_Flags & VWFF_RECTBLOCKS))
	    {
		UPD(thisvw->vw_BlockS.pos_Col, thisvw->vw_BlockS.pos_Line);
		UPD(thisvw->vw_BlockE.pos_Col, thisvw->vw_BlockE.pos_Line);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
    {
	UPD(VPOS(thismark->mk_Pos).pos_Col, VPOS(thismark->mk_Pos).pos_Line);
    }
    UPD(tx->tx_SavedCPos.pos_Col, tx->tx_SavedCPos.pos_Line);
    UPD(tx->tx_SavedWPos.pos_Col, tx->tx_SavedWPos.pos_Line);
    UPD(tx->tx_SavedBlockPos[0].pos_Col, tx->tx_SavedBlockPos[0].pos_Line);
    UPD(tx->tx_SavedBlockPos[1].pos_Col, tx->tx_SavedBlockPos[1].pos_Line);
#if 1
    if((tx->tx_ModStart.pos_Line == ypos) && (tx->tx_ModStart.pos_Col > xpos))
	tx->tx_ModStart.pos_Col += addx;
#else
    UPD(tx->tx_ModStart.pos_Col, tx->tx_ModStart.pos_Line);
#endif
    UPD(tx->tx_ModEnd.pos_Col, tx->tx_ModEnd.pos_Line);

#undef UPD
}

void
adjust_marks_sub_x(TX *tx, long subx, long xpos, long ypos)
{
    VW *thisvw;
    Mark *thismark;

#define UPD(x,y)				\
    do {					\
	if((y == ypos) && (x >= xpos))		\
	{					\
	    if((x -= subx) < xpos)		\
		x = xpos;			\
	}					\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos.pos_Col, thisvw->vw_CursorPos.pos_Line);
            if(!(thisvw->vw_Flags & VWFF_RECTBLOCKS))
	    {
		UPD(thisvw->vw_BlockS.pos_Col, thisvw->vw_BlockS.pos_Line);
		UPD(thisvw->vw_BlockE.pos_Col, thisvw->vw_BlockE.pos_Line);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
    {
	UPD(VPOS(thismark->mk_Pos).pos_Col, VPOS(thismark->mk_Pos).pos_Line);
    }
    UPD(tx->tx_SavedCPos.pos_Col, tx->tx_SavedCPos.pos_Line);
    UPD(tx->tx_SavedWPos.pos_Col, tx->tx_SavedWPos.pos_Line);
    UPD(tx->tx_SavedBlockPos[0].pos_Col, tx->tx_SavedBlockPos[0].pos_Line);
    UPD(tx->tx_SavedBlockPos[1].pos_Col, tx->tx_SavedBlockPos[1].pos_Line);

    UPD(tx->tx_ModStart.pos_Col, tx->tx_ModStart.pos_Line);
#if 1
    if((tx->tx_ModEnd.pos_Line == ypos) && (tx->tx_ModEnd.pos_Col > xpos))
    {
	if((tx->tx_ModEnd.pos_Col -= subx) < xpos)
	    tx->tx_ModEnd.pos_Col = xpos;
    }
#else
    UPD(tx->tx_ModEnd.pos_Col, tx->tx_ModEnd.pos_Line);
#endif

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

#define UPD(y)					\
    do {					\
	if(y >= ypos)				\
	    y += addy;				\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD(thisvw->vw_CursorPos.pos_Line);
	    UPD(thisvw->vw_BlockS.pos_Line);
	    UPD(thisvw->vw_BlockE.pos_Line);
	    if(thisvw != curr_vw)
	    {
		UPD(thisvw->vw_StartLine);
		UPD(thisvw->vw_LastDisplayOrigin.pos_Line);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
    {
	UPD(VPOS(thismark->mk_Pos).pos_Line);
    }

    if(tx->tx_LogicalStart > ypos)
	tx->tx_LogicalStart += addy;
    UPD(tx->tx_LogicalEnd);

    UPD(tx->tx_SavedCPos.pos_Line);
    UPD(tx->tx_SavedWPos.pos_Line);
    UPD(tx->tx_SavedBlockPos[0].pos_Line);
    UPD(tx->tx_SavedBlockPos[1].pos_Line);

#if 0
#if 1
    if(tx->tx_ModStart.pos_Line > ypos)
	tx->tx_ModStart.pos_Line += addy;
#else
    UPD(tx->tx_ModStart.pos_Line);
#endif
    UPD(tx->tx_ModEnd.pos_Line);
#endif

#undef UPD
}

/*
 * Whole lines only please
 */
void
adjust_marks_sub_y(TX *tx, long suby, long ypos)
{
    VW *thisvw;
    Mark *thismark;

#define UPD(y)					\
    do {					\
	if(y > ypos)				\
	{					\
	    if((y -= suby) < ypos)		\
		y = ypos;			\
	}					\
    } while(0)

#define UPD2(x,y)				\
    if(y >= ypos)				\
    {						\
	if((y -= suby) < ypos)			\
	{					\
	    y = ypos;				\
	    x = 0;				\
	}					\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD2(thisvw->vw_CursorPos.pos_Col, thisvw->vw_CursorPos.pos_Line);
            if(thisvw->vw_Flags & VWFF_RECTBLOCKS)
	    {
		UPD(thisvw->vw_BlockS.pos_Line);
		UPD(thisvw->vw_BlockE.pos_Line);
	    }
            else
	    {
                UPD2(thisvw->vw_BlockS.pos_Col, thisvw->vw_BlockS.pos_Line);
                UPD2(thisvw->vw_BlockE.pos_Col, thisvw->vw_BlockE.pos_Line);
	    }
	    if(thisvw != curr_vw)
	    {
		UPD(thisvw->vw_StartLine);
		UPD(thisvw->vw_LastDisplayOrigin.pos_Line);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
    {
	UPD2(VPOS(thismark->mk_Pos).pos_Col, VPOS(thismark->mk_Pos).pos_Line);
    }

    UPD(tx->tx_LogicalStart);
    UPD(tx->tx_LogicalEnd);

    UPD2(tx->tx_SavedCPos.pos_Col, tx->tx_SavedCPos.pos_Line);
    UPD2(tx->tx_SavedWPos.pos_Col, tx->tx_SavedWPos.pos_Line);
    UPD2(tx->tx_SavedBlockPos[0].pos_Col, tx->tx_SavedBlockPos[0].pos_Line);
    UPD2(tx->tx_SavedBlockPos[1].pos_Col, tx->tx_SavedBlockPos[1].pos_Line);

    UPD2(tx->tx_ModStart.pos_Col, tx->tx_ModStart.pos_Line);
#if 1
    if(tx->tx_ModEnd.pos_Line > ypos)
    {
	if((tx->tx_ModEnd.pos_Line -= suby) < ypos)
	{
	    tx->tx_ModEnd.pos_Line = ypos;
	    tx->tx_ModEnd.pos_Col = 0;
	}
    }
#else
    UPD2(tx->tx_ModEnd.pos_Col, tx->tx_ModEnd.pos_Line);
#endif

#undef UPD
#undef UPD2
}

/*
 * Use when splitting a line into 2, cursor should be at position of split
 */
void
adjust_marks_split_y(TX *tx, long xpos, long ypos)
{
    VW *thisvw;
    Mark *thismark;

#define UPD(y)					\
    do {					\
	if(y > ypos)				\
	    y++;				\
    } while(0)

#define UPD2(x,y)				\
    do {					\
	if((y == ypos) && (x >= xpos))		\
	{					\
	    x -= xpos;				\
	    y++;				\
	}					\
	else if(y > ypos)			\
	    y++;				\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD2(thisvw->vw_CursorPos.pos_Col, thisvw->vw_CursorPos.pos_Line);
            if(thisvw->vw_Flags & VWFF_RECTBLOCKS)
	    {
		UPD(thisvw->vw_BlockS.pos_Line);
		UPD(thisvw->vw_BlockE.pos_Line);
	    }
            else
	    {
                UPD2(thisvw->vw_BlockS.pos_Col, thisvw->vw_BlockS.pos_Line);
                UPD2(thisvw->vw_BlockE.pos_Col, thisvw->vw_BlockE.pos_Line);
	    }
	    if(thisvw != curr_vw)
	    {
		UPD(thisvw->vw_StartLine);
		UPD(thisvw->vw_LastDisplayOrigin.pos_Line);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
    {
	UPD2(VPOS(thismark->mk_Pos).pos_Col, VPOS(thismark->mk_Pos).pos_Line);
    }

    UPD(tx->tx_LogicalStart);
    UPD(tx->tx_LogicalEnd);

    UPD2(tx->tx_SavedCPos.pos_Col, tx->tx_SavedCPos.pos_Line);
    UPD2(tx->tx_SavedWPos.pos_Col, tx->tx_SavedWPos.pos_Line);
    UPD2(tx->tx_SavedBlockPos[0].pos_Col, tx->tx_SavedBlockPos[0].pos_Line);
    UPD2(tx->tx_SavedBlockPos[1].pos_Col, tx->tx_SavedBlockPos[1].pos_Line);

#if 1
    if((tx->tx_ModStart.pos_Line == ypos) && (tx->tx_ModStart.pos_Col > xpos))
    {
	tx->tx_ModStart.pos_Col -= xpos;
	tx->tx_ModStart.pos_Line++;
    }
    else if(tx->tx_ModStart.pos_Line > ypos)
	tx->tx_ModStart.pos_Line++;
#else
    UPD2(tx->tx_ModStart.pos_Col, tx->tx_ModStart.pos_Line);
#endif
    UPD2(tx->tx_ModEnd.pos_Col, tx->tx_ModEnd.pos_Line);

#undef UPD
#undef UPD2
}

/*
 * Use when compacting 2 adjacent lines into one
 */
void
adjust_marks_join_y(TX *tx, long xpos, long ypos)
{
    VW *thisvw;
    Mark *thismark;

#define UPD(y)					\
    do {					\
	if(y > ypos)				\
	    y--;				\
    } while(0)

#define UPD2(x,y)				\
    do {					\
	if(y > ypos)				\
	{					\
	    if(y == ypos + 1)			\
	    {					\
		x += xpos;			\
		y--;				\
	    }					\
	}					\
    } while(0)

    for(thisvw = view_chain; thisvw; thisvw = thisvw->vw_Next)
    {
	if(thisvw->vw_Tx == tx)
	{
	    UPD2(thisvw->vw_CursorPos.pos_Col, thisvw->vw_CursorPos.pos_Line);
            if(thisvw->vw_Flags & VWFF_RECTBLOCKS)
	    {
		UPD(thisvw->vw_BlockS.pos_Line);
		UPD(thisvw->vw_BlockE.pos_Line);
	    }
            else
	    {
                UPD2(thisvw->vw_BlockS.pos_Col, thisvw->vw_BlockS.pos_Line);
                UPD2(thisvw->vw_BlockE.pos_Col, thisvw->vw_BlockE.pos_Line);
	    }
	    if(thisvw != curr_vw)
	    {
		UPD(thisvw->vw_StartLine);
		UPD(thisvw->vw_LastDisplayOrigin.pos_Line);
	    }
	}
    }
    for(thismark = tx->tx_MarkChain; thismark; thismark = thismark->mk_Next)
    {
	UPD2(VPOS(thismark->mk_Pos).pos_Col, VPOS(thismark->mk_Pos).pos_Line);
    }

    UPD(tx->tx_LogicalStart);
    UPD(tx->tx_LogicalEnd);

    UPD2(tx->tx_SavedCPos.pos_Col, tx->tx_SavedCPos.pos_Line);
    UPD2(tx->tx_SavedWPos.pos_Col, tx->tx_SavedWPos.pos_Line);
    UPD2(tx->tx_SavedBlockPos[0].pos_Col, tx->tx_SavedBlockPos[0].pos_Line);
    UPD2(tx->tx_SavedBlockPos[1].pos_Col, tx->tx_SavedBlockPos[1].pos_Line);

    UPD2(tx->tx_ModStart.pos_Col, tx->tx_ModStart.pos_Line);
    UPD2(tx->tx_ModEnd.pos_Col, tx->tx_ModEnd.pos_Line);

#undef UPD
#undef UPD2
}

/* These routines are called to recalculate the cursor's position on the
   screen...  */
static void
resync_x(VW *vw)
{
    long offset;
    calc_cursor_offset(vw);
    offset = vw->vw_LastCursorOffset;
    while((offset - vw->vw_StartCol) >= vw->vw_MaxX)
    {
	vw->vw_StartCol += vw->vw_XStep;
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
    }
    while(offset < vw->vw_StartCol)
    {
	vw->vw_StartCol -= vw->vw_XStep;
	if(vw->vw_StartCol < 0)
	    vw->vw_StartCol = 0;
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
    }
}

static void
resync_y(VW *vw)
{
    TX *tx = vw->vw_Tx;
    long y;

    if(vw->vw_CursorPos.pos_Line < tx->tx_LogicalStart)
    {
	vw->vw_CursorPos.pos_Line = tx->tx_LogicalStart;
	vw->vw_CursorPos.pos_Col = 0;
    }
    if(vw->vw_CursorPos.pos_Line >= tx->tx_LogicalEnd)
    {
	vw->vw_CursorPos.pos_Line = tx->tx_LogicalEnd - 1;
	vw->vw_CursorPos.pos_Col
	    = tx->tx_Lines[vw->vw_CursorPos.pos_Line].ln_Strlen - 1;
    }

    y = vw->vw_CursorPos.pos_Line - vw->vw_StartLine;
    if(y < 0 || vw->vw_StartLine < tx->tx_LogicalStart)
    {
	if(-y > vw->vw_YStep)
	    vw->vw_StartLine = vw->vw_CursorPos.pos_Line - (vw->vw_MaxY / 2);
	else
	    vw->vw_StartLine -= vw->vw_YStep;
	if(vw->vw_StartLine < tx->tx_LogicalStart)
	    vw->vw_StartLine = tx->tx_LogicalStart;
	else if(vw->vw_StartLine >= tx->tx_LogicalEnd)
	    vw->vw_StartLine = tx->tx_LogicalEnd - 1;
    }
    else if(y >= vw->vw_MaxY || vw->vw_StartLine >= tx->tx_LogicalEnd)
    {
	if((vw->vw_MaxY + vw->vw_YStep) <= y)
	    vw->vw_StartLine = vw->vw_CursorPos.pos_Line - (vw->vw_MaxY / 2);
	else
	    vw->vw_StartLine += vw->vw_YStep;
	if(vw->vw_StartLine < tx->tx_LogicalStart)
	    vw->vw_StartLine = tx->tx_LogicalStart;
	else if(vw->vw_StartLine >= tx->tx_LogicalEnd)
	    vw->vw_StartLine = tx->tx_LogicalEnd - 1;
	/* Check for a `gap' at the bottom of the display */
	if((tx->tx_NumLines >= vw->vw_MaxY)
	   && ((tx->tx_NumLines - vw->vw_StartLine) < vw->vw_MaxY))
	{
	    vw->vw_StartLine = tx->tx_NumLines - vw->vw_MaxY;
	}
    }
}

void
resync_xy(VW *vw)
{
    if(vw->vw_Tx != vw->vw_LastRefTx)
	/* kludge */
	vw->vw_LastDisplayOrigin = vw->vw_DisplayOrigin;
    resync_x(vw);
    resync_y(vw);
}

void
set_start_col(VW *vw, long col)
{
    if(vw->vw_StartCol != col)
    {
	vw->vw_Flags |= VWFF_FORCE_REFRESH;
	vw->vw_StartCol = col;
    }
}

void
set_start_line(VW *vw, long line)
{
    long cline = vw->vw_StartLine;
    if(line != cline)
    {
	long yord = vw->vw_CursorPos.pos_Line - cline;
	vw->vw_StartLine = line;
	vw->vw_CursorPos.pos_Line = line + yord;
	if(vw->vw_CursorPos.pos_Line >= vw->vw_Tx->tx_LogicalEnd)
	    vw->vw_CursorPos.pos_Line = vw->vw_Tx->tx_LogicalEnd - 1;
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
	    thisvw->vw_CursorPos.pos_Col = 0;
	    thisvw->vw_CursorPos.pos_Line = 0;
	    thisvw->vw_StartCol = 0;
	    thisvw->vw_StartLine = 0;
	    thisvw->vw_BlockStatus = -1;
	    thisvw->vw_Flags |= VWFF_FORCE_REFRESH;
#if 0
	    thisvw->vw_Flags &= ~VWFF_MESSAGE;
#endif
	}
	tx->tx_SavedCPos.pos_Col = 0;
	tx->tx_SavedCPos.pos_Line = 0;
	tx->tx_SavedWPos.pos_Col = 0;
	tx->tx_SavedWPos.pos_Line = 0;
	tx->tx_SavedBlockStatus = -1;
    }
}
