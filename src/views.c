/* views.c -- Handling of panes in each window
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

#include <stdlib.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR void kill_all_views(WIN *w);
_PR void update_views_dimensions(WIN *w);
_PR void update_status_buffer(VW *vw);
_PR void views_init(void);
_PR void views_kill(void);
_PR void view_sweep(void);
_PR void view_prin(VALUE, VALUE);
_PR VW *make_view(VW *, WIN *, TX *, long, bool);

_PR VALUE sym_make_view_hook, sym_destroy_view_hook;
DEFSYM(make_view_hook, "make-view-hook");
DEFSYM(destroy_view_hook, "destroy-view-hook");

static void set_scroll_steps(VW *vw);
static void recalc_measures(WIN *w);

/* view_chain is a list of all allocated VW structures, linked through
   their vw_Next fields. curr_vw is the currently active view; a mirror
   of curr_win->vw_CurrVW. */
_PR VW *view_chain, *curr_vw;
VW *view_chain, *curr_vw;

/* This buffer is put into minibuffer views when they're not being
   used. */
_PR TX *mb_unused_buffer;
TX *mb_unused_buffer;
DEFSTRING(unused_mb, "*unused-minibuf*");

/* Copy some preferences from SRC to DEST; SRC may be null */
static void
copy_view_prefs(VW *dest, VW *src)
{
    if(src)
    {
	dest->vw_XStepRatio = src->vw_XStepRatio;
	dest->vw_YStepRatio = src->vw_YStepRatio;
    }
    else
    {
	dest->vw_XStepRatio = 4;
#ifdef HAVE_AMIGA
	dest->vw_YStepRatio = 0;
#else
	dest->vw_YStepRatio = 4;
#endif
    }
}

DEFSTRING(too_few_lines, "Too few lines to split");

/* Create a new view. SIBLING if non-null is the view to split into
   half to create space. PARENT if non-null is the window to install it
   in (if null the current window is used).

   If LINES is greater than 0 it defines the number of lines to give
   the view.

   MINIBUF-P controls whether or not the buffer is a minibuffer. */
VW *
make_view(VW *sibling, WIN *parent, TX *tx, long lines, bool minibuf_p)
{
    VW *vw;

    /* Try to initialise PARENT, SIBLING and TX. PARENT must be
       non-null, the others don't have to be if it's not possible. */
    if(sibling && sibling->vw_Flags & VWFF_MINIBUF)
    {
	/* Try to find the view above the minibuffer view */
	sibling = sibling->vw_Win->w_ViewList;
	while(sibling && sibling->vw_NextView
	      && sibling->vw_NextView->vw_NextView)
	    sibling = sibling->vw_NextView;
    }
    if(sibling)
    {
	if(!tx)
	    tx = sibling->vw_Tx;
	parent = sibling->vw_Win;
    }
    else
    {
	if(!parent)
	    parent = curr_win;
	if(!tx)
	{
	    if(parent && parent->w_ViewList && parent->w_ViewList->vw_Tx)
		tx = parent->w_ViewList->vw_Tx;
	    else
		tx = buffer_chain; /* whatever */
	}
	if(parent && parent->w_ViewList)
	    sibling = parent->w_ViewList;
    }

    /* Make sure that LINES is initialised to a value greater than
       zero. */
    if(lines == 0 && !minibuf_p)
    {
	if(sibling)
	{
	    lines = ((sibling->vw_MaxY + 1) / 2) - 1;
	    if(lines < 1)
		lines = 1;
	}
	else
	    lines = parent->w_MaxY - 2;
    }

    /* Check to see if there's space for the new view. */
    if(!minibuf_p)
    {
	if(sibling)
	{
	    if(sibling->vw_MaxY + 1 < lines + 3)
		goto size_error;
	}
	else
	{
	    if(parent->w_MaxY < lines + 2)
	    {
	    size_error:
		cmd_signal(sym_window_error,
			   list_2(VAL(parent), VAL(&too_few_lines)));
		return NULL;
	    }
	}
    }

    /* Now the construction of the view proper... */
    vw = ALLOC_OBJECT(sizeof(VW));
    if(vw != NULL)
    {
	memset(vw, 0, sizeof(VW));
	vw->vw_Car = V_View;
	vw->vw_Next = view_chain;
	view_chain = vw;
	vw->vw_Win = parent;
	parent->w_ViewCount++;
	copy_view_prefs(vw, sibling ? sibling : curr_vw);
	vw->vw_BlockStatus = -1;
	vw->vw_BufferList = sym_nil;
	vw->vw_StatusBuf = str_alloc(STATUS_BUFSIZ);

	/* Initialise the size of the new view, and resize its
	   SIBLING if it has one. */
	if(minibuf_p)
	{
	    VW *x;
	    /* This view is destined to be a minibuffer */
	    vw->vw_MaxX = parent->w_MaxX;
	    vw->vw_MaxY = 1;
	    swap_buffers(vw, mb_unused_buffer);
	    vw->vw_Flags |= VWFF_MINIBUF;
	    parent->w_MiniBuf = vw;
	    for(x = parent->w_ViewList; x->vw_NextView; x = x->vw_NextView)
		;
	    x->vw_NextView = vw;
	    vw->vw_NextView = NULL;
	}
	else if(sibling)
	{
	    /* Divide up the space used by SIBLING.  We know
	       that there's enough room for LINES new lines. */
	    vw->vw_NextView = sibling->vw_NextView;
	    sibling->vw_NextView = vw;
	    vw->vw_MaxX = sibling->vw_MaxX;
	    vw->vw_MaxY = lines;
	    sibling->vw_MaxY = ((sibling->vw_MaxY + 1)
				- (vw->vw_MaxY + 1)) - 1;
	    set_scroll_steps(sibling);
	}
	else
	{
	    /* All space in PARENT goes to the new view. */
	    vw->vw_NextView = NULL;
	    parent->w_ViewList = vw;
	    vw->vw_MaxX = parent->w_MaxX;
	    vw->vw_MaxY = parent->w_MaxY - 2;	/* status & minibuf */
	}
	recalc_measures(parent);
	set_scroll_steps(vw);

	/* Now try to initialise the new view's buffer. */
	if(!minibuf_p && tx)
	{
	    if(sibling && sibling->vw_Tx == tx)
	    {
		/* copy configuration of SIBLING */
		vw->vw_Tx = tx;
		vw->vw_CursorPos = sibling->vw_CursorPos;
		vw->vw_DisplayOrigin = sibling->vw_DisplayOrigin;
		vw->vw_BlockS = sibling->vw_BlockS;
		vw->vw_BlockE = sibling->vw_BlockE;
		vw->vw_BlockStatus = sibling->vw_BlockStatus;
	    }
	    else
	    {
		/* this doesn't always work as well as the above. */
		swap_buffers(vw, tx);
	    }
	    cmd_call_hook(sym_make_view_hook, LIST_1(VAL(vw)), sym_nil);
#ifndef NOSCRLBAR
	    sys_update_scroller(vw);
#endif
	}
	if(curr_vw == 0)
	    curr_vw = vw;
	if(parent->w_CurrVW == 0)
	    parent->w_CurrVW = vw;

	return vw;
    }
    return NULL;
}

_PR VALUE cmd_make_view(VALUE split_vw, VALUE tx, VALUE lines);
DEFUN("make-view", cmd_make_view, subr_make_view, (VALUE split_vw, VALUE tx, VALUE lines), V_Subr3, DOC_make_view) /*
::doc:make_view::
make-view [VIEW-TO-SPLIT] [BUFFER] [LINES]

Make a new view, using the space of the view VIEW-TO-SPLIT. BUFFER is
used to specify the buffer displayed in the new view. LINES can define
the number of lines desired in the new view.
::end:: */
{
    if(!VIEWP(split_vw))
	split_vw = VAL(curr_vw);
    return VAL(make_view(VVIEW(split_vw), VVIEW(split_vw)->vw_Win,
			 BUFFERP(tx) ? VTX(tx) : VVIEW(split_vw)->vw_Tx,
			 INTP(lines) ? VINT(lines) : 0, FALSE));
}

DEFSTRING(sole_view, "Can't kill the sole view in a window");
DEFSTRING(mini_view, "Can't kill minibuffer view");

_PR VALUE cmd_destroy_view(VALUE view);
DEFUN("destroy-view", cmd_destroy_view, subr_destroy_view, (VALUE view), V_Subr1, DOC_destroy_view) /*
::doc:destroy_view::
destroy-view [VIEW]

Close VIEW (or the current view). It's not allowed to have less than two
views (minibuffer and one other) in any window.
::end:: */
{
    VW *vw = VIEWP(view) ? VVIEW(view) : curr_vw;
    VW *pred;
    if(vw->vw_Win->w_ViewCount <= 2)
    {
	/* Only two views are left. Don't destroy it. */
	return cmd_signal(sym_window_error, list_2(VAL(&sole_view), VAL(vw)));
    }
    else if(vw->vw_Flags & VWFF_MINIBUF)
    {
	/* Can't kill the minibuffer */
	return cmd_signal(sym_window_error, list_2(VAL(&mini_view), VAL(vw)));
    }
    cmd_call_hook(sym_destroy_view_hook, LIST_1(VAL(vw)), sym_nil);
    sys_kill_vw(vw);
    vw->vw_Tx = NULL;
    vw->vw_BufferList = sym_nil;

    if(vw->vw_Win->w_ViewList == vw)
    {
	/* There's no predecessor to VW. So give its space to
	   the following view. If possible fix the display origin
	   of the following view to minimise scrolling. */
	long new_origin_col, new_origin_row;
	pred = vw->vw_NextView;
	vw->vw_Win->w_ViewList = pred;
	if(!skip_glyph_rows_backwards(pred, vw->vw_MaxY + 1,
				      VCOL(pred->vw_DisplayOrigin),
				      VROW(pred->vw_DisplayOrigin),
				      &new_origin_col, &new_origin_row))
	{
	    new_origin_col = 0;
	    new_origin_row = pred->vw_Tx->tx_LogicalStart;
	}
	pred->vw_DisplayOrigin = make_pos(new_origin_col, new_origin_row);
    }
    else
    {
	/* Find this view's predecessor. */
	for(pred = vw->vw_Win->w_ViewList; pred != 0; pred = pred->vw_NextView)
	{
	    if(pred->vw_NextView == vw)
		break;
	}
	pred->vw_NextView = vw->vw_NextView;
    }
    if(vw->vw_Win->w_CurrVW == vw)
	vw->vw_Win->w_CurrVW = pred;
    if(curr_vw == vw)
	curr_vw = pred;
    /* VW is now unlinked; now gives its window-space to PRED. */
    pred->vw_MaxY += vw->vw_MaxY + 1;
    recalc_measures(pred->vw_Win);
    set_scroll_steps(pred);
    vw->vw_Win->w_ViewCount--;
    vw->vw_Win = NULL;
    vw->vw_NextView = NULL;
    return(VAL(vw));
}

/* Destroy all views of window W. */
void
kill_all_views(WIN *w)
{
    VW *vw = w->w_ViewList;
    while(vw != 0)
    {
	VW *next = vw->vw_NextView;
	vw->vw_NextView = NULL;
	vw->vw_Tx = NULL;
	vw->vw_Win = NULL;
	vw->vw_BufferList = sym_nil;
	vw = next;
    }
    w->w_ViewCount = 0;
}

/* Initialise the scroll steps in VW, from the size of the view and 
   the desired ratios. */
static void
set_scroll_steps(VW *vw)
{
    if((vw->vw_XStepRatio <= 0)
       || ((vw->vw_XStep = vw->vw_MaxX / vw->vw_XStepRatio) <= 0)
       || (vw->vw_XStep > vw->vw_MaxX))
	vw->vw_XStep = 1;
    if((vw->vw_YStepRatio <= 0)
       || ((vw->vw_YStep = vw->vw_MaxY / vw->vw_YStepRatio) <= 0)
       || (vw->vw_YStep > vw->vw_MaxY))
	vw->vw_YStep = 1;
}

/* For each view in window W, recalculate all view positions from
   the MaxX and MaxY settings. */
static void
recalc_measures(WIN *w)
{
    VW *vw;
    int row = 0;
    for(vw = w->w_ViewList; vw != 0; vw = vw->vw_NextView)
    {
	vw->vw_FirstX = 0;
	vw->vw_FirstY = row;
	row += vw->vw_MaxY + ((vw->vw_Flags & VWFF_MINIBUF) ? 0 : 1);
    }
}

/* Recalibrate the sizes of each view in window W. If the size of the
   window has changed recently the views dimensions will have to be
   changed; it tries to do this such that the general weighting between
   each view stays the same. Horrible things could happen if there's not
   enough window space for all the views... */
void
update_views_dimensions(WIN *w)
{
    int lines_given = 0;
    int old_total_lines = 0;
    VW *vw;
    for(vw = w->w_ViewList; vw != 0; vw = vw->vw_NextView)
	old_total_lines += vw->vw_MaxY + 1;
    old_total_lines--;		/* minibuf has no status line */
    if(old_total_lines == w->w_MaxY && w->w_MaxX == w->w_ViewList->vw_MaxX)
	return;
    for(vw = w->w_ViewList; vw != 0; vw = vw->vw_NextView)
    {
	vw->vw_FirstX = 0;
	vw->vw_MaxX = w->w_MaxX;
	if(vw->vw_Flags & VWFF_MINIBUF)
	    vw->vw_MaxY = 1;
	else
	{
	    if(vw->vw_NextView->vw_Flags & VWFF_MINIBUF)
		/* Last view. Give it the remainder (except
		   the minibuf's line). */
		vw->vw_MaxY = w->w_MaxY - lines_given - 2;
	    else
		/* Otherwise try to keep the old weighting. */
		vw->vw_MaxY = (((vw->vw_MaxY + 1) * w->w_MaxY)
			       / old_total_lines) - 1;
	}
	vw->vw_FirstY = lines_given;
	lines_given += vw->vw_MaxY + 1;
	set_scroll_steps(vw);
    }
}

/* Reformat the status string of VW, and flag that it needs to be
   redrawn. */
void
update_status_buffer(VW *vw)
{
    TX *tx = vw->vw_Tx;
    u_char *block;
    bool restriction = (tx->tx_LogicalStart != 0)
			|| (tx->tx_LogicalEnd != tx->tx_NumLines);
    long lines = tx->tx_LogicalEnd - tx->tx_LogicalStart;
    long glyph_col = get_cursor_column(vw);
    char *position, position_buf[5];

    if(vw->vw_Flags & VWFF_MINIBUF
       || vw->vw_Flags & VWFF_CUSTOM_STATUS)
	return;

    if(vw->vw_BlockStatus >= 0)
    {
	if(vw->vw_BlockStatus == 0)
	    block = "B";
	else
	    block = "b";
    }
    else
	block = "";

    if(VROW(vw->vw_DisplayOrigin) <= tx->tx_LogicalStart)
    {
	if(vw->vw_Flags & VWFF_AT_BOTTOM)
	    position = "All";
	else
	    position = "Top";
    }
    else if(vw->vw_Flags & VWFF_AT_BOTTOM)
	position = "Bottom";
    else
    {
	sprintf(position_buf, "%ld%%",
		((VROW(vw->vw_DisplayOrigin) - tx->tx_LogicalStart)
		 * 100) / lines);
	position = position_buf;
    }

    /* TODO: allow customisation via mode-line-format and % formatters */

    sprintf(vw->vw_StatusBuf, "%s%s %c%s%s%c %c%ld,%ld%c %s of %ld %s %s",
	    VSTR(tx->tx_BufferName),
	    ((tx->tx_Changes != tx->tx_ProperSaveChanges)
	     && (!(tx->tx_Flags & TXFF_SPECIAL)))
	    ? "+" : (tx->tx_Flags & TXFF_RDONLY ? "-" : ""),
	    (recurse_depth ? '[' : '('),
	    (tx->tx_ModeName ? (char *)VSTR(tx->tx_ModeName) : "generic"),
	    VSTR(tx->tx_MinorModeNameString),
	    (recurse_depth ? ']' : ')'),
	    restriction ? '[' : '(',
	    glyph_col + 1, VROW(vw->vw_CursorPos) - tx->tx_LogicalStart + 1,
	    restriction ? ']' : ')',
	    position,
	    lines, lines != 1 ? "lines" : "line",
	    block);
}

_PR VALUE var_y_scroll_step_ratio(VALUE val);
DEFUN("y-scroll-step-ratio", var_y_scroll_step_ratio, subr_y_scroll_step_ratio, (VALUE val), V_Var, DOC_y_scroll_step_ratio) /*
::doc:y_scroll_step_ratio::
Controls the actual number of lines scrolled when the cursor moves out of
view. The number of lines to move the display origin is calcualted with the
formula:
  LINES_TO_SCROLL = TOTAL_LINES_IN_VIEW / y-scroll-step-ratio
If the value is 0 then the window will be scrolled by one line.
::end:: */
{
    VW *vw = curr_vw;
    if(val)
    {
	if(INTP(val))
	{
	    vw->vw_YStepRatio = VINT(val);
	    set_scroll_steps(vw);
	}
	return LISP_NULL;
    }
    return(MAKE_INT(vw->vw_YStepRatio));
}

_PR VALUE var_x_scroll_step_ratio(VALUE val);
DEFUN("x-scroll-step-ratio", var_x_scroll_step_ratio, subr_x_scroll_step_ratio, (VALUE val), V_Var, DOC_x_scroll_step_ratio) /*
::doc:x_scroll_step_ratio::
Controls the actual number of columns scrolled when the cursor moves out of
view. The number of lines to move the display origin is calcualted with the
formula:
  COLUMNS_TO_SCROLL = TOTAL_COLUMNS_IN_VIEW / x-scroll-step-ratio
If the value is 0 then the window will be scrolled by one column.
::end:: */
{
    VW *vw = curr_vw;
    if(val)
    {
	if(INTP(val))
	{
	    vw->vw_XStepRatio = VINT(val);
	    set_scroll_steps(vw);
	}
	return LISP_NULL;
    }
    return(MAKE_INT(vw->vw_XStepRatio));
}

_PR VALUE cmd_rect_blocks_p(VALUE vw);
DEFUN("rect-blocks-p", cmd_rect_blocks_p, subr_rect_blocks_p, (VALUE vw), V_Subr1, DOC_rect_blocks_p) /*
::doc:rect_blocks_p::
rect-blocks-p [VIEW]

Returns t if blocks marked in VIEW (or the current one) are treated as
rectangles.
::end:: */
{
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    if(VVIEW(vw)->vw_Flags & VWFF_RECTBLOCKS)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_set_rect_blocks(VALUE vw, VALUE stat);
DEFUN("set-rect-blocks", cmd_set_rect_blocks, subr_set_rect_blocks, (VALUE vw, VALUE stat), V_Subr2, DOC_set_rect_blocks) /*
::doc:set_rect_blocks::
set-rect-blocks VIEW STATUS

Controls whether or not blocks are taken as contiguous regions of text or as
rectangles in VIEW. When STATUS is t rectangles are used.
::end:: */
{
    int oflags;
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    oflags = VVIEW(vw)->vw_Flags;
    if(NILP(stat))
	VVIEW(vw)->vw_Flags &= ~VWFF_RECTBLOCKS;
    else
	VVIEW(vw)->vw_Flags |= VWFF_RECTBLOCKS;
    return(stat);
}

_PR VALUE cmd_current_view(VALUE win);
DEFUN("current-view", cmd_current_view, subr_current_view, (VALUE win), V_Subr1,  DOC_current_view) /*
::doc:current_view::
current-view [WINDOW]

Returns the currently active view in WINDOW.
::end:: */
{
    return(WINDOWP(win) ? VAL(VWIN(win)->w_CurrVW) : VAL(curr_vw));
}

_PR VALUE cmd_set_current_view(VALUE vw, VALUE activ);
DEFUN("set-current-view", cmd_set_current_view, subr_set_current_view, (VALUE vw, VALUE activ), V_Subr2,  DOC_set_current_view) /*
::doc:set_current_view::
set-current-view VIEW [ACTIVATE-P]

Sets the VIEW which jade reguards as current in the window containing it.
If ACTIVATE-P is t then the window containing VIEW will be made current
and activated.
::end:: */
{
    DECLARE1(vw, VIEWP);
    VVIEW(vw)->vw_Win->w_CurrVW = VVIEW(vw);
    if(VVIEW(vw)->vw_Win == curr_win)
    {
	curr_vw = VVIEW(vw);
	curr_vw->vw_Win->w_CurrVW = curr_vw;
    }
    if(!NILP(activ))
	cmd_set_current_window(VAL(VVIEW(vw)->vw_Win), activ);
    return(vw);
}

_PR VALUE var_buffer_list(VALUE val);
DEFUN("buffer-list", var_buffer_list, subr_buffer_list, (VALUE val), V_Var, DOC_buffer_list) /*
::doc:buffer_list::
List of buffers in most-recently-used order. Each view has it's own.
::end:: */
{
    if(val)
	curr_vw->vw_BufferList = val;
    return(curr_vw->vw_BufferList);
}

_PR VALUE cmd_next_view(VALUE win, VALUE allp);
DEFUN("next-view", cmd_next_view, subr_next_view, (VALUE win, VALUE allp), V_Subr2, DOC_next_view) /*
::doc:next_view::
next-view [WINDOW] [ALL-WINDOWS-P]

Return the next view in WINDOW. If ALL-WINDOWS-P is t then views in
other windows will be returned after the last in WINDOW (or the current
window) is encountered.
If WINDOW is actually a view, the following view will be returned,
according to the same rules.
::end:: */
{
    VW *curr;
    if(VIEWP(win))
    {
	curr = VVIEW(win);
	win = VAL(curr->vw_Win);
    }
    else if(WINDOWP(win))
	curr = VWIN(win)->w_CurrVW;
    else
    {
	curr = curr_vw;
	win = VAL(curr_win);
    }
    /* If possible just return the next view in the original
       window. Minibuffer views are only allowed if they're used. */
    if(curr->vw_NextView != 0
       && (!(curr->vw_NextView->vw_Flags & VWFF_MINIBUF)
	   || MINIBUFFER_ACTIVE_P(curr_vw->vw_NextView->vw_Win)))
	return VAL(curr->vw_NextView);
    else
    {
	if(NILP(allp))
	    /* First view of the original window. */
	    return VAL(VWIN(win)->w_ViewList);
	else
	{
	    if(VWIN(win)->w_Next != 0)
		return VAL(VWIN(win)->w_Next->w_ViewList);
	    else
		return VAL(win_chain->w_ViewList);
	}
    }
}

_PR VALUE cmd_previous_view(VALUE win, VALUE allp);
DEFUN("previous-view", cmd_previous_view, subr_previous_view, (VALUE win, VALUE allp), V_Subr2, DOC_previous_view) /*
::doc:previous_view::
previous-view [WINDOW] [ALL-WINDOWS-P]

Return the previous view in WINDOW. ALL-WINDOWS-P controls whether or
not to cycle through the views in other windows after reaching the last
view of WINDOW (or the current window).
If WINDOW is actually a view, the following view will be returned,
according to the same rules.
::end:: */
{
    VW *vw;
    VW *curr;
    if(VIEWP(win))
    {
	curr = VVIEW(win);
	win = VAL(curr->vw_Win);
    }
    else if(WINDOWP(win))
	curr = VWIN(win)->w_CurrVW;
    else
    {
	curr = curr_vw;
	win = VAL(curr_win);
    }
    if(curr == VWIN(win)->w_ViewList)
    {
	/* current view is first in this window. If ALLP is t
	   need to find the previous window and the last view in it.
	   otherwise the last view in the current window. */
	WIN *w;
	if(!NILP(allp))
	{
	    w = VWIN(win);
	    if(w == win_chain)
	    {
		/* first window, find last */
		while(w->w_Next != 0)
		    w = w->w_Next;
	    }
	    else
	    {
		/* find predecessor of W */
		while(w->w_Next != w)
		    w = w->w_Next;
	    }
	}
	else
	    w = VWIN(win);
	/* now simply find the last view in W, handling minibuffer
	   views appropriately. */
	vw = w->w_ViewList;
	if(MINIBUFFER_ACTIVE_P(w))
	{
	    while(vw->vw_NextView != 0)
		vw = vw->vw_NextView;
	}
	else
	{
	    while(!(vw->vw_NextView->vw_Flags & VWFF_MINIBUF))
		vw = vw->vw_NextView;
	}
    }
    else
    {
	/* find the predecessor of the current view */
	vw = VWIN(win)->w_ViewList;
	while(vw->vw_NextView != curr)
	    vw = vw->vw_NextView;
    }
    return VAL(vw);
}

_PR VALUE cmd_with_view(VALUE args);
DEFUN("with-view", cmd_with_view, subr_with_view, (VALUE args), V_SF, DOC_with_view) /*
::doc:with_view::
with-view VIEW FORMS...

Set the editor's current view to VIEW (and the current window to that
containing VIEW) evaluate FORMS..., then reinstall the originals
afterwards, returning the value of (progn FORMS...).
::end:: */
{
    if(CONSP(args))
    {
	GC_root gc_args;
	VALUE res;
	PUSHGC(gc_args, args);
	if((res = cmd_eval(VCAR(args))) && VIEWP(res))
	{
	    VALUE oldvw = VAL(curr_vw);
	    GC_root gc_oldvw;
	    curr_vw = VVIEW(res);
	    curr_win = curr_vw->vw_Win;
	    curr_win->w_CurrVW = curr_vw;

	    PUSHGC(gc_oldvw, oldvw);
	    res = cmd_progn(VCDR(args));
	    POPGC;

	    /* Reinstall the old view */
	    if(VVIEW(oldvw)->vw_Win
	       && VVIEW(oldvw)->vw_Win->w_Window != WINDOW_NIL)
	    {
		curr_vw = VVIEW(oldvw);
		curr_win = curr_vw->vw_Win;
		curr_win->w_CurrVW = curr_vw;
	    }
	}
	else
	    res = signal_arg_error(res, 1);
	POPGC;
	return(res);
    }
    return LISP_NULL;
}

_PR VALUE cmd_view_origin(VALUE vw);
DEFUN("view-origin", cmd_view_origin, subr_view_origin, (VALUE vw), V_Subr1, DOC_view_origin) /*
::doc:view_origin::
view-origin [VIEW]

Return the position of the character displayed in the top-left corner of
either VIEW or the current view.
::end:: */
{
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    return VVIEW(vw)->vw_DisplayOrigin;
}

_PR VALUE cmd_view_dimensions(VALUE vw);
DEFUN("view-dimensions", cmd_view_dimensions, subr_view_dimensions, (VALUE vw), V_Subr2, DOC_view_dimensions) /*
::doc:view_dimensions::
view-dimensions [VIEW]

Returns (COLUMNS . ROWS) defining the size (in glyphs) of VIEW (by default
the current view).
::end:: */
{
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    return cmd_cons(MAKE_INT(VVIEW(vw)->vw_MaxX),
		    MAKE_INT(VVIEW(vw)->vw_MaxY));
}

DEFSTRING(no_view, "No view to expand into");
DEFSTRING(no_room, "Not enough room");

_PR VALUE cmd_set_view_dimensions(VALUE vw, VALUE cols, VALUE rows);
DEFUN("set-view-dimensions", cmd_set_view_dimensions, subr_set_view_dimensions, (VALUE vw, VALUE cols, VALUE rows), V_Subr3, DOC_set_view_dimensions) /*
::doc:set_view_dimensions::
set-view-dimensions [VIEW] [COLUMNS] [ROWS]

Set the size of VIEW (or the current view) to COLUMNSxROWS glyphs. This is
done by changing the size of the following view, but no others (except VIEW
of course). If there isn't enough room a window-error is signalled.

Note that due to horizontal division of windows not actually being supported
the COLUMNS parameter is always ignored (for the moment).
::end:: */
{
    VW *sibling;
    long new_sibling_height;
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    if(!INTP(rows))
	return vw;
    sibling = VVIEW(vw)->vw_NextView;
    if(sibling == 0 || sibling->vw_Flags & VWFF_MINIBUF)
    {
	return cmd_signal(sym_window_error, list_2(VAL(&no_view), vw));
    }
    new_sibling_height = sibling->vw_MaxY - (VINT(rows) - VVIEW(vw)->vw_MaxY);
    if(new_sibling_height < 1 || VINT(rows) < 1)
    {
	return cmd_signal(sym_window_error, list_2(VAL(&no_room), vw));
    }
    VVIEW(vw)->vw_MaxY = VINT(rows);
    sibling->vw_MaxY = new_sibling_height;
    recalc_measures(VVIEW(vw)->vw_Win);
    set_scroll_steps(VVIEW(vw));
    set_scroll_steps(sibling);
    return vw;
}

_PR VALUE cmd_find_view_by_pos(VALUE pos, VALUE win);
DEFUN("find-view-by-pos", cmd_find_view_by_pos, subr_find_view_by_pos, (VALUE pos, VALUE win), V_Subr2, DOC_find_view_by_pos) /*
::doc:find_view_by_pos::
find-view-by-pos POS [WINDOW]

Attempt to find the view in the current window (or in WINDOW), that includes
the glyph at position POS in the window. Returns nil if no such view exists.
::end:: */
{
    WIN *w = WINDOWP(win) ? VWIN(win) : curr_win;
    VW *vw = w->w_ViewList;
    DECLARE1(pos, POSP);
    if(VROW(pos) < 0)
	return sym_nil;
    while(vw != NULL)
    {
	/* vw_MaxY doesn't include the status line */
	long bottom = vw->vw_FirstY + vw->vw_MaxY;
	if(vw->vw_Flags & VWFF_MINIBUF)
	   bottom++;
	if(VROW(pos) < bottom)
	    return VAL(vw);
	vw = vw->vw_NextView;
    }
    return sym_nil;
}

_PR VALUE cmd_translate_pos_to_view(VALUE pos, VALUE vw);
DEFUN("translate-pos-to-view", cmd_translate_pos_to_view, subr_translate_pos_to_view, (VALUE pos, VALUE vw), V_Subr2, DOC_translate_pos_to_view) /*
::doc:translate_pos_to_view::
translate-pos-to-view POS [VIEW]

Return the screen position in the current view (or in VIEW) that corresponds
to the screen position POSITION in the view's window.
::end:: */
{
    long col, row;
    DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    col = VCOL(pos) - VVIEW(vw)->vw_FirstX;
    row = VROW(pos) - VVIEW(vw)->vw_FirstY;
    return make_pos(col, row);
}

_PR VALUE cmd_minibuffer_view_p(VALUE vw);
DEFUN("minibuffer-view-p", cmd_minibuffer_view_p, subr_minibuffer_view_p, (VALUE vw), V_Subr1,  DOC_minibuffer_view_p) /*
::doc:minibuffer_view_p::
minibuffer-view-p [VIEW]

Returns t if VIEW is a view of a minibuffer.
::end:: */
{
    if(!VIEWP(vw))
	vw = VAL(curr_vw);
    return (VVIEW(vw)->vw_Flags & VWFF_MINIBUF) ? sym_t : sym_nil;
}

_PR VALUE cmd_minibuffer_view(VALUE win);
DEFUN("minibuffer-view", cmd_minibuffer_view, subr_minibuffer_view, (VALUE win), V_Subr1,  DOC_minibuffer_view) /*
::doc:minibuffer_view::
minibuffer-view [WINDOW]

Returns the view of the minibuffer in WINDOW (or the current window).
::end:: */
{
    return VAL(WINDOWP(win) ? VWIN(win)->w_MiniBuf : curr_win->w_MiniBuf);
}

_PR VALUE cmd_minibuffer_active_p(VALUE win);
DEFUN("minibuffer-active-p", cmd_minibuffer_active_p, subr_minibuffer_active_p, (VALUE win), V_Subr1,  DOC_minibuffer_active_p) /*
::doc:minibuffer_active_p::
minibuffer-active-p [WINDOW]

Returns t if the minibuffer of WINDOW is being used.
::end:: */
{
    return MINIBUFFER_ACTIVE_P(WINDOWP(win) ? VWIN(win) : curr_win)
	? sym_t : sym_nil;
}

_PR VALUE cmd_set_status_message(VALUE text, VALUE vw);
DEFUN("set-status-message", cmd_set_status_message, subr_set_status_message, (VALUE text, VALUE vw), V_Subr2, DOC_set_status_message) /*
::doc:set_status_message:
set-status-message TEXT [VIEW]

Overrides the normal behaviour of the status line in VIEW (or the current
view), displaying the string TEXT instead of the normal status information.
If TEXT is the symbol nil, the normal behaviour is reinstated.
::end:: */
{
    VALUE res = sym_nil;
    if(!VIEWP(vw))
	vw = VAL(curr_vw);

    if(VVIEW(vw)->vw_Flags & VWFF_CUSTOM_STATUS)
	res = string_dup(VVIEW(vw)->vw_StatusBuf);

    if(STRINGP(text))
    {
	int len = STRING_LEN(text);
	if(len >= STATUS_BUFSIZ)
	    len = STATUS_BUFSIZ - 1;
	memcpy(VVIEW(vw)->vw_StatusBuf, VSTR(text), len);
	VVIEW(vw)->vw_StatusBuf[len] = 0;
	VVIEW(vw)->vw_Flags |= VWFF_CUSTOM_STATUS;
    }
    else
	VVIEW(vw)->vw_Flags &= ~VWFF_CUSTOM_STATUS;
    return res;
}
	
void
views_init(void)
{
    mb_unused_buffer = VTX(cmd_make_buffer(VAL(&unused_mb), sym_nil, sym_t));
    cmd_set_buffer_special(VAL(mb_unused_buffer), sym_t);
    cmd_set_buffer_read_only(VAL(mb_unused_buffer), sym_t);

    ADD_SUBR(subr_make_view);
    ADD_SUBR(subr_destroy_view);
    ADD_SUBR(subr_y_scroll_step_ratio);
    ADD_SUBR(subr_x_scroll_step_ratio);
    ADD_SUBR(subr_rect_blocks_p);
    ADD_SUBR(subr_set_rect_blocks);
    ADD_SUBR(subr_current_view);
    ADD_SUBR(subr_set_current_view);
    ADD_SUBR(subr_buffer_list);
    ADD_SUBR(subr_next_view);
    ADD_SUBR(subr_previous_view);
    ADD_SUBR(subr_with_view);
    ADD_SUBR(subr_view_origin);
    ADD_SUBR(subr_view_dimensions);
    ADD_SUBR(subr_set_view_dimensions);
    ADD_SUBR(subr_find_view_by_pos);
    ADD_SUBR(subr_translate_pos_to_view);
    ADD_SUBR(subr_minibuffer_view_p);
    ADD_SUBR(subr_minibuffer_view);
    ADD_SUBR(subr_minibuffer_active_p);
    ADD_SUBR(subr_set_status_message);
    INTERN(make_view_hook);
    INTERN(destroy_view_hook);
}

void
views_kill(void)
{
    VW *vw = view_chain;
    while(vw != 0)
    {
	VW *next = vw->vw_Next;
	FREE_OBJECT(vw);
	vw = next;
    }
    view_chain = NULL;
}

void
view_sweep(void)
{
    VW *vw = view_chain;
    view_chain = NULL;
    while(vw)
    {
	VW *next = vw->vw_Next;
	if(GC_CELL_MARKEDP(VAL(vw)))
	{
	    GC_CLR_CELL(VAL(vw));
	    vw->vw_Next = view_chain;
	    view_chain = vw;
	}
	else
	    FREE_OBJECT(vw);
	vw = next;
    }
}

void
view_prin(VALUE stream, VALUE vw)
{
    char buf[32];
    if(VVIEW(vw)->vw_Win == 0)
	stream_puts(stream, "#<dead-view>", -1, FALSE);
    else
    {
	sprintf(buf, "#<view %d,%d", VVIEW(vw)->vw_MaxX, VVIEW(vw)->vw_MaxY);
	stream_puts(stream, buf, -1, FALSE);
	if(VVIEW(vw)->vw_Tx)
	{
	    stream_putc(stream, ' ');
	    stream_puts(stream, VPTR(VVIEW(vw)->vw_Tx->tx_BufferName),
			-1, TRUE);
	}
	stream_putc(stream, '>');
    }
}
