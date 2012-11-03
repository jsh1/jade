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
#include <stdlib.h>
#include <string.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

static void kill_view(VW *vw);

DEFSYM(split_view_hook, "split-view-hook");
DEFSYM(delete_view_hook, "delete-view-hook"); /*
::doc:split-view-hook::
Hook called whenever a new view is created; called with a single argument,
the view that's just been made.
::end::
::doc:delete-view-hook::
Hook called whenever a view is deleted, called with the view as its sole
argument.
::end:: */

int view_type;

DEFSYM(mode_line_format, "mode-line-format");

static void set_scroll_steps(VW *vw);
static void recalc_measures(WIN *w);

/* view_chain is a list of all allocated VW structures, linked through
   their vw_Next fields. curr_vw is the currently active view; a mirror
   of curr_win->vw_CurrVW. */
VW *view_chain, *curr_vw;

/* This buffer is put into minibuffer views when they're not being
   used. */
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
	    if(parent && parent->w_CurrVW && parent->w_CurrVW->vw_Tx)
		tx = parent->w_CurrVW->vw_Tx;
	    else
		tx = buffer_chain; /* whatever */
	}
	if(parent && parent->w_CurrVW
	   && (parent->w_CurrVW->vw_Flags & VWFF_MINIBUF) == 0)
	    sibling = parent->w_CurrVW;
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
	if((sibling && sibling->vw_MaxY + 1 < lines + 3)
	   || parent->w_MaxY < lines + 2)
	{
	    Fsignal(Qwindow_error,
		       rep_list_2(rep_VAL(parent), rep_VAL(&too_few_lines)));
	    return NULL;
	}
    }

    /* Now the construction of the view proper... */
    vw = rep_ALLOC_CELL(sizeof(VW));
    if(vw != NULL)
    {
	memset(vw, 0, sizeof(VW));
	vw->vw_Car = view_type;
	vw->vw_Next = view_chain;
	view_chain = vw;
	vw->vw_Win = parent;
	parent->w_ViewCount++;
	copy_view_prefs(vw, sibling ? sibling : curr_vw);
	vw->vw_BlockStatus = -1;
	vw->vw_BufferList = Qnil;

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
		vw->vw_BufferList = Fcopy_sequence(sibling->vw_BufferList);
	    }
	    else
	    {
		/* this doesn't always work as well as the above. */
		swap_buffers(vw, tx);
		vw->vw_BufferList = Qnil;
	    }
	    Fcall_hook(Qsplit_view_hook, rep_LIST_1(rep_VAL(vw)), Qnil);
	}
	if(curr_vw == 0)
	    curr_vw = vw;
	if(parent->w_CurrVW == 0)
	    parent->w_CurrVW = vw;

	return vw;
    }
    return NULL;
}

DEFUN_INT("split-view", Fsplit_view, Ssplit_view,
	  (repv sib, repv lines), rep_Subr2, "") /*
::doc:split-view::
split-view [VIEW] [SIZE]

Split VIEW (or the selected view) into two. Returns a new view directly
below VIEW containing SIZE rows of the window (or half the original size
of VIEW if undefined), displaying the same buffer as VIEW is.
The currently selected view doesn't change.

A window-error will be signalled if there is insufficient room in VIEW
to contain the new view.
::end:: */
{
    if(!VIEWP(sib))
	sib = rep_VAL(curr_vw);
    return rep_VAL(make_view(VVIEW(sib), VVIEW(sib)->vw_Win,
			 VVIEW(sib)->vw_Tx, rep_INTP(lines) ? rep_INT(lines) : 0,
			 FALSE));
}

/* Destroy one view. It should have been removed from the w_ViewList */
static void
kill_view(VW *vw)
{
    WIN *w = vw->vw_Win;
    vw->vw_NextView = NULL;
    vw->vw_Tx = NULL;
    vw->vw_Win = NULL;
    vw->vw_BufferList = Qnil;
    w->w_ViewCount--;
    if(w->w_CurrVW == vw)
    {
	w->w_CurrVW = w->w_ViewList;
	if(curr_win == w)
	    curr_vw = w->w_CurrVW;
    }
}

DEFSTRING(sole_view, "Can't kill the sole view in a window");
DEFSTRING(mini_view, "Can't kill minibuffer view");

DEFUN_INT("delete-view", Fdelete_view, Sdelete_view, (repv view),
	  rep_Subr1, "") /*
::doc:delete-view::
delete-view [VIEW]

Delete VIEW (or the current view) from the window containing it. Its window
space will be assigned to the view immediately above it (unless VIEW is the
first in the window, in which case the following view is given the space).

A window-error will be signalled when deleting the view would leave less
than two views in the window (including the minibuffer view), or when
VIEW is the minibuffer view.
::end:: */
{
    VW *vw = VIEWP(view) ? VVIEW(view) : curr_vw;
    VW *pred;
    if(vw->vw_Win->w_ViewCount <= 2)
    {
	/* Only two views are left. Don't destroy it. */
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&sole_view), rep_VAL(vw)));
    }
    else if(vw->vw_Flags & VWFF_MINIBUF)
    {
	/* Can't kill the minibuffer */
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&mini_view), rep_VAL(vw)));
    }
    Fcall_hook(Qdelete_view_hook, rep_LIST_1(rep_VAL(vw)), Qnil);

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
	    new_origin_row = pred->vw_Tx->logical_start;
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
    kill_view(vw);
    /* VW is now unlinked; now gives its window-space to PRED. */
    pred->vw_MaxY += vw->vw_MaxY + 1;
    recalc_measures(pred->vw_Win);
    set_scroll_steps(pred);
    return(rep_VAL(vw));
}

/* Destroy all views of window W. */
void
kill_all_views(WIN *w)
{
    VW *vw = w->w_ViewList;
    while(vw != 0)
    {
	VW *next = vw->vw_NextView;
	kill_view(vw);
	vw = next;
    }
    w->w_CurrVW = 0;
    w->w_ViewList = 0;
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
	/* No changes */
	return;

    while(w->w_MaxY < (w->w_ViewCount * 2) - 1)
    {
	/* Not enough lines for the number of existing views. Delete
	   views until there is */
	VW *dead = w->w_ViewList;
	w->w_ViewList = w->w_ViewList->vw_NextView;
	kill_view(dead);
    }

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
	    {
		/* Otherwise try to keep the old weighting. */
		vw->vw_MaxY = (((vw->vw_MaxY + 1) * w->w_MaxY)
			       / old_total_lines) - 1;
		vw->vw_MaxY = MAX(vw->vw_MaxY, 1);
	    }
	}
	vw->vw_FirstY = lines_given;
	lines_given += vw->vw_MaxY + 1;
	set_scroll_steps(vw);
    }
}

/* Expand format characters */
static long
format_mode_string(char *fmt, VW *vw, char *buf, long buf_len)
{
    TX *tx = vw->vw_Tx;
    while(*fmt && buf_len > 0)
    {
	while(buf_len > 0 && *fmt && *fmt != '%')
	{
	    *buf++ = *fmt++;
	    buf_len--;
	}
	if(buf_len <= 0 || !*fmt)
	    break;
	fmt++;
	switch(*fmt++)
	{
	    int len;

	case 'b':			/* buffer-name */
	case 'B':			/* buffer-status-id */
	case 'f':			/* file-name */
	{
	    repv str = (fmt[-1] == 'b'
			 ? tx->buffer_name
			 : (fmt[-1] == 'B'
			    ? tx->status_string : tx->file_name));
	    if(rep_STRINGP(str))
	    {
		len = rep_STRING_LEN(str);
		len = MIN(len, buf_len);
		memcpy(buf, rep_STR(str), len);
		buf += len; buf_len -= len;
	    }
	}
	break;

	case 'l':			/* line-number */
	case 'L':			/* global line-number */
	    len = sprintf(buf, "%ld", VROW(vw->vw_CursorPos) + 1
			  - (fmt[-1] == 'l' ? tx->logical_start : 0));
	    buf += len; buf_len -= len;
	    break;

	case 'c':			/* column-number */
	    len = sprintf(buf, "%ld", get_cursor_column(vw) + 1);
	    buf += len; buf_len -= len;
	    break;

	case 'p':			/* `Top', `Bot', or `XX%' */
	{
	    char *position, position_buf[4];
	    
	    if(VROW(vw->vw_DisplayOrigin) <= tx->logical_start)
	    {
		if(vw->vw_Flags & VWFF_AT_BOTTOM)
		    position = "All";
		else
		    position = "Top";
	    }
	    else if(vw->vw_Flags & VWFF_AT_BOTTOM)
		position = "Bot";
	    else
	    {
		int percent = (((VROW(vw->vw_DisplayOrigin)
				 - tx->logical_start) * 100)
			       / (tx->logical_end - tx->logical_start));
		position_buf[0] = (percent / 10) + '0';
		position_buf[1] = (percent % 10) + '0';
		position_buf[2] = '%';
		position_buf[3] = 0;
		position = position_buf;
	    }
	    
	    strcpy(buf, position);
	    buf += 3; buf_len -= 3;
	    break;
	}

	case 'm':
	    *buf++ = "-+<>"[vw->vw_BlockStatus+1];
	    buf_len--;
	    break;

	case '(':			/* `(' if in top-level, else `[' */
	    *buf++ = (rep_recurse_depth == 0) ? '(' : '[';
	    buf_len--;
	    break;

	case ')':			/* similar to '(' */
	    *buf++ = (rep_recurse_depth == 0) ? ')' : ']';
	    buf_len--;
	    break;

	case '[':			/* '[' if narrowed, else '(' */
	case ']':			/* similar to '[' */
	{
	    bool restr = ((tx->logical_start != 0)
			  || (tx->logical_end != tx->line_count));
	    *buf++ = (fmt[-1] == '['
		      ? (restr ? '[' : '(') : (restr ? ']' : ')'));
	    buf_len--;
	    break;
	}

	case '*':			/* %, * or hyphen */
	case '+':			/* *, % or hyphen */
	{
	    repv tem = Fbuffer_symbol_value(Qread_only,
						vw->vw_CursorPos,
						rep_VAL(tx), Qt);
	    bool mod = tx->change_count != tx->proper_saved_changed_count;
	    if(rep_VOIDP(tem))
		tem = Qnil;
	    if(mod && !rep_NILP(tem))
		*buf = (fmt[-1] == '*') ? '%' : '*';
	    else if(mod)
		*buf = '*';
	    else if(!rep_NILP(tem))
		*buf = '%';
	    else
		*buf = '-';
	    buf++; buf_len--;
	    break;
	}

	case '-':			/* infinite dashes */
	    memset(buf, '-', buf_len);
	    buf += buf_len; buf_len = 0;
	    break;

	case '%':			/* percent character */
	    *buf++ = '%'; buf_len--;
	    break;
	}
    }

    return buf_len;
}

static long
format_mode_value(repv format, VW *vw, char *buf, long buf_len)
{
    TX *tx = vw->vw_Tx;

    if(rep_SYMBOLP(format))
    {
	repv tem = Fbuffer_symbol_value(format, vw->vw_CursorPos,
					    rep_VAL(tx), Qt);
	if(rep_VOIDP(tem))
	    tem = Fdefault_value(format, Qt);
	if(rep_STRINGP(tem))
	{
	    int len = rep_STRING_LEN(tem);
	    len = MIN(len, buf_len);
	    memcpy(buf, rep_STR(tem), len);
	    buf += len; buf_len -= len;
	    return buf_len;
	}
	else
	    format = tem;
    }

    if(rep_STRINGP(format))
	return format_mode_string(rep_STR(format), vw, buf, buf_len);

    while(buf_len > 0 && rep_CONSP(format))
    {
	repv item = rep_CAR(format);
	format = rep_CDR(format);

	if(rep_STRINGP(item))
	{
	    u_long done = buf_len - format_mode_string(rep_STR(item), vw,
						       buf, buf_len);
	    buf += done; buf_len -= done;
	}
	else if(rep_SYMBOLP(item))
	{
	    repv tem = Fbuffer_symbol_value(item, vw->vw_CursorPos,
						rep_VAL(tx), Qt);
	    if(rep_VOIDP(tem))
		tem = Fdefault_value(item, Qt);
	    if(rep_STRINGP(tem))
	    {
		int len = rep_STRING_LEN(tem);
		len = MIN(len, buf_len);
		memcpy(buf, rep_STR(tem), len);
		buf += len; buf_len -= len;
	    }
	    else
	    {
		u_long done = buf_len - format_mode_value(tem, vw,
							  buf, buf_len);
		buf += done; buf_len -= done;
	    }
	}
	else if(rep_CONSP(item))
	{
	    u_long done = 0;
	    repv first = rep_CAR(item);
	    if(rep_STRINGP(first))
		done = buf_len - format_mode_string(rep_STR(first), vw,
						    buf, buf_len);
	    else if(rep_SYMBOLP(first))
	    {
		repv tem = Fbuffer_symbol_value(first, vw->vw_CursorPos,
						    rep_VAL(tx), Qt);
		if(rep_VOIDP(tem))
		    tem = Fdefault_value(first, Qt);
		if(!rep_VOIDP(tem) && !rep_NILP(tem))
		{
		    if(rep_CONSP(rep_CDR(item)))
			tem = rep_CAR(rep_CDR(item));
		    else
			tem = Qnil;
		}
		else if(rep_CONSP(rep_CDR(item)) && rep_CONSP(rep_CDR(rep_CDR(item))))
		    tem = rep_CAR(rep_CDR(rep_CDR(item)));
		else
		    tem = Qnil;
		if(tem != rep_NULL && !rep_NILP(tem))
		    done = buf_len - format_mode_value(tem, vw, buf, buf_len);
	    }
	    buf += done; buf_len -= done;
	}
    }
    return buf_len;
}

/* Reformat the status string of VW. */
void
update_status_buffer(VW *vw, char *buf, long buf_len)
{
    if(!(vw->vw_Flags & VWFF_MINIBUF))
    {
	u_long done;
	TX *tx = vw->vw_Tx;
	repv format = Fbuffer_symbol_value(Qmode_line_format,
					       vw->vw_CursorPos,
					       rep_VAL(tx), Qt);
	if(rep_VOIDP(format))
	{
	    format = Fdefault_value(Qmode_line_format, Qt);
	    if(rep_VOIDP(format))
		return;
	}
	
	done = buf_len - format_mode_value(format, vw, buf, buf_len);
	buf += done; buf_len -= done;
	if(buf_len > 0)
	    memset(buf, ' ', buf_len);
    }
}

DEFUN("y-scroll-step-ratio", Fy_scroll_step_ratio, Sy_scroll_step_ratio, (repv val), rep_Subr1) /*
::doc:y-scroll-step-ratio::
y-scroll-step-ratio [VALUE]

Controls the actual number of lines scrolled when the cursor moves out of
view. The number of lines to move the display origin is calcualted with the
formula:
  LINES_TO_SCROLL = TOTAL_LINES_IN_VIEW / y-scroll-step-ratio
If the value is 0 then the window will be scrolled by one line.
::end:: */
{
    return rep_handle_var_int (val, &curr_vw->vw_YStepRatio);
}

DEFUN("x-scroll-step-ratio", Fx_scroll_step_ratio, Sx_scroll_step_ratio, (repv val), rep_Subr1) /*
::doc:x-scroll-step-ratio::
x-scroll-step-ratio [VALUE]
Controls the actual number of columns scrolled when the cursor moves out of
view. The number of lines to move the display origin is calcualted with the
formula:
  COLUMNS_TO_SCROLL = TOTAL_COLUMNS_IN_VIEW / x-scroll-step-ratio
If the value is 0 then the window will be scrolled by one column.
::end:: */
{
    return rep_handle_var_int (val, &curr_vw->vw_XStepRatio);
}

DEFUN("rect-blocks-p", Frect_blocks_p, Srect_blocks_p, (repv vw), rep_Subr1) /*
::doc:rect-blocks-p::
rect-blocks-p [VIEW]

Returns t if blocks marked in VIEW (or the current one) are treated as
rectangles.
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    if(VVIEW(vw)->vw_Flags & VWFF_RECTBLOCKS)
	return(Qt);
    return(Qnil);
}

DEFUN("set-rect-blocks", Fset_rect_blocks, Sset_rect_blocks, (repv vw, repv stat), rep_Subr2) /*
::doc:set-rect-blocks::
set-rect-blocks VIEW STATUS

Controls whether or not blocks are taken as contiguous regions of text or as
rectangles in VIEW. When STATUS is t rectangles are used.
::end:: */
{
    int oflags;
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    oflags = VVIEW(vw)->vw_Flags;
    if(rep_NILP(stat))
	VVIEW(vw)->vw_Flags &= ~VWFF_RECTBLOCKS;
    else
	VVIEW(vw)->vw_Flags |= VWFF_RECTBLOCKS;
    return(stat);
}

DEFUN("current-view", Fcurrent_view, Scurrent_view, (repv win), rep_Subr1) /*
::doc:current-view::
current-view [WINDOW]

Returns the currently active view in WINDOW.
::end:: */
{
    return(WINDOWP(win) ? rep_VAL(VWIN(win)->w_CurrVW) : rep_VAL(curr_vw));
}

DEFUN("set-current-view", Fset_current_view, Sset_current_view, (repv vw, repv activ), rep_Subr2) /*
::doc:set-current-view::
set-current-view VIEW [ACTIVATE-P]

Sets the VIEW which jade reguards as current in the window containing it.
If ACTIVATE-P is t then the window containing VIEW will be made current
and activated.
::end:: */
{
    rep_DECLARE1(vw, VIEWP);
    VVIEW(vw)->vw_Win->w_CurrVW = VVIEW(vw);
    if(VVIEW(vw)->vw_Win == curr_win)
    {
	curr_vw = VVIEW(vw);
	curr_vw->vw_Win->w_CurrVW = curr_vw;
    }
    if(!rep_NILP(activ))
	Fset_current_window(rep_VAL(VVIEW(vw)->vw_Win), activ);
    return(vw);
}

DEFUN("buffer-list", Fbuffer_list, Sbuffer_list, (void), rep_Subr0) /*
::doc:buffer-list::
List of buffers in most-recently-used order. Each view has it's own.
::end:: */
{
    return(curr_vw->vw_BufferList);
}

DEFUN("set-buffer-list", Fset_buffer_list, Sset_buffer_list, (repv val), rep_Subr1) /*
::doc:set-buffer-list::
List of buffers in most-recently-used order. Each view has it's own.
::end:: */
{
    curr_vw->vw_BufferList = val;
    return val;
}

DEFUN("get-buffer-view", Fget_buffer_view, Sget_buffer_view,
      (repv buffer, repv all_windows), rep_Subr2) /*
::doc:get-buffer-view::
get-buffer-view BUFFER [ALL-WINDOWS]

Return a view that is currently displaying BUFFER. 

Searching progresses from the current view, through all views in the
current window, then, if ALL-WINDOWS is non-nil, through all views of
the next window and so on. If no view displaying BUFFER is found, nil
is returned.
::end:: */
{
    WIN *w = curr_win;
    rep_DECLARE1(buffer, BUFFERP);
    do {
	VW *vw = w->w_CurrVW;
	do {
	    if(vw->vw_Tx == VTX(buffer))
		return rep_VAL(vw);
	    vw = vw->vw_NextView;
	    if(vw == 0)
		vw = w->w_ViewList;
	} while(vw != w->w_CurrVW);
	w = w->w_Next;
	if(w == 0)
	    w = win_chain;
    } while(!rep_NILP(all_windows) && w != curr_win);
    return Qnil;
}

DEFUN("next-view", Fnext_view, Snext_view, (repv win, repv allp), rep_Subr2) /*
::doc:next-view::
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
	win = rep_VAL(curr->vw_Win);
    }
    else if(WINDOWP(win))
	curr = VWIN(win)->w_CurrVW;
    else
    {
	curr = curr_vw;
	win = rep_VAL(curr_win);
    }
    /* If possible just return the next view in the original window */
    if(curr->vw_NextView != 0)
	return rep_VAL(curr->vw_NextView);
    else
    {
	if(rep_NILP(allp))
	    /* First view of the original window. */
	    return rep_VAL(VWIN(win)->w_ViewList);
	else
	{
	    if(VWIN(win)->w_Next != 0)
		return rep_VAL(VWIN(win)->w_Next->w_ViewList);
	    else
		return rep_VAL(win_chain->w_ViewList);
	}
    }
}

DEFUN("previous-view", Fprevious_view, Sprevious_view, (repv win, repv allp), rep_Subr2) /*
::doc:previous-view::
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
	win = rep_VAL(curr->vw_Win);
    }
    else if(WINDOWP(win))
	curr = VWIN(win)->w_CurrVW;
    else
    {
	curr = curr_vw;
	win = rep_VAL(curr_win);
    }
    if(curr == VWIN(win)->w_ViewList)
    {
	/* current view is first in this window. If ALLP is t
	   need to find the previous window and the last view in it.
	   otherwise the last view in the current window. */
	WIN *w;
	if(!rep_NILP(allp))
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
	while(vw->vw_NextView != 0)
	    vw = vw->vw_NextView;
    }
    else
    {
	/* find the predecessor of the current view */
	vw = VWIN(win)->w_ViewList;
	while(vw->vw_NextView != curr)
	    vw = vw->vw_NextView;
    }
    return rep_VAL(vw);
}

DEFUN("view-origin", Fview_origin, Sview_origin, (repv vw), rep_Subr1) /*
::doc:view-origin::
view-origin [VIEW]

Return the glyph position of the character displayed in the top-left corner
of either VIEW or the current view.
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    /* Make sure that we get the position that would be at the top-left
       after the _next_ redisplay.. */
    recenter_cursor(VVIEW(vw));
    return VVIEW(vw)->vw_DisplayOrigin;
}

DEFUN("view-dimensions", Fview_dimensions, Sview_dimensions, (repv vw), rep_Subr1) /*
::doc:view-dimensions::
view-dimensions [VIEW]

Returns (COLUMNS . ROWS) defining the size (in glyphs) of VIEW (by default
the current view).
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return Fcons(rep_MAKE_INT(VVIEW(vw)->vw_MaxX),
		    rep_MAKE_INT(VVIEW(vw)->vw_MaxY));
}

DEFUN("view-position", Fview_position, Sview_position, (repv vw), rep_Subr1) /*
::doc:view-position::
view-position [VIEW]

Returns the screen position of VIEW in relation to the top-left hand corner of
its containing window.
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return make_pos(VVIEW(vw)->vw_FirstX, VVIEW(vw)->vw_FirstY);
}

DEFSTRING(no_view, "No view to expand into");
DEFSTRING(no_room, "Not enough room");

DEFUN("set-view-dimensions", Fset_view_dimensions, Sset_view_dimensions, (repv vw, repv cols, repv rows), rep_Subr3) /*
::doc:set-view-dimensions::
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
	vw = rep_VAL(curr_vw);
    if(!rep_INTP(rows))
	return vw;
    sibling = VVIEW(vw)->vw_NextView;
    if(sibling == 0)
    {
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&no_view), vw));
    }
    new_sibling_height = sibling->vw_MaxY - (rep_INT(rows) - VVIEW(vw)->vw_MaxY);
    if(new_sibling_height < 1 || rep_INT(rows) < 1)
    {
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&no_room), vw));
    }
    VVIEW(vw)->vw_MaxY = rep_INT(rows);
    sibling->vw_MaxY = new_sibling_height;
    recalc_measures(VVIEW(vw)->vw_Win);
    set_scroll_steps(VVIEW(vw));
    set_scroll_steps(sibling);
    return vw;
}

DEFUN("find-view-by-pos", Ffind_view_by_pos, Sfind_view_by_pos, (repv pos, repv win), rep_Subr2) /*
::doc:find-view-by-pos::
find-view-by-pos POS [WINDOW]

Attempt to find the view in the current window (or in WINDOW), that includes
the glyph at position POS in the window. Returns nil if no such view exists.
::end:: */
{
    WIN *w = WINDOWP(win) ? VWIN(win) : curr_win;
    VW *vw = w->w_ViewList;
    rep_DECLARE1(pos, POSP);
    if(VROW(pos) < 0)
	return Qnil;
    while(vw != NULL)
    {
	/* vw_MaxY doesn't include the status line */
	long bottom = (vw->vw_FirstY + vw->vw_MaxY
		       + ((vw->vw_Flags & VWFF_MINIBUF) ? 0 : 1));
	if(VROW(pos) < bottom)
	    return rep_VAL(vw);
	vw = vw->vw_NextView;
    }
    return Qnil;
}

DEFUN("translate-pos-to-view", Ftranslate_pos_to_view, Stranslate_pos_to_view, (repv pos, repv vw), rep_Subr2) /*
::doc:translate-pos-to-view::
translate-pos-to-view POS [VIEW]

Return the screen position in the current view (or in VIEW) that corresponds
to the screen position POSITION in the view's window.

If no position in VIEW corresponds to POS, return nil. If POS is in the
status line of VIEW, return t.
::end:: */
{
    long col, row;
    rep_DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    col = VCOL(pos) - VVIEW(vw)->vw_FirstX;
    row = VROW(pos) - VVIEW(vw)->vw_FirstY;
    if(col < 0 || col >= VVIEW(vw)->vw_MaxX
       || row < 0 || row > VVIEW(vw)->vw_MaxY)
	return Qnil;
    else if(row == VVIEW(vw)->vw_MaxY)
	return Qt;
    else
	return make_pos(col, row);
}

DEFUN("minibuffer-view-p", Fminibuffer_view_p, Sminibuffer_view_p, (repv vw), rep_Subr1) /*
::doc:minibuffer-view-p::
minibuffer-view-p [VIEW]

Returns t if VIEW is a view of a minibuffer.
::end:: */
{
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    return (VVIEW(vw)->vw_Flags & VWFF_MINIBUF) ? Qt : Qnil;
}

DEFUN("minibuffer-view", Fminibuffer_view, Sminibuffer_view, (repv win), rep_Subr1) /*
::doc:minibuffer-view::
minibuffer-view [WINDOW]

Returns the view of the minibuffer in WINDOW (or the current window).
::end:: */
{
    return rep_VAL(WINDOWP(win) ? VWIN(win)->w_MiniBuf : curr_win->w_MiniBuf);
}

DEFUN("minibuffer-active-p", Fminibuffer_active_p, Sminibuffer_active_p, (repv win), rep_Subr1) /*
::doc:minibuffer-active-p::
minibuffer-active-p [WINDOW]

Returns t if the minibuffer of WINDOW is being used.
::end:: */
{
    return MINIBUFFER_ACTIVE_P(WINDOWP(win) ? VWIN(win) : curr_win)
	? Qt : Qnil;
}

DEFUN("viewp", Fviewp, Sviewp, (repv arg), rep_Subr1) /*
::doc:viewp::
viewp ARG

Returns t if ARG is a view object.
::end:: */
{
    return VIEWP(arg) ? Qt : Qnil;
}

static void
view_sweep(void)
{
    VW *vw = view_chain;
    view_chain = NULL;
    while(vw)
    {
	VW *next = vw->vw_Next;
	if(rep_GC_CELL_MARKEDP(rep_VAL(vw)))
	{
	    rep_GC_CLR_CELL(rep_VAL(vw));
	    vw->vw_Next = view_chain;
	    view_chain = vw;
	}
	else
	    rep_FREE_CELL(vw);
	vw = next;
    }
}

static void
view_mark (repv val)
{
    while (val != rep_NULL)
    {
	rep_MARKVAL(rep_VAL(VVIEW(val)->vw_Tx));
	rep_MARKVAL(VVIEW(val)->vw_BufferList);
	rep_MARKVAL(VVIEW(val)->vw_CursorPos);
	rep_MARKVAL(VVIEW(val)->vw_LastCursorPos);
	rep_MARKVAL(VVIEW(val)->vw_DisplayOrigin);
	rep_MARKVAL(VVIEW(val)->vw_BlockS);
	rep_MARKVAL(VVIEW(val)->vw_BlockE);
	val = rep_VAL(VVIEW(val)->vw_NextView);
	if (val != rep_NULL)
	    rep_GC_SET_CELL (val);
    }
}    

static void
view_prin(repv stream, repv vw)
{
    char buf[32];
    if(VVIEW(vw)->vw_Win == 0)
	rep_stream_puts(stream, "#<dead-view>", -1, FALSE);
    else
    {
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf),
		 "#<view %d,%d", VVIEW(vw)->vw_MaxX, VVIEW(vw)->vw_MaxY);
#else
	sprintf(buf, "#<view %d,%d", VVIEW(vw)->vw_MaxX, VVIEW(vw)->vw_MaxY);
#endif
	rep_stream_puts(stream, buf, -1, FALSE);
	if(VVIEW(vw)->vw_Tx)
	{
	    rep_stream_putc(stream, ' ');
	    rep_stream_puts(stream, rep_PTR(VVIEW(vw)->vw_Tx->buffer_name),
			-1, TRUE);
	}
	rep_stream_putc(stream, '>');
    }
}

static repv
view_bind (repv vw)
{
    if (!VIEWP(vw))
	return Qnil;
    else
    {
	repv handle = Fcons (rep_VAL(VVIEW(vw)->vw_Win->w_CurrVW),
			     rep_VAL(curr_win));
	curr_vw = VVIEW(vw);
	curr_win = VVIEW(vw)->vw_Win;
	curr_win->w_CurrVW = curr_vw;
	return handle;
    }
}

static void
view_unbind (repv handle)
{
    VW *vw = VVIEW(rep_CAR(handle));
    WIN *win = VWIN(rep_CDR(handle));
    if (vw->vw_Win && vw->vw_Win->w_Window != WINDOW_NIL)
    {
	vw->vw_Win->w_CurrVW = vw;
	curr_win = win;
	curr_vw = curr_win->w_CurrVW;
    }
}

void
views_init(void)
{
    view_type = rep_register_new_type ("view", 0, view_prin, view_prin,
				       view_sweep, view_mark, 0,
				       0, 0, 0, 0,
				       view_bind, view_unbind);

    mb_unused_buffer = VTX(Fmake_buffer(rep_VAL(&unused_mb), Qnil, Qt));

    rep_ADD_SUBR_INT(Ssplit_view);
    rep_ADD_SUBR_INT(Sdelete_view);
    rep_ADD_SUBR(Sy_scroll_step_ratio);
    rep_ADD_SUBR(Sx_scroll_step_ratio);
    rep_ADD_SUBR(Srect_blocks_p);
    rep_ADD_SUBR(Sset_rect_blocks);
    rep_ADD_SUBR(Scurrent_view);
    rep_ADD_SUBR(Sset_current_view);
    rep_ADD_SUBR(Sbuffer_list);
    rep_ADD_SUBR(Sset_buffer_list);
    rep_ADD_SUBR(Sget_buffer_view);
    rep_ADD_SUBR(Snext_view);
    rep_ADD_SUBR(Sprevious_view);
    rep_ADD_SUBR(Sview_origin);
    rep_ADD_SUBR(Sview_dimensions);
    rep_ADD_SUBR(Sview_position);
    rep_ADD_SUBR(Sset_view_dimensions);
    rep_ADD_SUBR(Sfind_view_by_pos);
    rep_ADD_SUBR(Stranslate_pos_to_view);
    rep_ADD_SUBR(Sminibuffer_view_p);
    rep_ADD_SUBR(Sminibuffer_view);
    rep_ADD_SUBR(Sminibuffer_active_p);
    rep_ADD_SUBR(Sviewp);
    rep_INTERN_SPECIAL(split_view_hook);
    rep_INTERN_SPECIAL(delete_view_hook);
    rep_INTERN_SPECIAL(mode_line_format);
}

void
views_kill(void)
{
    VW *vw = view_chain;
    while(vw != 0)
    {
	VW *next = vw->vw_Next;
	rep_FREE_CELL(vw);
	vw = next;
    }
    view_chain = NULL;
}
