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

static void kill_view(Lisp_View *vw);

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

static void set_scroll_steps(Lisp_View *vw);
static void recalc_measures(Lisp_Window *w);

/* view_chain is a list of all allocated VW structures, linked through
   their next fields. curr_vw is the currently active view; a mirror
   of curr_win->vw_CurrVW. */
Lisp_View *view_chain, *curr_vw;

/* This buffer is put into minibuffer views when they're not being
   used. */
Lisp_Buffer *mb_unused_buffer;
DEFSTRING(unused_mb, "*unused-minibuf*");

/* Copy some preferences from SRC to DEST; SRC may be null */
static void
copy_view_prefs(Lisp_View *dest, Lisp_View *src)
{
    if(src)
    {
	dest->scroll_ratio_x = src->scroll_ratio_x;
	dest->scroll_ratio_y = src->scroll_ratio_y;
    }
    else
    {
	dest->scroll_ratio_x = 4;
#ifdef HAVE_AMIGA
	dest->scroll_ratio_y = 0;
#else
	dest->scroll_ratio_y = 4;
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
Lisp_View *
make_view(Lisp_View *sibling, Lisp_Window *parent, Lisp_Buffer *tx,
	  intptr_t lines, bool minibuf_p)
{
    Lisp_View *vw;

    /* Try to initialise PARENT, SIBLING and TX. PARENT must be
       non-null, the others don't have to be if it's not possible. */
    if(sibling && sibling->car & VWFF_MINIBUF)
    {
	/* Try to find the view above the minibuffer view */
	sibling = sibling->window->view_list;
	while(sibling && sibling->next_view
	      && sibling->next_view->next_view)
	    sibling = sibling->next_view;
    }
    if(sibling)
    {
	if(!tx)
	    tx = sibling->tx;
	parent = sibling->window;
    }
    else
    {
	if(!parent)
	    parent = curr_win;
	if(!tx)
	{
	    if(parent && parent->current_view && parent->current_view->tx)
		tx = parent->current_view->tx;
	    else
		tx = buffer_chain; /* whatever */
	}
	if(parent && parent->current_view
	   && (parent->current_view->car & VWFF_MINIBUF) == 0)
	    sibling = parent->current_view;
    }

    /* Make sure that LINES is initialised to a value greater than
       zero. */
    if(lines == 0 && !minibuf_p)
    {
	if(sibling)
	{
	    lines = ((sibling->height + 1) / 2) - 1;
	    if(lines < 1)
		lines = 1;
	}
	else
	    lines = parent->row_count - 2;
    }

    /* Check to see if there's space for the new view. */
    if(!minibuf_p)
    {
	if((sibling && sibling->height + 1 < lines + 3)
	   || parent->row_count < lines + 2)
	{
	    Fsignal(Qwindow_error,
		       rep_list_2(rep_VAL(parent), rep_VAL(&too_few_lines)));
	    return NULL;
	}
    }

    /* Now the construction of the view proper... */
    vw = rep_alloc(sizeof(Lisp_View));
    if(vw != NULL)
    {
	memset(vw, 0, sizeof(Lisp_View));
	vw->car = view_type;
	vw->next = view_chain;
	view_chain = vw;
	vw->window = parent;
	parent->view_count++;
	copy_view_prefs(vw, sibling ? sibling : curr_vw);
	vw->block_state = -1;
	vw->buffer_list = Qnil;

	/* Initialise the size of the new view, and resize its
	   SIBLING if it has one. */
	if(minibuf_p)
	{
	    Lisp_View *x;
	    /* This view is destined to be a minibuffer */
	    vw->width = parent->column_count;
	    vw->height = 1;
	    swap_buffers(vw, mb_unused_buffer);
	    vw->car |= VWFF_MINIBUF;
	    parent->mini_buffer_view = vw;
	    for(x = parent->view_list; x->next_view; x = x->next_view)
		;
	    x->next_view = vw;
	    vw->next_view = NULL;
	}
	else if(sibling)
	{
	    /* Divide up the space used by SIBLING.  We know
	       that there's enough room for LINES new lines. */
	    vw->next_view = sibling->next_view;
	    sibling->next_view = vw;
	    vw->width = sibling->width;
	    vw->height = lines;
	    sibling->height = ((sibling->height + 1)
				- (vw->height + 1)) - 1;
	    set_scroll_steps(sibling);
	}
	else
	{
	    /* All space in PARENT goes to the new view. */
	    vw->next_view = NULL;
	    parent->view_list = vw;
	    vw->width = parent->column_count;
	    vw->height = parent->row_count - 2;	/* status & minibuf */
	}
	recalc_measures(parent);
	set_scroll_steps(vw);

	/* Now try to initialise the new view's buffer. */
	if(!minibuf_p && tx)
	{
	    if(sibling && sibling->tx == tx)
	    {
		/* copy configuration of SIBLING */
		vw->tx = tx;
		vw->cursor_pos = sibling->cursor_pos;
		vw->display_origin = sibling->display_origin;
		vw->block_start = sibling->block_start;
		vw->block_end = sibling->block_end;
		vw->block_state = sibling->block_state;
		vw->buffer_list = Fcopy_sequence(sibling->buffer_list);
	    }
	    else
	    {
		/* this doesn't always work as well as the above. */
		swap_buffers(vw, tx);
		vw->buffer_list = Qnil;
	    }
	    Fcall_hook(Qsplit_view_hook, rep_LIST_1(rep_VAL(vw)), Qnil);
	}
	if(curr_vw == 0)
	    curr_vw = vw;
	if(parent->current_view == 0)
	    parent->current_view = vw;

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
    return rep_VAL(make_view(VVIEW(sib), VVIEW(sib)->window,
			 VVIEW(sib)->tx, rep_INTP(lines) ? rep_INT(lines) : 0,
			 false));
}

/* Destroy one view. It should have been removed from the view_list */
static void
kill_view(Lisp_View *vw)
{
    Lisp_Window *w = vw->window;
    vw->next_view = NULL;
    vw->tx = NULL;
    vw->window = NULL;
    vw->buffer_list = Qnil;
    w->view_count--;
    if(w->current_view == vw)
    {
	w->current_view = w->view_list;
	if(curr_win == w)
	    curr_vw = w->current_view;
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
    Lisp_View *vw = VIEWP(view) ? VVIEW(view) : curr_vw;
    Lisp_View *pred;
    if(vw->window->view_count <= 2)
    {
	/* Only two views are left. Don't destroy it. */
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&sole_view), rep_VAL(vw)));
    }
    else if(vw->car & VWFF_MINIBUF)
    {
	/* Can't kill the minibuffer */
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&mini_view), rep_VAL(vw)));
    }
    Fcall_hook(Qdelete_view_hook, rep_LIST_1(rep_VAL(vw)), Qnil);

    if(vw->window->view_list == vw)
    {
	/* There's no predecessor to VW. So give its space to
	   the following view. If possible fix the display origin
	   of the following view to minimise scrolling. */
	intptr_t new_origin_col, new_origin_row;
	pred = vw->next_view;
	vw->window->view_list = pred;
	if(!skip_glyph_rows_backwards(pred, vw->height + 1,
				      VCOL(pred->display_origin),
				      VROW(pred->display_origin),
				      &new_origin_col, &new_origin_row))
	{
	    new_origin_col = 0;
	    new_origin_row = pred->tx->logical_start;
	}
	pred->display_origin = make_pos(new_origin_col, new_origin_row);
    }
    else
    {
	/* Find this view's predecessor. */
	for(pred = vw->window->view_list; pred != 0; pred = pred->next_view)
	{
	    if(pred->next_view == vw)
		break;
	}
	pred->next_view = vw->next_view;
    }
    kill_view(vw);
    /* VW is now unlinked; now gives its window-space to PRED. */
    pred->height += vw->height + 1;
    recalc_measures(pred->window);
    set_scroll_steps(pred);
    return(rep_VAL(vw));
}

/* Destroy all views of window W. */
void
kill_all_views(Lisp_Window *w)
{
    Lisp_View *vw = w->view_list;
    while(vw != 0)
    {
	Lisp_View *next = vw->next_view;
	kill_view(vw);
	vw = next;
    }
    w->current_view = 0;
    w->view_list = 0;
}

/* Initialise the scroll steps in VW, from the size of the view and 
   the desired ratios. */
static void
set_scroll_steps(Lisp_View *vw)
{
    if((vw->scroll_ratio_x <= 0)
       || ((vw->scroll_step_x = vw->width / vw->scroll_ratio_x) <= 0)
       || (vw->scroll_step_x > vw->width))
	vw->scroll_step_x = 1;
    if((vw->scroll_ratio_y <= 0)
       || ((vw->scroll_step_y = vw->height / vw->scroll_ratio_y) <= 0)
       || (vw->scroll_step_y > vw->height))
	vw->scroll_step_y = 1;
}

/* For each view in window W, recalculate all view positions from
   the MaxX and MaxY settings. */
static void
recalc_measures(Lisp_Window *w)
{
    Lisp_View *vw;
    int row = 0;
    for(vw = w->view_list; vw != 0; vw = vw->next_view)
    {
	vw->min_x = 0;
	vw->min_y = row;
	row += vw->height + ((vw->car & VWFF_MINIBUF) ? 0 : 1);
    }
}

/* Recalibrate the sizes of each view in window W. If the size of the
   window has changed recently the views dimensions will have to be
   changed; it tries to do this such that the general weighting between
   each view stays the same. Horrible things could happen if there's not
   enough window space for all the views... */
void
update_views_dimensions(Lisp_Window *w)
{
    int lines_given = 0;
    int old_total_lines = 0;
    Lisp_View *vw;

    for(vw = w->view_list; vw != 0; vw = vw->next_view)
	old_total_lines += vw->height + 1;
    old_total_lines--;		/* minibuf has no status line */

    if(old_total_lines == w->row_count && w->column_count == w->view_list->width)
	/* No changes */
	return;

    while(w->row_count < (w->view_count * 2) - 1)
    {
	/* Not enough lines for the number of existing views. Delete
	   views until there is */
	Lisp_View *dead = w->view_list;
	w->view_list = w->view_list->next_view;
	kill_view(dead);
    }

    for(vw = w->view_list; vw != 0; vw = vw->next_view)
    {
	vw->min_x = 0;
	vw->width = w->column_count;
	if(vw->car & VWFF_MINIBUF)
	    vw->height = 1;
	else
	{
	    if(vw->next_view->car & VWFF_MINIBUF)
		/* Last view. Give it the remainder (except
		   the minibuf's line). */
		vw->height = w->row_count - lines_given - 2;
	    else
	    {
		/* Otherwise try to keep the old weighting. */
		vw->height = (((vw->height + 1) * w->row_count)
			       / old_total_lines) - 1;
		vw->height = MAX(vw->height, 1);
	    }
	}
	vw->min_y = lines_given;
	lines_given += vw->height + 1;
	set_scroll_steps(vw);
    }
}

/* Expand format characters */
static intptr_t
format_mode_string(char *fmt, Lisp_View *vw, char *buf, intptr_t buf_len)
{
    Lisp_Buffer *tx = vw->tx;
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
	    intptr_t len;

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
	    len = sprintf(buf, "%ld", VROW(vw->cursor_pos) + 1
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
	    
	    if(VROW(vw->display_origin) <= tx->logical_start)
	    {
		if(vw->car & VWFF_AT_BOTTOM)
		    position = "All";
		else
		    position = "Top";
	    }
	    else if(vw->car & VWFF_AT_BOTTOM)
		position = "Bot";
	    else
	    {
		int percent = (((VROW(vw->display_origin)
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
	    *buf++ = "-+<>"[vw->block_state+1];
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
						vw->cursor_pos,
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

static intptr_t
format_mode_value(repv format, Lisp_View *vw, char *buf, intptr_t buf_len)
{
    Lisp_Buffer *tx = vw->tx;

    if(rep_SYMBOLP(format))
    {
	repv tem = Fbuffer_symbol_value(format, vw->cursor_pos,
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
	    intptr_t done = buf_len - format_mode_string(rep_STR(item), vw,
						       buf, buf_len);
	    buf += done; buf_len -= done;
	}
	else if(rep_SYMBOLP(item))
	{
	    repv tem = Fbuffer_symbol_value(item, vw->cursor_pos,
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
		intptr_t done = buf_len - format_mode_value(tem, vw,
							  buf, buf_len);
		buf += done; buf_len -= done;
	    }
	}
	else if(rep_CONSP(item))
	{
	    intptr_t done = 0;
	    repv first = rep_CAR(item);
	    if(rep_STRINGP(first))
		done = buf_len - format_mode_string(rep_STR(first), vw,
						    buf, buf_len);
	    else if(rep_SYMBOLP(first))
	    {
		repv tem = Fbuffer_symbol_value(first, vw->cursor_pos,
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
		if(tem != 0 && !rep_NILP(tem))
		    done = buf_len - format_mode_value(tem, vw, buf, buf_len);
	    }
	    buf += done; buf_len -= done;
	}
    }
    return buf_len;
}

/* Reformat the status string of VW. */
void
update_status_buffer(Lisp_View *vw, char *buf, intptr_t buf_len)
{
    if(!(vw->car & VWFF_MINIBUF))
    {
	intptr_t done;
	Lisp_Buffer *tx = vw->tx;
	repv format = Fbuffer_symbol_value(Qmode_line_format,
					       vw->cursor_pos,
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
    return rep_handle_var_int (val, &curr_vw->scroll_ratio_y);
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
    return rep_handle_var_int (val, &curr_vw->scroll_ratio_x);
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
    if(VVIEW(vw)->car & VWFF_RECTBLOCKS)
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
    oflags = VVIEW(vw)->car;
    if(rep_NILP(stat))
	VVIEW(vw)->car &= ~VWFF_RECTBLOCKS;
    else
	VVIEW(vw)->car |= VWFF_RECTBLOCKS;
    return(stat);
}

DEFUN("current-view", Fcurrent_view, Scurrent_view, (repv win), rep_Subr1) /*
::doc:current-view::
current-view [WINDOW]

Returns the currently active view in WINDOW.
::end:: */
{
    return(WINDOWP(win) ? rep_VAL(VWINDOW(win)->current_view) : rep_VAL(curr_vw));
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
    VVIEW(vw)->window->current_view = VVIEW(vw);
    if(VVIEW(vw)->window == curr_win)
    {
	curr_vw = VVIEW(vw);
	curr_vw->window->current_view = curr_vw;
    }
    if(!rep_NILP(activ))
	Fset_current_window(rep_VAL(VVIEW(vw)->window), activ);
    return(vw);
}

DEFUN("buffer-list", Fbuffer_list, Sbuffer_list, (void), rep_Subr0) /*
::doc:buffer-list::
List of buffers in most-recently-used order. Each view has it's own.
::end:: */
{
    return(curr_vw->buffer_list);
}

DEFUN("set-buffer-list", Fset_buffer_list, Sset_buffer_list, (repv val), rep_Subr1) /*
::doc:set-buffer-list::
List of buffers in most-recently-used order. Each view has it's own.
::end:: */
{
    curr_vw->buffer_list = val;
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
    Lisp_Window *w = curr_win;
    rep_DECLARE1(buffer, BUFFERP);
    do {
	Lisp_View *vw = w->current_view;
	do {
	    if(vw->tx == VBUFFER(buffer))
		return rep_VAL(vw);
	    vw = vw->next_view;
	    if(vw == 0)
		vw = w->view_list;
	} while(vw != w->current_view);
	w = w->next;
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
    Lisp_View *curr;
    if(VIEWP(win))
    {
	curr = VVIEW(win);
	win = rep_VAL(curr->window);
    }
    else if(WINDOWP(win))
	curr = VWINDOW(win)->current_view;
    else
    {
	curr = curr_vw;
	win = rep_VAL(curr_win);
    }
    /* If possible just return the next view in the original window */
    if(curr->next_view != 0)
	return rep_VAL(curr->next_view);
    else
    {
	if(rep_NILP(allp))
	    /* First view of the original window. */
	    return rep_VAL(VWINDOW(win)->view_list);
	else
	{
	    if(VWINDOW(win)->next != 0)
		return rep_VAL(VWINDOW(win)->next->view_list);
	    else
		return rep_VAL(win_chain->view_list);
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
    Lisp_View *vw;
    Lisp_View *curr;
    if(VIEWP(win))
    {
	curr = VVIEW(win);
	win = rep_VAL(curr->window);
    }
    else if(WINDOWP(win))
	curr = VWINDOW(win)->current_view;
    else
    {
	curr = curr_vw;
	win = rep_VAL(curr_win);
    }
    if(curr == VWINDOW(win)->view_list)
    {
	/* current view is first in this window. If ALLP is t
	   need to find the previous window and the last view in it.
	   otherwise the last view in the current window. */
	Lisp_Window *w;
	if(!rep_NILP(allp))
	{
	    w = VWINDOW(win);
	    if(w == win_chain)
	    {
		/* first window, find last */
		while(w->next != 0)
		    w = w->next;
	    }
	    else
	    {
		/* find predecessor of W */
		while(w->next != w)
		    w = w->next;
	    }
	}
	else
	    w = VWINDOW(win);
	/* now simply find the last view in W, handling minibuffer
	   views appropriately. */
	vw = w->view_list;
	while(vw->next_view != 0)
	    vw = vw->next_view;
    }
    else
    {
	/* find the predecessor of the current view */
	vw = VWINDOW(win)->view_list;
	while(vw->next_view != curr)
	    vw = vw->next_view;
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
    return VVIEW(vw)->display_origin;
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
    return Fcons(rep_MAKE_INT(VVIEW(vw)->width),
		    rep_MAKE_INT(VVIEW(vw)->height));
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
    return make_pos(VVIEW(vw)->min_x, VVIEW(vw)->min_y);
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
    Lisp_View *sibling;
    intptr_t new_sibling_height;
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    if(!rep_INTP(rows))
	return vw;
    sibling = VVIEW(vw)->next_view;
    if(sibling == 0)
    {
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&no_view), vw));
    }
    new_sibling_height = sibling->height - (rep_INT(rows) - VVIEW(vw)->height);
    if(new_sibling_height < 1 || rep_INT(rows) < 1)
    {
	return Fsignal(Qwindow_error, rep_list_2(rep_VAL(&no_room), vw));
    }
    VVIEW(vw)->height = rep_INT(rows);
    sibling->height = new_sibling_height;
    recalc_measures(VVIEW(vw)->window);
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
    Lisp_Window *w = WINDOWP(win) ? VWINDOW(win) : curr_win;
    Lisp_View *vw = w->view_list;
    rep_DECLARE1(pos, POSP);
    if(VROW(pos) < 0)
	return Qnil;
    while(vw != NULL)
    {
	/* height doesn't include the status line */
	intptr_t bottom = (vw->min_y + vw->height
			       + ((vw->car & VWFF_MINIBUF) ? 0 : 1));
	if(VROW(pos) < bottom)
	    return rep_VAL(vw);
	vw = vw->next_view;
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
    intptr_t col, row;
    rep_DECLARE1(pos, POSP);
    if(!VIEWP(vw))
	vw = rep_VAL(curr_vw);
    col = VCOL(pos) - VVIEW(vw)->min_x;
    row = VROW(pos) - VVIEW(vw)->min_y;
    if(col < 0 || col >= VVIEW(vw)->width
       || row < 0 || row > VVIEW(vw)->height)
	return Qnil;
    else if(row == VVIEW(vw)->height)
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
    return (VVIEW(vw)->car & VWFF_MINIBUF) ? Qt : Qnil;
}

DEFUN("minibuffer-view", Fminibuffer_view, Sminibuffer_view, (repv win), rep_Subr1) /*
::doc:minibuffer-view::
minibuffer-view [WINDOW]

Returns the view of the minibuffer in WINDOW (or the current window).
::end:: */
{
    return rep_VAL(WINDOWP(win) ? VWINDOW(win)->mini_buffer_view : curr_win->mini_buffer_view);
}

DEFUN("minibuffer-active-p", Fminibuffer_active_p, Sminibuffer_active_p, (repv win), rep_Subr1) /*
::doc:minibuffer-active-p::
minibuffer-active-p [WINDOW]

Returns t if the minibuffer of WINDOW is being used.
::end:: */
{
    return MINIBUFFER_ACTIVE_P(WINDOWP(win) ? VWINDOW(win) : curr_win)
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
    Lisp_View *vw = view_chain;
    view_chain = NULL;
    while(vw)
    {
	Lisp_View *next = vw->next;
	if(rep_GC_CELL_MARKEDP(rep_VAL(vw)))
	{
	    rep_GC_CLR_CELL(rep_VAL(vw));
	    vw->next = view_chain;
	    view_chain = vw;
	}
	else
	    rep_free(vw);
	vw = next;
    }
}

static void
view_mark (repv val)
{
    while (val != 0)
    {
	rep_MARKVAL(rep_VAL(VVIEW(val)->tx));
	rep_MARKVAL(VVIEW(val)->buffer_list);
	rep_MARKVAL(VVIEW(val)->cursor_pos);
	rep_MARKVAL(VVIEW(val)->last_cursor_pos);
	rep_MARKVAL(VVIEW(val)->display_origin);
	rep_MARKVAL(VVIEW(val)->block_start);
	rep_MARKVAL(VVIEW(val)->block_end);
	val = rep_VAL(VVIEW(val)->next_view);
	if (val != 0)
	    rep_GC_SET_CELL (val);
    }
}    

static void
view_prin(repv stream, repv vw)
{
    char buf[32];
    if(VVIEW(vw)->window == 0)
	rep_stream_puts(stream, "#<dead-view>", -1, false);
    else
    {
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf),
		 "#<view %d,%d", VVIEW(vw)->width, VVIEW(vw)->height);
#else
	sprintf(buf, "#<view %d,%d", VVIEW(vw)->width, VVIEW(vw)->height);
#endif
	rep_stream_puts(stream, buf, -1, false);
	if(VVIEW(vw)->tx)
	{
	    rep_stream_putc(stream, ' ');
	    rep_stream_puts(stream, rep_PTR(VVIEW(vw)->tx->buffer_name),
			-1, true);
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
	repv handle = Fcons (rep_VAL(VVIEW(vw)->window->current_view),
			     rep_VAL(curr_win));
	curr_vw = VVIEW(vw);
	curr_win = VVIEW(vw)->window;
	curr_win->current_view = curr_vw;
	return handle;
    }
}

static void
view_unbind (repv handle)
{
    Lisp_View *vw = VVIEW(rep_CAR(handle));
    Lisp_Window *win = VWINDOW(rep_CDR(handle));
    if (vw->window && vw->window->w_Window != WINDOW_NIL)
    {
	vw->window->current_view = vw;
	curr_win = win;
	curr_vw = curr_win->current_view;
    }
}

void
views_init(void)
{
    view_type = rep_register_new_type ("view", 0, view_prin, view_prin,
				       view_sweep, view_mark, 0,
				       0, 0, 0, 0,
				       view_bind, view_unbind);

    mb_unused_buffer = VBUFFER(Fmake_buffer(rep_VAL(&unused_mb), Qnil, Qt));

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
    Lisp_View *vw = view_chain;
    while(vw != 0)
    {
	Lisp_View *next = vw->next;
	rep_free(vw);
	vw = next;
    }
    view_chain = NULL;
}
