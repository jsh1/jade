/* x11_eventloop.c -- Eventloop for X11
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

#ifdef HAVE_UNIX
# include <sys/types.h>
# include <sys/time.h>
# include <errno.h>
#else
  you lose
#endif

_PR VALUE event_loop(void);

static VALUE
handle_event(XEvent *xev)
{
    WIN *oldwin = curr_win, *ev_win;
    VALUE result = sym_nil;
    ev_win = x11_find_window(xev->xany.window);
    if(ev_win)
    {
	switch(xev->type)
	{
	    u_long code, mods;

	case MappingNotify:
	    XRefreshKeyboardMapping(&xev->xmapping);
	    break;

	case Expose:
	    if(ev_win->w_Flags & WINFF_SLEEPING)
	    {
		/* Guess that the wm uniconified us? */
		ev_win->w_Flags &= ~WINFF_SLEEPING;
	    }
	    if(ev_win->w_Flags & WINFF_FORCE_REFRESH)
	    {
		/* Wait until the last Expose then do a total redraw.  */
		if(xev->xexpose.count == 0)
		{
		    refresh_window(ev_win);
		}
	    }
	    else
		x11_handle_expose(ev_win, &xev->xexpose);
	    if(ev_win == oldwin)
		cursor(ev_win->w_CurrVW, CURS_ON);
	    break;

	case ConfigureNotify:
	    if((ev_win->w_WindowSys.ws_Width != xev->xconfigure.width)
	       || (ev_win->w_WindowSys.ws_Height != xev->xconfigure.height))
	    {
		if(ev_win == oldwin)
		    cursor(ev_win->w_CurrVW, CURS_OFF);
		if((ev_win->w_WindowSys.ws_Height != 0)
		   && (ev_win->w_WindowSys.ws_Height < xev->xconfigure.height))
		ev_win->w_WindowSys.ws_Width = xev->xconfigure.width;
		ev_win->w_WindowSys.ws_Height = xev->xconfigure.height;
		x11_update_dimensions(ev_win, xev->xconfigure.width,
				      xev->xconfigure.height);
		update_views_dimensions(ev_win);
		if(ev_win == oldwin)
		    cursor(ev_win->w_CurrVW, CURS_ON);
	    }
	    break;

	case ClientMessage:
	    if((xev->xclient.format == 32)
		&& (xev->xclient.data.l[0] == x11_wm_del_win))
	    {
		curr_win = ev_win;
		if(ev_win != oldwin)
		{
		    curr_vw = curr_win->w_CurrVW;
		    /* Window switch */
		    undo_distinct();
		    cursor(oldwin->w_CurrVW, CURS_OFF);
		}
		else
		    cursor(ev_win->w_CurrVW, CURS_OFF);
		result = cmd_eval_hook2(MKSTR("window-closed-hook"), sym_nil);
		if(curr_win)
		{
		    refresh_world();
		    cursor(curr_vw, CURS_ON);
		}
	    }
	    break;

	case FocusIn:
	    if(ev_win != oldwin)
	    {
		cursor(oldwin->w_CurrVW, CURS_OFF);
		cursor(ev_win->w_CurrVW, CURS_ON);
		curr_win = ev_win;
		curr_vw = curr_win->w_CurrVW;
		undo_distinct();
	    }
	    break;

	case MotionNotify:
	    /* Swallow any pending motion events as well. */
	    while(XCheckMaskEvent(x11_display, ButtonMotionMask, xev))
		;
	    /* FALL THROUGH */

	case ButtonPress:
	case ButtonRelease:
	case KeyPress:
	    code = mods = 0;
	    translate_event(&code, &mods, xev);
	    if(mods & EV_TYPE_MASK)
	    {
		curr_win = ev_win;
		if(oldwin != ev_win)
		{
		    curr_vw = curr_win->w_CurrVW;
		    cursor(oldwin->w_CurrVW, CURS_OFF);
		    undo_distinct();
		}
		reset_message(ev_win);
		result = usekey(xev, code, mods, (ev_win == oldwin));
	    }
	    break;

	case SelectionRequest:
	    x11_convert_selection(&xev->xselectionrequest);
	    break;

	case SelectionClear:
	    x11_lose_selection(xev->xselectionclear.selection);
	    break;
	}
    }
    return(result);
}

VALUE
event_loop(void)
{
    VALUE result = sym_nil;
    recurse_depth++;
    curr_vw->vw_Flags |= VWFF_REFRESH_STATUS;
    refresh_world_curs();
    while(curr_win)
    {
#ifdef HAVE_UNIX
	fd_set copy;
	struct timeval timeout;
	int number, i;
	bool refreshp;
#endif
	/*
	 * HERE... Read out all events in Q
	 */
	while(INT_P || XEventsQueued(x11_display, QueuedAfterReading) > 0)
	{
	    if(INT_P)
		result = NULL;
	    else
	    {
		XEvent ev;
		XNextEvent(x11_display, &ev);
		result = handle_event(&ev);
	    }
	    if(!result)
	    {
		if(throw_value)
		{
		    VALUE tv = throw_value;
		    VALUE car = VCAR(tv);
		    throw_value = NULL;
		    if(car == sym_exit)
		    {
			result = VCDR(tv);
			if(recurse_depth > 0)
			    goto end;
		    }
		    else if((car == sym_top_level) && (recurse_depth == 0))
			result = VCDR(tv);
		    else if(car == sym_quit)
			goto end;
		    else if(car == sym_user_interrupt)
		    {
			handle_error(car, sym_nil);
			result = sym_nil;
		    }
		    else if(car == sym_error)
		    {
			handle_error(VCAR(VCDR(tv)), VCDR(VCDR(tv)));
			result = sym_nil;
		    }
		    else if(recurse_depth == 0)
		    {
			result = sym_nil;
			handle_error(sym_no_catcher, LIST_1(car));
		    }
		    else
		    {
			throw_value = tv;
			goto end;
		    }
		}
		else
		    result = sym_nil;
	    }
	    if(!curr_win)
		goto end;
	}

# ifdef HAVE_SUBPROCESSES
	if(proc_notification())
	{
	    curr_vw->vw_Flags |= VWFF_REFRESH_STATUS;
	    refresh_world_curs();
	}
# endif

	XFlush(x11_display);

#ifdef HAVE_UNIX
	copy = x11_fd_read_set;
	timeout.tv_sec = EVENT_TIMEOUT_LENGTH;
	timeout.tv_usec = 0;
	/* Don't want select() to restart after a SIGCHLD; there may be
	   a notification to dispatch.  */
# ifdef HAVE_SUBPROCESSES
	sigchld_restart(FALSE);
# endif
	number = select(FD_SETSIZE, &copy, NULL, NULL, &timeout);
# ifdef HAVE_SUBPROCESSES
	sigchld_restart(TRUE);
# endif
	refreshp = FALSE;
	if(number > 0)
	{
	    /*
	     * no need to test first 3 descriptors
	     */
	    i = 3;
	    while(number > 0)
	    {
		if(FD_ISSET(i, &copy))
		{
		    number--;
		    if(x11_fd_read_action[i])
		    {
			x11_fd_read_action[i](i);
			refreshp = TRUE;
		    }
		}
		i++;
	    }
	}
	else if(number == 0)
	{
	    /* A timeout; do one of:
	         Remove messages in minibuffers
		 Print the current key-prefix
		 Auto-save a buffer
		 GC if enough data allocated
		 Run the `idle-hook'  */
	    if(remove_all_messages(TRUE)
	       || print_event_prefix()
	       || auto_save_buffers())
		refreshp = TRUE;
	    else if(data_after_gc > idle_gc_threshold)
	    {
		/* nothing was saved so try a GC */
		cmd_garbage_collect(sym_t);
	    }
	    else
	    {
		VALUE hook = cmd_symbol_value(sym_idle_hook, sym_t);
		if(!VOIDP(hook) && !NILP(hook))
		{
		    cmd_eval_hook2(sym_idle_hook, sym_nil);
		    refreshp = TRUE;
		}
	    }
	}
#else
	whatever you like...
#endif /* HAVE_UNIX */

	if(refreshp)
	{
	    curr_vw->vw_Flags |= VWFF_REFRESH_STATUS;
	    refresh_world_curs();
	}
    }
end:
    recurse_depth--;
    return(result);
}
