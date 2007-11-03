/* x11_misc.c -- Miscellaneous functions for X11
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

void
sys_beep(WIN *w)
{
    XBell(WINDOW_XDPY(w)->display, 0);
}


/* Selection handling */

enum Sel_type {
    Sel_area = 0, Sel_string
};

static struct selection_info {
    Window owner;
    Time birthdate;
    repv data;				/* either a string or a buffer */
    repv start, end;
    enum Sel_type type;
} selection_info[2];

DEFSYM(xa_primary, "xa-primary");
DEFSYM(xa_secondary, "xa-secondary");

DEFSTRING(no_atom, "No atom for symbol");

static inline int
selection_atom_to_index(Atom atom)
{
    return (atom == XA_PRIMARY) ? 0 : 1;
}

static Atom
symbol_to_atom(repv sym)
{
    if(sym == Qxa_primary)
	return XA_PRIMARY;
    else if(sym == Qxa_secondary)
	return XA_SECONDARY;
    else
	return (Atom) 0;
}

DEFUN("x11-set-selection", Fx11_set_selection, Sx11_set_selection, (repv sel, repv start, repv end, repv buffer), rep_Subr4) /*
::doc:x11-set-selection::
x11-set-selection SELECTION [ STRING | START END [BUFFER] ]

Defines the X11 selection whose name corresponds to the symbol SELECTION
(either `xa-primary' or `xa-secondary'). The selection can be set to
either an arbitrary piece of text if the second argument is a string,
or to area of BUFFER between START and END if the second argument is a
position.

Returns t if the current selection is now what was requested, nil
otherwise.
::end:: */
{
    Atom selection;

    rep_DECLARE1(sel, rep_SYMBOLP);

    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	enum Sel_type type;

	if (start == Qnil)
	{
	    XSetSelectionOwner (WINDOW_XDPY(curr_win)->display,
				selection, None, CurrentTime);
	    return Qt;
	}
	else if(rep_STRINGP(start))
	    type = Sel_string;
	else
	{
	    rep_DECLARE2(start, POSP);
	    rep_DECLARE3(end, POSP);
	    if(!BUFFERP(buffer))
		buffer = rep_VAL(curr_vw->vw_Tx);
	    type = Sel_area;
	}

	XSetSelectionOwner(WINDOW_XDPY(curr_win)->display,
			   selection, curr_win->w_Window, CurrentTime);
	if(XGetSelectionOwner(WINDOW_XDPY(curr_win)->display,
			      selection) == curr_win->w_Window)
	{
	    /* We've now got the selection. */
	    selection_info[selno].owner = curr_win->w_Window;
	    selection_info[selno].type = type;
	    selection_info[selno].birthdate = x11_last_event_time;
	    if(type == Sel_area)
	    {
		selection_info[selno].data = buffer;
		selection_info[selno].start = start;
		selection_info[selno].end = end;
	    }
	    else
		selection_info[selno].data = start;
	    return Qt;
	}
	else
	{
	    selection_info[selno].owner = WINDOW_NIL;
	    selection_info[selno].data = Qnil;
	    return Qnil;
	}
    }
    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_atom), sel));
}

static Bool
selnotify_pred(Display *dpy, XEvent *ev, XPointer arg)
{
    return ev->type == SelectionNotify;
}

DEFUN("x11-selection-active-p", Fx11_selection_active_p, Sx11_selection_active_p, (repv sel), rep_Subr1) /*
::doc:x11-selection-active-p::
x11-selection-active-p SELECTION

Returns t if the X11 selection defined by the symbol SELECTION (either
`xa-primary' or `xa-secondary') is available for reading.
::end:: */
{
    Atom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != WINDOW_NIL
	   || XGetSelectionOwner(WINDOW_XDPY(curr_win)->display,
				 selection) != None)
	{
	    return Qt;
	}
    }
    return Qnil;
}

DEFUN("x11-own-selection-p", Fx11_own_selection_p, Sx11_own_selection_p, (repv sel), rep_Subr1) /*
::doc:x11-own-selection-p::
x11-own-selection-p SELECTION

Returns t if the X11 selection defined by the symbol SELECTION (either
`xa-primary' or `xa-secondary') is owned by Jade.
::end:: */
{
    Atom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != WINDOW_NIL)
	    return Qt;
    }
    return Qnil;
}

DEFUN("x11-get-selection", Fx11_get_selection, Sx11_get_selection, (repv sel), rep_Subr1) /*
::doc:x11-get-selection::
x11-get-selection SELECTION

Returns the string corresponding to the current value of the X11 selection
defined by the symbol SELECTION (either `xa-primary' or `xa-secondary').

If the selection currently has no value, nil is returned.
::end:: */
{
    Atom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	repv res = Qnil;
	if(selection_info[selno].owner != WINDOW_NIL)
	{
	    /* We own this selection, avoid the server. */
	    if(selection_info[selno].type == Sel_string)
		res = selection_info[selno].data;
	    else if(selection_info[selno].type == Sel_area)
	    {
		if(check_section(VTX(selection_info[selno].data),
				 &selection_info[selno].start,
				 &selection_info[selno].end))
		{
		    long tlen = section_length(VTX(selection_info[selno].data),
					       selection_info[selno].start,
					       selection_info[selno].end);
		    res = rep_make_string(tlen + 1);
		    if(res)
		    {
			copy_section(VTX(selection_info[selno].data),
				     selection_info[selno].start,
				     selection_info[selno].end, rep_STR(res));
			rep_STR(res)[tlen] = 0;
		    }
		}
	    }
	    else
		abort();		/* shouldn't happen */
	}
	else
	{
	    /* Selection lies with another application. */
	    struct x11_display *dpy = WINDOW_XDPY(curr_win);
	    Window owner = XGetSelectionOwner(dpy->display, selection);
	    if(owner != None)
	    {
		XEvent ev;
		XConvertSelection(dpy->display, selection, XA_STRING,
				  dpy->jade_selection, curr_win->w_Window,
				  CurrentTime);
		XIfEvent(dpy->display, &ev, selnotify_pred, (XPointer)0);
		if(ev.xselection.property != None)
		{
		    /* First find the size of the property. */
		    Atom actual_type;
		    int actual_format;
		    unsigned long nitems, bytes_after;
		    unsigned char *prop;          
		    int r;
		    int offset;
		    r = XGetWindowProperty(dpy->display, curr_win->w_Window,
					   dpy->jade_selection, 0, 0, False,
					   AnyPropertyType, &actual_type,
					   &actual_format, &nitems,
					   &bytes_after, &prop);
		    if(r != Success)
			return Qnil;
		    XFree(prop);
		    if(actual_type == None || actual_format != 8)
			return Qnil;
		    res = rep_make_string(bytes_after + 1);
		    if(!res)
			return rep_mem_error();
		    offset = 0;
		    while(bytes_after > 0)
		    {
			r = XGetWindowProperty(dpy->display,
					       curr_win->w_Window,
					       dpy->jade_selection, offset/4,
					       (bytes_after / 4) + 1,
					       False, AnyPropertyType,
					       &actual_type, &actual_format,
					       &nitems, &bytes_after, &prop);
			if(r != Success)
			    return Qnil;
			memcpy(rep_STR(res) + offset, prop, nitems);
			XFree(prop);
			offset += nitems;
		    }
		    XDeleteProperty(dpy->display, curr_win->w_Window,
				    dpy->jade_selection);
		    rep_STR(res)[offset] = 0;
		}
	    }
	}
	return res;
    }
    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_atom), sel));
}

void
x11_convert_selection(XSelectionRequestEvent *ev)
{
    int selno = selection_atom_to_index(ev->selection);
    XEvent send_ev;
    send_ev.xselection.type = SelectionNotify;
    send_ev.xselection.selection = ev->selection;
    send_ev.xselection.target = ev->target;
    send_ev.xselection.requestor = ev->requestor;
    send_ev.xselection.time = ev->time;
    send_ev.xselection.property = None;
    if(ev->target == XA_STRING)
    {
	/* Convert to text. */
	if(selection_info[selno].type == Sel_string)
	{
	    XChangeProperty(ev->display, ev->requestor, ev->property,
			    XA_STRING, 8, PropModeReplace,
			    (u_char *)rep_STR(selection_info[selno].data),
			    rep_STRING_LEN(selection_info[selno].data));
	}
	else if(selection_info[selno].type == Sel_area)
	{
	    if(check_section(VTX(selection_info[selno].data),
			     &selection_info[selno].start,
			     &selection_info[selno].end))
	    {
		long tlen = section_length(VTX(selection_info[selno].data),
					   selection_info[selno].start,
					   selection_info[selno].end);
		char *string = rep_alloc(tlen + 1);
		if(string)
		{
		    copy_section(VTX(selection_info[selno].data),
				 selection_info[selno].start,
				 selection_info[selno].end, string);
		    string[tlen] = 0;
		    XChangeProperty(ev->display, ev->requestor, ev->property,
				    XA_STRING, 8, PropModeReplace,
				    (u_char *)string, tlen);
		    rep_free(string);
		}
	    }
	}
	else
	    abort();			/* shouldn't happen */
	send_ev.xselection.property = ev->property;
    }
    XSendEvent(ev->display, ev->requestor, False, 0, &send_ev);
}

void
x11_lose_selection(XSelectionClearEvent *ev)
{
    int selno = selection_atom_to_index(ev->selection);
    if(ev->time != CurrentTime
       && ev->time > selection_info[selno].birthdate)
    {
	selection_info[selno].owner = WINDOW_NIL;
	selection_info[selno].data = Qnil;
    }
}

void
x11_window_lose_selections(WIN *w)
{
    int i;
    for(i = 0; i < 2; i++)
    {
	if(selection_info[i].owner == w->w_Window)
	{
	    selection_info[i].owner = WINDOW_NIL;
	    selection_info[i].data = Qnil;
	    XSetSelectionOwner(WINDOW_XDPY(w)->display,
			       (i == 0) ? XA_PRIMARY : XA_SECONDARY,
			       None, CurrentTime);
	}
    }
}

DEFUN("x11-lose-selection", Fx11_lose_selection, Sx11_lose_selection, (repv sel), rep_Subr1) /*
::doc:x11-lose-selection::
x11-lose-selection SELECTION

If the X11 selection specified by the symbol SELECTION is currently owned
by Jade, relinquish ownership.
::end:: */
{
    Atom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != WINDOW_NIL)
	{
	    XSetSelectionOwner(curr_win->w_WindowSys.ws_Display->display,
			       selection, None, x11_last_event_time);
	    selection_info[selno].owner = WINDOW_NIL;
	    selection_info[selno].data = Qnil;
	    return Qt;
	}
	return Qnil;
    }
    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_atom), sel));
}



DEFUN("x11-cursor-shape", Fx11_cursor_shape, Sx11_cursor_shape, (repv arg), rep_Subr1) /*
::doc:x11-cursor-shape::
x11-cursor-shape [VALUE]

An integer identifying the X cursor to use for editor windows. See
<X11/cursorfont.h> for the list of available cursors.
::end:: */
{
    if (rep_INTP(arg) && rep_INT(arg) != x11_cursor_shape)
    {
	struct x11_display *dpy = x11_display_list;
	WIN *win = win_chain;
	x11_cursor_shape = rep_INT(arg);
	while (dpy != 0)
	{
	    dpy->text_cursor = XCreateFontCursor (dpy->display,
						  x11_cursor_shape);
	    dpy = dpy->next;
	}
	sys_recolor_cursor (mouse_cursor_face);
	while (win != 0)
	{
	    XDefineCursor (WINDOW_XDPY(win)->display, win->w_Window,
			   WINDOW_XDPY(win)->text_cursor);
	    win = win->w_Next;
	}
	return arg;
    }
    else
	return rep_MAKE_INT(x11_cursor_shape);
}
    
void
x11_misc_init(void)
{
    int i;
    for(i = 0; i < 2; i++)
    {
	rep_mark_static(&selection_info[i].data);
	rep_mark_static(&selection_info[i].start);
	rep_mark_static(&selection_info[i].end);
    }
    rep_INTERN(xa_primary);
    rep_INTERN(xa_secondary);
    rep_ADD_SUBR(Sx11_set_selection);
    rep_ADD_SUBR(Sx11_selection_active_p);
    rep_ADD_SUBR(Sx11_own_selection_p);
    rep_ADD_SUBR(Sx11_get_selection);
    rep_ADD_SUBR(Sx11_lose_selection);
    rep_ADD_SUBR(Sx11_cursor_shape);
}
