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
#include "jade_protos.h"

#include <string.h>
#include <errno.h>
#include <sys/stat.h>

_PR int write_clip(int, char *, int);
_PR VALUE read_clip(int);
_PR void beep(VW *);

_PR void x11_convert_selection(XSelectionRequestEvent *ev);
_PR void x11_lose_selection(XSelectionClearEvent *ev);
_PR void x11_window_lose_selections(Window win);
_PR void x11_misc_init(void);

int
write_clip(int buffer, char *str, int len)
{
    int rc = TRUE;
    if((buffer >= 0) && (buffer <= 7))
	XStoreBuffer(x11_display, str, len, buffer);
    else
    {
	cmd_signal(sym_error, list_2(MKSTR("No cut buffer"), make_number(buffer)));
	rc = FALSE;
    }
    return(rc);
}

VALUE
read_clip(int buffer)
{
    if((buffer >= 0) && (buffer <= 7))
    {
	int len;
	u_char *mem = XFetchBuffer(x11_display, &len, buffer);
	if(mem)
	    return(string_dupn(mem, len));
	return(NULL);
    }
    cmd_signal(sym_error, list_2(MKSTR("Not cut-buffer"), make_number(buffer)));
    return(NULL);
}

void
beep(VW *vw)
{
    XBell(x11_display, 0);
}


/* Selection handling */

enum Sel_type {
    Sel_area = 0, Sel_string
};

static struct selection_info {
    Window owner;
    Time birthdate;
    VALUE data;				/* either a string or a buffer */
    POS start, end;
    enum Sel_type type;
} selection_info[2];

VALUE sym_xa_primary, sym_xa_secondary;

static INLINE int
selection_atom_to_index(Atom atom)
{
    return (atom == XA_PRIMARY) ? 0 : 1;
}

static Atom
symbol_to_atom(VALUE sym)
{
    if(sym == sym_xa_primary)
	return XA_PRIMARY;
    else if(sym == sym_xa_secondary)
	return XA_SECONDARY;
    else
	return (Atom) 0;
}

_PR VALUE cmd_x11_set_selection(VALUE sel, VALUE lstart, VALUE lend, VALUE buffer);
DEFUN("x11-set-selection", cmd_x11_set_selection, subr_x11_set_selection, (VALUE sel, VALUE lstart, VALUE lend, VALUE buffer), V_Subr4, DOC_x11_set_selection) /*
::doc:x11_set_selection::
x11-set-selection SELECTION [ STRING | START END [BUFFER] ]

Defines the X11 selection whose name corresponds to the symbol SELECTION
(either `xa-primary' or `xa-secondary'). The selection can be set to
either an arbitrary piece of text if the second argument is a string,
or to area of BUFFER betwee START and END if the second argument is a
position.

Returns t if the current selection is now what was requested, nil
otherwise.
::end:: */
{
    Atom selection;
    enum Sel_type type;

    DECLARE1(sel, SYMBOLP);

    if(STRINGP(lstart))
	type = Sel_string;
    else
    {
	DECLARE2(lstart, POSP);
	DECLARE3(lend, POSP);
	if(!BUFFERP(buffer))
	    buffer = VAL(curr_vw->vw_Tx);
	type = Sel_area;
    }
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	XSetSelectionOwner(x11_display, selection,
			   curr_win->w_Window, CurrentTime);
	if(XGetSelectionOwner(x11_display, selection) == curr_win->w_Window)
	{
	    /* We've now got the selection. */
	    selection_info[selno].owner = curr_win->w_Window;
	    selection_info[selno].type = type;
	    selection_info[selno].birthdate = x11_last_event_time;
	    if(type == Sel_area)
	    {
		selection_info[selno].data = buffer;
		selection_info[selno].start = VPOS(lstart);
		selection_info[selno].end = VPOS(lend);
	    }
	    else
		selection_info[selno].data = lstart;
	    return sym_t;
	}
	else
	{
	    selection_info[selno].owner = WINDOW_NIL;
	    selection_info[selno].data = sym_nil;
	    return sym_nil;
	}
    }
    return cmd_signal(sym_error, list_2(MKSTR("No atom for symbol"), sel));
}

static Bool
selnotify_pred(Display *dpy, XEvent *ev, XPointer arg)
{
    return ev->type == SelectionNotify;
}

_PR VALUE cmd_x11_selection_active_p(VALUE sel);
DEFUN("x11-selection-active-p", cmd_x11_selection_active_p, subr_x11_selection_active_p, (VALUE sel), V_Subr1, DOC_x11_selection_active_p) /*
::doc:x11_selection_active_p::
x11-selection-active-p SELECTION

Returns t if the X11 selection defined by the symbol SELECTION (either
`xa-primary' or `xa-secondary') is available for reading.
::end:: */
{
    Atom selection;
    DECLARE1(sel, SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != WINDOW_NIL
	   || XGetSelectionOwner(x11_display, selection) != None)
	{
	    return sym_t;
	}
    }
    return sym_nil;
}

_PR VALUE cmd_x11_own_selection_p(VALUE sel);
DEFUN("x11-own-selection-p", cmd_x11_own_selection_p, subr_x11_own_selection_p, (VALUE sel), V_Subr1, DOC_x11_own_selection_p) /*
::doc:x11_own_selection_p::
x11-own-selection-p SELECTION

Returns t if the X11 selection defined by the symbol SELECTION (either
`xa-primary' or `xa-secondary') is owned by Jade.
::end:: */
{
    Atom selection;
    DECLARE1(sel, SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != WINDOW_NIL)
	    return sym_t;
    }
    return sym_nil;
}

_PR VALUE cmd_x11_get_selection(VALUE sel);
DEFUN("x11-get-selection", cmd_x11_get_selection, subr_x11_get_selection, (VALUE sel), V_Subr1, DOC_x11_get_selection) /*
::doc:x11_get_selection::
x11-get-selection SELECTION

Returns the string corresponding to the current value of the X11 selection
defined by the symbol SELECTION (either `xa-primary' or `xa-secondary').

If the selection currently has no value, nil is returned.
::end:: */
{
    Atom selection;
    DECLARE1(sel, SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	VALUE res = sym_nil;
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
					       &selection_info[selno].start,
					       &selection_info[selno].end);
		    res = make_string(tlen + 1);
		    if(res)
		    {
			copy_section(VTX(selection_info[selno].data),
				     &selection_info[selno].start,
				     &selection_info[selno].end, VSTR(res));
			VSTR(res)[tlen] = 0;
		    }
		}
	    }
	    else
		abort();		/* shouldn't happen */
	}
	else
	{
	    /* Selection lies with another application. */
	    Window owner = XGetSelectionOwner(x11_display, selection);
	    if(owner != None)
	    {
		XEvent ev;
		XConvertSelection(x11_display, selection, XA_STRING,
				  x11_jade_sel, curr_win->w_Window,
				  CurrentTime);
		XIfEvent(x11_display, &ev, selnotify_pred, (XPointer)0);
		if(ev.xselection.property != None)
		{
		    /* First find the size of the property. */
		    Atom actual_type;
		    int actual_format;
		    unsigned long nitems, bytes_after;
		    unsigned char *prop;          
		    int r;
		    int offset;
		    r = XGetWindowProperty(x11_display, curr_win->w_Window,
					   x11_jade_sel, 0, 0, False,
					   AnyPropertyType, &actual_type,
					   &actual_format, &nitems,
					   &bytes_after, &prop);
		    if(r != Success)
			return sym_nil;
		    XFree(prop);
		    if(actual_type == None || actual_format != 8)
			return sym_nil;
		    res = make_string(bytes_after + 1);
		    if(!res)
			return mem_error();
		    offset = 0;
		    while(bytes_after > 0)
		    {
			r = XGetWindowProperty(x11_display,
					       curr_win->w_Window,
					       x11_jade_sel, offset/4,
					       (bytes_after / 4) + 1,
					       False, AnyPropertyType,
					       &actual_type, &actual_format,
					       &nitems, &bytes_after, &prop);
			if(r != Success)
			    return sym_nil;
			memcpy(VSTR(res) + offset, prop, nitems);
			XFree(prop);
			offset += nitems;
		    }
		    XDeleteProperty(x11_display, curr_win->w_Window,
				    x11_jade_sel);
		    VSTR(res)[offset] = 0;
		}
	    }
	}
	return res;
    }
    return cmd_signal(sym_error, list_2(MKSTR("No atom for symbol"), sel));
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
	    XChangeProperty(x11_display, ev->requestor, ev->property,
			    XA_STRING, 8, PropModeReplace,
			    VSTR(selection_info[selno].data),
			    STRING_LEN(selection_info[selno].data));
	}
	else if(selection_info[selno].type == Sel_area)
	{
	    if(check_section(VTX(selection_info[selno].data),
			     &selection_info[selno].start,
			     &selection_info[selno].end))
	    {
		long tlen = section_length(VTX(selection_info[selno].data),
					   &selection_info[selno].start,
					   &selection_info[selno].end);
		char *string = str_alloc(tlen + 1);
		if(string)
		{
		    copy_section(VTX(selection_info[selno].data),
				 &selection_info[selno].start,
				 &selection_info[selno].end, string);
		    string[tlen] = 0;
		    XChangeProperty(x11_display, ev->requestor, ev->property,
				    XA_STRING, 8, PropModeReplace,
				    string, tlen);
		    str_free(string);
		}
	    }
	}
	else
	    abort();			/* shouldn't happen */
	send_ev.xselection.property = ev->property;
    }
    XSendEvent(x11_display, ev->requestor, False, 0, &send_ev);
}

void
x11_lose_selection(XSelectionClearEvent *ev)
{
    int selno = selection_atom_to_index(ev->selection);
    if(ev->time != CurrentTime
       && ev->time > selection_info[selno].birthdate)
    {
	selection_info[selno].owner = WINDOW_NIL;
	selection_info[selno].data = sym_nil;
    }
}

void
x11_window_lose_selections(Window win)
{
    int i;
    for(i = 0; i < 2; i++)
    {
	if(selection_info[i].owner == win)
	{
	    selection_info[i].owner = WINDOW_NIL;
	    selection_info[i].data = sym_nil;
	    XSetSelectionOwner(x11_display,
			       (i == 0) ? XA_PRIMARY : XA_SECONDARY,
			       None, CurrentTime);
	}
    }
}

_PR VALUE cmd_x11_lose_selection(VALUE sel);
DEFUN("x11-lose-selection", cmd_x11_lose_selection, subr_x11_lose_selection, (VALUE sel), V_Subr1, DOC_x11_lose_selection) /*
::doc:x11_lose_selection::
x11-lose-selection SELECTION

If the X11 selection specified by the symbol SELECTION is currently owned
by Jade, relinquish ownership.
::end:: */
{
    Atom selection;
    DECLARE1(sel, SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == XA_PRIMARY || selection == XA_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != WINDOW_NIL)
	{
	    XSetSelectionOwner(x11_display, selection, None,
			       x11_last_event_time);
	    selection_info[selno].owner = WINDOW_NIL;
	    selection_info[selno].data = sym_nil;
	    return sym_t;
	}
	return sym_nil;
    }
    return cmd_signal(sym_error, list_2(MKSTR("No atom for symbol"), sel));
}

void
x11_misc_init(void)
{
    INTERN(sym_xa_primary, "xa-primary");
    INTERN(sym_xa_secondary, "xa-secondary");
    ADD_SUBR(subr_x11_set_selection);
    ADD_SUBR(subr_x11_selection_active_p);
    ADD_SUBR(subr_x11_own_selection_p);
    ADD_SUBR(subr_x11_get_selection);
    ADD_SUBR(subr_x11_lose_selection);
    mark_static(&selection_info[0].data);
    mark_static(&selection_info[1].data);
}
