/* gtk_select.c -- Selection management for GTK
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
#include <gtk/gtksignal.h>
#include <gtk/gtkselection.h>
#include <gdk/gdk.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef HAVE_X11
  /* XXX Yet again I'm resorting to Xlib. I should really do something
     XXX about this, but it's working for now... */
# include <gdk/gdkprivate.h>
# include <X11/Xlib.h>
# include <X11/Xatom.h>
#endif


/* Selection handling */

enum Sel_type {
    Sel_area = 0, Sel_string
};

static struct selection_info {
    repv owner;				/* a window or nil */
    unsigned long birthdate;
    repv data;				/* either a string or a buffer */
    repv start, end;
    enum Sel_type type;
} selection_info[2];

DEFSYM(xa_primary, "xa-primary");
DEFSYM(xa_secondary, "xa-secondary");

DEFSTRING(no_atom, "No atom for symbol");

static inline int
selection_atom_to_index(GdkAtom atom)
{
    return (atom == GDK_SELECTION_PRIMARY) ? 0 : 1;
}

static GdkAtom
symbol_to_atom(repv sym)
{
    if(sym == Qxa_primary)
	return GDK_SELECTION_PRIMARY;
    else if(sym == Qxa_secondary)
	return GDK_SELECTION_SECONDARY;
    else
	return (GdkAtom) 0;
}

DEFUN("gtk-jade-set-selection", Fgtk_jade_set_selection,
      Sgtk_jade_set_selection,
      (repv sel, repv start, repv end, repv buffer), rep_Subr4) /*
::doc:gtk-jade-set-selection::
gtk-jade-set-selection SELECTION [ STRING | START END [BUFFER] ]

Defines the selection whose name corresponds to the symbol SELECTION
(either `xa-primary' or `xa-secondary'). The selection can be set to
either an arbitrary piece of text if the second argument is a string,
or to area of BUFFER between START and END if the second argument is a
position.

Returns t if the current selection is now what was requested, nil
otherwise.
::end:: */
{
    GdkAtom selection;
    enum Sel_type type;

    rep_DECLARE1(sel, rep_SYMBOLP);

    selection = symbol_to_atom(sel);
    if(selection == GDK_SELECTION_PRIMARY
       || selection == GDK_SELECTION_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);

	if (start == Qnil)
	{
	    gtk_selection_owner_set (0, selection, gtk_jade_last_event_time);
	    return Qt;
	}
	else if(rep_STRINGP(start))
	    type = Sel_string;
	else
	{
	    rep_DECLARE2(start, POSP);
	    rep_DECLARE3(end, POSP);
	    if(!BUFFERP(buffer))
		buffer = rep_VAL(curr_vw->tx);
	    type = Sel_area;
	}

	if (gtk_selection_owner_set(GTK_WIDGET (curr_win->w_Window),
				    selection,
				    gtk_jade_last_event_time))
	{
	    /* We've now got the selection. */
	    selection_info[selno].owner = rep_VAL (curr_win);
	    selection_info[selno].type = type;
	    selection_info[selno].birthdate = gtk_jade_last_event_time;
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
	    selection_info[selno].owner = Qnil;
	    selection_info[selno].data = Qnil;
	    return Qnil;
	}
    }
    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_atom), sel));
}

DEFUN("gtk-jade-selection-active-p", Fgtk_jade_selection_active_p,
      Sgtk_jade_selection_active_p, (repv sel), rep_Subr1) /*
::doc:gtk-jade-selection-active-p::
gtk-jade-selection-active-p SELECTION

Returns t if the selection defined by the symbol SELECTION (either
`xa-primary' or `xa-secondary') is available for reading.
::end:: */
{
    GdkAtom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == GDK_SELECTION_PRIMARY
       || selection == GDK_SELECTION_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
#ifdef HAVE_X11
	if(selection_info[selno].owner != Qnil
	   || XGetSelectionOwner(gdk_display, selection) != 0)
#else
	   /* XXX Arrgh! This only returns non-null if GDK
	      XXX created the window. How fucking stupid.. */
	if(selection_info[selno].owner != Qnil
	   || gdk_selection_owner_get(selection) != 0)
#endif
	{
	    return Qt;
	}
    }
    return Qnil;
}

DEFUN("gtk-jade-own-selection-p", Fgtk_jade_own_selection_p,
      Sgtk_jade_own_selection_p, (repv sel), rep_Subr1) /*
::doc:gtk-jade-own-selection-p::
gtk-jade-own-selection-p SELECTION

Returns t if the selection defined by the symbol SELECTION (either
`xa-primary' or `xa-secondary') is owned by Jade.
::end:: */
{
    GdkAtom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == GDK_SELECTION_PRIMARY
       || selection == GDK_SELECTION_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != Qnil)
	    return Qt;
    }
    return Qnil;
}

#ifdef HAVE_X11
static Bool
selnotify_pred(Display *dpy, XEvent *ev, XPointer arg)
{
    return ev->type == SelectionNotify;
}
#endif

DEFUN("gtk-jade-get-selection", Fgtk_jade_get_selection,
      Sgtk_jade_get_selection, (repv sel), rep_Subr1) /*
::doc:gtk-jade-get-selection::
gtk-jade-get-selection SELECTION

Returns the string corresponding to the current value of the selection
defined by the symbol SELECTION (either `xa-primary' or `xa-secondary').

If the selection currently has no value, nil is returned.
::end:: */
{
    GdkAtom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == GDK_SELECTION_PRIMARY
       || selection == GDK_SELECTION_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	repv res = Qnil;
	if(selection_info[selno].owner != Qnil)
	{
	    /* We own this selection, avoid the server. */
	    if(selection_info[selno].type == Sel_string)
		res = selection_info[selno].data;
	    else if(selection_info[selno].type == Sel_area)
	    {
		if(check_section(VBUFFER(selection_info[selno].data),
				 &selection_info[selno].start,
				 &selection_info[selno].end))
		{
		    long tlen = section_length(VBUFFER(selection_info[selno].data),
					       selection_info[selno].start,
					       selection_info[selno].end);
		    res = rep_allocate_string(tlen + 1);
		    if(res)
		    {
			copy_section(VBUFFER(selection_info[selno].data),
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
#ifdef HAVE_X11
	    Window win = XGetSelectionOwner (gdk_display, selection);
	    if (win != None)
	    {
		GdkWindowPrivate *private
		    = (GdkWindowPrivate *)curr_win->w_Window->widget.window;
		XEvent ev;
		XConvertSelection (gdk_display, selection, XA_STRING,
				   gdk_selection_property, private->xwindow,
				   CurrentTime);
		XIfEvent(gdk_display, &ev, selnotify_pred, (XPointer)0);
		if(ev.xselection.property != None)
		{
#else
# error "Can't synchronously get the selection-notify in pure GDK!?"
#endif
		    guchar *data;
		    GdkAtom ret_type;
		    gint ret_format;
		    gint len = gdk_selection_property_get
			(curr_win->w_Window->widget.window,
			 &data, &ret_type, &ret_format);
		    if (len != 0
			&& ret_type != GDK_NONE
			&& ret_format == 8)
		    {
			res = rep_string_copy_n (data, len);
		    }
		    if (data != 0)
			g_free (data);
		    gdk_property_delete (curr_win->w_Window->widget.window,
					 gdk_selection_property);
		}
	    }
	}
	return res;
    }
    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_atom), sel));
}

void
gtk_jade_add_selection_targets (GtkJade *jade)
{
    GdkAtom target = gdk_atom_intern ("STRING", 0);
    gtk_selection_add_target (GTK_WIDGET (jade),
			      GDK_SELECTION_PRIMARY, target, 0);
    gtk_selection_add_target (GTK_WIDGET (jade), 
			      GDK_SELECTION_SECONDARY, target, 0);
    gtk_signal_connect (GTK_OBJECT (jade), "selection_received",
			GTK_SIGNAL_FUNC (gtk_jade_selection_get), (gpointer)0);
}

void
gtk_jade_selection_get(GtkWidget *widget, GtkSelectionData *sel_data,
		       guint info, guint time)
{
    gint len;
    guchar *data;
    bool free_data = false;
    int selno = selection_atom_to_index(sel_data->selection);
    if (selection_info[selno].owner == Qnil)
	return;
    /* Convert to text. */
    if(selection_info[selno].type == Sel_string)
    {
	len = rep_STRING_LEN(selection_info[selno].data);
	data = rep_STR(selection_info[selno].data);
    }
    else if(selection_info[selno].type == Sel_area)
    {
	if(check_section(VBUFFER(selection_info[selno].data),
			 &selection_info[selno].start,
			 &selection_info[selno].end))
	{
	    len = section_length(VBUFFER(selection_info[selno].data),
				 selection_info[selno].start,
				 selection_info[selno].end);
	    data = rep_alloc(len + 1);
	    if(data)
	    {
		copy_section(VBUFFER(selection_info[selno].data),
			     selection_info[selno].start,
			     selection_info[selno].end, data);
		data[len] = 0;
		free_data = true;
	    }
	}
	else
	{
	    len = 0;
	    data = "";
	}
    }
    else
	abort();			/* shouldn't happen */
    gtk_selection_data_set (sel_data, GDK_SELECTION_TYPE_STRING,
			    8*sizeof(gchar), data, len);
    if (free_data)
	rep_free (data);
}

gint
gtk_jade_selection_clear(GtkWidget *widget, GdkEventSelection *event)
{
    int selno = selection_atom_to_index(event->selection);
    if(event->time != GDK_CURRENT_TIME
       && event->time > selection_info[selno].birthdate)
    {
	selection_info[selno].owner = Qnil;
	selection_info[selno].data = Qnil;
    }
    return false;
}

void
gtk_jade_window_lose_selections(Lisp_Window *w)
{
    int i;
    for(i = 0; i < 2; i++)
    {
	if(selection_info[i].owner == rep_VAL (w))
	{
	    selection_info[i].owner = Qnil;
	    selection_info[i].data = Qnil;
	    gtk_selection_owner_set (0, (i == 0) ? GDK_SELECTION_PRIMARY
				     : GDK_SELECTION_SECONDARY,
				     GDK_CURRENT_TIME);
	}
    }
}

DEFUN("gtk-jade-lose-selection", Fgtk_jade_lose_selection,
      Sgtk_jade_lose_selection, (repv sel), rep_Subr1) /*
::doc:gtk-jade-lose-selection::
gtk-jade-lose-selection SELECTION

If the selection specified by the symbol SELECTION is currently owned by
Jade, relinquish ownership.
::end:: */
{
    GdkAtom selection;
    rep_DECLARE1(sel, rep_SYMBOLP);
    selection = symbol_to_atom(sel);
    if(selection == GDK_SELECTION_PRIMARY
       || selection == GDK_SELECTION_SECONDARY)
    {
	int selno = selection_atom_to_index(selection);
	if(selection_info[selno].owner != Qnil)
	{
	    gtk_selection_owner_set (0, selection, gtk_jade_last_event_time);
	    selection_info[selno].owner = 0;
	    selection_info[selno].data = Qnil;
	    return Qt;
	}
	return Qnil;
    }
    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_atom), sel));
}


/* Initialisation */

void
gtk_misc_init(void)
{
    int i;
    for(i = 0; i < 2; i++)
    {
	selection_info[i].owner = Qnil;
	rep_mark_static(&selection_info[i].owner);
	rep_mark_static(&selection_info[i].data);
	rep_mark_static(&selection_info[i].start);
	rep_mark_static(&selection_info[i].end);
    }
    rep_INTERN(xa_primary);
    rep_INTERN(xa_secondary);
    rep_ADD_SUBR(Sgtk_jade_set_selection);
    rep_ADD_SUBR(Sgtk_jade_selection_active_p);
    rep_ADD_SUBR(Sgtk_jade_own_selection_p);
    rep_ADD_SUBR(Sgtk_jade_get_selection);
    rep_ADD_SUBR(Sgtk_jade_lose_selection);
}
