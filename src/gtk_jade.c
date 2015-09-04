/* gtk_jade.c -- the jade GTK widget
   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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
#include <gtk/gtk.h>
#include <string.h>
#include <assert.h>

#ifdef HAVE_X11
  /* XXX Yet again I'm resorting to Xlib. */
# include <gdk/gdkprivate.h>
# include <X11/Xlib.h>
# include <X11/keysym.h>
#endif

#define INPUT_EVENTS (GDK_BUTTON_PRESS_MASK		\
		      | GDK_BUTTON_RELEASE_MASK		\
		      | GDK_KEY_PRESS_MASK		\
		      | GDK_EXPOSURE_MASK		\
		      | GDK_VISIBILITY_NOTIFY_MASK	\
		      | GDK_ENTER_NOTIFY_MASK		\
		      | GDK_LEAVE_NOTIFY_MASK 		\
		      | GDK_POINTER_MOTION_MASK		\
		      | GDK_POINTER_MOTION_HINT_MASK)

static void gtk_jade_class_init (GtkJadeClass *klass);
static void gtk_jade_init (GtkJade *jade);
static void gtk_jade_destroy (GtkObject *object);
static void gtk_jade_realize (GtkWidget *widget);
static void gtk_jade_size_request (GtkWidget *widget,
				   GtkRequisition *requisition);
static void gtk_jade_size_allocate (GtkWidget *widget,
				    GtkAllocation *allocation);
static gint gtk_jade_expose (GtkWidget *widget, GdkEventExpose *event);
static gint gtk_jade_visibility_event (GtkWidget *widget,
				       GdkEventVisibility *event);
static gint gtk_jade_input_event (GtkWidget *widget, GdkEvent *event);
static gint gtk_jade_enter_notify (GtkWidget *widget, GdkEventCrossing *event);
static gint gtk_jade_leave_notify (GtkWidget *widget, GdkEventCrossing *event);
static void gtk_jade_drag_data_received (GtkWidget *widget,
					 GdkDragContext *context,
					 gint x, gint y,
					 GtkSelectionData *selection_data,
					 guint info, guint time, void *data);

static GtkWidgetClass *parent_class = 0;

static int cursor_shape = GDK_XTERM;
static GdkCursor *window_cursor = 0;

/* When true, sys_new_window _won't_ stick the GtkJade widget in a
   containing GtkWindow. */
static bool new_window_no_frame = false;

/* When true, sys_new_window _won't_ call gtk_widget_show on the GtkJade
   widget. */
static bool new_window_no_show = false;

/* Timestamp from a recent event */
unsigned long gtk_jade_last_event_time;

DEFSYM (gtk_jade_new_hook, "gtk-jade-new-hook"); /*
::doc:gtk-jade-new-hook::
This hook is called when a new gtk-jade widget is created, with the
widget as its single argument. Any function in the hook trying to add
the widget to a container should actually add the top-level widget, not
the argument itself.
::end:: */

DEFSYM (dnd_drop_uri_list, "dnd-drop-uri-list");


/* GtkJade widget mechanics */

GtkType
gtk_jade_get_type (void)
{
    static GtkType range_type = 0;

    if (!range_type)
    {
	static const GtkTypeInfo jade_info = {
	    "GtkJade",
	    sizeof (GtkJade),
	    sizeof (GtkJadeClass),
	    (GtkClassInitFunc) gtk_jade_class_init,
	    (GtkObjectInitFunc) gtk_jade_init,
	    NULL, NULL, (GtkClassInitFunc) NULL,
	};

	range_type = gtk_type_unique (GTK_TYPE_WIDGET, &jade_info);
    }

    return range_type;
}

static void
gtk_jade_class_init (GtkJadeClass *class)
{
    GtkObjectClass *object_class;
    GtkWidgetClass *widget_class;

    object_class = (GtkObjectClass*) class;
    widget_class = (GtkWidgetClass*) class;

    parent_class = gtk_type_class (GTK_TYPE_WIDGET);

    object_class->destroy = gtk_jade_destroy;

    widget_class->size_request = gtk_jade_size_request;
    widget_class->realize = gtk_jade_realize;
    widget_class->size_allocate = gtk_jade_size_allocate;
    widget_class->expose_event = gtk_jade_expose;
    widget_class->visibility_notify_event = gtk_jade_visibility_event;
    widget_class->button_press_event = (void *)gtk_jade_input_event;
    widget_class->button_release_event = (void *)gtk_jade_input_event;
    widget_class->motion_notify_event = (void *)gtk_jade_input_event;
    widget_class->key_press_event = (void *)gtk_jade_input_event;
    widget_class->enter_notify_event = gtk_jade_enter_notify;
    widget_class->leave_notify_event = gtk_jade_leave_notify;
    widget_class->selection_clear_event = gtk_jade_selection_clear;
    widget_class->selection_get = gtk_jade_selection_get;
}

static void
gtk_jade_init (GtkJade *jade)
{
    GTK_WIDGET_SET_FLAGS (jade, GTK_CAN_FOCUS);
    jade->win = 0;
    jade->font = 0;
    jade->bold_font = 0;
    jade->gc = 0;
    jade->gc_values_mask = 0;
    jade->width = jade->height = 0;
    jade->has_focus = 0;
    jade->unobscured = 0;
}

bool
gtk_jade_set_font (GtkJade *jade)
{
    GdkFont *font = gdk_font_load (rep_STR (jade->win->font_name));
    if (font == 0)
	font = gdk_font_load (DEFAULT_FONT);
    if (font == 0)
	return false;

    if (jade->font != 0)
	gdk_font_unref (jade->font);
    jade->font = font;

    if (jade->bold_font != 0)
    {
	gdk_font_unref (jade->bold_font);
	jade->bold_font = 0;
    }

    jade->win->font_width = gdk_text_width (font, "M", 1);
    jade->win->font_height = font->ascent + font->descent;

    if (jade->win->font_width <= 0)
	jade->win->font_width = 1;
    if (jade->win->font_height <= 0)
	jade->win->font_height = 1;

    /* Now for the bold font. It doesn't look as though GDK
       allows us to find the fully expanded name of the font? */
    {
	char *name = rep_STR (jade->win->font_name);
	char *tem = name;
	int dashes = 0;
	while(*tem && dashes != 3)
	{
	    if(*tem++ == '-')
		dashes++;
	}
	if(dashes == 3)
	{
	    /* So the next part of the string should be the weight. */
	    char buf[256];
	    memcpy(buf, name, tem - name);
	    strcpy(buf + (tem - name), "bold");
	    while(*tem)
	    {
		if(*tem++ == '-')
		    break;
	    }
	    strcat(buf, tem - 1);
	    jade->bold_font = gdk_font_load (buf);

	    /* XXX check for same size as jade->font.. */
	}
    }
    if (GTK_WIDGET_REALIZED (GTK_WIDGET (jade)))
    {
	sys_update_dimensions (jade->win);
	update_window_dimensions (jade->win);
    }
    return true;
}

void
gtk_jade_get_size (GtkJade *jade, gint *widthp, gint *heightp)
{
    if (jade->width <= 0 || jade->height <= 0)
	gdk_window_get_size (jade->widget.window, &jade->width, &jade->height);

    *widthp = jade->width;
    *heightp = jade->height;
}

GtkWidget *
gtk_jade_new (Lisp_Window *win, int width, int height)
{
    static GtkTargetEntry drag_types[] = { { "text/uri-list", 0, 0 } };
    static gint n_drag_types = sizeof (drag_types) / sizeof (drag_types [0]);

    GtkJade *jade;
    repv hook;
    jade = gtk_type_new (gtk_jade_get_type ());

    jade->win = win;
    jade->width = width;
    jade->height = height;
    gtk_jade_set_font (jade);

    /* this is copied from gedit */
    gtk_drag_dest_set (GTK_WIDGET (jade),
		       GTK_DEST_DEFAULT_ALL,
		       drag_types, n_drag_types,
		       GDK_ACTION_COPY);
    gtk_signal_connect (GTK_OBJECT (jade), "drag_data_received",
			GTK_SIGNAL_FUNC (gtk_jade_drag_data_received), 0);

    hook = Fsymbol_value (Qgtk_jade_new_hook, Qt);
    if (hook && rep_CONSP (hook))
    {
	repv args = Fcons ((*gtk_jade_wrap_gtkobj) (GTK_OBJECT (jade)), Qnil);
	Fcall_hook (hook, args, Qnil);
    }

    return GTK_WIDGET (jade);
}

static void
gtk_jade_destroy (GtkObject *object)
{
    GtkJade *jade;
    g_return_if_fail (object != NULL);
    g_return_if_fail (GTK_IS_JADE (object));
    jade = GTK_JADE (object);

    if (jade->font != 0)
	gdk_font_unref (jade->font);
    if (jade->bold_font != 0)
	gdk_font_unref (jade->bold_font);
    if (jade->gc != 0)
	gdk_gc_unref (jade->gc);

    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
gtk_jade_realize (GtkWidget *widget)
{
    GtkJade *jade;
    GdkWindowAttr attributes;
    gint attributes_mask;
    repv face, fg = 0, bg = 0;

    g_return_if_fail (widget != NULL);
    g_return_if_fail (GTK_IS_JADE (widget));
    GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);
    jade = GTK_JADE (widget);

    jade->width = widget->allocation.width;
    jade->height = widget->allocation.height;

    attributes.x = widget->allocation.x;
    attributes.y = widget->allocation.y;
    attributes.width = widget->allocation.width;
    attributes.height = widget->allocation.height;
    attributes.wclass = GDK_INPUT_OUTPUT;
    attributes.window_type = GDK_WINDOW_CHILD;
    attributes.event_mask = gtk_widget_get_events (widget) | INPUT_EVENTS;
    attributes.visual = gtk_widget_get_visual (widget);
    attributes.colormap = gtk_widget_get_colormap (widget);
    
    attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
    widget->window = gdk_window_new (widget->parent->window,
				     &attributes, attributes_mask);
    widget->style = gtk_style_attach (widget->style, widget->window);
    
    gdk_window_set_user_data (widget->window, widget);
    gtk_style_set_background (widget->style, widget->window, GTK_STATE_ACTIVE);

    /* Set background pixel */
    face = Fsymbol_value (Qdefault_face, Qt);
    if (face && FACEP(face))
    {
	fg = VFACE(face)->foreground;
	bg = VFACE(face)->background;
	gdk_window_set_background (widget->window, &VCOLOR(bg)->color);
    }	    

    /* Create gc */
    jade->gc_values.line_width = 0;
    if (fg != 0)
	jade->gc_values.foreground = VCOLOR(fg)->color;
    if (bg != 0)
	jade->gc_values.background = VCOLOR(bg)->color;
    jade->gc_values.font = jade->font;
    jade->gc_values.function = GDK_COPY;
    jade->gc_values.graphics_exposures = true;
    jade->gc_values_mask = (GDK_GC_LINE_WIDTH | GDK_GC_FOREGROUND
			    | GDK_GC_BACKGROUND | GDK_GC_FONT
			    | GDK_GC_FUNCTION | GDK_GC_EXPOSURES);
    jade->gc = gdk_gc_new_with_values (widget->window,
				       &jade->gc_values,
				       jade->gc_values_mask);

    gdk_window_set_cursor (jade->widget.window, window_cursor);
    gdk_window_show (jade->widget.window);

    /* Impolite?! */
    gtk_widget_grab_focus (GTK_WIDGET (jade));
}

static void
gtk_jade_size_request (GtkWidget *widget,
		       GtkRequisition *requisition)
{
    GtkJade *jade;
    g_return_if_fail (widget != NULL);
    g_return_if_fail (GTK_IS_JADE (widget));
    jade = GTK_JADE (widget);

    requisition->width = 4 * jade->win->font_width;
    requisition->height = 4 * jade->win->font_height;
}

static void
gtk_jade_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
    GtkJade *jade;
    g_return_if_fail (widget != NULL);
    g_return_if_fail (GTK_IS_JADE (widget));
    jade = GTK_JADE (widget);

    widget->allocation = *allocation;
    if (GTK_WIDGET_REALIZED (widget))
    {
	gdk_window_move_resize (widget->window,
				allocation->x, allocation->y,
				allocation->width, allocation->height);
    }

    /* It seems that the notebook widget sizes undisplayed windows as 1x1.
       If I propagate this everything loses (update_views_dimensions will
       delete _all_ views since there's no room). So just pretend the
       window was iconified.. */
    if (allocation->width >= jade->win->font_width
	&& allocation->height >= jade->win->font_height)
    {
	jade->width = allocation->width;
	jade->height = allocation->height;
	jade->win->car &= ~WINFF_SLEEPING;
    }
    else
	jade->win->car |= WINFF_SLEEPING;

    if (jade->win != 0)
    {
	sys_update_dimensions (jade->win);
	update_window_dimensions (jade->win);
	GTK_JADE_CALLBACK_POSTFIX;
    }
}


/* Event handlers */

static gint
gtk_jade_expose (GtkWidget *widget, GdkEventExpose *event)
{
    GtkJade *jade;
    int x, y, width, height;

    g_return_val_if_fail (GTK_IS_JADE (widget), false);
    jade = GTK_JADE (widget);

    x = (event->area.x - jade->win->pixel_left) / jade->win->font_width;
    y = (event->area.y - jade->win->pixel_top) / jade->win->font_height;
    /* Why +2? It seems to be necessary.. */
    width = (event->area.width / jade->win->font_width) + 2;
    height = (event->area.height / jade->win->font_height) + 2;

    garbage_glyphs(jade->win, x, y, width, height);

    if (!jade->unobscured)
    {
	/* Avoid cascading graphics expose events by never copying */
	redisplay_set_no_copy ();
    }

    if (event->count == 0)
	Fredisplay_window (rep_VAL (jade->win), Qnil);

    return false;
}

static gint
gtk_jade_visibility_event (GtkWidget *widget, GdkEventVisibility *event)
{
    GtkJade *jade;

    g_return_val_if_fail (GTK_IS_JADE (widget), false);
    jade = GTK_JADE (widget);

    jade->unobscured = (event->state == GDK_VISIBILITY_UNOBSCURED);

    return false;
}

static gint
gtk_jade_input_event (GtkWidget *widget, GdkEvent *event)
{
    GtkJade *jade;
    unsigned long code = 0, mods = 0;
    bool redisplay = false;
    g_return_val_if_fail (widget != NULL, false);
    g_return_val_if_fail (GTK_IS_JADE (widget), false);
    g_return_val_if_fail (event != NULL, false);
    jade = GTK_JADE (widget);

    switch ((int) event->type)
    {
	int x, y;

    case GDK_MOTION_NOTIFY:
	gtk_jade_last_event_time = event->motion.time;
	gdk_window_get_pointer (widget->window, &x, &y, 0);
	goto do_motion;

    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
	gtk_jade_last_event_time = event->button.time;
	x = event->button.x;
	y = event->button.y;
    do_motion:
	x = (x - jade->win->pixel_left) / jade->win->font_width;
	y = (y - jade->win->pixel_top) / jade->win->font_height;
	redisplay = update_pointer_extent (jade->win, x, y);
	break;

    case GDK_KEY_PRESS:
	gtk_jade_last_event_time = event->key.time;
	break;
    }

    translate_event (&code, &mods, event);
    if(mods & EV_TYPE_MASK
       /* Don't pass modifier-less motion-events through */
       && ((mods & EV_TYPE_MASK) != EV_TYPE_MOUSE
	   || code != EV_CODE_MOUSE_MOVE
	   || (mods & EV_MOD_BUTTON_MASK) != 0))
    {
	if(curr_win != jade->win)
	    curr_vw = jade->win->current_view;
	curr_win = jade->win;
	reset_message(jade->win);
	eval_input_event(event, code, mods);
	undo_end_of_command();
	GTK_JADE_CALLBACK_POSTFIX;
	return true;
    }
    else if (redisplay)
    {
	Fredisplay (Qnil);
	return true;
    }
    
    return false;
}

static gint
gtk_jade_enter_notify (GtkWidget *widget, GdkEventCrossing *event)
{
    GtkJade *jade;
    g_return_val_if_fail (widget != NULL, false);
    g_return_val_if_fail (GTK_IS_JADE (widget), false);
    g_return_val_if_fail (event != NULL, false);
    jade = GTK_JADE (widget);

    gtk_jade_last_event_time = event->time;
    return false;
}

static gint
gtk_jade_leave_notify (GtkWidget *widget, GdkEventCrossing *event)
{
    GtkJade *jade;
    g_return_val_if_fail (widget != NULL, false);
    g_return_val_if_fail (GTK_IS_JADE (widget), false);
    g_return_val_if_fail (event != NULL, false);
    jade = GTK_JADE (widget);

    gtk_jade_last_event_time = event->time;
    return false;
}

static void
gtk_jade_drag_data_received (GtkWidget *widget, GdkDragContext *context,
			     gint x, gint y, GtkSelectionData *selection_data,
			     guint info, guint time, void *unused)
{
    repv list = Qnil, pos;
    GtkJade *jade;
    char *data;

    g_return_if_fail (widget != NULL);
    g_return_if_fail (GTK_IS_JADE (widget));
    jade = GTK_JADE (widget);

    pos = make_pos ((x - jade->win->pixel_left) / jade->win->font_width,
		    (y - jade->win->pixel_top) / jade->win->font_height);

    data = (char *)selection_data->data;

    /* DATA is a sequence of URI's, each terminated by a \r\n */
    while (*data != 0)
    {
	char *end = data + strcspn (data, "\r\n");
	if (end == data)
	    end = data + strlen (data);
	list = Fcons (rep_string_copy_n (data, end - data), list);
	data = end + strspn (end, "\r\n");
    }

    Ffuncall(Fcons (Fsymbol_value (Qdnd_drop_uri_list, Qnil),
		    Fcons (Freverse (list),
			   Fcons (rep_VAL (jade->win),
				  Fcons (pos, Qnil)))));

    GTK_JADE_CALLBACK_POSTFIX;
}



/* Low level drawing */

static inline void
face_to_gc(GtkJade *jade, Merged_Face *f, bool invert)
{
    GdkColor *c;
    GdkFont *font;

    if(f->car & FACEFF_INVERT)
	invert = !invert;

    c = &VCOLOR(invert ? f->background : f->foreground)->color;
    if(c != 0)
    {
	if(jade->gc_values.foreground.pixel != c->pixel)
	{
	    jade->gc_values.foreground = *c;
	    gdk_gc_set_foreground (jade->gc, c);
	}
    }

    c = &VCOLOR(invert ? f->foreground : f->background)->color;
    if(c != 0)
    {
	if(jade->gc_values.background.pixel != c->pixel)
	{
	    jade->gc_values.background = *c;
	    gdk_gc_set_background (jade->gc, c);
	}
    }

    /* FIXME: italic?! */

    if((f->car & FACEFF_BOLD) && jade->bold_font)
	font = jade->bold_font;
    else
	font = jade->font;
    if(jade->gc_values.font != font)
    {
	jade->gc_values.font = font;
#if 0
	gdk_gc_set_font (jade->gc, font);
#endif
    }
}

void
sys_draw_glyphs(Lisp_Window *w, int col, int row, uint8_t attr, char *str,
		int len, bool all_spaces)
{
    GtkJade *jade = w->w_Window;
    GdkWindow *win = jade->widget.window;
    bool invert = false;
    Merged_Face *f;
    int x, y;

    if (!jade || !GTK_WIDGET_REALIZED (GTK_WIDGET (jade)))
	return;

    assert(attr <= GA_LastFace);

    f = &w->merged_faces[attr];
    if(!f->valid)
	return;
    
    x = w->pixel_left + w->font_width * col;
    y = w->pixel_top + w->font_height * row;

    face_to_gc(w->w_Window, f, !invert);
    gdk_draw_rectangle (win, jade->gc, 1, x, y, len * w->font_width, w->font_height);

    if(!all_spaces)
    {
	face_to_gc(w->w_Window, f, invert);
	gdk_draw_text (win, jade->gc_values.font, jade->gc,
		       x, y + jade->font->ascent, str, len);
    }

    if(f->car & FACEFF_UNDERLINE)
    {
	if(all_spaces)
	    face_to_gc(w->w_Window, f, invert);
	gdk_draw_line (win, jade->gc, 
		       x, y + jade->font->ascent + 1,
		       x + len * w->font_width - 1, y + jade->font->ascent + 1);
    }

    if(f->car & FACEFF_BOXED)
    {
	int i;
	if(all_spaces)
	    face_to_gc(w->w_Window, f, invert);
	for(i = 0; i < len; i++)
	{
	    gdk_draw_rectangle (win, jade->gc, 0, x, y,
				w->font_width - 1, w->font_height - 1);
	    x += w->font_width;
	}
    }
}


/* Map a function over all GtkJade widgets under a container */

struct map_data {
    GtkCallback fun;
    void *data;
};

static void
map_callback (GtkWidget *root, gpointer data)
{
    struct map_data *mdata = data;
    if (GTK_IS_JADE (root))
	mdata->fun (root, mdata->data);
    else if (GTK_IS_CONTAINER (root))
	gtk_container_foreach (GTK_CONTAINER (root), map_callback, data);
}

void
gtk_jade_foreach (GtkContainer *root, GtkCallback fun, gpointer data)
{
    struct map_data mdata;
    mdata.fun = fun;
    mdata.data = data;
    gtk_container_foreach (root, map_callback, &mdata);
}


/* System-dependent jade functions */

void
sys_recolor_cursor(repv face)
{
    /* XXX No way to do this in GDK..? */
}

void
sys_update_dimensions(Lisp_Window *w)
{
    if(w->w_Window && ((w->car & WINFF_SLEEPING) == 0))
    {
	int width, height;
	/* XXX move this to gtk_jade.c */
	gtk_jade_get_size (w->w_Window, &width, &height);
	w->pixel_left = 0;
	w->pixel_top = 0;
	w->pixel_right = width;
	w->pixel_bottom = height;
	w->pixel_width = w->pixel_right - w->pixel_left;
	w->pixel_height = w->pixel_bottom - w->pixel_top;
    }
}

/* pointer is lisp window object */
static gint
window_deleted_callback (GtkWidget *widget, GdkEvent *ev, gpointer data)
{
    Fdelete_window(rep_VAL(data));
    GTK_JADE_CALLBACK_POSTFIX;
    return true;
}

static void
focus_callback (GtkJade *jade, gpointer data)
{
    jade->has_focus = (data != 0);
    if (jade->win != 0 && jade->win->current_view != 0)
	Fredisplay_window (rep_VAL(jade->win), Qnil);
}    

static gint
focus_in_callback (GtkWidget *widget, GdkEvent *ev, gpointer data)
{
    GTK_WIDGET_SET_FLAGS (widget, GTK_HAS_FOCUS);
    gtk_jade_foreach (GTK_CONTAINER (widget),
		      (GtkCallback) focus_callback, (gpointer) 1);
    return true;
}

static gint
focus_out_callback (GtkWidget *widget, GdkEvent *ev, gpointer data)
{
    GTK_WIDGET_UNSET_FLAGS (widget, GTK_HAS_FOCUS);
    gtk_jade_foreach (GTK_CONTAINER(widget),
		      (GtkCallback) focus_callback, (gpointer) 0);
    return true;
}

/* The only thing necessary in W is the font stuff (I think) */
GtkJade *
sys_new_window(Lisp_Window *oldW, Lisp_Window *w, int *dims)
{
    unsigned int x = -1, y = -1, width = 80, height = 24;
    GtkWidget *frame = 0;

    if (batch_mode_p ())
	new_window_no_show = true;

    if(dims[0] >= 0)
	x = dims[0];
    if(dims[1] >= 0)
	y = dims[1];
    if(dims[2] > 0)
	width = dims[2];
    if(dims[3] > 0)
	height = dims[3];

    if (!new_window_no_frame)
    {
	frame = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_container_set_border_width (GTK_CONTAINER (frame), 2);
	gtk_signal_connect (GTK_OBJECT (frame), "delete_event",
			    GTK_SIGNAL_FUNC (window_deleted_callback), w);
	gtk_signal_connect (GTK_OBJECT (frame), "focus_in_event",
			    GTK_SIGNAL_FUNC (focus_in_callback), 0);
	gtk_signal_connect (GTK_OBJECT (frame), "focus_out_event",
			    GTK_SIGNAL_FUNC (focus_out_callback), 0);
	if (x > 0 && y > 0)
	    gtk_widget_set_uposition (frame, x, y);
    }

    w->w_Window = GTK_JADE (gtk_jade_new (w, width, height));
    gtk_jade_add_selection_targets (w->w_Window);
    gtk_jade_set_font (w->w_Window);

    if (frame != 0)
    {
	GdkGeometry hints;
	gtk_container_add (GTK_CONTAINER (frame),
			   gtk_widget_get_toplevel (GTK_WIDGET (w->w_Window)));
	if (width > 0 && height > 0)
	    gtk_window_set_default_size (GTK_WINDOW (frame),
					 width * w->font_width + 4,
					 height * w->font_height + 4);
	hints.base_width = hints.base_height = 0;
	hints.width_inc = w->font_width;
	hints.height_inc = w->font_height;
	hints.min_width = w->font_width * 4;
	hints.min_height = w->font_height * 4;
	gtk_window_set_geometry_hints (GTK_WINDOW (frame),
				       GTK_WIDGET (w->w_Window),
				       &hints,
				       GDK_HINT_RESIZE_INC
				       | GDK_HINT_MIN_SIZE
				       | GDK_HINT_BASE_SIZE);
    }

    if (!new_window_no_show)
	gtk_widget_show_all (GTK_WIDGET (w->w_Window));

    if (frame != 0 && !new_window_no_show)
	gtk_widget_show_all (frame);

    return w->w_Window;
}

void
sys_kill_window(Lisp_Window *w)
{
    GtkWidget *toplevel = gtk_widget_get_toplevel (GTK_WIDGET (w->w_Window));

    gtk_jade_window_lose_selections(w);

    if (toplevel == 0)
	toplevel = GTK_WIDGET (w->w_Window);

    if (toplevel != 0)
	gtk_widget_destroy (toplevel);

    w->w_Window = WINDOW_NIL;
}

bool
sys_sleep_win(Lisp_Window *w)
{
    return true;
}

bool
sys_unsleep_win(Lisp_Window *w)
{
    return true;
}

bool
sys_set_font(Lisp_Window *w)
{
    if (w->w_Window == WINDOW_NIL)
	return true;
    else
	return gtk_jade_set_font (w->w_Window);
	
}

void
sys_unset_font(Lisp_Window *w)
{
}

void
sys_activate_win(Lisp_Window *w)
{
}

void
sys_set_win_pos(Lisp_Window *win, long x, long y, long w, long h)
{
    GtkWidget *toplevel = gtk_widget_get_toplevel (GTK_WIDGET (win->w_Window));
    if (toplevel != 0)
    {
	gtk_widget_set_uposition (toplevel, x, y);
	gtk_widget_set_usize (toplevel, w, h);
    }
}

void
sys_set_win_name(Lisp_Window *win, char *name)
{
    GtkWidget *toplevel = gtk_widget_get_toplevel (GTK_WIDGET (win->w_Window));
    if (toplevel != 0 && GTK_IS_WINDOW (toplevel))
	gtk_window_set_title (GTK_WINDOW (toplevel), name);
}

static void
deleting_callback (GtkJade *jade, gpointer data)
{
    int *counter = data;
    (*counter)++;
}

bool
sys_deleting_window_would_exit (Lisp_Window *win)
{
    if (win->w_Window == 0)
	return false;
    else
    {
	GtkWidget *toplevel = (gtk_widget_get_toplevel
			       (GTK_WIDGET (win->w_Window)));
	int deleted = 0, total = 0;
	gtk_jade_foreach (GTK_CONTAINER (toplevel),
			  (GtkCallback) deleting_callback,
			  (gpointer) &deleted);
	for (win = win_chain; win != 0; win = win->next)
	{
	    if (win->w_Window != WINDOW_NIL)
		total++;
	}
	return deleted == total;
    }
}


/* Some Lisp functions */

DEFUN ("gtk-jade-p", Fgtk_jade_p, Sgtk_jade_p, (repv arg), rep_Subr1) /*
::doc:gtk-jade-p::
gtk-jade-p ARG

Returns t if ARG is a GtkJade widget.
::end:: */
{
    GtkObject *obj = (*gtk_jade_get_gtkobj) (arg);
    return (obj != 0 && GTK_IS_JADE (obj)) ? Qt : Qnil;
}

DEFUN ("gtk-jade-new", Fgtk_jade_new, Sgtk_jade_new,
       (repv width, repv height), rep_Subr2) /*
::doc:gtk-jade-new::
gtk-jade-new [WIDTH] [HEIGHT]

Create and return a newly-allocated, un-realised, GTK widget representing
an editor window. This is guaranteed to be the gtk-jade widget, however
it may have already been added to a container; so use gtk-widget-get-toplevel
if adding to another container.
::end:: */
{
    repv win;

    new_window_no_frame = true;
    new_window_no_show = true;
    win = Fmake_window (rep_LIST_1 (Fcons (Qposition, Fcons (width, height))));
    new_window_no_frame = false;
    new_window_no_show = false;

    if (win && WINDOWP (win))
    {
	GtkJade *jade = VWINDOW (win)->w_Window;
	return (*gtk_jade_wrap_gtkobj) (GTK_OBJECT (jade));
    }
    else
	return win;
}

DEFUN ("gtk-jade-window", Fgtk_jade_window, Sgtk_jade_window,
       (repv widget), rep_Subr1) /*
::doc:gtk-jade-window::
gtk-jade-window WIDGET

Return the window associated with the GTK widget WIDGET.
::end:: */
{
    GtkObject *obj = (*gtk_jade_get_gtkobj) (widget);
    if (obj == NULL || !GTK_IS_JADE (obj))
	return rep_signal_arg_error (widget, 1);
    else
	return rep_VAL (GTK_JADE (obj)->win);
}

DEFUN ("gtk-jade-window-widget", Fgtk_jade_window_widget,
       Sgtk_jade_window_widget, (repv win), rep_Subr1) /*
::doc:gtk-jade-window-widget::
gtk-jade-window-widget [WINDOW]

Return the GTK widget associated with window WINDOW.
::end:: */
{
    if (!WINDOWP (win))
	win = rep_VAL (curr_win);
    return (*gtk_jade_wrap_gtkobj) (GTK_OBJECT (VWINDOW(win)->w_Window));
}

DEFUN("flush-output", Fflush_output, Sflush_output, (void), rep_Subr0) /*
::doc:flush-output::
flush-output

Forces any cached window output to be drawn. This is usually unnecessary.
::end:: */
{
    gdk_flush ();
    return Qt;
}

DEFUN ("make-window-on-display", Fmake_window_on_display,
       Smake_window_on_display, (repv display), rep_Subr1)
{
    char *current_dpy = gdk_get_display ();
    rep_DECLARE1 (display, rep_STRINGP);
    /* XXX this should be a bit sloppier, i.e. append `.0' if not
       XXX present, be sensible about host names, etc... */
    if (strcmp (rep_STR (display), current_dpy) == 0)
    {
	return Fmake_window (Qnil);
    }
    else
	return Fsignal (Qerror, rep_list_2 (rep_string_copy
					    ("can't connect to display"),
					    display));
}

DEFUN("gtk-cursor-shape", Fgtk_cursor_shape, Sgtk_cursor_shape, (repv arg), rep_Subr1) /*
::doc:gtk-cursor-shape::
gtk-cursor-shape [NEW-VALUE]

An integer identifying the cursor to use for editor windows. See
<gdk/gdkcursors.h> for the list of available cursors.
::end:: */
{
    if (rep_INTP(arg) && rep_INT(arg) != cursor_shape)
    {
	Lisp_Window *w = win_chain;
	cursor_shape = rep_INT(arg);
	gdk_cursor_destroy (window_cursor);
	window_cursor = gdk_cursor_new (cursor_shape);
	while (w != 0)
	{
	    gdk_window_set_cursor (w->w_Window->widget.window, window_cursor);
	    w = w->next;
	}
	return arg;
    }
    else
	return rep_MAKE_INT(cursor_shape);
}

DEFUN("gtk-last-timestamp", Fgtk_last_timestamp, 
      Sgtk_last_timestamp, (void), rep_Subr0)
{
    return rep_make_long_int(gtk_jade_last_event_time);
}


/* Asyncronous event handling. X11 specific. */

#if 0

#ifdef HAVE_X11
static Bool
async_event_pred (Display *dpy, XEvent *ev, XPointer arg)
{
    Lisp_Window *w;
    for (w = win_chain; w != 0; w = w->next)
    {
	if (w->w_Window && GTK_WIDGET_REALIZED (GTK_WIDGET (w->w_Window)))
	{
	    GdkWindowPrivate *pri = (GdkWindowPrivate *)w->w_Window->widget.window;
	    if (pri->xwindow == ev->xany.window)
		break;
	}
    }
    if (w != 0)
    {
	*(Lisp_Window **)arg = w;
	switch (ev->xany.type)
	{
	case Expose:
	    return True;

	case KeyPress:
	    if (ev->xkey.state == ControlMask)
	    {
		KeySym symbol = XKeycodeToKeysym (dpy, ev->xkey.keycode, 0);
		if (symbol == XK_g)
		{
		    rep_throw_value = rep_int_cell;
		    return True;
		}
	    }
	    break;
	}
    }
    return False;
}
#endif

static void
gtk_jade_handle_async_input (void)
{
    bool need_redisplay = false;
    if (!redisplay_lock)
    {
#ifdef HAVE_X11
	Lisp_Window *ev_win;
	XEvent xev;
	if (XCheckIfEvent (gdk_display, &xev,
			   &async_event_pred, (XPointer)&ev_win))
	{
	    switch (xev.type)
	    {
		int x, y, width, height;
		Lisp_Window *w;

	    case Expose:
		x = (xev.xexpose.x - ev_win->pixel_left) / ev_win->font_width;
		y = (xev.xexpose.y - ev_win->pixel_top) / ev_win->font_height;
		/* Why +2? It seems to be necessary.. */
		width = (xev.xexpose.width / ev_win->font_width) + 2;
		height = (xev.xexpose.height / ev_win->font_height) + 2;

		/* We're in the middle of doing something else,
		   don't let the expose cause the current display
		   state to be redrawn; preserve the window contents
		   at the last redisplay */
		for(w = win_chain; w != 0; w = w->next)
		{
		    if(!(w->car & WINFF_PRESERVING))
		    {
			copy_glyph_buf(w->new_content, w->content);
			w->car |= WINFF_PRESERVING;
		    }
		}

		garbage_glyphs(ev_win, x, y, width, height);
		if (xev.xexpose.count == 0)
		    need_redisplay = true;
	    }
	}
#endif /* HAVE_X11 */
    }
    if (need_redisplay)
	Fredisplay (Qnil);
}

#endif /* 0 */


/* Initialisation */

void
sys_windows_init(void)
{
    window_cursor = gdk_cursor_new (cursor_shape);
    rep_ADD_SUBR (Sgtk_jade_p);
    rep_ADD_SUBR (Sgtk_jade_new);
    rep_ADD_SUBR (Sgtk_jade_window);
    rep_ADD_SUBR (Sgtk_jade_window_widget);
    rep_ADD_SUBR (Sflush_output);
    rep_ADD_SUBR (Smake_window_on_display);
    rep_ADD_SUBR (Sgtk_cursor_shape);
    rep_ADD_SUBR (Sgtk_last_timestamp);

    rep_INTERN_SPECIAL (gtk_jade_new_hook);
    rep_INTERN (dnd_drop_uri_list);
#if 0
    /* XXX this doesn't work. I sometimes get errors:
       XXX	Xlib: unexpected async reply (sequence 0xc60)!
       XXX and the only thing to do is `kill -9'. If I run with
       XXX `--sync' it seems to hang within gdk_copy_area ().
       XXX I'm wondering if there's a problem with GraphicsExpose
       XXX events..? */
    rep_test_int_fun = gtk_jade_handle_async_input;
#endif
    gtk_misc_init();
}
