/* gtk_defs.h -- Declarations for GTK
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef JADE_GTK_DEFS_H
#define JADE_GTK_DEFS_H

#include <gdk/gdk.h>
#include <gtk/gtkwidget.h>
#include <gtk/gtkcontainer.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkmain.h>

/* standard font */
#define DEFAULT_FONT "fixed"

/* After GTK invokes one of our callback functions this macro
   must be executed. */
#define GTK_JADE_CALLBACK_POSTFIX ((*gtk_jade_callback_postfix) ())


/* Definitions for the GTK jade widget.

   Conceptually, this is a bit of a mess. The WIN and GtkJade structures
   have to be inextricably linked, so that they're both ``above'' each
   other depending on the point of view. */

#define GTK_JADE(obj) \
    GTK_CHECK_CAST (obj, gtk_jade_get_type (), GtkJade)

#define GTK_JADE_CLASS(klass) \
    GTK_CHECK_CLASS_CAST (klasss, gtk_jade_get_type (), GtkJadeClass)

#define GTK_IS_JADE(obj) \
    GTK_CHECK_TYPE (obj, gtk_jade_get_type ())

typedef struct _GtkJade GtkJade;
typedef struct _GtkJadeClass GtkJadeClass;

struct _GtkJade
{
    GtkWidget widget;
    struct _WIN *win;

    GdkFont *font;
    GdkFont *bold_font;	/* or null */
    GdkGC *gc;
    GdkGCValues gc_values;
    GdkGCValuesMask gc_values_mask;

    int width, height;
    int has_focus;
};

struct _GtkJadeClass
{
    GtkWidgetClass parent_class;
};


/* Definitions for Lisp WIN object */

#define W_WindowSys		GtkJade*
#define w_Window		w_WindowSys
#define WINDOW_NIL		(0)

#define WINDOW_META(w)		(gtk_meta_mod)
#define WINDOW_HAS_FOCUS(w)	((w)->w_Window->has_focus)

/* The batch-mode stuff here is an ugly hack. The problem is that
   in batch-mode we never show the window, so it will be collected,
   leaving us without a curr_win at all---not a happy situation, so.. */
#define WINDOW_NON_COLLECTABLE(w) 				\
    (rep_SYM (Qbatch_mode)->value != Qnil 			\
     || ((w)->w_Window && GTK_WIDGET_REALIZED ((w)->w_Window)))

#define SYS_COLOR_TYPE		GdkColor

/* Macros for drawing operations. These are used in redisplay.c for
   system-independent rendering. */

#define SYS_DRAW_GLYPHS sys_draw_glyphs

/* Copy WxH glyphs from (X1,Y1) to (X2,Y2)  */
#define COPY_GLYPHS(win, x1, y1, w, h, x2, y2)			\
    do {							\
	int x1pix = (win)->w_LeftPix + (win)->w_FontX * (x1);	\
	int y1pix = (win)->w_TopPix + (win)->w_FontY * (y1);	\
	int x2pix = (win)->w_LeftPix + (win)->w_FontX * (x2);	\
	int y2pix = (win)->w_TopPix + (win)->w_FontY * (y2);	\
	int width = (w) * (win)->w_FontX;			\
	int height = (h) * (win)->w_FontY;			\
	gdk_window_copy_area((win)->w_Window->widget.window,	\
			     (win)->w_Window->gc,		\
			     x2pix, y2pix,			\
			     (win)->w_Window->widget.window,	\
			     x1pix, y1pix, width, height);	\
    } while(0)

#endif /* JADE_GTK_DEFS_H */