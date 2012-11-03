/* gtk_main.c -- Main code for GTK window-system
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
#include <gtk/gtkmain.h>
#include <string.h>
#include <assert.h>

/* For the resource database functions, and geometry parser */
#ifdef HAVE_X11
# include <X11/Xlib.h>
# include <gdk/gdkprivate.h>
#endif

#ifdef HAVE_UNIX
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
#endif

/* Command line options, and their default values. */
static char *geom_str = "80x24";
static int opt_sync = 0;

static GdkColormap *color_map;

u_long gtk_meta_mod;

/* Default font name. */
DEFSTRING(def_font_str_data, DEFAULT_FONT);

#if rep_INTERFACE >= 10
DEFSYM (gtk_feature, "gui.gtk.gtk");
#else
DEFSYM (gtk_feature, "gtk");
#endif

/* Dynamically-bound interface to rep-gtk.c */
repv (*gtk_jade_wrap_gtkobj)(GtkObject *object);
GtkObject *(*gtk_jade_get_gtkobj)(repv obj);
void (*gtk_jade_callback_postfix)(void);


/* Resource/option management */

#ifdef HAVE_X11
/* Scan the resource db for the entries we're interested in. */
static void
get_resources(char *prog_name)
{
    char *s;
    if((s = XGetDefault(gdk_display, prog_name, "geometry"))
       || (s = XGetDefault(gdk_display, "Jade", "Geometry")))
	geom_str = s;
    if((s = XGetDefault(gdk_display, prog_name, "foreground"))
       || (s = XGetDefault(gdk_display, "Jade", "Foreground")))
	default_fg_color = s;
    if((s = XGetDefault(gdk_display, prog_name, "background"))
       || (s = XGetDefault(gdk_display, "Jade", "Background")))
	default_bg_color = s;
    if((s = XGetDefault(gdk_display, prog_name, "block"))
       || (s = XGetDefault(gdk_display, "Jade", "Block")))
	default_block_color = s;
    if((s = XGetDefault(gdk_display, prog_name, "highlight"))
       || (s = XGetDefault(gdk_display, "Jade", "Highlight")))
	default_hl_color = s;
    if((s = XGetDefault(gdk_display, prog_name, "modeline"))
       || (s = XGetDefault(gdk_display, "Jade", "Modeline")))
	default_ml_color = s;
    if((s = XGetDefault(gdk_display, prog_name, "font"))
       || (s = XGetDefault(gdk_display, "Jade", "Font")))
	def_font_str = rep_string_dup(s);
}
#endif

/* Scan the command line for options. */
static void
get_options(void)
{
    repv opt;
    if (rep_get_option("--sync", 0))
	opt_sync = 1;
    if (rep_get_option("--geometry", &opt))
	geom_str = strdup (rep_STR(opt));
    if (rep_get_option("--fg", &opt))
	default_fg_color = strdup (rep_STR(opt));
    if (rep_get_option("--bg", &opt))
	default_bg_color = strdup (rep_STR(opt));
    if (rep_get_option("--bl", &opt))
	default_block_color = strdup (rep_STR(opt));
    if (rep_get_option("--hl", &opt))
	default_hl_color = strdup (rep_STR(opt));
    if (rep_get_option("--ml", &opt))
	default_ml_color = strdup (rep_STR(opt));
    if (rep_get_option("--font", &opt))
	def_font_str = opt;
}

/* After parsing the command line and the resource database, use the
   information. */
static bool
use_options(void)
{
#ifdef HAVE_X11
    int x, y, w, h;
    int gflgs = XParseGeometry(geom_str, &x, &y, &w, &h);

    if(!(gflgs & WidthValue))
	w = -1;
    if(!(gflgs & HeightValue))
	h = -1;
    /* TODO: need to use -ve values properly */
    if(!(gflgs & XValue))
	x = -1;
    if(!(gflgs & YValue))
	y = -1;

    set_default_geometry (x, y, w, h);
#endif

#ifdef HAVE_X11
    if (opt_sync)
	XSynchronize (gdk_display, True);
#endif

    return TRUE;
}

static void
make_argv (repv list, int *argc, char ***argv)
{
    int c = rep_INT (Flength (list)), i;
    char **v;

    v = (char **)rep_alloc ((c+1) * sizeof(char**));
    for (i = 0; i < c; i++, list = rep_CDR (list))
    {
	if (!rep_STRINGP (rep_CAR (list)))
	{
	    rep_free ((char *)v);
	    return;
	}
	v[i] = strdup (rep_STR (rep_CAR (list)));
    }
    v[c] = NULL;
  
    *argv = v;
    *argc = c;
}

void
sys_beep (WIN *w)
{
    gdk_beep ();
}

/* Called from main(). */
bool
sys_init(char *program_name)
{
    int argc;
    char **argv;
    repv head, *last;

    gtk_set_locale ();

#ifdef HAVE_UNIX
    if (!batch_mode_p ())
	setpgid (0, 0);
#endif

    make_argv (Fcons (Fsymbol_value (Qprogram_name, Qt),
		      Fsymbol_value (Qcommand_line_args, Qt)), &argc, &argv);

    /* We need to initialise GTK now. The rep-gtk library will
       not reinitialise it.. */
    gtk_init (&argc, &argv);

    argc--; argv++;
    head = Qnil;
    last = &head;
    while(argc > 0)
    {
	*last = Fcons(rep_string_dup(*argv), Qnil);
	last = &rep_CDR(*last);
	argc--;
	argv++;
    }
    Fset (Qcommand_line_args, head);

    def_font_str = rep_VAL (&def_font_str_data);
#ifdef HAVE_X11
    get_resources (program_name);
#endif
    get_options ();
    use_options ();

    color_map = gdk_colormap_get_system ();
    gtk_meta_mod = gtk_find_meta ();

    /* Loading the gtk rep library will replace the usual
       event loop with one that works with GTK. */
    rep_INTERN(gtk_feature);
#if rep_INTERFACE >= 9
    Frequire (Qgtk_feature);
#else
    Fload (rep_string_dup ("gtk"), Qnil, Qnil, Qnil, Qnil);
#endif
    if (rep_throw_value == 0)
    {
	/* Find the gtkobj<->lispobj converters */
	gtk_jade_wrap_gtkobj = rep_find_dl_symbol (Qgtk_feature, "sgtk_wrap_gtkobj");
	gtk_jade_get_gtkobj = rep_find_dl_symbol (Qgtk_feature, "sgtk_get_gtkobj");
	gtk_jade_callback_postfix = rep_find_dl_symbol (Qgtk_feature, "sgtk_callback_postfix");
	assert (gtk_jade_wrap_gtkobj != 0
		&& gtk_jade_get_gtkobj != 0
		&& gtk_jade_callback_postfix != 0);
	return TRUE;
    }
    else
	return FALSE;
}

void
sys_kill (void)
{
    gdk_colormap_unref (color_map);
    color_map = 0;
}

/* Print the options. */
void
sys_usage(void)
{
    fputs("    --geometry WINDOW-GEOMETRY\n"
	  "    --fg FOREGROUND-COLOUR\n"
	  "    --bg BACKGROUND-COLOUR\n"
	  "    --hl HIGHLIGHT-COLOUR\n"
	  "    --ml MODELINE-COLOUR\n"
	  "    --bl BLOCK-COLOUR\n"
	  "    --font FONT-NAME\n"
	  "    --sync\n"
	  "    FILE         Load FILE into an editor buffer\n", stderr);
}

repv
sys_get_mouse_pos(WIN *w)
{
    int x, y;
    /* XXX track mouse pointer position in gtk_jade.c.. */
    gtk_widget_get_pointer (GTK_WIDGET (w->w_Window), &x, &y);
    return make_pos((x - w->pixel_left) / w->font_width,
		    (y - w->pixel_top) / w->font_height);
}


/* Color handling. */

DEFSTRING(no_parse_color, "Can't parse color");
DEFSTRING(no_alloc_color, "Can't allocate color");

repv
sys_make_color(Lisp_Color *c)
{
    if (gdk_color_parse (rep_STR (c->name), &c->color))
    {
	if (gdk_colormap_alloc_color (color_map, &c->color, 0, 1))
	    return rep_VAL (c);
	else
	    return Fsignal(Qerror, rep_list_2(rep_VAL(&no_alloc_color),
					      c->name));
    }
    else
	return Fsignal(Qerror, rep_list_2(rep_VAL(&no_parse_color), c->name));
}

void
sys_free_color(Lisp_Color *c)
{
    gdk_colormap_free_colors (color_map, &c->color, 1);
}
