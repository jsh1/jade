/* x11_display.c -- Initialisation for X11 window-system
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

#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#ifdef HAVE_UNIX
# include <sys/types.h>
# include <sys/time.h>
# include <signal.h>
# include <fcntl.h>
#else
  you lose
#endif

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR void sys_usage(void);
_PR int sys_init(int, char **);

#ifdef HAVE_UNIX
_PR fd_set x11_fd_read_set;
fd_set x11_fd_read_set;

_PR void (*x11_fd_read_action[])(int);
void (*x11_fd_read_action[FD_SETSIZE])(int);
#endif

_PR Display *x11_display;
_PR int x11_screen;
_PR Colormap x11_colour_map;
_PR u_long x11_fore_pixel, x11_back_pixel, x11_high_pixel;
_PR Atom x11_wm_del_win, x11_jade_sel;
_PR char **x11_argv;
_PR int x11_argc;
_PR Cursor x11_text_cursor;

Display *x11_display;
int x11_screen;
Colormap x11_colour_map;
u_long x11_fore_pixel, x11_back_pixel, x11_high_pixel;
Atom x11_wm_del_win, x11_jade_sel;
char **x11_argv;
int x11_argc;
Cursor x11_text_cursor;

static char *display_name = NULL;
static char *bg_str = "white";
static char *fg_str = "black";
static char *hl_str = "skyblue3";
static char *geom_str = "80x24";
static char *prog_name;

_PR VALUE def_font_str;
VALUE def_font_str = MKSTR(DEFAULT_FONT);

static int
get_resources(void)
{
    char *s;
    if((s = XGetDefault(x11_display, prog_name, "geometry"))
       || (s = XGetDefault(x11_display, "Jade", "geometry")))
	geom_str = s;
    if((s = XGetDefault(x11_display, prog_name, "foreground"))
       || (s = XGetDefault(x11_display, "Jade", "foreground")))
	fg_str = s;
    if((s = XGetDefault(x11_display, prog_name, "background"))
       || (s = XGetDefault(x11_display, "Jade", "background")))
	bg_str = s;
    if((s = XGetDefault(x11_display, prog_name, "highlight"))
       || (s = XGetDefault(x11_display, "Jade", "highlight")))
	hl_str = s;
    if((s = XGetDefault(x11_display, prog_name, "font"))
       || (s = XGetDefault(x11_display, "Jade", "font")))
	def_font_str = string_dup(s);
    return(TRUE);
}

void
sys_usage(void)
{
    fputs(
	"where SYSTEM-OPTIONS are,\n"
	"    -display DISPLAY-NAME\n"
	"    -name NAME\n"
	"    -geometry WINDOW-GEOMETRY\n"
	"    -fg FOREGROUND-COLOUR\n"
	"    -bg BACKGROUND-COLOUR\n"
	"    -hl HIGHLIGHT-COLOUR\n"
	"    -font FONT-NAME\n"
	"    -sync\n"
	, stderr);
}

static int
get_options(int *argc_p, char ***argv_p)
{
    int argc = *argc_p;
    char **argv = *argv_p;
    argc--;
    argv++;
    while((argc >= 1) && (**argv == '-'))
    {
	if(!strcmp("-sync", *argv))
	    XSynchronize(x11_display, True);
	else if(argc >= 2)
	{
	    if(!strcmp("-display", *argv))
		;
	    else if(!strcmp("-name", *argv))
		;
	    else if(!strcmp("-geometry", *argv))
		geom_str = argv[1];
	    else if(!strcmp("-fg", *argv))
		fg_str = argv[1];
	    else if(!strcmp("-bg", *argv))
		bg_str = argv[1];
	    else if(!strcmp("-hl", *argv))
		hl_str = argv[1];
	    else if(!strcmp("-font", *argv))
		def_font_str = string_dup(argv[1]);
	    else
		break;
	    argc--; argv++;
	}
	else
	    break;
	argc--; argv++;
    }
    *argc_p = argc;
    *argv_p = argv;
    return(TRUE);
}

static int
use_options(void)
{
    int x, y, w, h;
    int gflgs = XParseGeometry(geom_str, &x, &y, &w, &h);
    XColor tmpc;
    if(gflgs & WidthValue)
	def_dims[2] = w;
    else
	def_dims[2] = -1;
    if(gflgs & HeightValue)
	def_dims[3] = h;
    else
	def_dims[3] = -1;
    /*
     * need to use -ve values properly
     */
    if(gflgs & XValue)
	def_dims[0] = x;
    else
	def_dims[0] = -1;
    if(gflgs & YValue)
	def_dims[1] = y;
    else
	def_dims[1] = -1;

    if(!XParseColor(x11_display, x11_colour_map, fg_str, &tmpc))
	fprintf(stderr, "error: invalid fg colour\n");
    else if(!XAllocColor(x11_display, x11_colour_map, &tmpc))
	fprintf(stderr, "error: can't allocate fg colour\n");
    else
	x11_fore_pixel = tmpc.pixel;

    if(!XParseColor(x11_display, x11_colour_map, bg_str, &tmpc))
	fprintf(stderr, "error: invalid bg colour\n");
    else if(!XAllocColor(x11_display, x11_colour_map, &tmpc))
	fprintf(stderr, "error: can't allocate bg colour\n");
    else
	x11_back_pixel = tmpc.pixel;

    if(!XParseColor(x11_display, x11_colour_map, hl_str, &tmpc))
	fprintf(stderr, "error: invalid hl colour\n");
    else if(!XAllocColor(x11_display, x11_colour_map, &tmpc))
	fprintf(stderr, "error: can't allocate hl colour\n");
    else
	x11_high_pixel = tmpc.pixel;

    return(TRUE);
}

int
sys_init(int argc, char **argv)
{
    int i;
    prog_name = file_part(argv[0]);
    for(i = 1; i < argc; i++)
    {
	/* These options have to be found *before* resources are looked
	   for.	 */
	if(!strcmp("-display", argv[i]) && (i+1 < argc))
	    display_name = argv[++i];
	else if(!strcmp("-name", argv[i]) && (i+1 < argc))
	    prog_name = argv[++i];
    }
    x11_display = XOpenDisplay(display_name);
    if(x11_display)
    {
#ifdef HAVE_UNIX
	FD_ZERO(&x11_fd_read_set);
	/* close-on-exec = TRUE	 */
	fcntl(ConnectionNumber(x11_display), F_SETFD, 1);
	FD_SET(ConnectionNumber(x11_display), &x11_fd_read_set);
#endif /* HAVE_UNIX */

	x11_screen = DefaultScreen(x11_display);
	x11_colour_map = DefaultColormap(x11_display, x11_screen);
	if(get_resources() && get_options(&argc, &argv) && use_options())
	{
	    XColor fore, back;
	    int rc;
	    x11_wm_del_win = XInternAtom(x11_display, "WM_DELETE_WINDOW",
					 False);
	    x11_jade_sel = XInternAtom(x11_display, "JADE_SELECTION", False);
	    x11_text_cursor = XCreateFontCursor(x11_display, XC_xterm);
	    fore.pixel = x11_fore_pixel;
	    back.pixel = x11_back_pixel;
	    XQueryColor(x11_display, x11_colour_map, &fore);
	    XQueryColor(x11_display, x11_colour_map, &back);
	    XRecolorCursor(x11_display, x11_text_cursor, &fore, &back);

	    /* Call the main editor setup and event loop.  */
	    rc = inner_main(argc, argv);

	    if(x11_text_cursor)
		XFreeCursor(x11_display, x11_text_cursor);
	    XCloseDisplay(x11_display);

	    return(rc);
	}
    }
    else
    {
	fprintf(stderr, "jade: Can't open display: %s\n",
		display_name ? display_name : "");
    }
    return(5);
}
