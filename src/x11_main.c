/* x11_main.c -- Main code for X11 window-system
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
# include <fcntl.h>
#endif

_PR void sys_usage(void);
_PR int sys_init(int, char **);
static void x11_handle_input(int fd);

/* The window in which the current event occurred. */
_PR WIN *x11_current_event_win;
WIN *x11_current_event_win;

/* The mouse position of the current event, relative to the origin of
   the window that the event occurred in, measured in glyphs. */
_PR long x11_current_mouse_x, x11_current_mouse_y;
long x11_current_mouse_x, x11_current_mouse_y;

/* The last event received which had a timestamp, was at this time. */
_PR Time x11_last_event_time;
Time x11_last_event_time;

/* Out Display structure. */
_PR Display *x11_display;
Display *x11_display;

/* The screen on the display. */
_PR int x11_screen;
int x11_screen;

/* The colourmap */
_PR Colormap x11_colour_map;
Colormap x11_colour_map;

/* Allocated colours. */
_PR u_long x11_fore_pixel, x11_back_pixel, x11_high_pixel;
u_long x11_fore_pixel, x11_back_pixel, x11_high_pixel;

/* Atoms we use. */
_PR Atom x11_wm_del_win, x11_jade_sel;
Atom x11_wm_del_win, x11_jade_sel;

/* Command line arguments. */
_PR char **x11_argv;
_PR int x11_argc;
char **x11_argv;
int x11_argc;

/* The `I' type mouse pointer. */
_PR Cursor x11_text_cursor;
Cursor x11_text_cursor;

/* Command line options, and their default values. */
static char *display_name = NULL;
static char *bg_str = "white";
static char *fg_str = "black";
static char *hl_str = "skyblue3";
static char *geom_str = "80x24";
static char *prog_name;

/* Default font name. */
DEFSTRING(def_font_str_data, DEFAULT_FONT);
_PR VALUE def_font_str;
VALUE def_font_str;

/* Scan the resource db for the entries we're interested in. */
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

/* Print the X11 options. */
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

/* Scan the command line for the X11 options. Updating ARGC-P and ARGV-P to
   point to the following options. */
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

/* After parsing the command line and the resource database, use the
   information. */
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

/* Called from main(). */
int
sys_init(int argc, char **argv)
{
    int i;
    prog_name = file_part(argv[0]);
    def_font_str = VAL(&def_font_str_data);
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
	register_input_fd(ConnectionNumber(x11_display), x11_handle_input);
#ifdef HAVE_UNIX
	/* close-on-exec = TRUE	 */
	fcntl(ConnectionNumber(x11_display), F_SETFD, 1);
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
    return 5;
}


/* X11 event handling. */

static void
x11_handle_input(int fd)
{
    /* Read all events in the input queue. */
    while(throw_value == LISP_NULL
	  && XEventsQueued(x11_display, QueuedAfterReading) > 0)
    {
	XEvent xev;
	WIN *oldwin = curr_win, *ev_win;

	XNextEvent(x11_display, &xev);

	ev_win = x11_find_window(xev.xany.window);
	if(ev_win != NULL)
	{
	    switch(xev.type)
	    {
		u_long code, mods;

	    case MappingNotify:
		XRefreshKeyboardMapping(&xev.xmapping);
		break;

	    case Expose:
	    case GraphicsExpose:
		if(ev_win->w_Flags & WINFF_SLEEPING)
		{
		    /* Guess that the wm uniconified us? */
		    ev_win->w_Flags &= ~WINFF_SLEEPING;
		}
		if(!(ev_win->w_Flags & WINFF_FORCE_REFRESH))
		{
		    int x = (xev.xexpose.x - ev_win->w_LeftPix) / ev_win->w_FontX;
		    int y = (xev.xexpose.y - ev_win->w_TopPix) / ev_win->w_FontY;
		    /* Why +2? It seems to be necessary.. */
		    int width = (xev.xexpose.width / ev_win->w_FontX) + 2;
		    int height = (xev.xexpose.height / ev_win->w_FontY) + 2;
		    garbage_glyphs(ev_win, x, y, width, height);
		}
		break;

	    case ConfigureNotify:
		if((ev_win->w_WindowSys.ws_Width != xev.xconfigure.width)
		   || (ev_win->w_WindowSys.ws_Height != xev.xconfigure.height))
		{
		    if((ev_win->w_WindowSys.ws_Height != 0)
		       && (ev_win->w_WindowSys.ws_Height < xev.xconfigure.height))
			ev_win->w_WindowSys.ws_Width = xev.xconfigure.width;
		    ev_win->w_WindowSys.ws_Height = xev.xconfigure.height;
		    x11_update_dimensions(ev_win, xev.xconfigure.width,
					  xev.xconfigure.height);
		    update_window_dimensions(ev_win);
		}
		break;

	    case ClientMessage:
		if((xev.xclient.format == 32)
		   && (xev.xclient.data.l[0] == x11_wm_del_win))
		{
		    curr_win = ev_win;
		    if(ev_win != oldwin)
			curr_vw = curr_win->w_CurrVW;
		    cmd_call_hook(sym_window_closed_hook, sym_nil, sym_nil);
		}
		break;

	    case FocusIn:
		if(ev_win != oldwin)
		{
		    curr_win = ev_win;
		    curr_vw = curr_win->w_CurrVW;
		}
		break;

	    case MotionNotify:
	    {
		Window tmpw;
		int tmp;
		int x, y;
		
		/* Swallow any pending motion events as well. */
		while(XCheckMaskEvent(x11_display, ButtonMotionMask, &xev))
		    ;
		x11_last_event_time = xev.xmotion.time;

		/* It seems that further MotionNotify events are suspended
		   until the pointer's position has been queried. I should
		   check the Xlib manuals about this. */
		if(XQueryPointer(x11_display, ev_win->w_Window,
				 &tmpw, &tmpw, &tmp, &tmp,
				 &x, &y, &tmp))
		{
		    x11_current_mouse_x = x;
		    x11_current_mouse_y = y;
		}
		goto do_command;
	    }

	    case ButtonPress:
	    case ButtonRelease:
		x11_last_event_time = xev.xbutton.time;
		x11_current_mouse_x = xev.xbutton.x;
		x11_current_mouse_y = xev.xbutton.y;
		goto do_command;

	    case KeyPress:
		x11_last_event_time = xev.xkey.time;
		x11_current_mouse_x = xev.xkey.x;
		x11_current_mouse_y = xev.xkey.y;
		/* FALL THROUGH */

	    do_command:
		x11_current_event_win = ev_win;
		x11_current_mouse_x
		    = ((x11_current_mouse_x - ev_win->w_LeftPix)
		       / ev_win->w_FontX);
		x11_current_mouse_y
		    = ((x11_current_mouse_y - ev_win->w_TopPix)
		       / ev_win->w_FontY);
		code = mods = 0;
		translate_event(&code, &mods, &xev);
		if(mods & EV_TYPE_MASK)
		{
		    curr_win = ev_win;
		    if(oldwin != ev_win)
			curr_vw = curr_win->w_CurrVW;
		    reset_message(ev_win);
		    eval_input_event(&xev, code, mods);
		}
		x11_current_event_win = NULL;
		break;

	    case SelectionRequest:
		x11_last_event_time = xev.xselectionrequest.time;
		x11_convert_selection(&xev.xselectionrequest);
		break;

	    case SelectionClear:
		x11_last_event_time = xev.xselectionclear.time;
		x11_lose_selection(&xev.xselectionclear);
		break;
	    }
	    undo_end_of_command();
	}
    }
}
