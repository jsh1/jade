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
#include <lib/jade_protos.h>

#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <string.h>

#ifdef HAVE_UNIX
# include <fcntl.h>
#endif

_PR struct x11_display *x11_open_display(char *display_name);
_PR void x11_close_display(struct x11_display *xdisplay);
_PR void x11_close_all_displays(void);
static void x11_handle_sync_input(int fd);
_PR void x11_handle_async_input(void);

_PR void sys_usage(void);
_PR int sys_init(int, char **);

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

/* List of in-use displays */
_PR struct x11_display *x11_display_list;
struct x11_display *x11_display_list;

/* Command line arguments. */
_PR char **x11_argv;
_PR int x11_argc;
char **x11_argv;
int x11_argc;

/* Command line options, and their default values. */
static char *display_name = NULL;
static char *bg_str = "white";
static char *fg_str = "black";
static char *hl_str = "skyblue3";
static char *geom_str = "80x24";
static int x11_opt_sync = 0;
static char *prog_name;

_PR bool x11_opt_reverse_video;
bool x11_opt_reverse_video = FALSE;

/* Default font name. */
DEFSTRING(def_font_str_data, DEFAULT_FONT);
_PR VALUE def_font_str;
VALUE def_font_str;

_PR const int x11_attr_map[], x11_rattr_map[];
const int x11_attr_map[GA_MAX] = { P_TEXT, P_TEXT_RV, P_BLOCK, P_BLOCK_RV,
				   P_TEXT, P_TEXT_RV, P_BLOCK, P_BLOCK_RV };
const int x11_rattr_map[GA_MAX] = { P_TEXT_RV, P_TEXT, P_BLOCK_RV, P_BLOCK,
				    P_TEXT_RV, P_TEXT, P_BLOCK_RV, P_BLOCK };


/* Resource/option management */

static char **out_argv;
static int out_argc;

/* Called from main(). */
int
sys_init(int argc, char **argv)
{
    int i;
    struct x11_display *xdisplay;

    prog_name = argv[0];
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
    x11_argc = out_argc = argc;
    x11_argv = out_argv = argv;

    xdisplay = x11_open_display(display_name);
    if(xdisplay != 0)
    {
	/* Call the main editor setup and event loop.  */
	int rc = inner_main(out_argc, out_argv);

	x11_close_all_displays();
	return rc;
    }
    else
    {
	fprintf(stderr, "jade: Can't open display: %s\n",
		display_name ? display_name : "");
    }
    return 5;
}

/* Scan the resource db for the entries we're interested in. */
static void
get_resources(struct x11_display *xdisplay)
{
    char *s;
    if((s = XGetDefault(xdisplay->display, prog_name, "geometry"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Geometry")))
	geom_str = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "foreground"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Foreground")))
	fg_str = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "background"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Background")))
	bg_str = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "highlight"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Highlight")))
	hl_str = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "font"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Font")))
	def_font_str = string_dup(s);
    if((s = XGetDefault(xdisplay->display, prog_name, "reverseVideo"))
       || (s = XGetDefault(xdisplay->display, "Jade", "ReverseVideo")))
	x11_opt_reverse_video = (strcasecmp(s, "true") == 0);
}

/* Print the X11 options. */
void
sys_usage(void)
{
    fputs("where SYSTEM-OPTIONS are,\n"
	  "    -display DISPLAY-NAME\n"
	  "    -name NAME\n"
	  "    -geometry WINDOW-GEOMETRY\n"
	  "    -fg FOREGROUND-COLOUR\n"
	  "    -bg BACKGROUND-COLOUR\n"
	  "    -hl HIGHLIGHT-COLOUR\n"
	  "    -rv\n"
	  "    -font FONT-NAME\n"
	  "    -sync\n" , stderr);
}

/* Scan the command line for the X11 options. Updating ARGC-P and ARGV-P to
   point to the following options. */
static void
get_options(int *argc_p, char ***argv_p)
{
    int argc = *argc_p;
    char **argv = *argv_p;
    argc--;
    argv++;
    while((argc >= 1) && (**argv == '-'))
    {
	if(!strcmp("-sync", *argv))
	    x11_opt_sync = 1;
	else if(!strcmp("-rv", *argv))
	    x11_opt_reverse_video = !x11_opt_reverse_video;
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
}

/* After parsing the command line and the resource database, use the
   information. */
static bool
use_options(struct x11_display *xdisplay)
{
    int x, y, w, h;
    int gflgs = XParseGeometry(geom_str, &x, &y, &w, &h);
    Colormap cmap = DefaultColormap(xdisplay->display, xdisplay->screen);
    XColor tmpc;

    if(gflgs & WidthValue)
	def_dims[2] = w;
    else
	def_dims[2] = -1;
    if(gflgs & HeightValue)
	def_dims[3] = h;
    else
	def_dims[3] = -1;
    /* TODO: need to use -ve values properly */
    if(gflgs & XValue)
	def_dims[0] = x;
    else
	def_dims[0] = -1;
    if(gflgs & YValue)
	def_dims[1] = y;
    else
	def_dims[1] = -1;

    if(!XParseColor(xdisplay->display, cmap, fg_str, &tmpc))
	fprintf(stderr, "error: invalid fg colour\n");
    else if(!XAllocColor(xdisplay->display, cmap, &tmpc))
	fprintf(stderr, "error: can't allocate fg colour\n");
    else
	xdisplay->fore_pixel = tmpc.pixel;

    if(!XParseColor(xdisplay->display, cmap, bg_str, &tmpc))
	fprintf(stderr, "error: invalid bg colour\n");
    else if(!XAllocColor(xdisplay->display, cmap, &tmpc))
	fprintf(stderr, "error: can't allocate bg colour\n");
    else
	xdisplay->back_pixel = tmpc.pixel;

    if(!XParseColor(xdisplay->display, cmap, hl_str, &tmpc))
	fprintf(stderr, "error: invalid hl colour\n");
    else if(!XAllocColor(xdisplay->display, cmap, &tmpc))
    {
	fprintf(stderr, "error: can't allocate hl colour\n");
	xdisplay->high_pixel = xdisplay->fore_pixel;
    }
    else
	xdisplay->high_pixel = tmpc.pixel;

    return TRUE;
}


/* Display management */

struct x11_display *
x11_open_display(char *display_name)
{
    bool is_first = (x11_display_list == 0);
    Display *display = XOpenDisplay(display_name);
    if(display)
    {
	struct x11_display *xdisplay = str_alloc(sizeof(struct x11_display));
	if(xdisplay != 0)
	{
	    /* Add at end of list, since some functions grab the
	       display at the head of the list as the default. */
	    struct x11_display **ptr = &x11_display_list;
	    while(*ptr != 0)
		ptr = &((*ptr)->next);
	    *ptr = xdisplay;
	    xdisplay->next = 0;

	    xdisplay->window_count = 0;
	    xdisplay->display = display;
	    xdisplay->screen = DefaultScreen(display);

	    sys_register_input_fd(ConnectionNumber(display),
				  x11_handle_sync_input);

	    if(is_first)
	    {
		get_resources(xdisplay);
		get_options(&out_argc, &out_argv);
	    }

	    if(use_options(xdisplay))
	    {
		XColor fore, back;
		xdisplay->wm_delete_window
		    = XInternAtom(display, "WM_DELETE_WINDOW", False);
		xdisplay->jade_selection
		    = XInternAtom(display, "JADE_SELECTION", False);
		xdisplay->text_cursor = XCreateFontCursor(display, XC_xterm);
		fore.pixel = xdisplay->fore_pixel;
		back.pixel = xdisplay->back_pixel;
		XQueryColor(display,
			    DefaultColormap(display, xdisplay->screen),
			    &fore);
		XQueryColor(display,
			    DefaultColormap(display, xdisplay->screen),
			    &back);
		XRecolorCursor(display, xdisplay->text_cursor, &fore, &back);

		xdisplay->meta_mod = x11_find_meta(xdisplay);

		if(x11_opt_sync)
		    XSynchronize(xdisplay->display, True);

		return xdisplay;
	    }
	}
	XCloseDisplay(display);
    }
    return 0;
}

void
x11_close_display(struct x11_display *xdisplay)
{
    if(x11_display_list == xdisplay)
	x11_display_list = xdisplay->next;
    else
    {
	struct x11_display **ptr = &x11_display_list;
	while(*ptr != 0)
	{
	    if(*ptr == xdisplay)
	    {
		*ptr = xdisplay->next;
		break;
	    }
	    ptr = &((*ptr)->next);
	}
    }
    sys_deregister_input_fd(ConnectionNumber(xdisplay->display));
    XCloseDisplay(xdisplay->display);
    str_free(xdisplay);
}
    
void
x11_close_all_displays(void)
{
    while(x11_display_list != 0)
	x11_close_display(x11_display_list);
}


/* X11 event handling. */

static Bool
x11_async_event_pred(Display *dpy, XEvent *ev, XPointer arg)
{
    if(ev->xany.type == Expose
       || ev->xany.type == GraphicsExpose
       || ev->xany.type == MappingNotify
       || ev->xany.type == ConfigureNotify
       || ev->xany.type == SelectionRequest
       || ev->xany.type == SelectionClear)
	return True;
    else if(ev->xany.type == KeyPress)
    {
	u_long code = 0, mods = 0;
	translate_event(&code, &mods, ev);
	if(code == XK_g && mods == (EV_TYPE_KEYBD | EV_MOD_CTRL))
	{
	    /* Got one. */
	    throw_value = int_cell;
	    return True;
	}
    }
    return False;
}

static void
x11_handle_input(int fd, bool synchronous)
{
    struct x11_display *xdisplay = 0;

    /* Read all events in the input queue. */
    while(throw_value == LISP_NULL)
    {
	XEvent xev;
	WIN *oldwin = curr_win, *ev_win;

	if(xdisplay == 0)
	{
	    /* Find the display associated with this file descriptor. We
	       have to do this here, since it's possible that the
	       display can be closed while reading events from it. */
	    xdisplay = x11_display_list;
	    while(xdisplay != 0 && ConnectionNumber(xdisplay->display) != fd)
		xdisplay = xdisplay->next;
	    if(xdisplay == 0)
		break;
	}

	if(synchronous)
	{
	    if(XEventsQueued(xdisplay->display, QueuedAfterReading) <= 0)
		break;
	    XNextEvent(xdisplay->display, &xev);
	}
	else
	{
	    if(!XCheckIfEvent(xdisplay->display, &xev,
			      &x11_async_event_pred, NULL))
		break;

	    if(xev.xany.type == KeyPress)
		continue;

	    /* Should only be ``safe'' events that pass through here */
	}

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
		   && (xev.xclient.data.l[0] == xdisplay->wm_delete_window))
		{
		    curr_win = ev_win;
		    if(ev_win != oldwin)
			curr_vw = curr_win->w_CurrVW;
		    cmd_delete_window(sym_nil);
		    xdisplay = 0;
		}
		break;

	    case FocusIn:
		ev_win->w_WindowSys.ws_HasFocus = TRUE;
		if(ev_win != oldwin)
		{
		    curr_win = ev_win;
		    curr_vw = curr_win->w_CurrVW;
		}
		undo_end_of_command();
		break;

	    case FocusOut:
		ev_win->w_WindowSys.ws_HasFocus = FALSE;
		undo_end_of_command();
		break;

	    case MotionNotify:
	    {
		Window tmpw;
		int tmp;
		int x, y;
		
		/* Swallow any pending motion events as well. */
		while(XCheckMaskEvent(xdisplay->display,
				      ButtonMotionMask, &xev))
		    ;
		x11_last_event_time = xev.xmotion.time;

		/* It seems that further MotionNotify events are suspended
		   until the pointer's position has been queried. I should
		   check the Xlib manuals about this. */
		if(XQueryPointer(xdisplay->display, ev_win->w_Window,
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
		xdisplay = 0;
		undo_end_of_command();
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
	}
    }
}

static void
x11_handle_sync_input(int fd)
{
    x11_handle_input(fd, TRUE);
}

void
x11_handle_async_input(void)
{
    struct x11_display *dpy = WINDOW_XDPY(curr_win);
    int fd = ConnectionNumber(dpy->display);
    if(sys_poll_input(fd))
    {
	x11_handle_input(fd, FALSE);
	cmd_redisplay(sym_nil);
    }
}
