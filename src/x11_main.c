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
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <string.h>
#include <assert.h>

#ifdef HAVE_UNIX
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
#endif

static void x11_handle_sync_input(int fd);

/* The window in which the current event occurred. */
WIN *x11_current_event_win;

/* The mouse position of the current event, relative to the origin of
   the window that the event occurred in, measured in glyphs. */
long x11_current_mouse_x, x11_current_mouse_y;

/* The last event received which had a timestamp, was at this time. */
Time x11_last_event_time;

/* List of in-use displays */
struct x11_display *x11_display_list;

/* Command line arguments. */
char **x11_argv;
int x11_argc;

/* Command line options, and their default values. */
static char *geom_str = "80x24";
static int x11_opt_sync = 0;
static char *prog_name;
static char *visual_name;

bool x11_opt_reverse_video = FALSE;

/* Default font name. */
DEFSTRING(def_font_str_data, DEFAULT_FONT);

int x11_cursor_shape = XC_xterm;


/* Resource/option management */

/* Called from main(). */
bool
sys_init(char *program_name)
{
    struct x11_display *xdisplay;
    char *display_name = 0;
    repv opt;

#ifdef HAVE_UNIX
    if (!batch_mode_p ())
	setpgid (0, 0);
#endif

    prog_name = program_name;
    def_font_str = rep_VAL (&def_font_str_data);

    if (rep_get_option ("--display", &opt))
	display_name = strdup (rep_STR(opt));
    if (rep_get_option ("--name", &opt))
	prog_name = strdup (rep_STR(opt));

    if (display_name == 0)
	display_name = getenv("DISPLAY");

    xdisplay = x11_open_display(display_name);
    if(xdisplay != 0)
	return TRUE;
    else
    {
	fprintf(stderr, "jade: Can't open display: %s\n",
		display_name ? display_name : "");
	return FALSE;
    }
}

void
sys_kill (void)
{
    x11_close_all_displays ();
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
	default_fg_color = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "background"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Background")))
	default_bg_color = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "block"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Block")))
	default_block_color = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "highlight"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Highlight")))
	default_hl_color = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "modeline"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Modeline")))
	default_ml_color = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "font"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Font")))
	def_font_str = rep_string_dup(s);
    if((s = XGetDefault(xdisplay->display, prog_name, "visual"))
       || (s = XGetDefault(xdisplay->display, "Jade", "Visual")))
	visual_name = s;
    if((s = XGetDefault(xdisplay->display, prog_name, "reverseVideo"))
       || (s = XGetDefault(xdisplay->display, "Jade", "ReverseVideo")))
	x11_opt_reverse_video = (strcasecmp(s, "true") == 0);
}

/* Print the X11 options. */
void
sys_usage(void)
{
    fputs("    --display DISPLAY-NAME\n"
	  "    --name NAME\n"
	  "    --geometry WINDOW-GEOMETRY\n"
	  "    --visual VISUAL-NAME\n"
	  "    --fg FOREGROUND-COLOUR\n"
	  "    --bg BACKGROUND-COLOUR\n"
	  "    --hl HIGHLIGHT-COLOUR\n"
	  "    --ml MODELINE-COLOUR\n"
	  "    --bl BLOCK-COLOUR\n"
	  "    --rv\n"
	  "    --font FONT-NAME\n"
	  "    --sync\n"
	  "    FILE         Load FILE into an editor buffer\n", stderr);
}

/* Scan the command line for the X11 options. Updating ARGC-P and ARGV-P to
   point to the following options. */
static void
get_options(void)
{
    repv opt;
    if (rep_get_option("--sync", 0))
	x11_opt_sync = 1;
    if (rep_get_option("--rv", 0))
	x11_opt_reverse_video = !x11_opt_reverse_video;
    if (rep_get_option("--geometry", &opt))
	geom_str = strdup (rep_STR(opt));
    if (rep_get_option("--visual", &opt))
	visual_name = strdup (rep_STR(opt));
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
use_options(struct x11_display *xdpy)
{
    int id;
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

    /* Pick a visual */
    xdpy->visual = 0;
    id = 0;
    if(visual_name != 0)
    {
	if(!strcasecmp("StaticGray", visual_name))
	    id = StaticGray;
	else if(!strcasecmp("StaticColor", visual_name))
	    id = StaticColor;
	else if(!strcasecmp("TrueColor", visual_name))
	    id = TrueColor;
	else if(!strcasecmp("GrayScale", visual_name)
		|| !strcasecmp("GreyScale", visual_name))
	    id = GrayScale;
	else if(!strcasecmp("PseudoColor", visual_name))
	    id = PseudoColor;
	else if(!strcasecmp("DirectColor", visual_name))
	    id = DirectColor;
    }
    if(id != 0)
    {
	XVisualInfo in, *out;
	int n_out;
	in.class = id;
	in.screen = xdpy->screen;
	out = XGetVisualInfo(xdpy->display,
			     VisualClassMask | VisualScreenMask,
			     &in, &n_out);
	if(out != 0)
	{
	    /* If more than one, choose the deepest? */
	    int i, best;
	    for(i = 0, best = 0; i < n_out; i++)
	    {
		if(out[i].depth > out[best].depth
		   || (out[i].depth == out[best].depth
		       && out[i].colormap_size > out[best].colormap_size))
		{
		    best = i;
		}
	    }
	    if(best < n_out)
	    {
		xdpy->visual = out[best].visual;
		xdpy->depth = out[best].depth;
		xdpy->colormap
		    = XCreateColormap(xdpy->display,
				      DefaultRootWindow(xdpy->display),
				      xdpy->visual, AllocNone);
		if(xdpy->colormap == 0)
		    xdpy->visual = 0; /* fall back to default below */
	    }
	    XFree(out);
	}
    }
    if(xdpy->visual == 0)
    {
	if(visual_name != 0)
	    fprintf(stderr, "warning: using default visual\n");
	xdpy->visual = DefaultVisual(xdpy->display, xdpy->screen);
	xdpy->depth = DefaultDepth(xdpy->display, xdpy->screen);
	xdpy->colormap = DefaultColormap(xdpy->display, xdpy->screen);
    }

    return TRUE;
}


/* Display management */

struct x11_display *
x11_get_display(char *display_name)
{
    struct x11_display *dpy = x11_display_list;
    while (dpy != 0 && strcmp(display_name, dpy->name) != 0)
	dpy = dpy->next;
    return dpy;
}

struct x11_display *
x11_open_display(char *display_name)
{
    struct x11_display *xdisplay;
    bool is_first;
    Display *display;

    if (display_name != 0)
    {
	xdisplay = x11_get_display(display_name);
	if (xdisplay != 0)
	    return xdisplay;
    }

    is_first = (x11_display_list == 0);
    display = XOpenDisplay(display_name);
    if(display)
    {
	if (display_name == 0)
	    display_name = DisplayString(display);
	xdisplay = rep_alloc(sizeof(struct x11_display));
	if(xdisplay != 0)
	{
	    xdisplay->name = rep_alloc(strlen(display_name) + 1);
	    if (xdisplay->name != 0)
	    {
		/* Add at end of list, since some functions grab the
		   display at the head of the list as the default. */
		struct x11_display **ptr = &x11_display_list;
		while(*ptr != 0)
		    ptr = &((*ptr)->next);
		*ptr = xdisplay;
		xdisplay->next = 0;

		strcpy(xdisplay->name, display_name);
		xdisplay->window_count = 0;
		xdisplay->display = display;
		xdisplay->screen = DefaultScreen(display);

		rep_register_input_fd(ConnectionNumber(display),
				      x11_handle_sync_input);

		if(is_first)
		{
		    get_resources(xdisplay);
		    get_options();
		}

		if(use_options(xdisplay))
		{
		    xdisplay->wm_delete_window
			= XInternAtom(display, "WM_DELETE_WINDOW", False);
		    xdisplay->jade_selection
			= XInternAtom(display, "JADE_SELECTION", False);
		    xdisplay->text_cursor
			= XCreateFontCursor(display, x11_cursor_shape);
		    sys_recolor_cursor(mouse_cursor_face);
		    xdisplay->meta_mod = x11_find_meta(xdisplay);

		    if(x11_opt_sync)
			XSynchronize(xdisplay->display, True);

		    return xdisplay;
		}
	    }
	    rep_free(xdisplay);
	}
	XCloseDisplay(display);
    }
    return 0;
}

void
x11_close_display(struct x11_display *xdisplay)
{
    x11_free_dpy_colors(xdisplay);
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
    rep_deregister_input_fd(ConnectionNumber(xdisplay->display));
    if(xdisplay->colormap != DefaultColormap(xdisplay->display,
					     xdisplay->screen))
	XFreeColormap(xdisplay->display, xdisplay->colormap);
    XCloseDisplay(xdisplay->display);
    rep_free(xdisplay);
}
    
void
x11_close_all_displays(void)
{
    while(x11_display_list != 0)
	x11_close_display(x11_display_list);
}


/* Color handling. */

DEFSTRING(no_color, "Can't allocate color");

struct x11_color *
x11_make_color_dpy(Lisp_Color *c, struct x11_display *dpy)
{
    struct x11_color *cell = rep_alloc(sizeof(struct x11_color));
    if(cell != 0)
    {
	XColor tem;
	cell->next = c->color;
	cell->dpy = dpy;
	if(XAllocNamedColor(dpy->display, dpy->colormap,
			    rep_STR(c->name), &cell->color, &tem) != 0)
	{
	    c->color = cell;
	    return cell;
	}
	rep_free(cell);
	Fsignal(Qerror, rep_list_2(rep_VAL(&no_color), c->name));
	return 0;
    }
    rep_mem_error();
    return 0;
}

repv
sys_make_color(Lisp_Color *c)
{
    c->color = 0;
    if(x11_display_list != 0)
	return x11_make_color_dpy(c, x11_display_list) ? rep_VAL(c) : rep_NULL;
    else
	return rep_VAL(c);			/* lazy */
}

struct x11_color *
x11_get_color_dpy(Lisp_Color *c, struct x11_display *dpy)
{
    struct x11_color *x = c->color;
    while(x != 0)
    {
	if(x->dpy == dpy)
	    return x;
	x = x->next;
    }
    return x11_make_color_dpy(c, dpy);
}

void
sys_free_color(Lisp_Color *c)
{
    struct x11_color *x = c->color;
    while(x != 0)
    {
	struct x11_color *next = x->next;
	XFreeColors(x->dpy->display, x->dpy->colormap, &x->color.pixel, 1, 0);
	rep_free(x);
	x = next;
    }
    c->color = 0;
}

void
x11_free_dpy_colors(struct x11_display *dpy)
{
    Lisp_Color *c;
    for(c = allocated_colors; c != 0; c = c->next)
    {
	struct x11_color **x = &c->color;
	while(*x != 0)
	{
	    if((*x)->dpy == dpy)
	    {
		struct x11_color *next = (*x)->next;
		XFreeColors(dpy->display, dpy->colormap,
			    &(*x)->color.pixel, 1, 0);
		rep_free(*x);
		*x = next;
		break;
	    }
	    x = &((*x)->next);
	}
    }
}

void
sys_recolor_cursor(repv face)
{
    if(face && FACEP(face)
       && COLORP(VFACE(face)->background)
       && COLORP(VFACE(face)->foreground))
    {
	Lisp_Color *fg = VCOLOR(VFACE(face)->foreground);
	Lisp_Color *bg = VCOLOR(VFACE(face)->background);
	struct x11_display *xdpy = x11_display_list;
	while(xdpy != 0)
	{
	    struct x11_color *xf = x11_get_color_dpy(fg, xdpy);
	    struct x11_color *xb = x11_get_color_dpy(bg, xdpy);
	    if(xf != 0 && xb != 0)
	    {
		XRecolorCursor(xdpy->display, xdpy->text_cursor,
			       &xf->color, &xb->color);
	    }
	    xdpy = xdpy->next;
	}
    }
}


/* X11 event handling. */

/* arg == x11_display */
static Bool
x11_async_event_pred(Display *dpy, XEvent *ev, XPointer arg)
{
    if(ev->xany.type == Expose
       || ev->xany.type == GraphicsExpose
       || ev->xany.type == MappingNotify
       || ev->xany.type == SelectionRequest
       || ev->xany.type == SelectionClear)
	return True;
    else if(ev->xany.type == KeyPress)
    {
	u_long code = 0, mods = 0;
	translate_event(&code, &mods, ev, (struct x11_display *)arg);
	if(code == XK_g && mods == (EV_TYPE_KEYBD | EV_MOD_CTRL))
	{
	    /* Got one. */
	    rep_throw_value = rep_int_cell;
	    return True;
	}
    }
    /* This event has now been read, so select() wouldn't notice it.. */
    rep_mark_input_pending(ConnectionNumber(dpy));
    return False;
}

static bool
x11_handle_input(int fd, bool synchronous)
{
    struct x11_display *xdisplay = 0;
    bool need_redisplay = FALSE;

    /* Read all events in the input queue. */
    while(rep_throw_value == rep_NULL)
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
			      &x11_async_event_pred, (XPointer)xdisplay))
		break;

	    if(xev.xany.type == KeyPress)
		continue;

	    /* Should only be ``safe'' events that pass through here */
	}

	ev_win = x11_find_window(xev.xany.window);
	if(ev_win != NULL)
	{
	    int old_mouse_x = x11_current_mouse_x;
	    int old_mouse_y = x11_current_mouse_y;
	    WIN *old_mouse_win = x11_current_event_win;

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

		    if(!synchronous)
		    {
			/* If we're in the middle of doing something else,
			   don't let the expose cause the current display
			   state to be redrawn; preserve the window contents
			   at the last redisplay */
			WIN *w;;
			for(w = win_chain; w != 0; w = w->w_Next)
			{
			    if(!(w->w_Flags & WINFF_PRESERVING))
			    {
				copy_glyph_buf(w->w_NewContent, w->w_Content);
				w->w_Flags |= WINFF_PRESERVING;
			    }
			}
		    }

		    garbage_glyphs(ev_win, x, y, width, height);
		}
		if(xev.xexpose.count == 0)
		    need_redisplay = TRUE;
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
		need_redisplay = TRUE;
		break;

	    case ClientMessage:
		if((xev.xclient.format == 32)
		   && (xev.xclient.data.l[0] == xdisplay->wm_delete_window))
		{
		    curr_win = ev_win;
		    if(ev_win != oldwin)
			curr_vw = curr_win->w_CurrVW;
		    rep_call_with_barrier (Fdelete_window, Qnil,
					   rep_TRUE, 0, 0, 0);
		    xdisplay = 0;
		}
		need_redisplay = TRUE;
		break;

	    case FocusIn:
		ev_win->w_WindowSys.ws_HasFocus = TRUE;
		if(ev_win != oldwin)
		{
		    curr_win = ev_win;
		    curr_vw = curr_win->w_CurrVW;
		}
		undo_end_of_command();
		need_redisplay = TRUE;
		break;

	    case FocusOut:
		ev_win->w_WindowSys.ws_HasFocus = FALSE;
		undo_end_of_command();
		need_redisplay = TRUE;
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
		if (ev_win != old_mouse_win
		    || old_mouse_x != x11_current_mouse_x
		    || old_mouse_y != x11_current_mouse_y)
		{
		    need_redisplay = update_mouse_extent (ev_win,
							  x11_current_mouse_x,
							  x11_current_mouse_y);
		}

		code = mods = 0;
		translate_event(&code, &mods, &xev, xdisplay);
		if((mods & EV_TYPE_MASK)
		   /* Don't pass modifier-less motion-events through */
		   && ((mods & EV_TYPE_MASK) != EV_TYPE_MOUSE
		       || code != EV_CODE_MOUSE_MOVE
		       || (mods & EV_MOD_BUTTON_MASK) != 0))
		{
		    curr_win = ev_win;
		    if(oldwin != ev_win)
			curr_vw = curr_win->w_CurrVW;
		    reset_message(ev_win);
		    eval_input_event(&xev, code, mods);
		    need_redisplay = TRUE;
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
    return need_redisplay;
}

static void
x11_handle_sync_input(int fd)
{
    x11_handle_input(fd, TRUE);
}

void
x11_handle_async_input(void)
{
    if(!redisplay_lock)
    {
	struct x11_display *dpy = WINDOW_XDPY(curr_win);
	int fd = ConnectionNumber(dpy->display);
	if(rep_poll_input(fd) && x11_handle_input(fd, FALSE))
	    Fredisplay(Qnil);
    }
}
