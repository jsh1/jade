/* windows.c -- System-independant window handling
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

DEFSYM(make_window_hook, "make-window-hook");
DEFSYM(delete_window_hook, "delete-window-hook");
DEFSYM(visible_bell, "visible-bell");
DEFSYM(visible_bell_length, "visible-bell-length"); /*
::doc:make-window-hook::
Hook called when a new window is created. Called with the new window
selected.
::end::
::doc:delete-window-hook::
Hook called when a window is deleted. Called with a single argument, the
window in question.
::end::
::doc:visible-bell::
When non-nil, the `beep' function attempts to visibly flash the window.
::end::
::doc:visible-bell-length::
The number of milliseconds to hold the inverted display for when the
`visible-bell' variable is set.
::end:: */

int window_type;

/* This can contain `dead' windows, ie w_Window==NULL, they have been
   close'd but must hang around until we're sure all refs are dead.  */
WIN *win_chain;

/* curr_win is the active window. When setting it's value curr_vw
   must be set to a view in the same window. */
WIN *curr_win;

/* The default window position and dimensions. */
static short def_dims[4] = { 0, 0, 80, 24 };

repv def_font_str;

DEFSYM(save_and_quit, "save-and-quit");

DEFSYM(dimensions, "dimensions");
DEFSYM(position, "position");
DEFSYM(buffer, "buffer");
DEFSYM(font, "font");

void
set_default_geometry (short x, short y, short w, short h)
{
    def_dims[0] = x;
    def_dims[1] = y;
    def_dims[2] = w;
    def_dims[3] = h;
}

DEFUN_INT("make-window", Fmake_window, Smake_window, (repv attrs), rep_Subr1, "") /*
::doc:make-window::
make-window ATTRS

Return and select a new window, it will be displaying the same buffer as
the originally selected window.

ATTRS is an alist with any of the following pairs:

	(font . FONT-NAME)
	(position . (X . Y))
	(dimensions . (COLS . ROWS))
	(buffer . BUFFER)
::end:: */
{
    WIN *w;
    repv tem, tx, font;
    short dims[4];

    memcpy (dims, def_dims, sizeof (dims));

    tem = Fassq (Qbuffer, attrs);
    if (!tem)
	return rep_NULL;
    if (tem != Qnil)
	tx = rep_CDR(tem);
    else
	tx = curr_vw ? rep_VAL(curr_vw->vw_Tx) : Qnil;

    tem = Fassq (Qposition, attrs);
    if (!tem)
	return rep_NULL;
    if (tem != Qnil && rep_CONSP(rep_CDR(tem)))
    {
	dims[0] = rep_INT(rep_CADR(tem));
	dims[1] = rep_INT(rep_CDDR(tem));
    }

    tem = Fassq (Qdimensions, attrs);
    if (!tem)
	return rep_NULL;
    if (tem != Qnil && rep_CONSP(rep_CDR(tem)))
    {
	dims[2] = rep_INT(rep_CADR(tem));
	dims[3] = rep_INT(rep_CDDR(tem));
    }

    tem = Fassq (Qfont, attrs);
    if (!tem)
	return rep_NULL;
    if (tem != Qnil)
	font = rep_CDR(tem);
    else if (curr_win != 0)
	font = curr_win->w_FontName;
    else
	font = def_font_str;

    w = rep_ALLOC_CELL(sizeof(WIN));
    if(w != NULL)
    {
	memset(w, 0, sizeof(WIN));
	w->w_Car = window_type;
	w->w_FontName = rep_STRINGP(font) ? font : def_font_str;
	if(sys_set_font(w))
	{
	    w->w_Window = sys_new_window(curr_win, w, dims);
	    if(w->w_Window)
	    {
		sys_update_dimensions(w);
		update_window_dimensions(w);
		/* First the main view.. */
		if(w->w_Content != 0 && w->w_NewContent != 0
		   && make_view(NULL, w, VTX(tx), 0, FALSE))
		{
		    /* ..then the minibuffer view. */
		    if(make_view(NULL, w, NULL, 0, TRUE))
		    {
			w->w_CurrVW = w->w_ViewList;
			w->w_Flags |= WINFF_FORCE_REFRESH;
			w->w_Next = win_chain;
			win_chain = w;
			if(curr_win == 0)
			{
			    curr_win = w;
			    curr_vw = w->w_CurrVW;
			}
			else
			{
			    w->w_CurrVW->vw_BufferList
				= Fcopy_sequence(curr_vw->vw_BufferList);
			}
			Fset_current_window(rep_VAL(w), Qnil);
			Fcall_hook(Qmake_window_hook, Qnil, Qnil);
			return rep_VAL(w);
		    }
		    kill_all_views(w);
		}
		if(w->w_NewContent)
		    free_glyph_buf(w->w_NewContent);
		if(w->w_Content)
		    free_glyph_buf(w->w_Content);
		free_visible_extents (w);
		sys_kill_window(w);
	    }
	    sys_unset_font(w);
	}
	rep_FREE_CELL(w);
    }
    return rep_NULL;
}

/* Close window W. */
static void
delete_window(WIN *w)
{
    Fcall_hook(Qdelete_window_hook, rep_LIST_1(rep_VAL(w)), Qnil);
    kill_all_views(w);
    sys_unset_font(w);
    sys_kill_window(w);
    free_glyph_buf(w->w_NewContent);
    free_glyph_buf(w->w_Content);
    w->w_NewContent = w->w_Content = NULL;
    free_visible_extents (w);
    /* This flags that this window is dead.  */
    w->w_Window = WINDOW_NIL;
    if(curr_win == w)
    {
	while((w = w->w_Next))
	{
	    if(w->w_Window)
	    {
		curr_win = w;
		curr_vw = w->w_CurrVW;
		return;
	    }
	}
	w = win_chain;
	while(w && (w != curr_win))
	{
	    if(w->w_Window)
	    {
		curr_win = w;
		curr_vw = w->w_CurrVW;
		return;
	    }
	    w = w->w_Next;
	}
	/* No living windows left :-( we'll die soon :-(  */
	curr_win->w_CurrVW = NULL;
	curr_win = NULL;
	curr_vw = NULL;
	rep_throw_value = Fcons(Qquit, rep_MAKE_INT(0)); /* experimental. */
    }
}

DEFUN_INT("delete-window", Fdelete_window, Sdelete_window,
	  (repv win), rep_Subr1, "") /*
::doc:delete-window::
delete-window [WINDOW]

Close WINDOW (or the current window). If there is only one window currently
open, then the function `save-and-quit' is called instead; this will take
care of any unsaved files, hooks to run, etc.

This function always returns nil.
::end:: */
{
    WIN *w = WINDOWP(win) ? VWIN(win) : curr_win;
    if(sys_deleting_window_would_exit (w))
	rep_call_lisp0(Fsymbol_value(Qsave_and_quit, Qt));
    else
	delete_window(w);
    return Qnil;
}

void
update_window_dimensions(WIN *w)
{
    long new_width = w->w_WidthPix / w->w_FontX;
    long new_height = w->w_HeightPix / w->w_FontY;
    if(new_width != w->w_MaxX || new_height != w->w_MaxY)
    {
	if(w->w_Content)
	    free_glyph_buf(w->w_Content);
	if(w->w_NewContent)
	    free_glyph_buf(w->w_NewContent);
	w->w_Content = alloc_glyph_buf(new_width, new_height);
	w->w_NewContent = alloc_glyph_buf(new_width, new_height);
	if(!w->w_Content || !w->w_NewContent)
	    abort();			/* TODO: this is evil */
	w->w_MaxX = new_width;
	w->w_MaxY = new_height;
	update_views_dimensions(w);
    }
}

DEFUN_INT("sleep-window", Fsleep_window, Ssleep_window, (repv win), rep_Subr1, "") /*
::doc:sleep-window::
sleep-window [WINDOW]

Iconifies the current window.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    if(((VWIN(win)->w_Flags & WINFF_SLEEPING) == 0)
       && sys_sleep_win(VWIN(win)))
	return(win);
    return(Qnil);
}

DEFUN_INT("unsleep-window", Funsleep_window, Sunsleep_window, (repv win), rep_Subr1, "") /*
::doc:unsleep-window::
unsleep-window [WINDOW]

Uniconifies the current window.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    if((VWIN(win)->w_Flags & WINFF_SLEEPING) && sys_unsleep_win(VWIN(win)))
	return(win);
    return(Qnil);
}

DEFUN_INT("next-window", Fnext_window, Snext_window, (repv win, repv activ), rep_Subr2, "!" rep_DS_NL "p") /*
::doc:next-window::
next-window [WINDOW] [ACTIVATE]

Cycles through the open windows forwards.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win->w_Next);
    while(VWIN(win) != curr_win)
    {
	if(!win)
	    win = rep_VAL(win_chain);
	if(VWIN(win)->w_Window)
	{
	    if(!rep_NILP(activ))
	    {
		curr_win = VWIN(win);
		sys_activate_win(VWIN(win));
	    }
	    return(win);
	}
	win = rep_VAL(VWIN(win)->w_Next);
    }
    return(rep_VAL(curr_win));
}

void
messagen(u_char *title, int length)
{
    WIN *w = curr_win;
    if((w->w_Flags & WINFF_SLEEPING) == 0)
    {
	if(w->w_Message != NULL)
	    rep_free(w->w_Message);
	w->w_Message = rep_str_dupn(title, length);
	w->w_MessageLen = length;
	w->w_Flags |= WINFF_MESSAGE;
    }
}

void
messagef(u_char *fmt, va_list args)
{
    WIN *w = curr_win;
    if((w->w_Flags & WINFF_SLEEPING) == 0)
    {
	u_char fmtbuff[256];
	u_long len;
#ifdef HAVE_SNPRINTF
	vsnprintf(fmtbuff, sizeof(fmtbuff), fmt, args);
#else
	vsprintf(fmtbuff, fmt, args);
#endif
	va_end(args);
	if(w->w_Message != NULL)
	    rep_free(w->w_Message);
	len = strlen(fmtbuff);
	w->w_Message = rep_str_dupn(fmtbuff, len);
	w->w_MessageLen = len;
	w->w_Flags |= WINFF_MESSAGE;
    }
}

void
reset_message (WIN *w)
{
    w->w_Flags &= ~WINFF_MESSAGE;
}

bool
remove_all_messages(bool from_idle_p)
{
    WIN *w;
    bool success = FALSE;
    for(w = win_chain; w != 0; w = w->w_Next)
    {
	/* Really if we're being called from idle-time we
	   only want to remove a message if the minibuffer is
	   in use. */
	if(w->w_Flags & WINFF_MESSAGE
	   && (!from_idle_p || MINIBUFFER_ACTIVE_P(w)))
	{
	    reset_message(w);
	    success = TRUE;
	}
    }
    return success;
}

static void
jade_message (enum rep_message fn, ...)
{
    va_list args;
    va_start (args, fn);
    switch (fn)
    {
	int len;
	u_char *msg;
	u_long *old_lenp;
	u_char **old_msgp;

    case rep_messagen:
	msg = (u_char *)va_arg(args, u_char *);
	len = (int)va_arg(args, int);
	messagen (msg, len);
	break;

    case rep_message:
	msg = (u_char *)va_arg(args, u_char *);
	messagen (msg, strlen(msg));
	break;

    case rep_messagef:
	msg = (u_char *)va_arg(args, u_char *);
	messagef (msg, args);
	break;

    case rep_reset_message:
	reset_message (curr_win);
	break;

    case rep_save_message:
	old_msgp = (u_char **)va_arg(args, u_char **);
	old_lenp = (u_long *)va_arg(args, u_long *);
	if(curr_win->w_Flags & WINFF_MESSAGE)
	{
	    /* a message is being displayed. */
	    *old_msgp = curr_win->w_Message;
	    *old_lenp = curr_win->w_MessageLen;
	    curr_win->w_Message = NULL;
	    curr_win->w_MessageLen = 0;
	}
	else
	{
	    *old_msgp = NULL;
	    *old_lenp = 0;
	}
	break;

    case rep_restore_message:
	msg = (u_char *)va_arg(args, u_char *);
	len = (int)va_arg(args, int);
	if (msg != 0)
	{
	    if(curr_win->w_Message != NULL)
		rep_free(curr_win->w_Message);
	    curr_win->w_Message = msg;
	    curr_win->w_MessageLen = len;
	    curr_win->w_Flags |= WINFF_MESSAGE;
	}
	break;

    case rep_append_message:
	msg = (u_char *)va_arg(args, u_char *);
	len = (int)va_arg(args, int);
	if (curr_win->w_Flags & WINFF_MESSAGE)
	{
	    WIN *w = curr_win;
	    u_char *s = rep_realloc(w->w_Message, w->w_MessageLen + len + 1);
	    if(s != 0)
	    {
		w->w_Message = s;
		memcpy (w->w_Message + w->w_MessageLen, msg, len);
		w->w_MessageLen += len;
		s[w->w_MessageLen] = 0;
		w->w_Flags |= WINFF_MESSAGE;
	    }
	}
	else
	    messagen (msg, len);
	break;

    case rep_redisplay_message:
	redisplay_message (curr_win);
	break;
    }
}

DEFUN("message", Fmessage, Smessage, (repv string, repv now), rep_Subr2) /*
::doc:message::
message STRING [DISPLAY-NOW]

Temporarily sets the status display to STRING, this won't happen until the
window is next refreshed unless DISPLAY-NOW is non-nil.
::end:: */
{
    rep_DECLARE1(string, rep_STRINGP);
    (*rep_message_fun)(rep_messagen, rep_STR(string), rep_STRING_LEN(string));
    if(!rep_NILP(now))
	(*rep_message_fun)(rep_redisplay_message);
    return(string);
}

DEFUN("font-name", Ffont_name, Sfont_name, (repv win), rep_Subr1) /*
::doc:font-name::
font-name [WINDOW]

Returns the name of the font being used in this window.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    return(VWIN(win)->w_FontName);
}

DEFUN("window-asleep-p", Fwindow_asleep_p, Swindow_asleep_p, (void), rep_Subr0) /*
::doc:window-asleep-p::
window-asleep-p

Returns t if window is currently iconified.
::end:: */
{
    if(curr_win->w_Flags & WINFF_SLEEPING)
	return(Qt);
    return(Qnil);
}

DEFUN("position-window", Fposition_window, Sposition_window, (repv left, repv top, repv width, repv height), rep_Subr4) /*
::doc:position-window::
position-window LEFT TOP WIDTH HEIGHT

Sets the position and dimensions of the current window. These are all
*pixel* measurememnts.
::end:: */
{
    rep_DECLARE1(left, rep_INTP);
    rep_DECLARE2(top, rep_INTP);
    rep_DECLARE3(width, rep_INTP);
    rep_DECLARE4(height, rep_INTP);
    sys_set_win_pos(curr_win, rep_INT(left), rep_INT(top),
		    rep_INT(width), rep_INT(height));
    return(Qt);
}

DEFUN("current-window", Fcurrent_window, Scurrent_window, (void), rep_Subr0) /*
::doc:current-window::
current-window

Returns the currently active window. Note that this is the editor's notion
of `current' -- it doesn't necessarily mean that this is the window to which
your window system will send input events to.
::end:: */
{
    return(rep_VAL(curr_win));
}

DEFUN("set-current-window", Fset_current_window, Sset_current_window, (repv win, repv activ), rep_Subr2) /*
::doc:set-current-window::
set-current-window WINDOW [ACTIVATE-P]

Sets the window which jade reguards as current.
If ACTIVATE-P is non-nil the window will be activated with respect to the
window-system (under X11 this means warping the pointer to the top left corner
of the window as well).
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    curr_win = VWIN(win);
    curr_vw = curr_win->w_CurrVW;
    if(!rep_NILP(activ))
	sys_activate_win(VWIN(win));
    return(rep_VAL(curr_win));
}

DEFUN("window-id", Fwindow_id, Swindow_id, (repv win), rep_Subr1) /*
::doc:window-id::
window-id [WINDOW]

Returns the identifier of the physical window that the Lisp window WINDOW
points to. This is window-system dependant, under X11 it will be some integer,
under Intuition a pointer (integer) to the window structure.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    return(rep_MAKE_LONG_INT((u_long)VWIN(win)->w_Window));
}

DEFUN("font-dimensions", Ffont_dimensions, Sfont_dimensions,
      (repv win), rep_Subr1) /*
::doc:font-dimensions::
font-dimensions [WINDOW]

Returns (WIDTH . HEIGHT) of the window's font (in pixels).
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    return Fcons(rep_MAKE_INT((long)VWIN(win)->w_FontX),
		 rep_MAKE_INT((long)VWIN(win)->w_FontY));
}

DEFUN("window-dimensions", Fwindow_dimensions, Swindow_dimensions, (repv win), rep_Subr2) /*
::doc:window-dimensions::
window-dimensions [VIEW]

Returns (COLUMNS . ROWS) defining the size (in glyphs) of WINDOW (by default
the current window).
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    return Fcons(rep_MAKE_INT(VWIN(win)->w_MaxX),
		    rep_MAKE_INT(VWIN(win)->w_MaxY));
}

DEFUN("window-list", Fwindow_list, Swindow_list, (void), rep_Subr0) /*
::doc:window-list::
window-list

Return a list of all non-deleted windows.
::end:: */
{
    repv head = Qnil;
    repv *ptr = &head;
    WIN *w = win_chain;
    while(w != 0)
    {
	if(w->w_Window)
	{
	    *ptr = Fcons(rep_VAL(w), Qnil);
	    ptr = &(rep_CDR(*ptr));
	}
	w = w->w_Next;
    }
    return head;
}

DEFUN("window-view-list", Fwindow_view_list, Swindow_view_list, (repv win), rep_Subr1) /*
::doc:window-view-list::
window-view-list [WINDOW]

Return a list of the views in WINDOW. The list will be ordered in top to
bottom order, ending with the mini-buffer.
::end:: */
{
    VW *vw;
    repv res = Qnil;
    repv *ptr = &res;
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    for(vw = VWIN(win)->w_ViewList; vw != 0; vw = vw->vw_NextView)
    {
	if(!(*ptr = Fcons(rep_VAL(vw), Qnil)))
	    return rep_NULL;
	ptr = &rep_CDR(*ptr);
    }
    return res;
}

DEFUN("window-view-count", Fwindow_view_count, Swindow_view_count, (repv win), rep_Subr1) /*
::doc:window-view-count::
window-view-count [WINDOW]

Return the number of views of window WINDOW. This *will* include the
minibuffer view, whether it's active or not.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    return rep_MAKE_INT(VWIN(win)->w_ViewCount);
}

DEFUN("window-first-view", Fwindow_first_view, Swindow_first_view, (repv win), rep_Subr1) /*
::doc:window-first-view::
window-first-view [WINDOW]

Returns the first view in WINDOW.
::end:: */
{
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    return rep_VAL(VWIN(win)->w_ViewList);
}

DEFUN("windowp", Fwindowp, Swindowp, (repv arg), rep_Subr1) /*
::doc:windowp::
windowp ARG

Returns t if ARG is a window object.
::end:: */
{
    return WINDOWP(arg) ? Qt : Qnil;
}

DEFSTRING(no_font, "Can't open font");

DEFUN_INT("set-font", Fset_font, Sset_font, (repv fontname, repv win), rep_Subr2, "sFont name: ") /*
::doc:set-font::
set-font FONT-NAME [WINDOW]

FONT-NAME specifies the font to use in WINDOW (or the active one), using
the standard window system conventions.
::end:: */
{
    repv oldfont;
    rep_DECLARE1(fontname, rep_STRINGP);
    if(!WINDOWP(win))
	win = rep_VAL(curr_win);
    oldfont = VWIN(win)->w_FontName;
    VWIN(win)->w_FontName = fontname;
    if(sys_set_font(VWIN(win)))
    {
	VWIN(win)->w_Flags |= WINFF_FORCE_REFRESH;
	return Qt;
    }
    else
    {
	VWIN(win)->w_FontName = oldfont;
	return Fsignal(Qerror, rep_list_2(rep_VAL(&no_font), fontname));
    }
}

static void
beep (void)
{
    repv tem = Fsymbol_value (Qvisible_bell, Qt);
    if (tem == Qnil)
	sys_beep (curr_win);
    else
    {
	tem = Fsymbol_value (Qvisible_bell_length, Qt);
	invert_all_faces = !invert_all_faces;
	Fredisplay_window (rep_VAL(curr_win), Qnil);
	Fsit_for (rep_MAKE_INT (0), rep_INTP (tem) ? tem : rep_MAKE_INT (250));
	Fflush_output ();
	invert_all_faces = !invert_all_faces;
    }
}

static void
window_sweep(void)
{
    WIN *w = win_chain;
    win_chain = NULL;
    while(w)
    {
	WIN *next = w->w_Next;
	if(rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->w_Next = win_chain;
	    win_chain = w;
	}
	else
	    rep_FREE_CELL(w);
	w = next;
    }
}

static void
window_mark (repv val)
{
    rep_MARKVAL(VWIN(val)->w_FontName);
    rep_MARKVAL(VWIN(val)->w_DisplayedName);
    rep_MARKVAL(rep_VAL(VWIN(val)->w_ViewList));
}

static void
window_mark_active (void)
{
    /* Don't want any open windows mysteriously vanishing so,  */
    WIN *win = win_chain;
    while(win != 0)
    {
#ifdef WINDOW_NON_COLLECTABLE
	if (WINDOW_NON_COLLECTABLE (win))
#else
	if (win->w_Window)
#endif
	{
	    rep_MARKVAL(rep_VAL(win));
	    mark_merged_faces(win);
	    mark_visible_extents (win);
	}
	win = win->w_Next;
    }
}

static void
window_prin(repv strm, repv win)
{
    u_char buf[40];
    if(VWIN(win)->w_Window)
    {
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf),
#else
	sprintf(buf,
#endif
#if defined (HAVE_GTK) || !defined (HAVE_X11)
		"#<window 0x%x", VWIN(win)->w_Window);
#else
		"#<window %ld", VWIN(win)->w_Window);
#endif
	rep_stream_puts(strm, buf, -1, FALSE);
	rep_stream_putc(strm, '>');
    }
    else
	rep_stream_puts(strm, "#<deleted window>", -1, FALSE);
}

static repv
window_bind (repv win)
{
    if (!WINDOWP(win))
	return Qnil;
    else
    {
	repv handle = rep_VAL(curr_win);
	curr_win = VWIN(win);
	curr_vw = curr_win->w_CurrVW;
	return handle;
    }
}

static void
window_unbind (repv handle)
{
    if (VWIN(handle)->w_Window != WINDOW_NIL)
    {
	curr_win = VWIN(handle);
	curr_vw = curr_win->w_CurrVW;
    }
}

void
windows_init(void)
{
    window_type = rep_register_new_type ("window", 0, window_prin, window_prin,
					 window_sweep, window_mark,
					 window_mark_active, 0, 0, 0, 0,
					 window_bind, window_unbind);

    if(Fsymbol_value (Qbatch_mode, Qt) == Qnil)
	rep_message_fun = jade_message;

    rep_ADD_SUBR_INT(Smake_window);
    rep_ADD_SUBR_INT(Sdelete_window);
    rep_ADD_SUBR_INT(Ssleep_window);
    rep_ADD_SUBR_INT(Sunsleep_window);
    rep_ADD_SUBR_INT(Snext_window);
    rep_ADD_SUBR(Smessage);
    rep_ADD_SUBR(Sfont_name);
    rep_ADD_SUBR(Swindow_asleep_p);
    rep_ADD_SUBR(Sposition_window);
    rep_ADD_SUBR(Scurrent_window);
    rep_ADD_SUBR(Sset_current_window);
    rep_ADD_SUBR(Swindow_id);
    rep_ADD_SUBR(Sfont_dimensions);
    rep_ADD_SUBR(Swindow_dimensions);
    rep_ADD_SUBR(Swindow_list);
    rep_ADD_SUBR(Swindow_view_list);
    rep_ADD_SUBR(Swindow_view_count);
    rep_ADD_SUBR(Swindow_first_view);
    rep_ADD_SUBR(Swindowp);
    rep_ADD_SUBR_INT(Sset_font);
    rep_INTERN_SPECIAL(make_window_hook);
    rep_INTERN_SPECIAL(delete_window_hook);
    rep_INTERN(save_and_quit);
    rep_INTERN_SPECIAL(visible_bell);
    Fset (Qvisible_bell, Qnil);
    rep_INTERN_SPECIAL(visible_bell_length);
    Fset (Qvisible_bell_length, rep_MAKE_INT (250));
    rep_beep_fun = beep;

    rep_INTERN(dimensions);
    rep_INTERN(position);
    rep_INTERN(buffer);
    rep_INTERN(font);
}

void
windows_kill(void)
{
    WIN *w, *next;
    while(curr_win)
	delete_window(curr_win);
    w = win_chain;
    while(w)
    {
	next = w->w_Next;
	rep_FREE_CELL(w);
	w = next;
    }
    win_chain = NULL;
}
