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
#include "jade_protos.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

_PR void update_window_dimensions(WIN *w);
_PR void messagen(u_char *, int);
_PR void message(u_char *);
_PR void messagef(u_char *, ...);
_PR void no_message(WIN *);
_PR void reset_message(WIN *);
_PR bool remove_all_messages(bool from_idle_p);
_PR bool save_message(WIN *, u_char **, u_long *);
_PR void restore_message(WIN *, u_char *, u_long);
_PR void windows_init(void);
_PR void windows_kill(void);
_PR void window_sweep(void);
_PR void window_prin(VALUE, VALUE);

_PR VALUE sym_make_window_hook, sym_destroy_window_hook;
DEFSYM(make_window_hook, "make-window-hook");
DEFSYM(destroy_window_hook, "destroy-window-hook");

_PR VALUE sym_window_closed_hook;
DEFSYM(window_closed_hook, "window-closed-hook");

/* This can contain `dead' windows, ie w_Window==NULL, they have been
   close'd but must hang around until we're sure all refs are dead.  */
_PR   WIN	 *win_chain;

/* curr_win is the active window. When setting it's value curr_vw
   must be set to a view in the same window. */
_PR   WIN	 *curr_win;

_PR   int	  window_count;
_PR   bool  log_messages;

WIN   *win_chain, *curr_win;
int   window_count;
bool  log_messages;

_PR short def_dims[4];
short def_dims[4] = { 0, 0, 80, 24 };

static void
copy_win_prefs(WIN *dest, WIN *src)
{
    if(src)
    {
	dest->w_Flags = src->w_Flags;
	dest->w_FontName = src->w_FontName;
#ifdef HAVE_AMIGA
	dest->w_WindowSys.ws_FontSize = src->w_WindowSys.ws_FontSize;
	dest->w_WindowSys.ws_ScreenName = src->w_WindowSys.ws_ScreenName;
#endif
    }
    else
    {
	dest->w_FontName = def_font_str;
#ifdef HAVE_AMIGA
	dest->w_WindowSys.ws_FontSize = ami_def_font_size;
	dest->w_WindowSys.ws_ScreenName = ami_def_pub_screen;
#endif
    }
}

_PR VALUE cmd_make_window(VALUE xv, VALUE yv, VALUE wv, VALUE hv);
DEFUN("make-window", cmd_make_window, subr_make_window, (VALUE xv, VALUE yv, VALUE wv, VALUE hv), V_Subr4, DOC_make_window) /*
::doc:make_window::
make-window [X] [Y] [WIDTH] [HEIGHT]

Return a new window, it will be displaying the same buffer as the currently
active window.
::end:: */
{
    WIN *w;
    VALUE tx = curr_vw ? VAL(curr_vw->vw_Tx) : sym_nil;
    if(INTP(xv))
	def_dims[0] = VINT(xv);
    if(INTP(yv))
	def_dims[1] = VINT(yv);
    if(INTP(wv))
	def_dims[2] = VINT(wv);
    if(INTP(hv))
	def_dims[3] = VINT(hv);
    w = ALLOC_OBJECT(sizeof(WIN));
    if(w != NULL)
    {
	memset(w, 0, sizeof(WIN));
	w->w_Car = V_Window;
	copy_win_prefs(w, curr_win);
	if(sys_set_font(w))
	{
	    w->w_Window = sys_new_window(curr_win, w, TRUE);
	    if(w->w_Window)
	    {
		window_count++;
		sys_update_dimensions(w);
		update_window_dimensions(w);
		/* First the main view.. */
		if(w->w_Content != 0 && w->w_NewContent != 0
		   && make_view(NULL, w, VTX(tx), 0, FALSE))
		{
		    /* ..then the minibuffer view. */
		    if(make_view(NULL, w, NULL, 0, TRUE))
		    {
			w->w_Flags |= WINFF_FORCE_REFRESH;
			w->w_Next = win_chain;
			win_chain = w;
			if(curr_win == 0)
			{
			    curr_win = w;
			    curr_vw = w->w_CurrVW;
			}
			return(VAL(w));
		    }
		    kill_all_views(w);
		}
		if(w->w_NewContent)
		    free_glyph_buf(w->w_NewContent);
		if(w->w_Content)
		    free_glyph_buf(w->w_Content);
		sys_kill_window(w);
	    }
	    sys_unset_font(w);
	}
	FREE_OBJECT(w);
    }
    return LISP_NULL;
}

_PR VALUE cmd_destroy_window(VALUE win);
DEFUN("destroy-window", cmd_destroy_window, subr_destroy_window, (VALUE win), V_Subr1, DOC_destroy_window) /*
::doc:destroy_window::
destroy-window [WINDOW]

Close WINDOW (or the current window), if this was the last one all files in
memory are flushed and jade will exit.
::end:: */
{
    WIN *w = WINDOWP(win) ? VWIN(win) : curr_win;
    cmd_call_hook(sym_destroy_window_hook, LIST_1(VAL(w)), sym_nil);
    kill_all_views(w);
    sys_kill_window(w);
    sys_unset_font(w);
    free_glyph_buf(w->w_NewContent);
    free_glyph_buf(w->w_Content);
    w->w_NewContent = w->w_Content = NULL;
    window_count--;
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
		return(VAL(w));
	    }
	}
	w = win_chain;
	while(w && (w != curr_win))
	{
	    if(w->w_Window)
	    {
		curr_win = w;
		curr_vw = w->w_CurrVW;
		return(VAL(w));
	    }
	    w = w->w_Next;
	}
	/* No living windows left :-( we'll die soon :-(  */
	curr_win->w_CurrVW = NULL;
	curr_win = NULL;
	curr_vw = NULL;
	throw_value = cmd_cons(sym_quit, MAKE_INT(0)); /* experimental. */
	return LISP_NULL;
    }
    return(VAL(curr_win));
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

_PR VALUE cmd_sleep_window(VALUE win);
DEFUN_INT("sleep-window", cmd_sleep_window, subr_sleep_window, (VALUE win), V_Subr1, DOC_sleep_window, "") /*
::doc:sleep_window::
sleep-window [WINDOW]

Iconifies the current window.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    if(((VWIN(win)->w_Flags & WINFF_SLEEPING) == 0)
       && sys_sleep_win(VWIN(win)))
	return(win);
    return(sym_nil);
}

_PR VALUE cmd_unsleep_window(VALUE win);
DEFUN_INT("unsleep-window", cmd_unsleep_window, subr_unsleep_window, (VALUE win), V_Subr1, DOC_unsleep_window, "") /*
::doc:unsleep_window::
unsleep-window [WINDOW]

Uniconifies the current window.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    if((VWIN(win)->w_Flags & WINFF_SLEEPING) && sys_unsleep_win(VWIN(win)))
	return(win);
    return(sym_nil);
}

_PR VALUE cmd_next_window(VALUE win, VALUE activ);
DEFUN_INT("next-window", cmd_next_window, subr_next_window, (VALUE win, VALUE activ), V_Subr2, DOC_next_window, "!" DS_NL "p") /*
::doc:next_window::
next-window [WINDOW] [ACTIVATE]

Cycles through the open windows forwards.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win->w_Next);
    while(VWIN(win) != curr_win)
    {
	if(!win)
	    win = VAL(win_chain);
	if(VWIN(win)->w_Window)
	{
	    if(!NILP(activ))
	    {
		curr_win = VWIN(win);
		sys_activate_win(VWIN(win));
	    }
	    return(win);
	}
	win = VAL(VWIN(win)->w_Next);
    }
    return(VAL(curr_win));
}

void
messagen(u_char *title, int length)
{
    WIN *w = curr_win;
    if(log_messages)
    {
	fwrite(title, 1, length, stderr);
	fputc('\n', stderr);
    }
    if((w->w_Flags & WINFF_SLEEPING) == 0)
    {
	if(w->w_Message != NULL)
	    str_free(w->w_Message);
	w->w_Message = str_dupn(title, length);
	w->w_MessageLen = length;
	w->w_Flags |= WINFF_MESSAGE;
    }
}

void
message(u_char *msg)
{
    messagen(msg, strlen(msg));
}

void
messagef(u_char *fmt, ...)
{
    WIN *w = curr_win;
    va_list args;
    if((w->w_Flags & WINFF_SLEEPING) == 0)
    {
	u_char fmtbuff[256];
	u_long len;
	va_start(args, fmt);
	vsprintf(fmtbuff, fmt, args);
	va_end(args);
	if(log_messages)
	   fprintf(stderr, "%s\n", fmtbuff);
	if(w->w_Message != NULL)
	    str_free(w->w_Message);
	len = strlen(fmtbuff);
	w->w_Message = str_dupn(fmtbuff, len);
	w->w_MessageLen = len;
	w->w_Flags |= WINFF_MESSAGE;
    }
}

void
no_message(WIN *w)
{
    if(((w->w_Flags & WINFF_SLEEPING) == 0) && w->w_Message)
    {
	if(w->w_Message != NULL)
	    str_free(w->w_Message);
	w->w_Message = NULL;
	w->w_MessageLen = 0;
	w->w_Flags &= ~WINFF_MESSAGE;
    }
}

void
reset_message(WIN *w)
{
    if(w->w_Flags & WINFF_MESSAGE)
    {
	w->w_Flags &= ~WINFF_MESSAGE;
    }
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

/* Returns the currently-displayed message in OLD-MSGP and OLD-MSG-LENP,
   removing it from the window. Store the old message, reset it then
   reinstall it to display a temporary message. */   
bool
save_message(WIN *w, u_char **old_msgp, u_long *old_msg_lenp)
{
    if(w->w_Flags & WINFF_MESSAGE)
    {
	/* a message is being displayed. */
	*old_msgp = w->w_Message;
	*old_msg_lenp = w->w_MessageLen;
	w->w_Message = NULL;
	w->w_MessageLen = 0;
	return(TRUE);
    }
    else
    {
	*old_msgp = NULL;
	*old_msg_lenp = 0;
	return(FALSE);
    }
}

void
restore_message(WIN *w, u_char *old_msg, u_long old_msg_len)
{
    if(old_msg != NULL)
    {
	if(w->w_Message != NULL)
	    str_free(w->w_Message);
	w->w_Message = old_msg;
	w->w_MessageLen = old_msg_len;
	w->w_Flags |= WINFF_MESSAGE;
    }
}

_PR VALUE cmd_message(VALUE string, VALUE now);
DEFUN("message", cmd_message, subr_message, (VALUE string, VALUE now), V_Subr2, DOC_message) /*
::doc:message::
message STRING [DISPLAY-NOW]

Temporarily sets the status display to STRING, this won't happen until the
window is next refreshed unless DISPLAY-NOW is non-nil.
::end:: */
{
    DECLARE1(string, STRINGP);
    message(VSTR(string));
    if(!NILP(now))
	cmd_redisplay(sym_nil);
    return(string);
}

_PR VALUE cmd_font_name(VALUE win);
DEFUN("font-name", cmd_font_name, subr_font_name, (VALUE win), V_Subr1, DOC_font_name) /*
::doc:font_name::
font-name [WINDOW]

Returns the name of the font being used in this window.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return(VWIN(win)->w_FontName);
}

_PR VALUE cmd_window_asleep_p(void);
DEFUN("window-asleep-p", cmd_window_asleep_p, subr_window_asleep_p, (void), V_Subr0, DOC_window_asleep_p) /*
::doc:window_asleep_p::
window-asleep-p

Returns t if window is currently iconified.
::end:: */
{
    if(curr_win->w_Flags & WINFF_SLEEPING)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_window_count(void);
DEFUN("window-count", cmd_window_count, subr_window_count, (void), V_Subr0, DOC_window_count) /*
::doc:window_count::
window-count

Number of opened windows.
::end:: */
{
    return(MAKE_INT(window_count));
}

_PR VALUE cmd_position_window(VALUE left, VALUE top, VALUE width, VALUE height);
DEFUN("position-window", cmd_position_window, subr_position_window, (VALUE left, VALUE top, VALUE width, VALUE height), V_Subr4, DOC_position_window) /*
::doc:position_window::
position-window LEFT TOP WIDTH HEIGHT

Sets the position and dimensions of the current window. These are all
*pixel* measurememnts.
::end:: */
{
    DECLARE1(left, INTP);
    DECLARE2(top, INTP);
    DECLARE3(width, INTP);
    DECLARE4(height, INTP);
    sys_set_win_pos(curr_win, VINT(left), VINT(top),
		    VINT(width), VINT(height));
    return(sym_t);
}

_PR VALUE cmd_current_window(void);
DEFUN("current-window", cmd_current_window, subr_current_window, (void), V_Subr0,  DOC_current_window) /*
::doc:current_window::
current-window

Returns the currently active window. Note that this is the editor's notion
of `current' -- it doesn't necessarily mean that this is the window to which
your window system will send input events to.
::end:: */
{
    return(VAL(curr_win));
}

_PR VALUE cmd_with_window(VALUE args);
DEFUN("with-window", cmd_with_window, subr_with_window, (VALUE args), V_SF, DOC_with_window) /*
::doc:with_window::
with-window WINDOW FORMS...

Set the editor's current window to WINDOW and evaluate FORMS, then
reinstall the original window as the current one.
::end:: */
{
    if(CONSP(args))
    {
	GC_root gc_args;
	VALUE res;
	PUSHGC(gc_args, args);
	if((res = cmd_eval(VCAR(args))) && WINDOWP(res))
	{
	    VALUE oldwin = VAL(curr_win);
	    GC_root gc_oldwin;
	    curr_win = VWIN(res);
	    curr_vw = curr_win->w_CurrVW;

	    PUSHGC(gc_oldwin, oldwin);
	    res = cmd_progn(VCDR(args));
	    POPGC;

	    if(VWIN(oldwin)->w_Window != WINDOW_NIL)
	    {
		curr_win = VWIN(oldwin);
		curr_vw = curr_win->w_CurrVW;
	    }
	}
	else
	    res = signal_arg_error(res, 1);
	POPGC;
	return(res);
    }
    return LISP_NULL;
}

_PR VALUE cmd_set_current_window(VALUE win, VALUE activ);
DEFUN("set-current-window", cmd_set_current_window, subr_set_current_window, (VALUE win, VALUE activ), V_Subr2,  DOC_set_current_window) /*
::doc:set_current_window::
set-current-window WINDOW [ACTIVATE-P]

Sets the window which jade reguards as current.
If ACTIVATE-P is non-nil the window will be activated with respect to the
window-system (under X11 this means warping the pointer to the top left corner
of the window as well).
::end:: */
{
    DECLARE1(win, WINDOWP);
    curr_win = VWIN(win);
    curr_vw = curr_win->w_CurrVW;
    if(!NILP(activ))
	sys_activate_win(VWIN(win));
    return(VAL(curr_win));
}

_PR VALUE cmd_window_id(VALUE win);
DEFUN("window-id", cmd_window_id, subr_window_id, (VALUE win), V_Subr1, DOC_window_id) /*
::doc:window_id::
window-id [WINDOW]

Returns the identifier of the physical window that the Lisp window WINDOW
points to. This is window-system dependant, under X11 it will be some integer,
under Intuition a pointer (integer) to the window structure.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return(MAKE_LONG_INT((u_long)VWIN(win)->w_Window));
}

_PR VALUE cmd_font_x_size(VALUE win);
DEFUN("font-x-size", cmd_font_x_size, subr_font_x_size, (VALUE win), V_Subr1, DOC_font_x_size) /*
::doc:font_x_size::
font-x-size [WINDOW]

Returns the width of the window's font (in pixels).
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return(MAKE_INT((long)VWIN(win)->w_FontX));
}

_PR VALUE cmd_font_y_size(VALUE win);
DEFUN("font-y-size", cmd_font_y_size, subr_font_y_size, (VALUE win), V_Subr1, DOC_font_x_size) /*
::doc:font_y_size::
font-y-size [WINDOW]

Returns the height of the window's font (in pixels).
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return(MAKE_INT((long)VWIN(win)->w_FontY));
}

_PR VALUE cmd_window_dimensions(VALUE win);
DEFUN("window-dimensions", cmd_window_dimensions, subr_window_dimensions, (VALUE win), V_Subr2, DOC_window_dimensions) /*
::doc:window_dimensions::
window-dimensions [VIEW]

Returns (COLUMNS . ROWS) defining the size (in glyphs) of WINDOW (by default
the current window).
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return cmd_cons(MAKE_INT(VWIN(win)->w_MaxX),
		    MAKE_INT(VWIN(win)->w_MaxY));
}

_PR VALUE cmd_window_view_list(VALUE win);
DEFUN("window-view-list", cmd_window_view_list, subr_window_view_list, (VALUE win), V_Subr1, DOC_window_view_list) /*
::doc:window_view_list::
window-view-list [WINDOW]

Return a list of the views in WINDOW. The list will be ordered in top to
bottom order, ending with the mini-buffer.
::end:: */
{
    VW *vw;
    VALUE res = sym_nil;
    VALUE *ptr = &res;
    if(!WINDOWP(win))
	win = VAL(curr_win);
    for(vw = VWIN(win)->w_ViewList; vw != 0; vw = vw->vw_NextView)
    {
	if(!(*ptr = cmd_cons(VAL(vw), sym_nil)))
	    return LISP_NULL;
	ptr = &VCDR(*ptr);
    }
    return res;
}

_PR VALUE cmd_window_view_count(VALUE win);
DEFUN("window-view-count", cmd_window_view_count, subr_window_view_count, (VALUE win), V_Subr1, DOC_window_view_count) /*
::doc:window_view_count::
window-view-count [WINDOW]

Return the number of views of window WINDOW. This *will* include the
minibuffer view, whether it's active or not.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return MAKE_INT(VWIN(win)->w_ViewCount);
}

_PR VALUE cmd_window_first_view(VALUE win);
DEFUN("window-first-view", cmd_window_first_view, subr_window_first_view, (VALUE win), V_Subr1, DOC_window_first_view) /*
::doc:window_first_view::
window-first-view [WINDOW]

Returns the first view in WINDOW.
::end:: */
{
    if(!WINDOWP(win))
	win = VAL(curr_win);
    return VAL(VWIN(win)->w_ViewList);
}

void
windows_init(void)
{
    ADD_SUBR(subr_make_window);
    ADD_SUBR(subr_destroy_window);
    ADD_SUBR_INT(subr_sleep_window);
    ADD_SUBR_INT(subr_unsleep_window);
    ADD_SUBR_INT(subr_next_window);
    ADD_SUBR(subr_message);
    ADD_SUBR(subr_font_name);
    ADD_SUBR(subr_window_asleep_p);
    ADD_SUBR(subr_window_count);
    ADD_SUBR(subr_position_window);
    ADD_SUBR(subr_current_window);
    ADD_SUBR(subr_set_current_window);
    ADD_SUBR(subr_with_window);
    ADD_SUBR(subr_window_id);
    ADD_SUBR(subr_font_x_size);
    ADD_SUBR(subr_font_y_size);
    ADD_SUBR(subr_window_dimensions);
    ADD_SUBR(subr_window_view_list);
    ADD_SUBR(subr_window_view_count);
    ADD_SUBR(subr_window_first_view);
    INTERN(make_window_hook);
    INTERN(destroy_window_hook);
    INTERN(window_closed_hook);
}

void
windows_kill(void)
{
    WIN *w, *next;
    while(curr_win)
	cmd_destroy_window(VAL(curr_win));
    w = win_chain;
    while(w)
    {
	next = w->w_Next;
	FREE_OBJECT(w);
	w = next;
    }
    win_chain = NULL;
}

void
window_sweep(void)
{
    WIN *w = win_chain;
    win_chain = NULL;
    while(w)
    {
	WIN *next = w->w_Next;
	if(GC_CELL_MARKEDP(VAL(w)))
	{
	    GC_CLR_CELL(VAL(w));
	    w->w_Next = win_chain;
	    win_chain = w;
	}
	else
	    FREE_OBJECT(w);
	w = next;
    }
}

void
window_prin(VALUE strm, VALUE win)
{
    u_char buf[40];
    if(VWIN(win)->w_Window)
    {
#ifdef HAVE_X11
	sprintf(buf, "#<window %ld", VWIN(win)->w_Window);
#else
	sprintf(buf, "#<window 0x%x", VWIN(win)->w_Window);
#endif
	stream_puts(strm, buf, -1, FALSE);
	stream_putc(strm, '>');
    }
    else
	stream_puts(strm, "#<dead-window>", -1, FALSE);
}
