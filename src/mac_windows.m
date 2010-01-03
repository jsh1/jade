/* mac_windows.m -- the Mac OS X window management
   Copyright (C) 1999-2007 John Harper <jsh@unfactored.org>

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
#include "mac_internal.h"
#include <string.h>
#include <assert.h>

#import <CoreText/CoreText.h>

static NSCursor *ibeam_cursor = 0;

/* When true, sys_new_window _won't_ show the window. */
static bool new_window_no_show = FALSE;

static int window_count;


@implementation JadeView

static inline int
flip_y (JadeView *view, int y)
{
    return view->_bounds.size.height - y;
}

- (id)initWithFrame:(NSRect)r
{
    _font_size = 10;
    return [super initWithFrame:r];
}

- (void)dealloc
{
    if (_font != 0)
	CGFontRelease (_font);
    if (_bold_font != 0)
	CGFontRelease (_bold_font);

    [[NSNotificationCenter defaultCenter] removeObserver:self];

    [super dealloc];
}

- (BOOL)isFlipped
{
    return NO;
}

- (BOOL)isOpaque
{
    /* we may not be opaque, but we always draw our background in copy
       mode, so we don't need AppKit to clear anything for us. */

    return YES;
}

- (int)setFont
{
    CGFontRef font;
    int i, advance;
    CFStringRef string;
    CGFloat units;
    UniChar uc[256];

    string = CFStringCreateWithCString (NULL, rep_STR (_win->w_FontName),
					kCFStringEncodingUTF8);
    font = CGFontCreateWithFontName (string);
    CFRelease (string);

    if (font == 0)
	font = CGFontCreateWithFontName (CFSTR (DEFAULT_FONT));
    if (font == 0)
	return 0;

    if (_font != 0)
	CGFontRelease (_font);

    _font = font;

    if (_bold_font != 0)
	CGFontRelease (_bold_font);
    /* FIXME: something else here. */
    _bold_font = CGFontRetain (_font);

    for (i = 0; i < 256; i++)
	uc[i] = i;

    CTFontRef ct_font = CTFontCreateWithGraphicsFont(font, 0, NULL, NULL);
    CTFontGetGlyphsForCharacters (ct_font, uc, _glyph_table, 256);
    CFRelease(ct_font);

    units = _font_size / CGFontGetUnitsPerEm (_font);

    CGFontGetGlyphAdvances (_font, &_glyph_table['M'], 1, &advance);

    _font_width = advance * units;
    _font_ascent = CGFontGetAscent (_font) * units;
    _font_descent = -CGFontGetDescent (_font) * units;
    _font_leading = CGFontGetLeading (_font) * units;
    
    _win->w_FontX = round (_font_width);
    _win->w_FontY = round (_font_ascent + _font_descent + _font_leading);

    if (_win->w_FontX <= 0)
	_win->w_FontX = 1;
    if (_win->w_FontY <= 0)
	_win->w_FontY = 1;

    if (sys_window_realized (_win))
    {
	sys_update_dimensions (_win);
	update_window_dimensions (_win);
    }

    [[self window] setContentResizeIncrements:
     NSMakeSize (_win->w_FontX, _win->w_FontY)];

    return 1;
}

- (BOOL)windowShouldClose:(id)sender
{
    curr_win = _win;
    curr_vw = _win->w_CurrVW;
    rep_call_with_barrier (Fdelete_window, Qnil, rep_TRUE, 0, 0, 0);
    mac_needs_redisplay = true;
    return NO;
}

- (BOOL)acceptsFirstResponder
{
    return YES;
}

- (void)windowDidBecomeKey:(NSNotification *)n
{
    _has_focus = TRUE;
    mac_needs_redisplay = true;
}

- (void)windowDidResignKey:(NSNotification *)n
{
    _has_focus = FALSE;
    mac_needs_redisplay = true;
}

- (void)drawRect:(NSRect)r
{
    int x, y, width, height;

    if (_win->w_Window == WINDOW_NIL)
	return;				/* still initializing */

    x = (r.origin.x - _win->w_LeftPix) / _win->w_FontX;
    y = (r.origin.y - _win->w_TopPix) / _win->w_FontY;
    /* Why +2? It seems to be necessary.. */
    width = (r.size.width / _win->w_FontX) + 2;
    height = (r.size.height / _win->w_FontY) + 2;

    garbage_glyphs(_win, x, y, width, height);

    mac_needs_redisplay = true;
}

- (void)handleEvent:(NSEvent *)e
{
    NSPoint p;
    u_long code = 0, mods = 0;

    /* May need to defer this event until latr. */

    if (mac_defer_event (self, e))
	return;

    curr_win = _win;
    curr_vw = _win->w_CurrVW;

    p = [self convertPoint:[e locationInWindow] fromView:nil];
    p.y = flip_y (self, p.y);

    update_mouse_extent (_win, p.x, p.y);

    sys_translate_event (&code, &mods, e);

    if(mods & EV_TYPE_MASK
       /* Don't pass modifier-less motion-events through */
       && ((mods & EV_TYPE_MASK) != EV_TYPE_MOUSE
	   || code != EV_CODE_MOUSE_MOVE
	   || (mods & EV_MOD_BUTTON_MASK) != 0))
    {
	if(curr_win != _win)
	    curr_vw = _win->w_CurrVW;
	curr_win = _win;
	reset_message(_win);
	eval_input_event(e, code, mods);
	undo_end_of_command();
    }

    mac_needs_redisplay = true;
}

- (void)mouseMoved:(NSEvent *)e {[self handleEvent:e];}
- (void)mouseDragged:(NSEvent *)e {[self handleEvent:e];}
- (void)rightMouseDragged:(NSEvent *)e {[self handleEvent:e];}
- (void)otherMouseDragged:(NSEvent *)e {[self handleEvent:e];}
- (void)scrollWheel:(NSEvent *)e {[self handleEvent:e];}
- (void)mouseDown:(NSEvent *)e {[self handleEvent:e];}
- (void)mouseUp:(NSEvent *)e {[self handleEvent:e];}
- (void)rightMouseDown:(NSEvent *)e {[self handleEvent:e];}
- (void)rightMouseUp:(NSEvent *)e {[self handleEvent:e];}
- (void)otherMouseDown:(NSEvent *)e {[self handleEvent:e];}
- (void)otherMouseUp:(NSEvent *)e {[self handleEvent:e];}
- (void)keyDown:(NSEvent *)e {[self handleEvent:e];}

- (void)windowDidResize:(NSNotification *)n
{
    sys_update_dimensions (_win);
    update_window_dimensions (_win);

    if (_tracking_tag != 0)
	[self removeTrackingRect:_tracking_tag];

    _tracking_tag = [self addTrackingRect:[self bounds]
		     owner:self userData:nil assumeInside:NO];

    mac_needs_redisplay = true;
}

- (void)mouseEntered:(NSEvent *)e
{
    if (!_inside)
    {
	[ibeam_cursor push];
	_inside = TRUE;
    }
}

- (void)mouseExited:(NSEvent *)e
{
    if (_inside)
    {
	[ibeam_cursor pop];
	_inside = FALSE;
    }
}


/* Pasteboard handling. */

static NSPasteboard *_pasteboard;
static NSArray *_pasteboard_types;
static repv _pasteboard_data, _pasteboard_start, _pasteboard_end;

- (void)pasteboard:(NSPasteboard *)sender provideDataForType:(NSString *)type
{
    NSString *string = nil;

    if (rep_STRINGP (_pasteboard_data))
    {
	string = [[NSString alloc] initWithUTF8String:
		  rep_STR (_pasteboard_data)];
    }
    else if (BUFFERP (_pasteboard_data))
    {
	if (check_section (VTX(_pasteboard_data),
			   &_pasteboard_start, &_pasteboard_end))
	{
	    long tlen;
	    char *str;

	    tlen = section_length (VTX (_pasteboard_data),
				   _pasteboard_start, _pasteboard_end);
	    str = rep_alloc (tlen + 1);

	    if (str != NULL)
	    {
		copy_section (VTX (_pasteboard_data),
			      _pasteboard_start, _pasteboard_end, str);
		str[tlen] = 0;
		string = [[NSString alloc] initWithUTF8String:str];
		rep_free (str);
	    }
	}
    }

    [sender setString:string forType:NSStringPboardType];
    [string release];
}

- (void)pasteboardChangedOwner:(NSPasteboard *)sender
{
    _pasteboard_data = Qnil;
    _pasteboard_start = _pasteboard_end = Qnil;
}

DEFUN("mac-set-pasteboard", Fmac_set_pasteboard, Smac_set_pasteboard, (repv start, repv end, repv buffer), rep_Subr3) /*
::doc:x11-set-selection::
mac-set-pasteboard [ STRING | START END [BUFFER] ]
::end:: */
{
    if (start == Qnil)
    {
	if (_pasteboard_data != Qnil)
	    [_pasteboard declareTypes:[NSArray array] owner:nil];

	_pasteboard_data = Qnil;
	_pasteboard_start = _pasteboard_end = Qnil;

	return Qt;
    }

    if (!rep_STRINGP (start))
    {
	rep_DECLARE2 (start, POSP);
	rep_DECLARE3 (end, POSP);
	if (!BUFFERP (buffer))
	    buffer = rep_VAL (curr_vw->vw_Tx);
    }

    OBJC_BEGIN

    if (_pasteboard == nil)
    {
	_pasteboard = [[NSPasteboard generalPasteboard] retain];
	_pasteboard_types = [[NSArray alloc] initWithObjects:
			     NSStringPboardType, nil];
    }

    [_pasteboard declareTypes:_pasteboard_types owner:curr_win->w_Window];

    OBJC_END

    if (rep_STRINGP (start))
    {
	_pasteboard_data = start;
	_pasteboard_start = _pasteboard_end = Qnil;
    }
    else
    {
	_pasteboard_data = buffer;
	_pasteboard_start = start;
	_pasteboard_end = end;
    }

    return Qt;
}

DEFUN("mac-get-pasteboard", Fmac_get_pasteboard, Smac_get_pasteboard, (repv sel), rep_Subr0) /*
::doc:mac-get-pasteboard::
mac-get-pasteboard
::end:: */
{
    NSString *string;
    repv ret = Qnil;

    OBJC_BEGIN

    if (_pasteboard == nil)
    {
	_pasteboard = [[NSPasteboard generalPasteboard] retain];
	_pasteboard_types = [[NSArray alloc] initWithObjects:
			     NSStringPboardType, nil];
    }

    string = [_pasteboard stringForType:NSStringPboardType];

    if (string != nil)
	ret = rep_string_dup ([string UTF8String]);

    OBJC_END

    return ret;
}

@end /* JadeView */


/* Low level drawing */

static CGContextRef current_context;

void
sys_begin_redisplay (WIN *w)
{
    JadeView *view = w->w_Window;

    OBJC_BEGIN
    [view lockFocus];
    current_context = [[NSGraphicsContext currentContext] graphicsPort];
    OBJC_END

    CGContextSaveGState (current_context);
    CGContextSetShouldAntialias (current_context, view->_antialias);
    CGContextSetTextMatrix (current_context, CGAffineTransformIdentity);
    CGContextSetLineWidth (current_context, (CGFloat) 1.);
}

void
sys_end_redisplay (WIN *w)
{
    JadeView *view = w->w_Window;

    CGContextRestoreGState (current_context);
    current_context = NULL;

    OBJC_BEGIN
    [view unlockFocus];
    [[view window] flushWindow];
    OBJC_END
}

void
sys_draw_glyphs(WIN *w, int col, int row, glyph_attr attr, char *str,
		int len, bool all_spaces)
{
    JadeView *view = w->w_Window;
    CGContextRef ctx;
    CGFontRef font;
    Merged_Face *f;
    CGRect r;
    CGPoint *pt;
    CGGlyph *glyphs;
    int i, x, y;
    const struct mac_color *fg_color, *bg_color;

    if (view == nil || !sys_window_realized (w))
	return;

    assert(attr <= GA_LastFace);

    f = &w->w_MergedFaces[attr];
    if(!f->valid)
	return;

    if (!(f->car & FACEFF_INVERT))
    {
	fg_color = &VCOLOR (f->foreground)->color;
	bg_color = &VCOLOR (f->background)->color;
    }
    else
    {
	fg_color = &VCOLOR (f->background)->color;
	bg_color = &VCOLOR (f->foreground)->color;
    }

    x = w->w_LeftPix + w->w_FontX * col;
    y = flip_y (view, w->w_TopPix + w->w_FontY * row);

    ctx = current_context;
    if (ctx == NULL)
	return;

    if (bg_color->cg_color != NULL)
    {
	CGContextSetFillColorWithColor (ctx, bg_color->cg_color);

	if (view->_opaque && CGColorGetAlpha (bg_color->cg_color) < 1)
	{
	    view->_opaque = false;
	    [[view window] setOpaque:NO];
	}
    }

    CGContextSetBlendMode (ctx, kCGBlendModeCopy);
    r.size.width = len * w->w_FontX; r.size.height = w->w_FontY;
    r.origin.x = x; r.origin.y = y - r.size.height;
    CGContextFillRect (ctx, r);
    CGContextSetBlendMode (ctx, kCGBlendModeNormal);

    if (fg_color->cg_color != NULL)
    {
	CGContextSetFillColorWithColor (ctx, fg_color->cg_color);
	CGContextSetStrokeColorWithColor (ctx, fg_color->cg_color);

	if (view->_opaque && CGColorGetAlpha (fg_color->cg_color) < 1)
	{
	    view->_opaque = false;
	    [[view window] setOpaque:NO];
	}
    }

    if(!all_spaces)
    {
	if((f->car & FACEFF_BOLD) && view->_bold_font)
	    font = view->_bold_font;
	else
	    font = view->_font;

	if (font != 0)
	{
	    CGContextSetFont (ctx, font);
	    CGContextSetFontSize (ctx, view->_font_size);
	}

	pt = alloca (sizeof (*pt) * len);
	glyphs = alloca (sizeof (*glyphs) * len);

	for (i = 0; i < len; i++)
	{
	    pt[i].x = x + w->w_FontX * i;
	    pt[i].y = y - view->_font_ascent;
	    glyphs[i] = view->_glyph_table[((u_char *)str)[i]];
	}

	CGContextShowGlyphsAtPositions (ctx, glyphs, pt, len);
    }

    if(f->car & FACEFF_UNDERLINE)
    {
	CGFloat ly = y - view->_font_ascent - .5f;
	CGContextBeginPath (ctx);
	CGContextMoveToPoint (ctx, x + .5f, ly);
	CGContextAddLineToPoint (ctx, x + len * w->w_FontX - .5f, ly);
	CGContextStrokePath (ctx);
    }

    if(f->car & FACEFF_BOXED)
    {
	r.size.width = w->w_FontX - 1;
	r.size.height = w->w_FontY - 1;
	r.origin.x = x + .5f;
	r.origin.y = y - r.size.height - .5f;
	for(i = 0; i < len; i++)
	{
	    CGContextStrokeRect (ctx, r);
	    r.origin.x += w->w_FontX;
	}
    }
}

void
sys_copy_glyphs (WIN *win, int x1, int y1, int w, int h, int x2, int y2)
{
    JadeView *view = win->w_Window;
    NSRect r;
    NSSize off;

    r.size.width = w * win->w_FontX;
    r.size.height = h * win->w_FontY;
    r.origin.x = win->w_LeftPix + win->w_FontX * x1;
    r.origin.y = flip_y (view, win->w_TopPix + win->w_FontY * y1) - r.size.height;

    off.width = win->w_LeftPix + win->w_FontX * (x2 - x1);
    off.height = win->w_TopPix + win->w_FontY * (y2 - y1);
    off.height = -off.height;

    OBJC_BEGIN
    [view scrollRect:r by:off];
    OBJC_END
}


/* System-dependent jade functions */

void
sys_recolor_cursor(repv face)
{
    /* FIXME: NSCursor does not support this. */
}

void
sys_update_dimensions(WIN *w)
{
    if(w->w_Window && ((w->w_Flags & WINFF_SLEEPING) == 0))
    {
	NSRect r = [(JadeView *)w->w_Window bounds];
	int width = r.size.width, height = r.size.height;
	w->w_LeftPix = 0;
	w->w_TopPix = 0;
	w->w_RightPix = width;
	w->w_BottomPix = height;
	w->w_WidthPix = w->w_RightPix - w->w_LeftPix;
	w->w_HeightPix = w->w_BottomPix - w->w_TopPix;
    }
}

/* The only thing necessary in W is the font stuff (I think) */

void *
sys_new_window(WIN *oldW, WIN *w, short *dims)
{
    unsigned int x = -1, y = -1, width = 80, height = 24;
    NSWindow *window;
    JadeView *view = 0, *old_view;

    if (batch_mode_p ())
	new_window_no_show = TRUE;

    if(dims[0] >= 0)
	x = dims[0];
    if(dims[1] >= 0)
	y = dims[1];
    if(dims[2] > 0)
	width = dims[2];
    if(dims[3] > 0)
	height = dims[3];

    OBJC_BEGIN

    view = [[JadeView alloc] initWithFrame:NSMakeRect (0, 0, 1, 1)];
    view->_win = w;
    view->_opaque = true;

    old_view = oldW ? oldW->w_Window : 0;
    if (old_view)
	view->_font_size = old_view->_font_size;

    [view setFont];

    if (curr_win != 0)
	view->_antialias = ((JadeView *)curr_win->w_Window)->_antialias;

    window = [[NSWindow alloc] initWithContentRect:
	      NSMakeRect (x, y, width * w->w_FontX, height * w->w_FontY)
	      styleMask:NSTitledWindowMask | NSClosableWindowMask
	      | NSMiniaturizableWindowMask | NSResizableWindowMask
	      backing:NSBackingStoreBuffered defer:YES];
    [view setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
    [window setContentView:view];
    [window setReleasedWhenClosed:YES];
    [window setOpaque:YES];
    [window setDelegate:(id)view];
    [window setContentResizeIncrements:NSMakeSize (w->w_FontX, w->w_FontY)];
    [[NSNotificationCenter defaultCenter] addObserver:view
     selector:@selector(windowDidResize:)
     name:NSWindowDidResizeNotification object:view];
    [[NSNotificationCenter defaultCenter] addObserver:view
     selector:@selector(windowDidBecomeKey:)
     name:NSWindowDidBecomeKeyNotification object:view];
    [[NSNotificationCenter defaultCenter] addObserver:view
     selector:@selector(windowDidResignKey:)
     name:NSWindowDidResignKeyNotification object:view];

    [view windowDidResize:nil];

    if (!new_window_no_show)
	[window makeKeyAndOrderFront:view];

    w->w_Window = view;
    [view release];			/* window retains it */
    window_count++;

    OBJC_END

    return w->w_Window;
}

void
sys_kill_window(WIN *w)
{
    if (w->w_Window == 0)
	return;

    OBJC_BEGIN
    [[(JadeView *)w->w_Window window] close];
    OBJC_END

    w->w_Window = WINDOW_NIL;
    window_count--;
}

int
sys_sleep_win(WIN *w)
{
    OBJC_BEGIN
    [[(JadeView *)w->w_Window window] miniaturize:nil];
    OBJC_END
    return 1;
}

int
sys_unsleep_win(WIN *w)
{
    OBJC_BEGIN
    [[(JadeView *)w->w_Window window] deminiaturize:nil];
    OBJC_END
    return 1;
}

int
sys_set_font(WIN *w)
{
    int ret = 0;

    if (w->w_Window == WINDOW_NIL)
	ret = 1;
    else
    {
	OBJC_BEGIN
	ret = [(JadeView *)w->w_Window setFont];
	OBJC_END
    }

    return ret;
}

void
sys_unset_font(WIN *w)
{
}

void
sys_activate_win(WIN *w)
{
    OBJC_BEGIN
    [[(JadeView *)w->w_Window window] makeKeyAndOrderFront:nil];
    OBJC_END
}

void
sys_set_win_pos(WIN *win, long x, long y, long w, long h)
{
    NSWindow *window;
    OBJC_BEGIN
    window = [(JadeView *)win->w_Window window];
    [window setFrameOrigin:NSMakePoint (x, y)];
    [window setContentSize:NSMakeSize (w, h)];
    OBJC_END
}

void
sys_set_win_name(WIN *win, char *name)
{
    NSWindow *w;
    CFStringRef str;

    OBJC_BEGIN
    w = [(JadeView *)win->w_Window window];
    str = CFStringCreateWithCString (NULL, name, kCFStringEncodingUTF8);
    [w setTitle:(id)str];
    CFRelease (str);
    OBJC_END
}

bool
sys_deleting_window_would_exit (WIN *win)
{
    return window_count == 1;
}

int
sys_window_has_focus (WIN *win)
{
    JadeView *view = win->w_Window;
    return mac_app_is_active && view->_has_focus;
}

int
sys_window_realized (WIN *win)
{
    JadeView *view = win->w_Window;
    return [view window] != nil;
}

repv
sys_get_mouse_pos(WIN *w)
{
    JadeView *v = w->w_Window;
    NSPoint p = [v convertPoint:[[[v window] currentEvent]
				 locationInWindow] fromView:nil];
    p.y = flip_y (v, p.y);
    return make_pos (((int) p.x - w->w_LeftPix) / w->w_FontX,
		     ((int) p.y - w->w_TopPix) / w->w_FontY);
}


/* Some Lisp functions */

DEFUN("flush-output", Fflush_output, Sflush_output, (void), rep_Subr0) /*
::doc:flush-output::
flush-output

Forces any cached window output to be drawn. This is usually unnecessary.
::end:: */
{
    /* FIXME: anything here? */
    return Qt;
}

DEFUN_INT("mac-set-antialias", Fmac_set_antialias, Smac_set_antialias, (repv win, repv state), rep_Subr2, "\nP") /*
::doc:mac-set-antialias::
mac-set-antialias [WIN] [STATE]
::end:: */
{
    JadeView *view;

    if (win == Qnil)
	win = rep_VAL (curr_win);

    view = VWIN (win)->w_Window;
    view->_antialias = state != Qnil;

    Fredisplay (Qt);
    return Qt;
}

DEFUN_INT("mac-set-font-size", Fmac_set_font_size, Smac_set_font_size, (repv win, repv fontsize), rep_Subr2, "\nNFont size:") /*
::doc:mac-set-font-size::
mac-set-font-size WIN FONT-SIZE
::end:: */
{
    JadeView *view;

    if (win == Qnil)
	win = rep_VAL (curr_win);

    rep_DECLARE2 (fontsize, rep_INTP);

    view = VWIN (win)->w_Window;
    view->_font_size = rep_INT (fontsize);

    sys_set_font (VWIN (win));
    Fredisplay (Qt);
    return Qt;
}


/* Initialisation */

void
sys_windows_init(void)
{
    ibeam_cursor = [NSCursor IBeamCursor];
    rep_ADD_SUBR (Sflush_output);
    rep_ADD_SUBR_INT (Smac_set_antialias);
    rep_ADD_SUBR (Smac_set_pasteboard);
    rep_ADD_SUBR (Smac_get_pasteboard);
    rep_ADD_SUBR_INT (Smac_set_font_size);
    rep_mark_static (&_pasteboard_data);
    rep_mark_static (&_pasteboard_start);
    rep_mark_static (&_pasteboard_end);
    mac_runloop_init ();
}
