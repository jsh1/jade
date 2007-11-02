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


@implementation JadeView

static int
flip_y (JadeView *view, int y)
{
    return view->_bounds.size.height - y;
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
    return NO;
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

    /* FIXME: pull from font name? */
    _font_size = 10;

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

    return 1;
}

- (BOOL)windowShouldClose:(id)sender
{
    rep_call_with_barrier (Fdelete_window, rep_VAL (_win), rep_TRUE, 0, 0, 0);
    mac_callback_postfix ();
    return NO;
}

- (BOOL)acceptsFirstResponder
{
    return YES;
}

- (BOOL)becomeFirstResponder
{
    _has_focus = 1;
    if (_win != 0 && _win->w_CurrVW != 0)
	Fredisplay_window (rep_VAL(_win), Qnil);
    return YES;
}

- (BOOL)resignFirstResponder
{
    _has_focus = 0;
    if (_win != 0 && _win->w_CurrVW != 0)
	Fredisplay_window (rep_VAL(_win), Qnil);
    return YES;
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

    Fredisplay_window (rep_VAL (_win), Qnil);
}

- (void)handleEvent:(NSEvent *)e
{
    NSPoint p;
    u_long code = 0, mods = 0;
    bool redisplay;

    p = [self convertPoint:[e locationInWindow] fromView:nil];
    p.y = flip_y (self, p.y);

    redisplay = update_mouse_extent (_win, p.x, p.y);

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
	mac_callback_postfix ();
	redisplay = true;
    }

    if (redisplay)
	Fredisplay (Qnil);
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
- (void)keyUp:(NSEvent *)e {[self handleEvent:e];}

- (void)windowDidResize:(NSNotification *)n
{
    sys_update_dimensions (_win);
    update_window_dimensions (_win);

    if (_tracking_tag != 0)
	[self removeTrackingRect:_tracking_tag];

    _tracking_tag = [self addTrackingRect:[self bounds]
		     owner:self userData:nil assumeInside:NO];
}

- (void)mouseEntered:(NSEvent *)e
{
    if (!_inside)
    {
	[ibeam_cursor push];
	_inside = true;
    }
}

- (void)mouseExited:(NSEvent *)e
{
    if (_inside)
    {
	[ibeam_cursor pop];
	_inside = false;
    }
}

@end /* JadeView */


/* Low level drawing */

void
sys_begin_redisplay (WIN *w)
{
    JadeView *view = w->w_Window;

    OBJC_BEGIN
    [view lockFocus];
    OBJC_END
}

void
sys_end_redisplay (WIN *w)
{
    JadeView *view = w->w_Window;

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
    CGColorRef c;
    CGFontRef font;
    bool invert;
    Merged_Face *f;
    CGRect r;
    CGPoint *pt;
    CGGlyph *glyphs;
    int i, x, y;

    if (view == nil || !sys_window_realized (w))
	return;

    assert(attr <= GA_LastFace);

    f = &w->w_MergedFaces[attr];
    if(!f->valid)
	return;

    invert = (f->car & FACEFF_INVERT) != 0;
    
    x = w->w_LeftPix + w->w_FontX * col;
    y = flip_y (view, w->w_TopPix + w->w_FontY * row);

    ctx = [[NSGraphicsContext currentContext] graphicsPort];
    if (ctx == NULL)
	return;

    CGContextSaveGState (ctx);
    CGContextSetShouldAntialias (ctx, view->_antialias);

    c = VCOLOR(invert ? f->foreground : f->background)->color;
    if (c != 0)
	CGContextSetFillColorWithColor (ctx, c);
    CGContextSetBlendMode (ctx, kCGBlendModeCopy);
    r.size.width = len * w->w_FontX; r.size.height = w->w_FontY;
    r.origin.x = x; r.origin.y = y - r.size.height;
    CGContextFillRect (ctx, r);
    CGContextSetBlendMode (ctx, kCGBlendModeNormal);

    c = VCOLOR(invert ? f->background : f->foreground)->color;
    if (c != 0)
	CGContextSetFillColorWithColor (ctx, c);

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
	    glyphs[i] = view->_glyph_table[(unsigned int)str[i]];
	}

	CGContextSetTextMatrix (ctx, CGAffineTransformIdentity);
	CGContextShowGlyphsAtPositions (ctx, glyphs, pt, len);
    }

    if(f->car & FACEFF_UNDERLINE)
    {
	CGFloat ly = flip_y (view, y - view->_font_ascent) - .5;
	CGContextBeginPath (ctx);
	CGContextSetLineWidth (ctx, 1.);
	CGContextMoveToPoint (ctx, x + .5, ly);
	CGContextAddLineToPoint (ctx, x + len * w->w_FontX - .5, ly);
	CGContextStrokePath (ctx);
    }

    if(f->car & FACEFF_BOXED)
    {
	int i;
	r.size.width = w->w_FontX - 1;
	r.size.height = w->w_FontY - 1;
	r.origin.x = x;
	r.origin.y = flip_y (view, y) - r.size.height;
	CGContextSetLineWidth (ctx, 1.);
	for(i = 0; i < len; i++)
	{
	    CGContextStrokeRect (ctx, r);
	    r.origin.x += w->w_FontX;
	}
    }

    CGContextRestoreGState (ctx);
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
    /* FIXME: NSCursor does support this. */
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
    JadeView *view = 0;

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
    [view setFont];

    if (curr_win != 0)
	view->_antialias = ((JadeView *)curr_win->w_Window)->_antialias;

    window = [[NSWindow alloc] initWithContentRect:
	      NSMakeRect (x, y, width * w->w_FontX, height * w->w_FontY)
	      styleMask:NSTitledWindowMask | NSClosableWindowMask
	      | NSMiniaturizableWindowMask | NSResizableWindowMask
	      backing:NSBackingStoreBuffered defer:NO];
    [view setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
    [window setContentView:view];
    [window setReleasedWhenClosed:YES];
    [window setOpaque:NO];
    [window setDelegate:view];
    [[NSNotificationCenter defaultCenter] addObserver:view
     selector:@selector(windowDidResize:)
     name:NSWindowDidResizeNotification object:view];

    [view windowDidResize:nil];

    if (!new_window_no_show)
	[window makeKeyAndOrderFront:view];

    w->w_Window = view;

    [view release];			/* window retains it */

    OBJC_END

    return w->w_Window;
}

void
sys_kill_window(WIN *w)
{
    OBJC_BEGIN
    [[(JadeView *)w->w_Window window] close];
    OBJC_END
    w->w_Window = WINDOW_NIL;
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
    if (win->w_Window == 0)
	return FALSE;
    else
	return TRUE;
}

int
sys_window_has_focus (WIN *win)
{
    JadeView *view = win->w_Window;
    return view->_has_focus;
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

DEFUN("mac-set-antialias", Fmac_set_antialias, Smac_set_antialias, (repv win, repv state), rep_Subr2) /*
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


/* Initialisation */

void
sys_windows_init(void)
{
    ibeam_cursor = [NSCursor IBeamCursor];
    rep_ADD_SUBR (Sflush_output);
    rep_ADD_SUBR (Smac_set_antialias);
    mac_runloop_init ();
}
