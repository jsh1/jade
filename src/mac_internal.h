/* mac_internal.h -- internal declarations for Mac OS X
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef JADE_MAC_INTERNAL_H
#define JADE_MAC_INTERNAL_H

#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>
#import <ApplicationServices/ApplicationServices.h>
#undef bool

/* Definitions for the jade view.

   Conceptually, this is a bit of a mess. The WIN and GtkJade structures
   have to be inextricably linked, so that they're both ``above'' each
   other depending on the point of view. */

@interface JadeView : NSView
{
@public
    struct _WIN *_win;

    CGFontRef _font;
    CGFontRef _bold_font;
    CGFloat _font_size;
    CGFloat _font_ascent;
    CGFloat _font_descent;
    CGFloat _font_leading;
    CGFloat _font_width;

    CGGlyph _glyph_table[256];

    u_int _has_focus :1;
};
@end

/* Use these macros to wrap Objective C within C modules. */

#define OBJC_BEGIN \
  { NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; \
    @try {

#define OBJC_END \
    } @catch (NSException *exc) { \
      NSLog (@"Jade: ignoring exception: %@", exc); \
    } \
    [pool drain]; }

#endif /* JADE_MAC_INTERNAL_H */
