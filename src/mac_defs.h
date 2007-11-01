/* mac_defs.h -- Declarations for Mac OS X
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

#ifndef JADE_MAC_DEFS_H
#define JADE_MAC_DEFS_H

/* standard font */
#define DEFAULT_FONT "Monaco"

/* Definitions for Lisp WIN object */

#define W_WindowSys		void*
#define w_Window		w_WindowSys
#define WINDOW_NIL		(0)

#define WINDOW_META(w)		mac_meta_mod
#define WINDOW_HAS_FOCUS(w)	sys_window_has_focus (w)

/* The batch-mode stuff here is an ugly hack. The problem is that
   in batch-mode we never show the window, so it will be collected,
   leaving us without a curr_win at all---not a happy situation, so.. */
#define WINDOW_NON_COLLECTABLE(w) 				\
    (batch_mode_p () || ((w)->w_Window && sys_window_realized (w)))

/* Macros for drawing operations. These are used in redisplay.c for
   system-independent rendering. */

#define SYS_COLOR_TYPE		void*
#define SYS_DRAW_GLYPHS		sys_draw_glyphs
#define COPY_GLYPHS		sys_copy_glyphs
#define SYS_BEGIN_REDISPLAY	sys_begin_redisplay
#define SYS_END_REDISPLAY	sys_end_redisplay

#endif /* JADE_MAC_DEFS_H */
