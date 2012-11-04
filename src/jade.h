/* jade.h -- Main include file, brings in all the rest
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef JADE_H
#define JADE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <rep.h>
#include <stdarg.h>

#if defined (HAVE_GTK)
# include "gtk_defs.h"
#elif defined (HAVE_MAC)
# include "mac_defs.h"
#elif defined (HAVE_X11)
# include "x11_defs.h"
#else
# error "Need a window-system"
#endif

#include "edit.h"
#include "keys.h"
#include "jade_subrs.h"

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#undef MAX
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#undef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#define POS(x)   MAX(x, 0)

/* Round the integer X to the next or previous multiple of Y. */
#define ROUND_UP_INT(x,y) ((((x) + (y)-1) / (y)) * (y))
#define ROUND_DOWN_INT(x,y) (((x) / (y)) * (y))

#ifndef HAVE_STPCPY
extern char *stpcpy(char *, const char *);
#endif

#ifndef HAVE_MEMCHR
extern void *memchr(const void *, int, size_t);
#endif

#endif /* JADE_H */
