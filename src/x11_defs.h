/* x11_defs.h -- Declarations for X11
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef _X11_DEFS_H
#define _X11_DEFS_H

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "x11_windowsys.h"

/* This is probably totally unnecessary */
#ifndef XlibSpecificationRelease
# define XlibSpecificationRelease 1
#endif

/* It seems that X11R4 doesn't have XPointer type?? */
#if XlibSpecificationRelease < 5
typedef char *XPointer;
#endif

#define NOSCRLBAR

/* standard font */
#define DEFAULT_FONT "fixed"

/* max number of milliseconds between double-clicks */
#define DOUBLE_CLICK_TIME 250

#endif /* _X11_DEFS_H */
