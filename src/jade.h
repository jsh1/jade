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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef _JADE_H
#define _JADE_H

#include <sys/types.h>
#include <stdio.h>

typedef char bool;

#include "revision.h"
#include "value.h"

/* This should be either a link to the target systems config.h.X file
   in configs/ or a file containing whatever your system needs.	 */
#define C_CONFIG
#include "config.h"

#ifndef HAVE_X11
# ifndef HAVE_AMIGA
#  error Need HAVE_X11 or HAVE_AMIGA defined
# endif
#endif

#include "edit.h"
#include "lisp.h"
#include "doc-strings.h"
#include "keys.h"
#include "stringmem.h"


/* Macros */

/* Some macros for using MinLists  */
#define AddMTail(l,n)	AddTail((struct List *)l, (struct Node *)n)
#define InsertM(l,n,ln) Insert((struct List *)l, (struct Node *)n, (struct Node *)ln)
#define RemoveM(n)	Remove((struct Node *)n)
#define NewMList(l)	NewList((struct List *)l)
#define IsMListEmpty(l) IsListEmpty((struct List *)l)
#define IsLastMNode(n)	(!((n)->mln_Succ))

/* Stringify X. Expands macros in X. */
#define QUOTE(x) __QUOTE(x)
#define __QUOTE(x) #x

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
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

#ifndef _PR
# define _PR extern
#endif

#ifndef INLINE
# ifdef __GNUC__
#  define INLINE __inline__
# else
#  define INLINE
# endif
#endif

#ifndef NULL
# define NULL ((void *)0)
#endif

#ifndef TRUE
# define TRUE (1)
#endif

#ifndef FALSE
# define FALSE (0)
#endif

#endif /* _JADE_H */
