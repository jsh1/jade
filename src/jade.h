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

#ifndef _JADE_H
#define _JADE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


/* Some low-level configuration options */

/* When defined, try to translate addresses to their symbols. */
#define DB_RESOLVE_SYMBOLS

/* Define this to track unfreed memory allocations */
/* #define DEBUG_SYS_ALLOC */


/* General stuff */

#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>

/* Stringify X. Expands macros in X. */
#define QUOTE(x) QUOTE__(x)
#define QUOTE__(x) #x

/* Concat two tokens. Expands macros in X and Y. */
#define CONCAT(x, y) CONCAT__(x, y)
#define CONCAT__(x, y) x##y

typedef char bool;

#include "revision.h"
#include "value.h"

#ifdef HAVE_X11
# include "x11_defs.h"
#else
# ifdef HAVE_AMIGA
#  include "amiga_defs.h"
# else
#  error Need HAVE_X11 or HAVE_AMIGA defined
# endif
#endif

#ifdef HAVE_UNIX
# include "unix_defs.h"
#else
# ifndef HAVE_AMIGA
#  error Need HAVE_UNIX or HAVE_AMIGA defined
# endif
#endif

#ifndef ALIGN_4
# ifdef __GNUC__
#  define ALIGN_4 __attribute__ ((aligned (4)))
# else
#  error Need ALIGN_4 macro
# endif
#endif
#ifndef ALIGN_8
# ifdef __GNUC__
#  define ALIGN_8 __attribute__ ((aligned (8)))
# else
#  error Need ALIGN_8 macro
# endif
#endif

#include "lisp.h"
#include "edit.h"
#include <lib/doc-strings.h>
#include "keys.h"
#include "regexp.h"


/* Macros */

/* Some macros for using MinLists  */
#define AddMTail(l,n)	AddTail((struct List *)l, (struct Node *)n)
#define InsertM(l,n,ln) Insert((struct List *)l, (struct Node *)n, (struct Node *)ln)
#define RemoveM(n)	Remove((struct Node *)n)
#define NewMList(l)	NewList((struct List *)l)
#define IsMListEmpty(l) IsListEmpty((struct List *)l)
#define IsLastMNode(n)	(!((n)->mln_Succ))

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#define POS(x)   MAX(x, 0)

/* Round the integer X to the next or previous multiple of Y. */
#define ROUND_UP_INT(x,y) ((((x) + (y)-1) / (y)) * (y))
#define ROUND_DOWN_INT(x,y) (((x) / (y)) * (y))

/* Return the offset in data type S of the field X. */
#define OFFSETOF(s,x) ((int)&(((s *)0)->x))


#ifndef HAVE_STPCPY
extern char *stpcpy(char *, const char *);
#endif

#ifndef HAVE_MEMCHR
extern void *memchr(const void *, int, size_t);
#endif

#ifndef _PR
# define _PR extern
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
