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
   you lose!
# endif
#endif

#include "edit.h"
#include "lisp.h"
#include "doc-strings.h"
#include "keys.h"
#include "stringmem.h"

/* Some macros for using MinLists  */
#define AddMTail(l,n)	AddTail((struct List *)l, (struct Node *)n)
#define InsertM(l,n,ln) Insert((struct List *)l, (struct Node *)n, (struct Node *)ln)
#define RemoveM(n)	Remove((struct Node *)n)
#define NewMList(l)	NewList((struct List *)l)
#define IsMListEmpty(l) IsListEmpty((struct List *)l)
#define IsLastMNode(n)	(!((n)->mln_Succ))

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

#define QUOTE(x) __QUOTE(x)
#define __QUOTE(x) #x

#endif /* _JADE_H */
