/* value.h -- Declaration of Lisp data-type
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef _VALUE_H
#define _VALUE_H

/* This is in its own file so I can remove some circular dependancies from
   the editor's header files.  */

/* A VALUE is a pointer to an object, not a real pointer; it's
   lowest three bits define its type. */

/* In case one of the configuration header files needs to re-define
   this. It should be an implicitly signed integer, the same size as a
   standard `void *' pointer on the target machine. */
#ifndef PTR_SIZED_INT
# define PTR_SIZED_INT long int
#endif

typedef unsigned PTR_SIZED_INT VALUE;

#endif /* _VALUE_H */
