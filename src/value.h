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

typedef struct {
    /* Tag defining the type of this object. Bit 7 is reserved for gc,
       at all other times it will be zero.  */
    u_char	type;
    /* Data follows, in real objects. */
} LispObject;

/* VALUE used to be defined as a (void *) but it posed some problems with
   a certain well-known Amiga compiler.  */

typedef LispObject *VALUE;

#endif /* _VALUE_H */
