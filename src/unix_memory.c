/* unix_memory.c -- Memory allocation for Unix
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

#include "jade.h"
#include "jade_protos.h"

#include <stdlib.h>
#include <assert.h>

_PR void *mymalloc(int length);
_PR void *mycalloc(int length);

/* _PR void myfree(void *mem);
   _PR int initmem(void);
   _PR void killmem(void);  */

void *
mymalloc(int length)
{
    void *mem = malloc(length);
    if(mem)
    {
	/* Check that the alignment promised actually occurs */
	assert((((PTR_SIZED_INT)mem) & (MALLOC_ALIGNMENT - 1)) == 0);
	return(mem);
    }
    sm_flush(&main_strmem);
    return(malloc(length));
}

void *
mycalloc(int length)
{
    void *mem = calloc(length, 1);
    if(mem)
	return(mem);
    sm_flush(&main_strmem);
    return(calloc(length, 1));
}

#if 0
/* These are now macros in `unix_defs.h'  */
void
myfree(void *mem)
{
    if(mem)
	free(mem);
}
int
initmem(void)
{
    return(TRUE);
}
void
killmem(void)
{
}
#endif
