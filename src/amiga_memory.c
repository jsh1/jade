/* amiga_memory.c -- Memory allocation for AmigaDOS
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

#include <clib/exec_protos.h>
#include <string.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR int	initmem(void);
_PR void killmem(void);
_PR void *mymalloc(int);
_PR void *mycalloc(int);
_PR void myfree(void *);

#if AMIGA_INCLUDE_VER >= 39
# define PUDDLESIZE 20480
# define THRESHSIZE 4096
  APTR mem_pool;
#endif

int
initmem(void)
{
#if AMIGA_INCLUDE_VER >= 39
    if(ami_os_version >= 39)
    {
	if(mem_pool = CreatePool(0, PUDDLESIZE, THRESHSIZE))
	    return(TRUE);
	return(FALSE);
    }
#endif
    return(TRUE);
}
void
killmem(void)
{
#if AMIGA_INCLUDE_VER >= 39
    if(mem_pool)
    {
	DeletePool(mem_pool);
	mem_pool = NULL;
    }
#endif
}

void *
mymalloc(int size)
{
    int *mem;
    size += sizeof(int);

#ifdef DEBUG_MALLOC
    if(size == DEBUG_MALLOC)
    {
loop:
	/* Set a breakpoint here. */
	goto loop;
    }
#endif

#if AMIGA_INCLUDE_VER >= 39
    if(!(mem = (mem_pool ? AllocPooled(mem_pool, size) : AllocMem(size, 0))))
#else
    if(!(mem = AllocMem(size, 0)))
#endif
    {
	sm_flush(&main_strmem);
#if AMIGA_INCLUDE_VER >= 39
	if(!(mem = (mem_pool
		    ? AllocPooled(mem_pool, size)
		    : AllocMem(size, 0))))
#else
	if(!(mem = AllocMem(size, 0)))
#endif
	    return(NULL);
    }
    *mem = size;
    return(mem + 1);
}

void *
mycalloc(int size)
{
    void *mem;
    if(mem = mymalloc(size))
	memset(mem, 0, size);
    return(mem);
}

void
myfree(void *mem)
{
    if(mem)
    {
	int size = *(--((int *)mem));
#if AMIGA_INCLUDE_VER >= 39
	if(mem_pool)
	    FreePooled(mem_pool, mem, size);
	else
#endif
	    FreeMem(mem, size);
    }
}
