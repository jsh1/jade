/* stringmem.c -- Allocation of small pieces of memory
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

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#ifdef STRMEM_STATS
# include <stdio.h>
#endif

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

/* Initialise SM for the first time. */
int
sm_init(StrMem *sm)
{
    int i;
    for(i = 0; i < NUMBUCKETS; i++)
    {
	NewMList(&(sm->sm_MemBuckets[i].mbu_MemBlocks));
	sm->sm_ChunksPerBlock[i] = MBLOCKSIZE / MCHNK_SIZE((i + 1) * GRAIN);

#ifdef STRMEM_STATS
	sm->sm_AllocCount[i] = sm->sm_FreeCount[i] = 0;
#endif
    }
    sm->sm_MallocChain = NULL;
    return(TRUE);
}

/* Release all memory allocated for SM, note that unless the `malloc-chain'
   option is used malloc'd blocks aren't freed. The SM is left ready for
   more allocations to be made.  */
void
sm_kill(StrMem *sm)
{
    int i;

#ifdef STRMEM_STATS
    fprintf(stderr, "StrMem *%p:\n", sm);
#endif

    for(i = 0; i < NUMBUCKETS; i++)
    {
	MemBlock *nxt, *mbl;
	mbl = (MemBlock *)sm->sm_MemBuckets[i].mbu_MemBlocks.mlh_Head;
	while((nxt = (MemBlock *)mbl->mbl_Node.mln_Succ))
	{
	    myfree(mbl);
	    mbl = nxt;
	}
	NewMList(&sm->sm_MemBuckets[i].mbu_MemBlocks);
	sm->sm_MemBuckets[i].mbu_FreeList = NULL;
	sm->sm_MemBuckets[i].mbu_FreeCount = 0;

#ifdef STRMEM_STATS
	fprintf(stderr, "\t bucket %d (upto %d bytes): %d alloced, %d freed\n",
		i, (i + 1) * GRAIN, sm->sm_AllocCount[i],
		sm->sm_FreeCount[i]);
	sm->sm_AllocCount[i] = sm->sm_FreeCount[i] = 0;
#endif
    }

    if(sm->sm_UseMallocChain)
    {
	MemChunk *mc = sm->sm_MallocChain;
	while(mc)
	{
	    MemChunk *nxtmc = mc->mc_Header.next;
	    myfree(mc);
	    mc = nxtmc;
	}
	sm->sm_MallocChain = NULL;
    }

#ifdef STRMEM_STATS
    fprintf(stderr, "\t malloc bucket: %d alloced, %d freed\n",
	    sm->sm_AllocCount[NUMBUCKETS], sm->sm_FreeCount[NUMBUCKETS]);
    sm->sm_AllocCount[NUMBUCKETS] = sm->sm_FreeCount[NUMBUCKETS] = 0;
#endif
}

static int
new_memblock(StrMem *sm, MemBucket *mbu, int sizeIndex)
{
    MemBlock *mbl;
    int numchunks = sm->sm_ChunksPerBlock[sizeIndex];
    int chnkbytes = (sizeIndex + 1) * GRAIN;
    mbl = mymalloc(MBLK_SIZE(chnkbytes, numchunks));
    if(mbl)
    {
	MemChunk *mc = mbl->mbl_Chunks;
	int i, mcsiz = MCHNK_SIZE(chnkbytes);
	AddMTail(&mbu->mbu_MemBlocks, &mbl->mbl_Node);
	for(i = 0; i < (numchunks - 1); i++)
	{
	    MemChunk *nxt = (MemChunk *)((char *)mc + mcsiz);
	    mc->mc_BlkType = MBT_FREE;
	    mc->mc_Mem.nextfree = nxt;
	    mc = nxt;
	}
	mc->mc_BlkType = MBT_FREE;
	mc->mc_Mem.nextfree = mbu->mbu_FreeList;
	mbu->mbu_FreeList = mbl->mbl_Chunks;
	mbu->mbu_FreeCount += numchunks;
	return(TRUE);
    }
    return(FALSE);
}

void *
sm_alloc(StrMem *sm, int size)
{
    MemChunk *mc;
    assert(size > 0);
    if(size > MAXBUCKETSIZE)
    {
#if MALLOC_ALIGNMENT >= STRMEM_ALIGNMENT
	mc = mymalloc(MCHNK_SIZE(size));
#else
  /* Note that if this is ever implemented properly, values.c:string_sweep()
     will probably need to be updated. */
# error Need an aligned malloc()
#endif
	if(mc)
	{
#ifdef STRMEM_STATS
	    sm->sm_AllocCount[NUMBUCKETS]++;
#endif
	    if(sm->sm_UseMallocChain)
	    {
		mc->mc_Header.next = sm->sm_MallocChain;
		sm->sm_MallocChain = mc;
	    }
	    else
		mc->mc_BlkType = MBT_MALLOC;
	}
	else
	    return(NULL);
    }
    else
    {
	MemBucket *mbu;
	int bucket = (size - 1) / GRAIN;
	mbu = &sm->sm_MemBuckets[bucket];
	if(!(mc = mbu->mbu_FreeList))
	{
	    if(!new_memblock(sm, mbu, bucket))
		return(NULL);
	    if(!(mc = mbu->mbu_FreeList))
		return(NULL);
	}
	mc->mc_BlkType = bucket;
	mbu->mbu_FreeList = mc->mc_Mem.nextfree;
	mbu->mbu_FreeCount--;

#ifdef STRMEM_STATS
	sm->sm_AllocCount[bucket]++;
#endif
    }
    return(mc->mc_Mem.mem);
}

u_char *
sm_strdupn(StrMem *sm, const u_char *old, int len)
{
    char *new = sm_alloc(sm, len + 1);
    if(new)
    {
	memcpy(new, old, len);
	new[len] = 0;
    }
    return(new);
}

u_char *
sm_strdup(StrMem *sm, const u_char *str)
{
    return(sm_strdupn(sm, str, strlen(str)));
}

/*
 * This scans through all allocated MemBlocks in a MemBucket looking
 * for any which totally consist of unused entrys, any found are
 * released.
 */
static void
flush_bucket(StrMem *sm, int bucketIndex)
{
    MemBucket *mbu = &sm->sm_MemBuckets[bucketIndex];
    MemBlock *mbl, *nxt;
    int chnksiz = MCHNK_SIZE((bucketIndex + 1) * GRAIN);
    int numchnks = sm->sm_ChunksPerBlock[bucketIndex];

    mbl = (MemBlock *)mbu->mbu_MemBlocks.mlh_Head;
    while(mbu->mbu_FreeCount >= numchnks
	  && (nxt = (MemBlock *)mbl->mbl_Node.mln_Succ) != NULL)
    {
	MemChunk *mc = mbl->mbl_Chunks;
	int j;
	for(j = 0; j < numchnks; j++)
	{
	    if(mc->mc_BlkType != MBT_FREE)
		break;
	    mc = (MemChunk *)((char *)mc + chnksiz);
	}
	if(j == numchnks)
	{
	    /* This whole block is free. Remove all references to
	       from the bucket's free list. */
	    mc = mbl->mbl_Chunks;
	    for(j = 0; j < numchnks; j++)
	    {
		MemChunk *last = NULL;
		MemChunk *list = mbu->mbu_FreeList;
		while(list)
		{
		    if(list == mc)
		    {
			if(last)
			    last->mc_Mem.nextfree = list->mc_Mem.nextfree;
			else
			    mbu->mbu_FreeList = list->mc_Mem.nextfree;
			break;
		    }
		    last = list;
		    list = list->mc_Mem.nextfree;
		}
		mc = (MemChunk *)((char *)mc + chnksiz);
	    }
	    RemoveM(&mbl->mbl_Node);
	    myfree(mbl);
	    mbu->mbu_FreeCount -= numchnks;
	}
	mbl = nxt;
    }
}

void
sm_free(StrMem *sm, void *mem)
{
    if(mem)
    {
	MemChunk *mc = (MemChunk *)(((char *)mem) - sizeof(union mc_header));
	int bucketnum = mc->mc_BlkType;
	if(bucketnum == MBT_MALLOC)
	{
	    myfree(mc);
#ifdef STRMEM_STATS
	    sm->sm_FreeCount[NUMBUCKETS]++;
#endif
	}
	else
	{
	    MemBucket *mbu;
	    assert(bucketnum <= NUMBUCKETS);
	    mbu = &sm->sm_MemBuckets[bucketnum];
	    mc->mc_Mem.nextfree = mbu->mbu_FreeList;
	    mc->mc_BlkType = MBT_FREE;
	    mbu->mbu_FreeList = mc;
	    mbu->mbu_FreeCount++;
	    if(sm->sm_FreesBeforeFlush != 0
	       && (mbu->mbu_FreeCount
	           >= (sm->sm_FreesBeforeFlush
		       * sm->sm_ChunksPerBlock[bucketnum])))
	    {
		/* flush this bucket... */
		flush_bucket(sm, bucketnum);
	    }

#ifdef STRMEM_STATS
	    sm->sm_FreeCount[bucketnum]++;
#endif
	}
    }
}

/*
 * called when malloc() fails, tries to release any memory we can
 */
void
sm_flush(StrMem *sm)
{
    int i;
    for(i = 0; i < NUMBUCKETS; i++)
	flush_bucket(sm, i);
}
