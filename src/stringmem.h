/* stringmem.h -- Declarations for stringmem.c
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

#ifndef _STRINGMEM_H
#define _STRINGMEM_H

/* #define STRMEM_STATS */

/* The lowest level, each allocated piece of memory is one of
   these. The pointer returned from sm_alloc() points to the mc_Mem
   union. */
typedef struct _MemChunk {
    /* This next union is a bit of a kludge. The problem is that I
       need some way of tracking MEMCHUNKs allocated straight from
       malloc() (so I can do garbage collection, etc) without wasting
       a lot of memory. For my own reasons you can't call sm_free() on
       one of these blocks. But since I only use this technique on
       Lisp memory (which is never freed except by the GC which is
       weird) everything is ok.  */
    union mc_header {
	struct _MemChunk *next;
	int		 blktype;
    }		    mc_Header;
#define mc_BlkType mc_Header.blktype

    union {
	struct _MemChunk *nextfree;
	u_char		 mem[0];
    }		    mc_Mem ALIGN_4;
} MemChunk;

#define MBT_FREE   -2		/* An unused chunk */
#define MBT_MALLOC -1		/* A malloc'd chunk */

#define MCHNK_SIZE(chunksiz) \
    ((chunksiz) + OFFSETOF(MemChunk, mc_Mem))

/* A group of chunks for allocation purposes. */
typedef struct {
    struct MinNode  mbl_Node;
    MemChunk	    mbl_Chunks[0] ALIGN_4;
} MemBlock;

#define MBLK_SIZE(chunksiz, numchunks) \
    ((MCHNK_SIZE(chunksiz) * (numchunks)) + sizeof(MemBlock))

/* Represents each bucket of similarly sized allocations. */
typedef struct {
    /* List of allocated blocks. */
    struct MinList  mbu_MemBlocks;

    /* Free chunks. */
    MemChunk	   *mbu_FreeList;

    /* Number of free operations since last scan for free blocks.  */
    int		    mbu_FreeCount;
} MemBucket;

/* difference in size between each bucket */
#define GRAIN	      8

/* allocations > this go to malloc() */
#define MAXBUCKETSIZE 128

#define NUMBUCKETS    (MAXBUCKETSIZE / GRAIN)

/* Each MemBlock should be around 2K */
#define MBLOCKSIZE    (2044 - sizeof(MemBlock))

/* for lisp.h: everything returned by sm_alloc() is aligned to a four
   byte boundary. */
#define STRMEM_ALIGNMENT 4

/* The top-level structure. */
typedef struct {
    /* The bucket list. The last one is used to record malloc'd chunks
       for allocations > MAXBUCKETSIZE */
    MemBucket	    sm_MemBuckets[NUMBUCKETS];

    /* The number of chunks to allocated per contiguous block, for
       the different bucket sizes. */
    int		    sm_ChunksPerBlock[NUMBUCKETS];

    /* This next number defines the number of sm_frees() which have to
       be called on memory from a particular bucket before that bucket
       is scanned for totally free blocks (any found are released to
       system). 
       The actual number is (FREESBEFOREFLUSH * ChunksPerBlock[bucket]).  */
    char	    sm_FreesBeforeFlush;

    /* Whether or not to use sm_MallocChain to chain all malloc() MemChunks
       together. If using this don't call sm_free().  */
    char	    sm_UseMallocChain;
    MemChunk	   *sm_MallocChain;

#ifdef STRMEM_STATS
    /* Statistics for each bucket. */
    int		    sm_AllocCount[NUMBUCKETS + 1];
    int		    sm_FreeCount[NUMBUCKETS + 1];
#endif
} StrMem;

extern int     sm_init(StrMem *);
extern void    sm_kill(StrMem *);
extern void   *sm_alloc(StrMem *, int);
extern u_char *sm_strdupn(StrMem *, const u_char *, int);
extern u_char *sm_strdup(StrMem *, const u_char *);
extern void    sm_free(StrMem *, void *);
extern void    sm_flush(StrMem *);

#define str_alloc(n)  sm_alloc(&main_strmem, n)
#define str_dupn(s,n) sm_strdupn(&main_strmem, s, n)
#define str_dup(s)    sm_strdup(&main_strmem, s)
#define str_free(s)   sm_free(&main_strmem, s)

#endif
