/* lists.h -- Amiga-style doubly-linked lists
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

#ifndef _LISTS_H
#define _LISTS_H

struct Node {
    struct Node *ln_Succ;
    struct Node *ln_Pred;
    char *ln_Name;
    /* Don't need the rest. */
};

struct MinNode {
    struct MinNode *mln_Succ;
    struct MinNode *mln_Pred;
};

struct List {
    struct Node *lh_Head;
    struct Node *lh_Tail;
    struct Node *lh_TailPred;
}; 

struct MinList {
    struct MinNode *mlh_Head;
    struct MinNode *mlh_Tail;
    struct MinNode *mlh_TailPred;
}; 

#define IsListEmpty(l)  (((l)->lh_TailPred) == (struct Node *)(l))

void NewList(struct List *);
void AddTail(struct List *, struct Node *);
void Insert(struct List *, struct Node *, struct Node *);
void Remove(struct Node *);

#endif /* _LISTS_H */
