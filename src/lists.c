/* lists.c -- Clones of the Amiga's exec-list functions
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
#include <lib/jade_protos.h>

void
NewList(struct List *list)
{
    list->lh_TailPred = (struct Node *)&list->lh_Head;
    list->lh_Tail = NULL;
    list->lh_Head = (struct Node *)&list->lh_Tail;
}

void
AddTail(struct List *list, struct Node *node)
{
    struct Node *tmp, *tmp2;
    tmp = (struct Node *)&list->lh_Tail;
    tmp2 = tmp->ln_Pred;
    tmp->ln_Pred = node;
    node->ln_Succ = tmp;
    node->ln_Pred = tmp2;
    tmp2->ln_Succ = node;
}

void
Insert(struct List *list, struct Node *node, struct Node *listNode)
{
    struct Node *tmp;
    if(listNode)
    {
	if((tmp = listNode->ln_Succ))
	{
	    node->ln_Succ = tmp;
	    node->ln_Pred = listNode;
	    tmp->ln_Pred = node;
	    listNode->ln_Succ = node;
	}
	else
	{
	    node->ln_Succ = listNode;
	    tmp = listNode->ln_Pred;
	    node->ln_Pred = tmp;
	    listNode->ln_Pred = node;
	    tmp->ln_Succ = node;
	}
    }
    else
    {
	tmp = list->lh_Head;
	list->lh_Head = node;
	node->ln_Succ = tmp;
	node->ln_Pred = (struct Node *)list;
	tmp->ln_Pred = node;
    }
}

void
Remove(struct Node *node)
{
    struct Node *tmp;
    tmp = node->ln_Succ;
    node = node->ln_Pred;
    node->ln_Succ = tmp;
    tmp->ln_Pred = node;
}
