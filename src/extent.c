/* extent.c -- buffer-region based plists
   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <lib/jade_protos.h>
#include <limits.h>

#define DEBUG

_PR Lisp_Extent *find_extent_forwards(Lisp_Extent *root, Pos *pos);
_PR Lisp_Extent *find_extent(Lisp_Extent *root, Pos *pos);
_PR void map_section_extents(void (*)(Lisp_Extent *, void *), Lisp_Extent *, Pos *, Pos *, void *);
_PR void make_global_extent(TX *tx);
_PR void reset_global_extent(TX *tx);
_PR void adjust_extents_add_cols(Lisp_Extent *, long, long, long);
_PR void adjust_extents_sub_cols(Lisp_Extent *, long, long, long);
_PR void adjust_extents_add_rows(Lisp_Extent *, long, long);
_PR void adjust_extents_sub_rows(Lisp_Extent *, long, long);
_PR void adjust_extents_split_row(Lisp_Extent *, long, long);
_PR void adjust_extents_join_rows(Lisp_Extent *, long, long);
_PR void mark_extent(Lisp_Extent *e);
_PR void extent_sweep(void);
_PR void extent_prin(VALUE strm, VALUE e);
_PR void extent_init(void);

DEFSYM(front_sticky, "front-sticky");
DEFSYM(rear_sticky, "rear-sticky");
_PR VALUE sym_front_sticky, sym_rear_sticky;

static Lisp_Extent *allocated_extents;

/* Create a new extent. All links are null. If CLONEE is non-null, copy
   the START, END, TX and PLIST fields from it. */
static Lisp_Extent *
alloc_extent(Lisp_Extent *clonee)
{
    Lisp_Extent *x = ALLOC_OBJECT(sizeof(Lisp_Extent));
    if(x == 0)
    {
	mem_error();
	return 0;
    }

    x->car = V_Extent;
    x->next = allocated_extents;
    allocated_extents = x;

    x->parent = 0;
    x->left_sibling = x->right_sibling = 0;
    x->first_child = x->last_child = 0;
    x->frag_next = x->frag_pred = 0;

    if(clonee != 0)
    {
	x->start = clonee->start;
	x->end = clonee->end;
	x->tx = clonee->tx;
	x->plist = clonee->plist;
	x->locals = clonee->locals;
    }

    return x;
}

/* Make all links in E null. */
static inline void
clean_node(Lisp_Extent *e)
{
    e->parent = 0;
    e->left_sibling = e->right_sibling = 0;
    e->first_child = e->last_child = 0;
    e->frag_next = e->frag_pred = 0;
}

/* Return the first fragment in the chain that E is a member of. */
static inline Lisp_Extent *
find_first_frag(Lisp_Extent *e)
{
    while(e->frag_pred != 0)
	e = e->frag_pred;
    return e;
}

/* Return the last fragment in the chain that E is a member of. */
static inline Lisp_Extent *
find_last_frag(Lisp_Extent *e)
{
    while(e->frag_next != 0)
	e = e->frag_next;
    return e;
}

/* For two adjacent fragments at the same level, LEFT and RIGHT, attempt
   to join them into a single fragment. Always absorbs RIGHT into LEFT,
   never LEFT into RIGHT. */
static void
try_to_coalesce(Lisp_Extent *left, Lisp_Extent *right)
{
    if(left->frag_next == right
       && left->parent == right->parent
       && PPOS_GREATER_EQUAL_P(&left->end, &right->start))
    {
	/* Yep. These two can be united. */

	left->right_sibling = right->right_sibling;
	left->end = right->end;
	if(left->parent->last_child == right)
	    left->parent->last_child = left;

	clean_node(right);
    }
}

/* Unlink the single fragment E. */
static void
unlink_extent_fragment(Lisp_Extent *e)
{
    if(e->frag_next != 0)
	e->frag_next->frag_pred = e->frag_pred;
    if(e->frag_pred != 0)
	e->frag_pred->frag_next = e->frag_next;

    if(e->first_child != 0)
    {
	/* Replace E by its children. */

	Lisp_Extent *x;

	/* Fix positions and parents.. */
	for(x = e->first_child; x != 0; x = x->right_sibling)
	{
	    x->start.row += e->start.row;
	    x->end.row += e->start.row;
	    x->parent = e->parent;
	}

	e->first_child->left_sibling = e->left_sibling;
	if(e->left_sibling != 0)
	    e->left_sibling->right_sibling = e->first_child;
	else
	    e->parent->first_child = e->first_child;

	e->last_child->right_sibling = e->right_sibling;
	if(e->right_sibling != 0)
	    e->right_sibling->left_sibling = e->last_child;
	else
	    e->parent->last_child = e->last_child;

	/* Do right before left since try_to_coalesce absorbs leftwards */
	if(e->right_sibling != 0)
	    try_to_coalesce(e->last_child, e->right_sibling);
	if(e->left_sibling != 0)
	    try_to_coalesce(e->left_sibling, e->first_child);
    }
    else
    {
	/* No children, tie the siblings together */

	if(e->left_sibling != 0)
	    e->left_sibling->right_sibling = e->right_sibling;
	else
	    e->parent->first_child = e->right_sibling;
	if(e->right_sibling != 0)
	    e->right_sibling->left_sibling = e->left_sibling;
	else
	    e->parent->last_child = e->left_sibling;

	if(e->left_sibling != 0 && e->right_sibling != 0)
	    try_to_coalesce(e->left_sibling, e->right_sibling);
    }
    clean_node(e);
}

/* Unlink extent E (and all fragments chained off it). */
static void
unlink_extent(Lisp_Extent *e)
{
    e = find_first_frag(e);
    while(e != 0)
    {
	Lisp_Extent *next = e->frag_next;
	unlink_extent_fragment(e);
	e = next;
    }
}

static void
unlink_extent_recursively(Lisp_Extent *e)
{
    while(e->first_child != 0)
	unlink_extent_recursively(e->first_child);
    if(e->parent != 0)
	unlink_extent_fragment(e);
}

/* Insert extent E into the tree rooted at extent ROOT. The row-positions
   in E are assumed to be relative to the row-position of ROOT. */
static void
insert_extent(Lisp_Extent *e, Lisp_Extent *root)
{
    Lisp_Extent *x;
top:
    x = root->first_child;
    while(x != 0)
    {
	if(PPOS_LESS_EQUAL_P(&e->end, &x->start))
	{
	    /* Insert E before X */

	    e->parent = root;
	    e->left_sibling = x->left_sibling;
	    if(e->left_sibling != 0)
		e->left_sibling->right_sibling = e;
	    x->left_sibling = e;
	    e->right_sibling = x;
	    if(root->first_child == x)
		root->first_child = e;

	    return;
	}
	else if(PPOS_LESS_P(&e->start, &x->start))
	{
	    /* X overlaps E. The end of E clashes with the start of X.
	       Break E into two fragments, insert the first before X,
	       the second within X. */

	    Lisp_Extent *frag = alloc_extent(e);
	    frag->end = x->start;
	    e->start = x->start;
	    frag->car |= EXTFF_OPEN_END;

	    frag->frag_pred = e->frag_pred;
	    frag->frag_next = e;
	    e->frag_pred = frag;

	    insert_extent(frag, x->parent);
	    continue;
	}
	else if(PPOS_LESS_P(&e->start, &x->end))
	{
	    /* E starts before X ends. */
	    if(PPOS_LESS_EQUAL_P(&e->end, &x->end))
	    {
		/* But E ends before X ends. So insert E in X. */

		e->start.row -= x->start.row;
		e->end.row -= x->start.row;
		root = x;
		goto top;
	    }
	    else
	    {
		/* E ends after X ends. So break E into two frags. */

		Lisp_Extent *frag = alloc_extent(e);
		frag->start = x->end;
		e->end = x->end;
		frag->car |= EXTFF_OPEN_START;

		frag->frag_next = e->frag_next;
		frag->frag_pred = e;
		e->frag_next = frag;

		e->start.row -= x->start.row;
		e->end.row -= x->start.row;
		insert_extent(e, x);
		e = frag;
		continue;
	    }
	}
	else
	{
	    /* X ends before E starts, keep going.. */
	}
	x = x->right_sibling;
    }

    /* Insert e at the end of the root. */
    e->parent = root;
    e->left_sibling = root->last_child;
    if(root->last_child != 0)
	root->last_child->right_sibling = e;
    root->last_child = e;
    if(root->first_child == 0)
	root->first_child = e;
}

/* Find the global offset of the first row of extent E. */
static long
row_delta(Lisp_Extent *e)
{
    long delta = 0;
    if(e != 0)
    {
	e = e->parent;
	while(e != 0)
	{
	    delta += e->start.row;
	    e = e->parent;
	}
    }
    return delta;
}

Lisp_Extent *
find_extent_forwards(Lisp_Extent *root, Pos *pos)
{
    Lisp_Extent *x = root;
    Pos copy = *pos;
    copy.row -= row_delta(root);
    while(x != 0)
    {
	if(PPOS_GREATER_EQUAL_P(&copy, &x->start)
	   && PPOS_LESS_P(&copy, &x->end))
	{
	    /* POS in X somewhere. */
	    root = x;
	    copy.row -= x->start.row;
	    x = x->first_child;
	    continue;
	}
	x = x->right_sibling;
    }
    return root;
}

/* Return the innermost extent containing position POS. */
Lisp_Extent *
find_extent(Lisp_Extent *root, Pos *pos)
{
    TX *tx = root->tx;
    int i, out = -1, oldest = 0;
    u_long oldest_lru = ULONG_MAX;
    static u_long lru_time;
    Lisp_Extent *x;

    /* First check the cache. */
    for(i = 0; i < EXTENT_CACHE_SIZE; i++)
    {
	struct cached_extent *ce = &tx->tx_ExtentCache[i];
	if(ce->extent)
	{
	    if(PPOS_EQUAL_P(pos, &ce->pos))
	    {
		/* Bingo! direct hit */
		ce->lru_clock = ++lru_time;
		return ce->extent;
	    }
	    else if(PPOS_GREATER_EQUAL_P(pos, &ce->extent->start)
		    && PPOS_LESS_P(pos, &ce->extent->end))
	    {
		/* Not direct. But ce[i] contains POS. If this isn't
		   the only item containing POS, use the innermost. */
		if(out == -1
		   || PPOS_GREATER_P(&ce->extent->start,
				     &tx->tx_ExtentCache[out].extent->start))
		    out = i;
	    }
	    if(ce->lru_clock < oldest_lru)
	    {
		oldest = i;
		oldest_lru = ce->lru_clock;
	    }
	}
	else
	{
	    oldest = i;
	    oldest_lru = 0;
	}
    }
    /* No direct hits. Any containing POS. */
    if(out >= 0)
	x = find_extent_forwards(tx->tx_ExtentCache[out].extent, pos);
    else
	x = find_extent_forwards(tx->tx_GlobalExtent, pos);

    /* Add X to the cache in place of OLDEST. */
    tx->tx_ExtentCache[oldest].extent = x;
    tx->tx_ExtentCache[oldest].pos = *pos;
    tx->tx_ExtentCache[oldest].lru_clock = ++lru_time;

    return x;
}

/* Map the function MAP_FUNC(E, DATA) over all innermost extents E
   containing part of the section of TX specified by START, END. */
void
map_section_extents(void (*map_func)(Lisp_Extent *x, void *data),
		    Lisp_Extent *root, Pos *start, Pos *end, void *data)
{
    Lisp_Extent *x = find_extent(root, start);
    Pos s_copy = *start, e_copy = *end;
    long delta = row_delta(x->parent);
    s_copy.row -= delta; e_copy.row -= delta;

    while(x != 0 && PPOS_LESS_P(&x->start, &e_copy))
    {
	if(PPOS_GREATER_P(&x->end, &s_copy))
	    map_func(x, data);

	/* Try to work downwards and rightwards as much as possible */
	if(x->first_child != 0)
	{
	    /* Map though X's children as well. */
	    s_copy.row -= x->start.row;
	    e_copy.row -= x->start.row;
	    x = x->first_child;
	}
	else if(x->right_sibling != 0)
	    x = x->right_sibling;
	else if(x->parent != 0)
	{
	    s_copy.row += x->parent->start.row;
	    e_copy.row += x->parent->start.row;
	    x = x->parent->right_sibling;
	}
	else
	    break;
    }
}

/* Notify the cache that some extents in TX were changed. */
static void
invalidate_extent_cache(TX *tx)
{
    int i;
    for(i = 0; i < EXTENT_CACHE_SIZE; i++)
	tx->tx_ExtentCache[i].extent = 0;
}

/* Create the all-encompassing root extent for buffer TX. */
void
make_global_extent(TX *tx)
{
    Lisp_Extent *e = alloc_extent(0);
    e->tx = tx;
    e->plist = sym_nil;
    e->locals = sym_nil;
    e->start.row = e->start.col = 0;
    e->end.row = e->end.col = 0;
    e->car |= EXTFF_OPEN_START | EXTFF_OPEN_END;
    tx->tx_GlobalExtent = e;
}

/* After loading a file into TX, call this. */
void
reset_global_extent(TX *tx)
{
    Lisp_Extent *e = tx->tx_GlobalExtent;
    unlink_extent_recursively(e);
    e->start.row = 0;
    e->start.col = 0;
    e->end.row = tx->tx_NumLines;
    e->end.col = tx->tx_Lines[tx->tx_NumLines-1].ln_Strlen - 1;
}


/* Lisp functions */

_PR VALUE cmd_make_extent(VALUE, VALUE, VALUE);
DEFUN("make-extent", cmd_make_extent, subr_make_extent,
      (VALUE start, VALUE end, VALUE plist), V_Subr3, DOC_make_extent) /*
::doc:make_extent::
make-extent START END [PLIST]

Create and return a new extent in the current buffer from START to END.
It's property list is initially set to PLIST. It will have no local
variables.
::end:: */
{
    Lisp_Extent *extent;
    TX *tx = curr_vw->vw_Tx;
    DECLARE1(start, POSP);
    DECLARE2(end, POSP);

    if(VROW(start) < 0 || VROW(start) >= tx->tx_NumLines
       || VROW(end) < 0 || VROW(end) >= tx->tx_NumLines
       || POS_GREATER_P(start, end))
	return cmd_signal(sym_invalid_pos, list_2(start, end));

    extent = alloc_extent(0);
    if(extent == 0)
	return LISP_NULL;

    COPY_VPOS(&extent->start, start);
    COPY_VPOS(&extent->end, end);
    extent->tx = tx;
    extent->plist = plist;
    extent->locals = sym_nil;

    insert_extent(extent, tx->tx_GlobalExtent);
    invalidate_extent_cache(tx);

    return VAL(extent);
}

DEFSTRING(no_delete_global, "Can't delete global extent");
_PR VALUE cmd_delete_extent(VALUE, VALUE);
DEFUN("delete-extent", cmd_delete_extent, subr_delete_extent,
      (VALUE extent, VALUE no_frags), V_Subr2, DOC_delete_extent) /*
::doc:delete_extent::
delete-extent EXTENT [THIS-FRAGMENT-ONLY]

Delete EXTENT. If THIS-FRAGMENT-ONLY is non-nil the only fragment to be
removed will be EXTENT itself. Otherwise all fragments making up the
extent as it was created are removed.
::end:: */
{
    DECLARE1(extent, EXTENTP);

    if(VEXTENT(extent) == VEXTENT(extent)->tx->tx_GlobalExtent)
	return cmd_signal(sym_error, list_2(VAL(&no_delete_global), extent));

    if(NILP(no_frags))
	unlink_extent(VEXTENT(extent));
    else
	unlink_extent_fragment(VEXTENT(extent));

    invalidate_extent_cache(VEXTENT(extent)->tx);
    return extent;
}

_PR VALUE cmd_delete_all_extents(VALUE);
DEFUN("delete-all-extents", cmd_delete_all_extents, subr_delete_all_extents,
      (VALUE extent), V_Subr1, DOC_delete_all_extents) /*
::doc:delete_all_extents::
delete-all-extents [ROOT]

Delete all extents contained within ROOT (or the root of the current buffer).
Note that it's not possible to delete the global extent of a buffer itself.
::end:: */
{
    if(!EXTENTP(extent))
	extent = VAL(curr_vw->vw_Tx->tx_GlobalExtent);
    unlink_extent_recursively(VEXTENT(extent));
    invalidate_extent_cache(VEXTENT(extent)->tx);
    return extent;
}

_PR VALUE cmd_move_extent(VALUE, VALUE, VALUE);
DEFUN("move-extent", cmd_move_extent, subr_move_extent,
      (VALUE extent, VALUE start, VALUE end), V_Subr3, DOC_move_extent) /*
::doc:move_extent::
move-extent EXTENT START END
::end:: */
{
    Lisp_Extent *e;
    DECLARE1(extent, EXTENTP);
    DECLARE2(start, POSP);
    DECLARE3(end, POSP);

    e = VEXTENT(extent)->frag_next;
    VEXTENT(extent)->frag_next = 0;
    while(e != 0)
    {
	Lisp_Extent *next = e->frag_next;
	unlink_extent_fragment(e);
	e = next;
    }

    unlink_extent_fragment(VEXTENT(extent));
    COPY_VPOS(&VEXTENT(extent)->start, start);
    COPY_VPOS(&VEXTENT(extent)->end, end);
    insert_extent(VEXTENT(extent), VEXTENT(extent)->tx->tx_GlobalExtent);

    return extent;
}

_PR VALUE cmd_get_extent(VALUE, VALUE);
DEFUN("get-extent", cmd_get_extent, subr_get_extent, (VALUE pos, VALUE tx),
      V_Subr2, DOC_get_extent) /*
::doc:get_extent::
get-extent [POSITION] [BUFFER]

Return the innermost extent at POSITION in BUFFER (by default the cursor
position of the current buffer).
::end:: */
{
    Lisp_Extent *e;
    Pos ppos;
    if(!BUFFERP(tx))
    {
	tx = VAL(curr_vw->vw_Tx);
	if(!POSP(pos))
	    pos = curr_vw->vw_CursorPos;
    }
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    COPY_VPOS(&ppos, pos);
    e = find_extent(VTX(tx)->tx_GlobalExtent, &ppos);

    return (e != 0) ? VAL(e) : sym_nil;
}

_PR VALUE cmd_extent_parent(VALUE);
DEFUN("extent-parent", cmd_extent_parent, subr_extent_parent, (VALUE extent),
      V_Subr1, DOC_extent_parent) /*
::doc:extent_parent::
extent-parent EXTENT

Return parent extent of EXTENT. Returns nil if EXTENT is the global extent
covering the entire buffer.
::end:: */
{
    DECLARE1(extent, EXTENTP);
    return VEXTENT(extent)->parent ? VAL(VEXTENT(extent)->parent) : sym_nil;
}

_PR VALUE cmd_extent_root(VALUE);
DEFUN("extent-root", cmd_extent_root, subr_extent_root, (VALUE tx),
      V_Subr1, DOC_extent_root) /*
::doc:extent_root::
extent-root [BUFFER]

Return the global extent covering the whole of BUFFER. This extent has no
parent.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return VAL(VTX(tx)->tx_GlobalExtent);
}

_PR VALUE cmd_extent_plist(VALUE);
DEFUN("extent-plist", cmd_extent_plist, subr_extent_plist, (VALUE extent),
      V_Subr1, DOC_extent_plist) /*
::doc:extent_plist::
extent-plist EXTENT

Return the property list associated with EXTENT.
::end:: */
{
    DECLARE1(extent, EXTENTP);
    return VEXTENT(extent)->plist;
}

_PR VALUE cmd_set_extent_plist(VALUE, VALUE);
DEFUN("set-extent-plist", cmd_set_extent_plist, subr_set_extent_plist,
      (VALUE extent, VALUE plist), V_Subr2, DOC_set_extent_plist) /*
::doc:set_extent_plist::
set-extent-plist EXTENT PLIST

Set the property list associated with EXTENT (and all other fragments of
this extent) to PLIST.
::end:: */
{
    Lisp_Extent *e;
    DECLARE1(extent, EXTENTP);

    /* Update all fragments of the extent. TODO: don't need to
       call fff, just go backwards then forwards. */
    e = find_first_frag(VEXTENT(extent));
    while(e != 0)
    {
	e->plist = plist;
	e = e->frag_next;
    }

    return plist;
}

_PR VALUE cmd_extent_get(VALUE, VALUE);
DEFUN("extent-get", cmd_extent_get, subr_extent_get,
      (VALUE prop, VALUE extent), V_Subr2, DOC_extent_get) /*
::doc:extent_get::
extent-get PROPERTY EXTENT

Return the value of PROPERTY (a symbol) in EXTENT, or any of its parents.
Returns nil if there is no value in this extent.

The special properties `front-sticky' and `rear-sticky' are always set
in each extent.
::end:: */
{
    Lisp_Extent *inner;
    DECLARE1(prop, SYMBOLP);
    DECLARE2(extent, EXTENTP);
    inner = VEXTENT(extent);

    if(prop == sym_front_sticky || prop == sym_rear_sticky)
    {
	extent = VAL((prop == sym_front_sticky
		      ? find_first_frag : find_last_frag)(VEXTENT(extent)));
	return (VEXTENT(extent)->car & (prop == sym_front_sticky
					? EXTFF_OPEN_START
					: EXTFF_OPEN_END)) ? sym_t : sym_nil;
    }

    while(inner != 0)
    {
	VALUE plist = inner->plist;
	while(CONSP(plist) && CONSP(VCDR(plist)))
	{
	    if(VCAR(plist) == prop)
		return VCAR(VCDR(plist));
	    plist = VCDR(VCDR(plist));
	}
	inner = inner->parent;
    }
    return sym_nil;
}

_PR VALUE cmd_extent_put(VALUE, VALUE, VALUE);
DEFUN("extent-put", cmd_extent_put, subr_extent_put,
      (VALUE prop, VALUE val, VALUE extent), V_Subr3, DOC_extent_put) /*
::doc:extent_put::
extent-put PROPERTY VALUE EXTENT

Set the value of PROPERTY (a symbol) in EXTENT to VALUE.

The special properties `front-sticky' and `rear-sticky' control whether
characters inserted adjacent to the start or end of the extent are
absorbed into the extent or not (if the related sticky property is non-
nil).
::end:: */
{
    VALUE plist;
    DECLARE1(prop, SYMBOLP);
    DECLARE3(extent, EXTENTP);

    if(prop == sym_front_sticky || prop == sym_rear_sticky)
    {
	u_long bit = (prop == sym_front_sticky
		      ? EXTFF_OPEN_START : EXTFF_OPEN_END);
	extent = VAL((prop == sym_front_sticky
		      ? find_first_frag : find_last_frag)(VEXTENT(extent)));
	VEXTENT(extent)->car &= ~bit;
	if(!NILP(val))
	    VEXTENT(extent)->car |= bit;
	return val;
    }

    plist = VEXTENT(extent)->plist;
    while(CONSP(plist) && CONSP(VCDR(plist)))
    {
	if(VCAR(plist) == prop)
	{
	    VCAR(VCDR(plist)) = val;
	    return val;
	}
	plist = VCDR(VCDR(plist));
    }
    cmd_set_extent_plist(extent,
			 cmd_cons(prop,
				  cmd_cons(val, VEXTENT(extent)->plist)));
    return val;
}

_PR VALUE cmd_buffer_get(VALUE, VALUE, VALUE);
DEFUN("buffer-get", cmd_buffer_get, subr_buffer_get,
      (VALUE prop, VALUE pos, VALUE tx), V_Subr3, DOC_buffer_get) /*
::doc:buffer_get::
buffer-get PROPERTY [POSITION] [BUFFER]

Get the value of PROPERTY (a symbol) at POSITION in BUFFER.
::end:: */
{
    VALUE e;
    DECLARE1(prop, SYMBOLP);

    e = cmd_get_extent(pos, tx);
    return cmd_extent_get(prop, e);
}

_PR VALUE cmd_buffer_put(VALUE, VALUE, VALUE, VALUE);
DEFUN("buffer-put", cmd_buffer_put, subr_buffer_put,
      (VALUE prop, VALUE val, VALUE pos, VALUE tx), V_Subr4, DOC_buffer_put) /*
::doc:buffer_put::
buffer-put PROPERTY VALUE [POSITION] [BUFFER]

Set the value of PROPERTY (a symbol) at POSITION in BUFFER to VALUE. Note
that this sets the property in the innermost extent containing POSITION,
possibly falling back to the global extent covering the entire buffer.
::end:: */
{
    VALUE e;
    DECLARE1(prop, SYMBOLP);

    e = cmd_get_extent(pos, tx);
    return cmd_extent_put(prop, val, e);
}

_PR VALUE cmd_buffer_symbol_value(VALUE, VALUE, VALUE, VALUE);
DEFUN("buffer-symbol-value", cmd_buffer_symbol_value, subr_buffer_symbol_value,
      (VALUE symbol, VALUE pos, VALUE tx, VALUE no_err), V_Subr4,
      DOC_buffer_symbol_value) /*
::doc:buffer_symbol_value::
buffer-symbol-value SYMBOL [POSITION | EXTENT] [BUFFER] [NO-ERROR-IF-VOID]

Return the local value of the variable named by SYMBOL at POSITION in
BUFFER. Alternatively, if an extent is given as the second argument
search from this extent upwards.

If NO-ERROR-IF-VOID is non-nil, no error will be signalled if the variable
has no value at the specified position, otherwise a void-value error
is signalled.
::end:: */
{
    VALUE e;
    DECLARE1(symbol, SYMBOLP);
    if(!EXTENTP(pos))
	e = cmd_get_extent(pos, tx);
    else
	e = pos;
    if(!NILP(e))
    {
	Lisp_Extent *inner = VEXTENT(e);
	while(inner != 0)
	{
	    VALUE cell = cmd_assq(symbol, inner->locals);
	    if(cell && CONSP(cell))
		return VCDR(cell);
	    inner = inner->parent;
	}
    }	    
    if(NILP(no_err))
	return cmd_signal(sym_void_value, LIST_1(symbol));
    else
	return void_value;
}

_PR VALUE cmd_extent_set(VALUE extent, VALUE symbol, VALUE val);
DEFUN("extent-set", cmd_extent_set, subr_extent_set,
      (VALUE symbol, VALUE val, VALUE extent), V_Subr3, DOC_extent_set) /*
::doc:extent_set::
extent-set SYMBOL VALUE EXTENT

Set the local value of the variable named SYMBOL in EXTENT to VALUE.
::end:: */
{
    VALUE vars;
    Lisp_Extent *e;
    DECLARE1(symbol, SYMBOLP);
    DECLARE3(extent, EXTENTP);
    vars = VEXTENT(extent)->locals;
    if(!NILP(vars))
    {
	VALUE cell = cmd_assq(symbol, vars);
	if(!NILP(cell))
	{
	    VCDR(cell) = val;
	    return val;
	}
    }
    vars = cmd_cons(cmd_cons(symbol, val), vars);

    /* Update all fragments of the extent. TODO: don't need to
       call fff, just go backwards then forwards. */
    e = find_first_frag(VEXTENT(extent));
    while(e != 0)
    {
	e->locals = vars;
	e = e->frag_next;
    }

    return val;
}


/* functions for housekeeping.c to call */

void
adjust_extents_add_cols(Lisp_Extent *x, long add_x, long col, long row)
{
    for(; x != 0 && x->end.row >= row; x = x->left_sibling)
    {
	if(x->first_child != 0 && x->start.row <= row && x->end.row >= row)
	    adjust_extents_add_cols(x->first_child, add_x,
				    col, row - x->start.row);
	if(x->start.row == row
	   && (x->start.col > col
	       || (!(x->car & EXTFF_OPEN_START)
		   && x->start.col == col)))
	{
	    x->start.col += add_x;
	}
	if(x->end.row == row
	   && (x->end.col > col
	       || ((x->car & EXTFF_OPEN_END)
		   && x->end.col == col)))
	{
	    x->end.col += add_x;
	}
    }
}

void
adjust_extents_sub_cols(Lisp_Extent *x, long sub_x, long col, long row)
{
    for(; x != 0 && x->end.row >= row; x = x->right_sibling)
    {
	if(x->first_child != 0 && x->start.row <= row && x->end.row >= row)
	    adjust_extents_sub_cols(x->first_child, sub_x,
				    col, row - x->start.row);
	if(x->start.row == row && x->start.col > col)
	    x->start.col = MIN(col, x->start.col - sub_x);
	if(x->end.row == row && x->end.col > col)
	    x->end.col = MIN(col, x->end.col - sub_x);
    }
}

void
adjust_extents_add_rows(Lisp_Extent *x, long add_y, long row)
{
    for(; x != 0 && x->end.row >= row; x = x->right_sibling)
    {
	if(x->start.row > row
	   || (x->start.row == row
	       && x->start.col == 0
	       && !(x->car & EXTFF_OPEN_START)))
	{
	    x->start.row += add_y;
	}
	else if(x->first_child != 0 && row <= x->end.row)
	    adjust_extents_add_rows(x->first_child, add_y,
				    row - x->start.row);
	if(x->end.row > row
	   || (x->end.col == 0
	       && x->end.row == row
	       && (x->car & EXTFF_OPEN_END)))
	{
	    x->end.row += add_y;
	}
    }
}

void
adjust_extents_sub_rows(Lisp_Extent *x, long sub_y, long row)
{
    for(; x != 0 && x->end.row >= row; x = x->right_sibling)
    {
	if(x->start.row > row)
	    x->start.row = POS(x->start.row - sub_y);
	else if(x->first_child != 0 && row <= x->end.row)
	    adjust_extents_sub_rows(x->first_child, sub_y,
				    row - x->start.row);
	if(x->end.row > row)
	    x->end.row = POS(x->end.row - sub_y);
    }
}

void
adjust_extents_split_row(Lisp_Extent *x, long col, long row)
{
    for(; x != 0 && x->end.row >= row; x = x->right_sibling)
    {
	if(x->start.row > row)
	{
	    if(x->first_child != 0)
		adjust_extents_add_rows(x->first_child, 1, row - x->start.row);
	    x->start.row++;
	}
	else if(x->start.row == row
		&& (x->start.col > col
		    || (x->start.col == col
			&& !(x->car & EXTFF_OPEN_START))))
	{
	    if(x->first_child != 0)
		adjust_extents_sub_cols(x->first_child, col, x->start.col, 0);
	    x->start.row++;
	    x->start.col -= col;
	}
	else if(x->first_child)
	    adjust_extents_split_row(x->first_child, col, row - x->start.row);

	if(x->end.row > row)
	    x->end.row++;
	else if(x->end.row == row
		&& (x->end.col > col
		    || (x->end.col == col
			&& (x->car & EXTFF_OPEN_END))))
	{
	    x->end.row++;
	    x->end.col -= col;
	}
    }
}

void
adjust_extents_join_rows(Lisp_Extent *x, long col, long row)
{
    for(; x != 0 && x->end.row >= row; x = x->right_sibling)
    {
	if(x->first_child != 0)
	    adjust_extents_join_rows(x->first_child, col, row - x->start.row);

	if(x->start.row == row + 1)
	{
	    x->start.row--;
	    x->start.col += col;
	}
	else if(x->start.row > row + 1)
	    x->start.row--;

	if(x->end.row == row + 1)
	{
	    x->end.row--;
	    x->start.col += col;
	}
	else if(x->end.row > row + 1)
	    x->end.row--;
    }
}


/* Misc. stuff */

void
mark_extent(Lisp_Extent *e)
{
    MARKVAL(VAL(e));
    e = e->first_child;
    while(e != 0)
    {
	mark_extent(e);
	e = e->right_sibling;
    }
}

void
extent_sweep(void)
{
    Lisp_Extent *e = allocated_extents;
    allocated_extents = 0;
    while(e != 0)
    {
	Lisp_Extent *next = e->next;
	if(!GC_CELL_MARKEDP(VAL(e)))
	    FREE_OBJECT(e);
	else
	{
	    GC_CLR_CELL(VAL(e));
	    e->next = allocated_extents;
	    allocated_extents = e;
	}
	e = next;
    }
}

void
extent_prin(VALUE strm, VALUE e)
{
    char buf[128];
    sprintf(buf, "#<extent (%ld,%ld)->(%ld,%ld)",
	    VEXTENT(e)->start.row, VEXTENT(e)->start.col,
	    VEXTENT(e)->end.row, VEXTENT(e)->end.col);
    stream_puts(strm, buf, -1, FALSE);
#ifdef DEBUG
    stream_putc(strm, ' ');
    print_val(strm, VEXTENT(e)->plist);
    stream_puts(strm, " [", -1, FALSE);
    {
	Lisp_Extent *x = VEXTENT(e)->first_child;
	while(x != 0)
	{
	    extent_prin(strm, VAL(x));
	    x = x->right_sibling;
	}
    }
    stream_putc(strm, ']');
#endif
    stream_putc(strm, '>');
}

void
extent_init(void)
{
    ADD_SUBR(subr_make_extent);
    ADD_SUBR(subr_delete_extent);
    ADD_SUBR(subr_delete_all_extents);
    ADD_SUBR(subr_move_extent);
    ADD_SUBR(subr_get_extent);
    ADD_SUBR(subr_extent_parent);
    ADD_SUBR(subr_extent_root);
    ADD_SUBR(subr_extent_plist);
    ADD_SUBR(subr_set_extent_plist);
    ADD_SUBR(subr_extent_get);
    ADD_SUBR(subr_buffer_get);
    ADD_SUBR(subr_extent_put);
    ADD_SUBR(subr_buffer_put);
    ADD_SUBR(subr_buffer_symbol_value);
    ADD_SUBR(subr_extent_set);
    INTERN(front_sticky);
    INTERN(rear_sticky);
}