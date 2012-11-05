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
#include <limits.h>
#include <setjmp.h>
#include <assert.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#define DEBUG

DEFSYM(front_sticky, "front-sticky");
DEFSYM(rear_sticky, "rear-sticky");
DEFSYM(local_variables, "local-variables");
DEFSYM(catch_variables, "catch-variables");

int extent_type;

static Lisp_Extent *allocated_extents;

#ifdef DEBUG
static void
assert_invariants (Lisp_Extent *e)
{
    if (e == 0)
	return;
    assert (e->left_sibling == 0 || e->left_sibling->right_sibling == e);
    assert (e->right_sibling == 0 || e->right_sibling->left_sibling == e);
    assert (e->first_child == 0 || e->first_child->parent == e);
    assert (e->last_child == 0 || e->last_child->parent == e);
    assert (e->frag_pred == 0 || e->frag_pred->frag_next == e);
    assert (e->frag_next == 0 || e->frag_next->frag_pred == e);
}
#else
# define assert_invariants(e) do { } while (0)
#endif

/* Make all links in E null. */
static inline void
clean_node(Lisp_Extent *e)
{
    e->parent = 0;
    e->left_sibling = e->right_sibling = 0;
    e->first_child = e->last_child = 0;
    e->frag_next = e->frag_pred = 0;
}

/* Create a new extent. All links are null. If CLONEE is non-null, copy
   the START, END, TX and PLIST fields from it. */
static Lisp_Extent *
alloc_extent(Lisp_Extent *clonee)
{
    Lisp_Extent *x = rep_alloc(sizeof(Lisp_Extent));
    if(x == 0)
    {
	rep_mem_error();
	return 0;
    }

    x->car = extent_type;
    x->next = allocated_extents;
    allocated_extents = x;
    clean_node(x);

    if(clonee != 0)
    {
	x->start = clonee->start;
	x->end = clonee->end;
	x->tx = clonee->tx;
	x->plist = clonee->plist;
	x->locals = clonee->locals;
    }
    else
    {
	x->locals = Qnil;
	x->plist = Qnil;
    }

    assert_invariants (x);

    return x;
}

/* Return the first fragment in the chain that E is a member of. */
static inline Lisp_Extent *
find_first_frag(Lisp_Extent *e)
{
    while(e->frag_pred != 0)
    {
	assert_invariants (e);
	e = e->frag_pred;
    }
    assert_invariants (e);
    return e;
}

/* Return the last fragment in the chain that E is a member of. */
static inline Lisp_Extent *
find_last_frag(Lisp_Extent *e)
{
    while(e->frag_next != 0)
    {
	assert_invariants (e);
	e = e->frag_next;
    }
    assert_invariants (e);
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
	left->frag_next = right->frag_next;

	clean_node(right);
	
	assert_invariants (left);
	assert_invariants (right);
    }
}

/* Unlink the single fragment E. */
static void
unlink_extent_fragment(Lisp_Extent *e)
{
    if(e->parent == 0)
	return;

    if(e->frag_next != 0)
	e->frag_next->frag_pred = e->frag_pred;
    if(e->frag_pred != 0)
	e->frag_pred->frag_next = e->frag_next;

    assert_invariants (e->frag_next);
    assert_invariants (e->frag_pred);

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
	    assert_invariants (x);
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

	assert_invariants (e->left_sibling);
	assert_invariants (e->right_sibling);
	assert_invariants (e->first_child);
	assert_invariants (e->last_child);
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

	assert_invariants (e->left_sibling);
	assert_invariants (e->right_sibling);
	assert_invariants (e->first_child);
	assert_invariants (e->last_child);
    }
    clean_node(e);
}

/* Unlink extent E (and all fragments chained off it). */
static void
unlink_extent(Lisp_Extent *e)
{
    int n, i;
    Lisp_Extent *x, **buf;

    if(e->parent == 0)
	return;

    e = find_first_frag(e);

    /* scan the frags into a buffer (the linkage gets broken) */
    n = 0;
    for (x = e; x != 0; x = x->frag_next)
	n++;
    buf = alloca (sizeof (Lisp_Extent *) * n);
    for (x = e, i = 0; x != 0; x = x->frag_next, i++)
	buf[i] = x;

    for (i = 0; i < n; i++)
    {
	unlink_extent_fragment(buf[i]);
	assert_invariants (buf[i]);
    }
}

static void
unlink_extent_recursively(Lisp_Extent *e)
{
    while(e->first_child != 0)
	unlink_extent_recursively(e->first_child);
    if(e->parent != 0)
	unlink_extent_fragment(e);
    assert_invariants (e);
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

	    assert_invariants (e);
	    assert_invariants (x);
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
	    if(e->frag_pred != 0)
		e->frag_pred->frag_next = frag;
	    frag->frag_next = e;
	    e->frag_pred = frag;

	    assert_invariants (e);
	    assert_invariants (x);
	    assert_invariants (frag);

	    insert_extent(frag, x->parent);

	    assert_invariants (e);
	    assert_invariants (x);
	    assert_invariants (frag);
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
		if(e->frag_next != 0)
		    e->frag_next->frag_pred = frag;
		frag->frag_pred = e;
		e->frag_next = frag;

		e->start.row -= x->start.row;
		e->end.row -= x->start.row;

		assert_invariants (e);
		assert_invariants (x);
		assert_invariants (frag);

		insert_extent(e, x);

		assert_invariants (e);
		assert_invariants (x);
		assert_invariants (frag);

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

    assert_invariants (e);
    assert_invariants (root);
}

/* Find the global offset of the first row of extent E. */
static intptr_t
row_delta(Lisp_Extent *e)
{
    intptr_t delta = 0;
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
	assert_invariants (x);
    }
    return root;
}

/* Cache stats */
static int extent_cache_misses, extent_cache_hits, extent_cache_near_misses;

/* Return the innermost extent containing position POS. */
Lisp_Extent *
find_extent(Lisp_Extent *root, Pos *pos)
{
    Lisp_Buffer *tx = root->tx;
    int i, out = -1, oldest = 0;
    uint32_t oldest_lru = UINT32_MAX;
    static uint32_t lru_time;
    Lisp_Extent *x;

    /* First check the cache. */
    for(i = 0; i < EXTENT_CACHE_SIZE; i++)
    {
	struct cached_extent *ce = &tx->extent_cache[i];
	if(ce->extent)
	{
	    if(PPOS_EQUAL_P(pos, &ce->pos))
	    {
		/* Bingo! direct hit */
		extent_cache_hits++;
		ce->lru_clock = ++lru_time;
		return ce->extent;
	    }
	    else
	    {
		intptr_t delta = row_delta(ce->extent);
		if((pos->row > ce->extent->start.row + delta
		    || (pos->row == ce->extent->start.row + delta
			&& pos->col >= ce->extent->start.col))
		   && (pos->row < ce->extent->end.row + delta
		       || (pos->row == ce->extent->end.row + delta
			   && pos->col < ce->extent->end.col)))
		{
		    /* Not direct. But ce[i] contains POS. If this isn't
		       the only item containing POS, use the innermost. */
		    if(out == -1)
			out = i;
		    /* FIXME: check which is innermost.. */
		}
		if(ce->lru_clock < oldest_lru)
		{
		    oldest = i;
		    oldest_lru = ce->lru_clock;
		}
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
    {
	extent_cache_near_misses++;
	x = find_extent_forwards(tx->extent_cache[out].extent, pos);
    }
    else
    {
	extent_cache_misses++;
	x = find_extent_forwards(tx->global_extent, pos);
    }

    /* Add X to the cache in place of OLDEST. */
    tx->extent_cache[oldest].extent = x;
    tx->extent_cache[oldest].pos = *pos;
    tx->extent_cache[oldest].lru_clock = ++lru_time;

    assert_invariants (x);
    return x;
}

/* Map the function MAP_FUNC(E, DATA) over all innermost extents E
   containing part of the section of TX specified by START, END.
   This function can be (and is) safely longjmp'd through. */
void
map_section_extents(void (*map_func)(Lisp_Extent *x, void *data),
		    Lisp_Extent *root, Pos *start, Pos *end, void *data)
{
    Lisp_Extent *x = find_extent(root, start);
    Pos s_copy = *start, e_copy = *end;
    intptr_t delta = row_delta(x->parent);
    s_copy.row -= delta; e_copy.row -= delta;

    while(x != 0 && PPOS_LESS_P(&x->start, &e_copy))
    {
	if(PPOS_GREATER_P(&x->end, &s_copy))
	{
	    /* Deleting X in here would screw things up..
	       Not much that can be done though. And it shouldn't
	       crash (famous last words..) */
	    map_func(x, data);
	}

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
	assert_invariants (x);
    }
}

/* Notify the cache that some extents in TX were changed. */
static inline void
invalidate_extent_cache(Lisp_Buffer *tx)
{
    int i;
    for(i = 0; i < EXTENT_CACHE_SIZE; i++)
	tx->extent_cache[i].extent = 0;
}

/* Create the all-encompassing root extent for buffer TX. */
void
make_global_extent(Lisp_Buffer *tx)
{
    tx->global_extent = alloc_extent(0);
    tx->global_extent->tx = tx;
    reset_global_extent(tx);
}

/* After loading a file into TX, call this. */
void
reset_global_extent(Lisp_Buffer *tx)
{
    Lisp_Extent *e = tx->global_extent;
    unlink_extent_recursively(e);
    e->start.row = 0;
    e->start.col = 0;
    e->end.row = tx->line_count;
    e->end.col = tx->lines[tx->line_count-1].ln_Strlen - 1;
    e->car = extent_type | EXTFF_OPEN_START | EXTFF_OPEN_END;
    invalidate_extent_cache(tx);
}


/* Lisp functions */

DEFUN("make-extent", Fmake_extent, Smake_extent,
      (repv start, repv end, repv plist), rep_Subr3) /*
::doc:make-extent::
make-extent START END [PLIST]

Create and return a new extent in the current buffer from START to END.
It's property list is initially set to PLIST. It will have no local
variables.

Note that an extent may not always be `eq' to itself. The `equal' comparison
will work reliably however.
::end:: */
{
    Lisp_Extent *extent;
    Lisp_Buffer *tx = curr_vw->tx;
    rep_DECLARE1(start, POSP);
    rep_DECLARE2(end, POSP);

    if(VROW(start) < 0 || VROW(start) >= tx->line_count
       || VROW(end) < 0 || VROW(end) >= tx->line_count
       || POS_GREATER_P(start, end))
	return Fsignal(Qinvalid_pos, rep_list_2(start, end));

    extent = alloc_extent(0);
    if(extent == 0)
	return 0;

    COPY_VPOS(&extent->start, start);
    COPY_VPOS(&extent->end, end);
    extent->tx = tx;
    extent->plist = plist;
    extent->locals = Qnil;

    insert_extent(extent, tx->global_extent);
    invalidate_extent_cache(tx);

    return rep_VAL(extent);
}

DEFSTRING(no_delete_root, "Deleting root extent");
DEFUN("delete-extent", Fdelete_extent, Sdelete_extent,
      (repv extent), rep_Subr1) /*
::doc:delete-extent::
delete-extent EXTENT

Delete EXTENT from the buffer containing it. An error is signalled if
EXTENT is the root extent covering the entire buffer.
::end:: */
{
    rep_DECLARE1(extent, EXTENTP);

    if(VEXTENT(extent) == VEXTENT(extent)->tx->global_extent)
	return Fsignal(Qerror, rep_list_2(rep_VAL(&no_delete_root), extent));

    unlink_extent(VEXTENT(extent));
    invalidate_extent_cache(VEXTENT(extent)->tx);
    return extent;
}

DEFUN_INT("delete-all-extents", Fdelete_all_extents,
	  Sdelete_all_extents, (repv extent), rep_Subr1, "") /*
::doc:delete-all-extents::
delete-all-extents [ROOT]

Delete all extents contained within ROOT (or the root of the current buffer).
Note that it's not possible to delete the global extent of a buffer itself.
::end:: */
{
    if(!EXTENTP(extent))
	extent = rep_VAL(curr_vw->tx->global_extent);
    unlink_extent_recursively(VEXTENT(extent));
    invalidate_extent_cache(VEXTENT(extent)->tx);
    return extent;
}

DEFUN("move-extent", Fmove_extent, Smove_extent,
      (repv extent, repv start, repv end), rep_Subr3) /*
::doc:move-extent::
move-extent EXTENT START END

Set the start and end positions of EXTENT to START and END respectively,
without changing the buffer that EXTENT refers to.
::end:: */
{
    rep_DECLARE1(extent, EXTENTP);
    rep_DECLARE2(start, POSP);
    rep_DECLARE3(end, POSP);

    unlink_extent(VEXTENT(extent));
    COPY_VPOS(&VEXTENT(extent)->start, start);
    COPY_VPOS(&VEXTENT(extent)->end, end);
    insert_extent(VEXTENT(extent), VEXTENT(extent)->tx->global_extent);
    invalidate_extent_cache(VEXTENT(extent)->tx);

    return extent;
}

DEFUN("get-extent", Fget_extent, Sget_extent, (repv pos, repv tx), rep_Subr2) /*
::doc:get-extent::
get-extent [POSITION] [BUFFER]

Return the innermost extent at POSITION in BUFFER (by default the cursor
position of the current buffer).
::end:: */
{
    Lisp_Extent *e;
    Pos ppos;
    if(!BUFFERP(tx))
    {
	tx = rep_VAL(curr_vw->tx);
	if(!POSP(pos))
	    pos = curr_vw->cursor_pos;
    }
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    COPY_VPOS(&ppos, pos);
    e = find_extent(VBUFFER(tx)->global_extent, &ppos);

    return (e != 0) ? rep_VAL(e) : Qnil;
}

struct map_extents_data {
    repv fun;
    jmp_buf exit;
};

static void
map_extents_callback (Lisp_Extent *e, void *data)
{
    struct map_extents_data *d = data;
    /* The call to rep_funcall will protect FUN and E thoughout. */
    if(!rep_call_lisp1(d->fun, rep_VAL(e)))
	longjmp(d->exit, 1);
}

DEFUN("map-extents", Fmap_extents, Smap_extents,
      (repv fun, repv start, repv end), rep_Subr3) /*
::doc:map-extents::
map-extents FUNCTION START END

Call (FUNCTION EXTENT) for all innermost extent fragments containing
positions between START and END in the current buffer.

Note that the behaviour of this function is undefined if extents are
deleted from within the callback function.
::end:: */
{
    Pos s_copy, e_copy;
    struct map_extents_data data;

    rep_DECLARE2(start, POSP);
    rep_DECLARE3(end, POSP);

    COPY_VPOS(&s_copy, start);
    COPY_VPOS(&e_copy, end);

    data.fun = fun;
    switch(setjmp(data.exit))
    {
    case 0:
	map_section_extents(map_extents_callback,
			    curr_vw->tx->global_extent,
			    &s_copy, &e_copy, &data);
	break;

    case 1:
	return 0;
    }

    return Qt;
}

DEFUN("extent-start", Fextent_start, Sextent_start, (repv extent), rep_Subr1) /*
::doc:extent-start::
extent-start EXTENT

Return the position of the first character in EXTENT.
::end:: */
{
    rep_DECLARE1(extent, EXTENTP);
    extent = rep_VAL(find_first_frag(VEXTENT(extent)));
    return make_pos(VEXTENT(extent)->start.col,
		    VEXTENT(extent)->start.row + row_delta(VEXTENT(extent)));
}

DEFUN("extent-end", Fextent_end, Sextent_end, (repv extent), rep_Subr1) /*
::doc:extent-end::
extent-end EXTENT

Return the position of the character following EXTENT.
::end:: */
{
    rep_DECLARE1(extent, EXTENTP);
    extent = rep_VAL(find_last_frag(VEXTENT(extent)));
    return make_pos(VEXTENT(extent)->end.col,
		    VEXTENT(extent)->end.row + row_delta(VEXTENT(extent)));
}

DEFUN("extent-parent", Fextent_parent, Sextent_parent, (repv extent), rep_Subr1) /*
::doc:extent-parent::
extent-parent EXTENT

Return parent extent of EXTENT. Returns nil if EXTENT is the global extent
covering the entire buffer.
::end:: */
{
    rep_DECLARE1(extent, EXTENTP);
    return VEXTENT(extent)->parent ? rep_VAL(VEXTENT(extent)->parent) : Qnil;
}

DEFUN("extent-root", Fextent_root, Sextent_root, (repv tx), rep_Subr1) /*
::doc:extent-root::
extent-root [BUFFER]

Return the global extent covering the whole of BUFFER. This extent has no
parent.
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return rep_VAL(VBUFFER(tx)->global_extent);
}

DEFUN("extent-plist", Fextent_plist, Sextent_plist, (repv extent), rep_Subr1) /*
::doc:extent-plist::
extent-plist EXTENT

Return the property list associated with EXTENT.
::end:: */
{
    rep_DECLARE1(extent, EXTENTP);
    return VEXTENT(extent)->plist;
}

DEFUN("set-extent-plist", Fset_extent_plist, Sset_extent_plist,
      (repv extent, repv plist), rep_Subr2) /*
::doc:set-extent-plist::
set-extent-plist EXTENT PLIST

Set the property list associated with EXTENT (and all other fragments of
this extent) to PLIST.
::end:: */
{
    Lisp_Extent *e;
    rep_DECLARE1(extent, EXTENTP);

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

static void
set_extent_locals(Lisp_Extent *e, repv value)
{
    e = find_first_frag(e);
    while(e != 0)
    {
	e->locals = value;
	e = e->frag_next;
    }
}

DEFUN("extent-get", Fextent_get, Sextent_get,
      (repv extent, repv prop), rep_Subr2) /*
::doc:extent-get::
extent-get EXTENT PROPERTY

Return the value of PROPERTY (a symbol) in EXTENT, or any of its parents.
Returns nil if there is no value in this extent.

The special properties `front-sticky', `rear-sticky', `local-variables', and
`catch-variables' have values in every extent.
::end:: */
{
    Lisp_Extent *inner;
    rep_DECLARE1(extent, EXTENTP);
    rep_DECLARE2(prop, rep_SYMBOLP);
    inner = VEXTENT(extent);

    if(prop == Qfront_sticky || prop == Qrear_sticky)
    {
	extent = rep_VAL((prop == Qfront_sticky
		      ? find_first_frag : find_last_frag)(VEXTENT(extent)));
	return (VEXTENT(extent)->car & (prop == Qfront_sticky
					? EXTFF_OPEN_START
					: EXTFF_OPEN_END)) ? Qt : Qnil;
    }
    else if(prop == Qcatch_variables)
    {
	return ((VEXTENT(extent)->car & EXTFF_CATCH_VARIABLES)
		? Qt : Qnil);
    }
    else if(prop == Qlocal_variables)
	return VEXTENT(extent)->locals;

    while(inner != 0)
    {
	repv plist = inner->plist;
	while(rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
	{
	    if(rep_CAR(plist) == prop)
		return rep_CAR(rep_CDR(plist));
	    plist = rep_CDR(rep_CDR(plist));
	}
	inner = inner->parent;
    }
    return Qnil;
}

DEFUN("extent-put", Fextent_put, Sextent_put,
      (repv extent, repv prop, repv val), rep_Subr3) /*
::doc:extent-put::
extent-put EXTENT PROPERTY repv

Set the value of PROPERTY (a symbol) in EXTENT to repv.

The special properties `front-sticky' and `rear-sticky' control whether
characters inserted adjacent to the start or end of the extent are
absorbed into the extent or not (if the related sticky property is non-
nil).

The special property `local-variables' is an alist of (SYMBOL . repv) pairs,
each defining the value of the variable named SYMBOL in the extent. See
the `extent-set' and `buffer-symbol-value' functions.

If the special property `catch-variables' is non-nil, any buffer-local
variables (that don't have a permanent-local property) set from a
position within this extent, that aren't bound in an extent inside this
one, will have their value set in this extent, using the `extent-set'
function.
::end:: */
{
    repv plist;
    rep_DECLARE1(extent, EXTENTP);
    rep_DECLARE2(prop, rep_SYMBOLP);

    if(prop == Qfront_sticky || prop == Qrear_sticky)
    {
	repv bit = (prop == Qfront_sticky
		    ?  : EXTFF_OPEN_END);
	extent = rep_VAL((prop == Qfront_sticky
		      ? find_first_frag : find_last_frag)(VEXTENT(extent)));
	VEXTENT(extent)->car &= ~bit;
	if(!rep_NILP(val))
	    VEXTENT(extent)->car |= bit;
    }
    else if(prop == Qcatch_variables)
    {
	extent = rep_VAL(find_first_frag(VEXTENT(extent)));
	while(VEXTENT(extent) != 0)
	{
	    VEXTENT(extent)->car &= ~EXTFF_CATCH_VARIABLES;
	    if(!rep_NILP(val))
		VEXTENT(extent)->car |= EXTFF_CATCH_VARIABLES;
	    extent = rep_VAL(VEXTENT(extent)->frag_next);
	}
    }
    else if(prop == Qlocal_variables)
    {
	if(!rep_LISTP(val))
	    return rep_signal_arg_error(val, 2);
	set_extent_locals(VEXTENT(extent), val);
    }
    else
    {
	plist = VEXTENT(extent)->plist;
	while(rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
	{
	    if(rep_CAR(plist) == prop)
	    {
		rep_CAR(rep_CDR(plist)) = val;
		return val;
	    }
	    plist = rep_CDR(rep_CDR(plist));
	}
	Fset_extent_plist(extent,
			     Fcons(prop,
				      Fcons(val, VEXTENT(extent)->plist)));
    }
    return val;
}

DEFUN("buffer-get", Fbuffer_get, Sbuffer_get,
      (repv prop, repv pos, repv tx), rep_Subr3) /*
::doc:buffer-get::
buffer-get PROPERTY [POSITION] [BUFFER]

Get the value of PROPERTY (a symbol) at POSITION in BUFFER.
::end:: */
{
    repv e;
    rep_DECLARE1(prop, rep_SYMBOLP);

    /* FIXME: this should search back up the stack. */
    e = Fget_extent(pos, tx);
    return Fextent_get(e, prop);
}

DEFUN("buffer-symbol-value", Fbuffer_symbol_value, Sbuffer_symbol_value,
      (repv symbol, repv pos, repv tx, repv no_err), rep_Subr4) /*
::doc:buffer-symbol-value::
buffer-symbol-value SYMBOL [POSITION | EXTENT] [BUFFER] [NO-ERROR-IF-VOID]

Return the local value of the variable named by SYMBOL at POSITION in
BUFFER. Alternatively, if an extent is given as the second argument
search from this extent upwards.

If NO-ERROR-IF-VOID is non-nil, no error will be signalled if the variable
has no value at the specified position, otherwise a void-value error
is signalled.
::end:: */
{
    repv e;
    rep_DECLARE1(symbol, rep_SYMBOLP);
    if(rep_SYM(symbol)->car & rep_SF_LOCAL)
    {
	if(!EXTENTP(pos))
	    e = Fget_extent(pos, tx);
	else
	    e = pos;
	if(!rep_NILP(e))
	{
	    Lisp_Extent *inner = VEXTENT(e);
	    while(inner != 0)
	    {
		repv cell = Fassq(symbol, inner->locals);
		if(cell && rep_CONSP(cell))
		    return rep_CDR(cell);
		inner = inner->parent;
	    }
	}
    }
    if(rep_NILP(no_err))
	return Fsignal(Qvoid_value, rep_LIST_1(symbol));
    else
	return rep_void_value;
}

static repv
deref_local_symbol (repv sym)
{
    return Fbuffer_symbol_value (sym, Qnil, Qnil, Qt);
}

DEFUN("extent-set", Fextent_set, Sextent_set,
      (repv extent, repv symbol, repv val), rep_Subr3) /*
::doc:extent-set::
extent-set EXTENT SYMBOL repv

Set the local value of the variable named SYMBOL in EXTENT to repv.
::end:: */
{
    repv vars;
    rep_DECLARE1(extent, EXTENTP);
    rep_DECLARE2(symbol, rep_SYMBOLP);
    vars = VEXTENT(extent)->locals;
    if(!rep_NILP(vars))
    {
	repv cell = Fassq(symbol, vars);
	if(cell && rep_CONSP(cell))
	{
	    rep_CDR(cell) = val;
	    return val;
	}
    }
    vars = Fcons(Fcons(symbol, val), vars);
    set_extent_locals(VEXTENT(extent), vars);
    if (!(rep_SYM(symbol)->car & rep_SF_SPECIAL))
	Fmake_variable_special (symbol);
    rep_SYM(symbol)->car |= rep_SF_LOCAL;
    return val;
}

/* Called by the `set' function. Sees if the value of SYMBOL (assuming it
   has the SF_BUFFER_LOCAL property) should be set in the current stack
   of extents. If so, sets it and returns true. */
bool
buffer_set_if_bound(repv symbol, repv value)
{
    Pos tem;
    Lisp_Extent *e;
    COPY_VPOS(&tem, curr_vw->cursor_pos);
    e = find_extent(curr_vw->tx->global_extent, &tem);
    while(e != 0)
    {
	repv cell = Fassq(symbol, e->locals);
	if(cell && rep_CONSP(cell))
	{
	    rep_CDR(cell) = value;
	    return true;
	}
	else if(e->car & EXTFF_CATCH_VARIABLES)
	{
	    repv tem = Fget(symbol, Qpermanent_local);
	    if(rep_NILP(tem))
	    {
		Fextent_set(rep_VAL(e), symbol, value);
		return true;
	    }
	}
	e = e->parent;
    }
    return false;
}

static repv
set_local_symbol (repv sym, repv value)
{
    Lisp_Buffer *tx = curr_vw->tx;
    if(buffer_set_if_bound(sym, value))
	return value;
    else if(rep_SYM(sym)->car & rep_SF_LOCAL)
    {
	/* Create a new buffer-local value */
	tx->global_extent->locals = Fcons(Fcons(sym, value),
					    tx->global_extent->locals);
	return value;
    }
    else
	return 0;
}

DEFUN("make-local-variable", Fmake_local_variable, Smake_local_variable,
      (repv sym), rep_Subr1) /*
::doc:make-local-variable::
make-local-variable SYMBOL

Gives the variable SYMBOL a buffer-local binding in the current buffer. It
will be the same as the default value to start with. If the current buffer
alread has a buffer-local binding for SYMBOL nothing happens.
Returns SYMBOL.
::end:: */
{
    repv slot;
    Lisp_Buffer *tx = curr_vw->tx;
    rep_DECLARE1(sym, rep_SYMBOLP);
    if (!(rep_SYM(sym)->car & rep_SF_SPECIAL))
	Fmake_variable_special (sym);
    rep_SYM(sym)->car |= rep_SF_LOCAL;
    slot = Fassq(sym, tx->global_extent->locals);
    if(!slot || !rep_CONSP(slot))
    {
	/* Need to create a binding. */
	repv value = Fsymbol_value (sym, Qt);
	tx->global_extent->locals = Fcons(Fcons(sym, value),
					    tx->global_extent->locals);
    }
    return sym;
}

DEFUN("make-variable-buffer-local", Fmake_variable_buffer_local,
      Smake_variable_buffer_local, (repv sym), rep_Subr1) /*
::doc:make-variable-buffer-local::
make-variable-buffer-local SYMBOL

Marks the variable SYMBOL as being automatically buffer-local. Any attempts
at setting SYMBOL result in the current buffer being given its own binding.
Returns SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if (!(rep_SYM(sym)->car & rep_SF_SPECIAL))
	Fmake_variable_special (sym);
    rep_SYM(sym)->car |= (rep_SF_LOCAL | rep_SF_SET_LOCAL);
    return sym;
}

DEFUN("buffer-variables", Fbuffer_variables, Sbuffer_variables,
      (repv tx), rep_Subr1) /*
::doc:buffer-variables::
buffer-variables [BUFFER]

Returns a list of (SYMBOL . VALUE) bindings which take effect when the
current buffer is BUFFER. (Only the buffer-wide bindings, not those
for each minor extent.)
::end:: */
{
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    return VBUFFER(tx)->global_extent->locals;
}

DEFUN("kill-all-local-variables", Fkill_all_local_variables,
      Skill_all_local_variables, (repv tx), rep_Subr1) /*
::doc:kill-all-local-variables::
kill-all-local-variables [BUFFER]

Remove all buffer-local variables from BUFFER that are not marked as being
permanent (i.e. their `permanent-local' property is unset or non-nil.)
::end:: */
{
    repv list;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    list = VBUFFER(tx)->global_extent->locals;
    VBUFFER(tx)->global_extent->locals = Qnil;
    while(rep_CONSP(list))
    {
	if(rep_NILP(Fget(rep_CAR(rep_CAR(list)), Qpermanent_local)))
	    list = rep_CDR(list);
	else
	{
	    repv next = rep_CDR(list);
	    rep_CDR(list) = VBUFFER(tx)->global_extent->locals;
	    VBUFFER(tx)->global_extent->locals = list;
	    list = next;
	}
    }
    kill_buffer_local_variables(VBUFFER(tx));
    return tx;
}

DEFUN("kill-local-variable", Fkill_local_variable, Skill_local_variable,
      (repv sym, repv tx), rep_Subr2) /*
::doc:kill-local-variable::
kill-local-variable SYMBOL [BUFFER]

Remove the buffer-local value of the symbol SYMBOL in the specified buffer.
::end:: */
{
    repv list;
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    list = VBUFFER(tx)->global_extent->locals;
    VBUFFER(tx)->global_extent->locals = Qnil;
    while(rep_CONSP(list))
    {
	repv nxt = rep_CDR(list);
	if(rep_CAR(list) != sym)
	{
	    rep_CDR(list) = VBUFFER(tx)->global_extent->locals;
	    VBUFFER(tx)->global_extent->locals = list;
	}
	list = nxt;
    }
    return sym;
}


/* functions for housekeeping.c to call */

void
adjust_extents_add_cols(Lisp_Extent *x, intptr_t add_x,
			intptr_t col, intptr_t row)
{
    if(x->parent == 0)
	invalidate_extent_cache(x->tx);
    for(; x != 0; x = x->right_sibling)
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

	if(x->end.row > row)
	    break;
    }
}

void
adjust_extents_sub_cols(Lisp_Extent *x, intptr_t sub_x,
			intptr_t col, intptr_t row)
{
    if(x->parent == 0)
	invalidate_extent_cache(x->tx);
    for(; x != 0; x = x->right_sibling)
    {
    top:
	if(x->first_child != 0 && x->start.row <= row && x->end.row >= row)
	    adjust_extents_sub_cols(x->first_child, sub_x,
				    col, row - x->start.row);
	if(x->start.row == row && x->start.col > col)
	    x->start.col = MAX(col, x->start.col - sub_x);
	if(x->end.row == row && x->end.col > col)
	    x->end.col = MAX(col, x->end.col - sub_x);

	if(x->start.row == x->end.row && x->start.col == x->end.col)
	{
	    /* Null extent. May as well be deleted. */
	    Lisp_Extent *r = x->right_sibling;
	    unlink_extent_fragment(x);
	    if(r != 0)
	    {
		x = r;
		goto top;
	    }
	    else
		break;
	}

	if(x->end.row > row)
	    break;
    }
}

void
adjust_extents_add_rows(Lisp_Extent *x, intptr_t add_y, intptr_t row)
{
    if(x->parent == 0)
	invalidate_extent_cache(x->tx);
    for(; x != 0; x = x->right_sibling)
    {
	if(x->start.row > row
	   || (x->start.row == row
	       && (x->start.col > 0
		   || !(x->car & EXTFF_OPEN_START))))
	{
	    x->start.row += add_y;
	}
	else if(x->first_child != 0 && row <= x->end.row)
	    adjust_extents_add_rows(x->first_child, add_y,
				    row - x->start.row);
	if(x->end.row > row
	   || (x->end.row == row
	       && (x->end.col > 0
		   || (x->car & EXTFF_OPEN_END))))
	{
	    x->end.row += add_y;
	}
    }
}

void
adjust_extents_sub_rows(Lisp_Extent *x, intptr_t sub_y, intptr_t row)
{
    if(x->parent == 0)
	invalidate_extent_cache(x->tx);
    for(; x != 0; x = x->right_sibling)
    {
    top:
	if(x->first_child != 0 && row >= x->start.row && row <= x->end.row)
	    adjust_extents_sub_rows(x->first_child, sub_y, row - x->start.row);
	if(x->start.row >= row)
	{
	    if(x->start.row >= row + sub_y)
		x->start.row -= sub_y;
	    else
	    {
		x->start.row = row;
		x->start.col = 0;
	    }
	}
	if(x->end.row >= row)
	{
	    if(x->end.row >= row + sub_y)
		x->end.row -= sub_y;
	    else
	    {
		x->end.row = row;
		x->end.col = 0;
	    }
	}

	if(x->start.row == x->end.row && x->start.col == x->end.col)
	{
	    /* Null extent. Delete it. */
	    Lisp_Extent *r = x->right_sibling;
	    unlink_extent_fragment(x);
	    if(r != 0)
	    {
		x = r;
		goto top;
	    }
	    else
		break;
	}
    }
}

void
adjust_extents_split_row(Lisp_Extent *x, intptr_t col, intptr_t row)
{
    if(x->parent == 0)
	invalidate_extent_cache(x->tx);
    for(; x != 0; x = x->right_sibling)
    {
	if(x->start.row > row)
	    x->start.row++;
	else if(x->start.row == row
		&& (x->start.col > col
		    || (x->start.col == col
			&& !(x->car & EXTFF_OPEN_START))))
	{
	    if(x->first_child != 0)
		adjust_extents_sub_cols(x->first_child, col, 0, 0);
	    x->start.row++;
	    x->start.col -= col;
	}
	else if(x->first_child != 0)
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
adjust_extents_join_rows(Lisp_Extent *x, intptr_t col, intptr_t row)
{
    if(x->parent == 0)
	invalidate_extent_cache(x->tx);
    for(; x != 0; x = x->right_sibling)
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
	    x->end.col += col;
	}
	else if(x->end.row > row + 1)
	    x->end.row--;
    }
}


/* Handling the visible-extents list. */

void
start_visible_extent (Lisp_View *vw, Lisp_Extent *e,
		      intptr_t start_col, intptr_t start_row)
{
    struct visible_extent *x;
    e = find_first_frag (e);
    x = vw->window->visible_extents;
    while (x != 0 && (x->extent != e || x->vw != vw))
	x = x->next;
    if (x != 0)
    {
	if (start_row < x->start_row
	    || (start_row == x->start_row && start_col < x->start_col))
	{
	    x->start_row = start_row;
	    x->start_col = start_col;
	}
    }
    else
    {
	x = rep_alloc (sizeof (struct visible_extent));
	x->next = vw->window->visible_extents;
	vw->window->visible_extents = x;
	x->extent = e;
	x->vw = vw;
	x->start_col = start_col;
	x->start_row = start_row;
	x->end_col = start_col;
	x->end_row = start_row;
    }
}

void
end_visible_extent (Lisp_View *vw, Lisp_Extent *e,
		    intptr_t end_col, intptr_t end_row)
{
    struct visible_extent *x;
    e = find_first_frag (e);
    x = vw->window->visible_extents;
    while (x != 0 && (x->extent != e || x->vw != vw))
	x = x->next;
    assert (x != 0);
    if (end_row > x->end_row
	|| (end_row == x->end_row && end_col > x->end_col))
    {
	x->end_col = end_col;
	x->end_row = end_row;
    }
}

void
free_visible_extents (Lisp_Window *w)
{
    struct visible_extent *x = w->visible_extents;
    w->visible_extents = 0;
    while (x != 0)
    {
	struct visible_extent *next = x->next;
	rep_free (x);
	x = next;
    }
}

void
map_visible_extents (Lisp_Window *w, intptr_t col, intptr_t row,
		     void (*fun)(struct visible_extent *x))
{
    struct visible_extent *x = w->visible_extents;
    while (x != 0)
    {
	if ((row > x->start_row && row < x->end_row)
	    || (x->start_row == x->end_row
		&& row == x->start_row
		&& col >= x->start_col
		&& col < x->end_col)
	    || (x->start_row != x->end_row
		&& ((row == x->start_row && col >= x->start_col)
		    || (row == x->end_row && col < x->end_col))))
	{
	    (*fun) (x);
	}
	x = x->next;
    }
}

static void
update_pointer_extent_callback (struct visible_extent *x)
{
    Lisp_View *vw = x->vw;
    if (vw->pointer_extents_count < MAX_POINTER_EXTENTS)
	vw->pointer_extents[vw->pointer_extents_count++] = x->extent;
}

/* Return true if the display should be redrawn. */
bool
update_pointer_extent (Lisp_Window *w, intptr_t mouse_col,
		       intptr_t mouse_row)
{
    /* XXX remove this GNU CC dependency */
    Lisp_Extent *old[w->view_count][MAX_POINTER_EXTENTS];
    int old_num[w->view_count];
    Lisp_View *vw;
    int i;

    for (i = 0, vw = w->view_list; vw != 0; i++, vw = vw->next_view)
    {
	old_num[i] = vw->pointer_extents_count;
	memcpy (&old[i][0], vw->pointer_extents,
		 old_num[i] * sizeof(Lisp_Extent *));
	vw->pointer_extents_count = 0;
    }

    map_visible_extents (w, mouse_col, mouse_row,
			 update_pointer_extent_callback);

    for (i = 0, vw = w->view_list; vw != 0; i++, vw = vw->next_view)
    {
	if (vw->pointer_extents_count != old_num[i]
	    || memcmp (&old[i][0], vw->pointer_extents,
		       old_num[i] * sizeof(Lisp_Extent *)) != 0)
	{
	    return true;
	}
    }
    return false;
}

void
mark_visible_extents (Lisp_Window *w)
{
    struct visible_extent *x = w->visible_extents;
    Lisp_View *vw;
    while (x != 0)
    {
	rep_MARKVAL (rep_VAL (x->extent));
	x = x->next;
    }
    for (vw = w->view_list; vw != 0; vw = vw->next_view)
    {
	int i;
	for (i = 0; i < vw->pointer_extents_count; i++)
	    rep_MARKVAL (rep_VAL (vw->pointer_extents[i]));
    }
}


/* Misc. stuff */

static void
extent_mark(repv val)
{
    Lisp_Extent *e = VEXTENT(val);
    rep_GC_SET_CELL(val);
    rep_MARKVAL(e->plist);
    rep_MARKVAL(e->locals);
    rep_MARKVAL(rep_VAL(e->tx));
    /* This is a bit naive. It could probably be done w/o recursion.. */
    rep_MARKVAL(rep_VAL(e->parent));
    rep_MARKVAL(rep_VAL(e->right_sibling));
    rep_MARKVAL(rep_VAL(e->left_sibling));
    rep_MARKVAL(rep_VAL(e->first_child));
    rep_MARKVAL(rep_VAL(e->last_child));
}

static void
extent_sweep(void)
{
    Lisp_Extent *e = allocated_extents;
    allocated_extents = 0;
    while(e != 0)
    {
	Lisp_Extent *next = e->next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(e)))
	    rep_free(e);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(e));
	    e->next = allocated_extents;
	    allocated_extents = e;
	}
	e = next;
    }
}

static void
extent_prin(repv strm, repv e)
{
    char buf[128];
    sprintf(buf, "#<extent (%ld,%ld)->(%ld,%ld)",
	    VEXTENT(e)->start.row, VEXTENT(e)->start.col,
	    VEXTENT(e)->end.row, VEXTENT(e)->end.col);
    rep_stream_puts(strm, buf, -1, false);
#ifdef DEBUG
    rep_stream_putc(strm, ' ');
    rep_print_val(strm, VEXTENT(e)->plist);
    rep_stream_puts(strm, " [", -1, false);
    {
	Lisp_Extent *x = VEXTENT(e)->first_child;
	while(x != 0)
	{
	    extent_prin(strm, rep_VAL(x));
	    x = x->right_sibling;
	}
    }
    rep_stream_putc(strm, ']');
#endif
    rep_stream_putc(strm, '>');
}

static int
extent_cmp(repv e1, repv e2)
{
    if(rep_TYPE(e1) == rep_TYPE(e2))
    {
	Lisp_Extent *f1 = find_first_frag(VEXTENT(e1));
	Lisp_Extent *f2 = find_first_frag(VEXTENT(e2));
	return !(f1 == f2);
    }
    else
	return 1;
}

void
extent_init(void)
{
    extent_type = rep_register_new_type ("extent", extent_cmp,
					 extent_prin, extent_prin,
					 extent_sweep, extent_mark,
					 0, 0, 0, 0, 0, 0, 0);

    rep_ADD_SUBR(Smake_extent);
    rep_ADD_SUBR(Sdelete_extent);
    rep_ADD_SUBR(Sdelete_all_extents);
    rep_ADD_SUBR(Smove_extent);
    rep_ADD_SUBR(Sget_extent);
    rep_ADD_SUBR(Smap_extents);
    rep_ADD_SUBR(Sextent_start);
    rep_ADD_SUBR(Sextent_end);
    rep_ADD_SUBR(Sextent_parent);
    rep_ADD_SUBR(Sextent_root);
    rep_ADD_SUBR(Sextent_plist);
    rep_ADD_SUBR(Sset_extent_plist);
    rep_ADD_SUBR(Sextent_get);
    rep_ADD_SUBR(Sbuffer_get);
    rep_ADD_SUBR(Sextent_put);
    rep_ADD_SUBR(Sbuffer_symbol_value);
    rep_ADD_SUBR(Sextent_set);
    rep_ADD_SUBR(Smake_local_variable);
    rep_ADD_SUBR(Smake_variable_buffer_local);
    rep_ADD_SUBR(Sbuffer_variables);
    rep_ADD_SUBR(Skill_all_local_variables);
    rep_ADD_SUBR(Skill_local_variable);
    rep_INTERN(front_sticky);
    rep_INTERN(rear_sticky);
    rep_INTERN(local_variables);
    rep_INTERN(catch_variables);

    rep_deref_local_symbol_fun = deref_local_symbol;
    rep_set_local_symbol_fun = set_local_symbol;
}
