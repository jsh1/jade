/* edit.h -- Data structures for the editor (buffers, marks, etc...)
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef EDIT_H
#define EDIT_H


/* Macros for editor types */

#define VMARK(v)	((Lisp_Mark *)rep_PTR(v))
#define VBUFFER(v)	((Lisp_Buffer *)rep_PTR(v))
#define VWINDOW(v)	((Lisp_Window *)rep_PTR(v))
#define VVIEW(v)	((Lisp_View *)rep_PTR(v))
#define VGLYPHTAB(v)	((glyph_table_t *)rep_PTR(v))
#define VEXTENT(v)	((Lisp_Extent *)rep_PTR(v))	
#define VFACE(v)	((Lisp_Face *)rep_PTR(v))	
#define VCOLOR(v)	((Lisp_Color *)rep_PTR(v))	

#define BUFFERP(v)	rep_CELL16_TYPEP(v, buffer_type)
#define MARKP(v)	rep_CELL16_TYPEP(v, mark_type)
#define WINDOWP(v)	(rep_CELL16_TYPEP(v, window_type) && VWINDOW(v)->w_Window)
#define VIEWP(v)	(rep_CELL16_TYPEP(v, view_type) && VVIEW(v)->window)
#define GLYPHTABP(v)	rep_CELL16_TYPEP(v, glyph_table_type)
#define EXTENTP(v)	rep_CELL16_TYPEP(v, extent_type)
#define FACEP(v)	rep_CELL16_TYPEP(v, face_type)
#define COLORP(v)	rep_CELL16_TYPEP(v, color_type)


/* Positions */

/* A pointer to a buffer position. There's a conventions that positions
   accessed via repv pointers (and VCOL, VROW macros) are _read_only_,
   while those accessed through Pos * pointers (and PCOL, PROW macros)
   are _read_write_, probably allocated on the stack. */

#define POSP(v) (rep_CONSP(v) && rep_INTP(rep_CAR(v)) && rep_INTP(rep_CDR(v)))

/* We define the column in the cdr and the row in the car, so that
   the normal cons-comparison (car first, then cdr) will work as the
   old pos-comparison used to (i.e. row-major). */

#define MAKE_POS(col, row) Fcons(rep_MAKE_INT(row), rep_MAKE_INT(col))
#define VCOL(v) (rep_INT(rep_CDR(v)))
#define VROW(v) (rep_INT(rep_CAR(v)))

/* These should never be used unless it's clear there can be
   no other references to V. */

#define VSETCOL(v,c) (rep_CDR(v) = rep_MAKE_INT(c))
#define VSETROW(v,r) (rep_CAR(v) = rep_MAKE_INT(r))

/* These all want repv pointers */

#define POS_EQUAL_P(s,e) \
    ((VROW(s) == VROW(e)) && (VCOL(s) == VCOL(e)))
#define POS_GREATER_P(s,e) \
    ((VROW(s) > VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) > VCOL(e))))
#define POS_GREATER_EQUAL_P(s,e) \
    ((VROW(s) > VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) >= VCOL(e))))
#define POS_LESS_P(s,e) \
    ((VROW(s) < VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) < VCOL(e))))
#define POS_LESS_EQUAL_P(s,e) \
    ((VROW(s) < VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) <= VCOL(e))))

/* A more conventional C structure, used in the editor internals to
   avoid the gratuitous masking and shifting otherwise required. */

typedef struct {
    rep_intptr_t row, col;
} Pos;

#define PCOL(p) ((p)->col)
#define PROW(p) ((p)->row)

#define COPY_VPOS(p, v) 	\
    do {			\
	PROW(p) = VROW(v);	\
	PCOL(p) = VCOL(v);	\
    } while(0)

#define COPY_POS(p) MAKE_POS(PCOL(p), PROW(p))

/* These all want Pos pointers */

#define PPOS_EQUAL_P(s,e) \
    ((PROW(s) == PROW(e)) && (PCOL(s) == PCOL(e)))
#define PPOS_GREATER_P(s,e) \
    ((PROW(s) > PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) > PCOL(e))))
#define PPOS_GREATER_EQUAL_P(s,e) \
    ((PROW(s) > PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) >= PCOL(e))))
#define PPOS_LESS_P(s,e) \
    ((PROW(s) < PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) < PCOL(e))))
#define PPOS_LESS_EQUAL_P(s,e) \
    ((PROW(s) < PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) <= PCOL(e))))


/* Line structure -- an array of these is in the TX->lines */

typedef struct LINE {
    char	   *ln_Line;
    rep_intptr_t    ln_Strlen;	/* includes '\0' */
} LINE;


/* Each bookmark has one of these */

typedef struct lisp_mark {
    repv car;

    /* When the file is resident this node is linked into its tx_Marks list,
       otherwise it's in a list of all non-resident marks.  */
    struct lisp_mark *next;

    /* Linked into the list of all allocated marks */
    struct lisp_mark *next_alloc;

    /* The position of the marked character. */
    repv pos;

    /* The file. Either a buffer, or the name of the file. */
    repv file;

    /* For non-resident marks, the canonical name of the file. */
    repv canon_file;
} Lisp_Mark;

#define MARK_RESIDENT_P(m) BUFFERP((m)->file)


/* Extents -- plists for buffer regions */

typedef struct lisp_extent {
    repv car;
    struct lisp_extent *next;		/* list of allocations */

    /* Linkage in the tree. Each extent fragment has a parent (or null),
       a sibling either side (or null), and an optional list of
       children. */
    struct lisp_extent *parent;
    struct lisp_extent *left_sibling, *right_sibling;
    struct lisp_extent *first_child, *last_child;

    /* The doubly-linked list of extent fragments making up one ``extent''. */
    struct lisp_extent *frag_next, *frag_pred;

    /* ``Temporary'' field. Used for traversing trees sometimes */
    struct lisp_extent *tem;

    struct lisp_buffer *tx;
    repv plist;
    repv locals;			/* alist of (SYMBOL . repv) */

    /* The start and end positions of the fragment. Note that the ``row''
       components are relative to the row in which the parent of this
       fragment begins. */
    Pos start, end;

} Lisp_Extent;

/* Absorb characters inserted immediately before the extent into the extent. */
#define EXTFF_OPEN_START	(1 << (rep_CELL16_TYPE_BITS + 0))

/* Absorb characters inserted immediately after the extent into the extent. */
#define EXTFF_OPEN_END		(1 << (rep_CELL16_TYPE_BITS + 1))

/* If setting a non-permanent-local buffer-local symbol in this extent,
   set its value in _this_ extent. */
#define EXTFF_CATCH_VARIABLES	(1 << (rep_CELL16_TYPE_BITS + 2))

#define EXTENT_CACHE_SIZE 4

struct cached_extent {
    Pos pos;
    Lisp_Extent *extent;
    uint32_t lru_clock;
};

/* Each window has a list of the extents that are in the current
   contents of the window, and their positions in the character
   grid of the window. */

struct visible_extent {
    struct visible_extent *next;
    Lisp_Extent *extent;
    struct lisp_view *vw;
    long start_col, start_row;
    long end_col, end_row;
};


/* colours */

typedef struct lisp_color {
    repv car;
    struct lisp_color *next;

    repv name;

    /* System-local representation of the color */
    SYS_COLOR_TYPE color;
} Lisp_Color;


/* faces */

typedef struct lisp_face {
    repv car;
    struct lisp_face *next;

    repv name;
    repv foreground, background;
} Lisp_Face;

typedef struct merged_face {
    unsigned long car;
    bool valid;
    Lisp_Color *foreground, *background;
} Merged_Face;

#define FACEFF_UNDERLINE	(1 << (rep_CELL16_TYPE_BITS + 0))
#define FACEFF_BOLD		(1 << (rep_CELL16_TYPE_BITS + 1))
#define FACEFF_ITALIC		(1 << (rep_CELL16_TYPE_BITS + 2))
#define FACEFF_INVERT		(1 << (rep_CELL16_TYPE_BITS + 3))
#define FACEFF_BOXED		(1 << (rep_CELL16_TYPE_BITS + 4))
#define FACEFF_MASK		(FACEFF_UNDERLINE | FACEFF_BOLD \
				 | FACEFF_ITALIC | FACEFF_INVERT \
				 | FACEFF_BOXED)


/* A buffer, often called 'tx'. */

typedef struct lisp_buffer {
    repv car;
    struct lisp_buffer *next;

    Lisp_Mark *mark_chain;
    LINE *lines;
    rep_intptr_t line_count, total_lines;	/* text-lines, array-length */

    /* line numbers of `narrowed' region */
    rep_intptr_t logical_start, logical_end;

    /* unique name of buffer */
    repv buffer_name;

    /* absolute name of the file in this buffer as the user sees it (or nil) */
    repv file_name;

    /* name of the file in this buffer such that we can compare two
       files by comparing their canonical names (or nil). */
    repv canonical_file_name;

    /* Data for status line and window title */
    repv status_string;
    
    int change_count;
    int last_saved_change_count;	/* count at last save */
    int proper_saved_changed_count;	/* at last `proper' save */
    int auto_save_interval;		/* seconds between saves */
    rep_intptr_t last_saved_time;	/* time at last save (auto or user) */

    int tab_size;

    /* This is an extent covering the _whole_ buffer. */
    Lisp_Extent *global_extent;
    struct cached_extent extent_cache[EXTENT_CACHE_SIZE];

    /* Undo information */
    repv undo_list;
    repv pending_undo_list;
    repv did_undo_list;

    /* Saved state for buffers which are not being displayed.  */
    repv saved_cursor_pos;
    repv saved_display_origin;
    repv saved_block[2];
    int saved_block_state;
} Lisp_Buffer;

/* No recording of undo information */
#define TXFF_NO_UNDO		(1 << (rep_CELL16_TYPE_BITS + 0))

/* don't wrap long lines */
#define TXFF_DONT_WRAP_LINES	(1 << (rep_CELL16_TYPE_BITS + 1))
#define TX_WRAP_LINES_P(tx)	(((tx)->car & TXFF_DONT_WRAP_LINES) == 0)

/* Remnants from the old redisplay code */
#define flag_insertion(tx, start, end)		((tx)->change_count++)
#define flag_deletion(tx, start, end)		((tx)->change_count++)
#define flag_modification(tx, start, end)	((tx)->change_count++)


/* Each view in a window is like this */

#define MAX_POINTER_EXTENTS 16

typedef struct lisp_view {
    repv car;
    struct lisp_view *next;

    Lisp_Buffer *tx;
    struct lisp_window *window;
    struct lisp_view *next_view;	/* for view_list */

    /* Cursor positioning data.  */
    repv cursor_pos;
    long last_cursor_offset;		/* number of glyphs from col 0 */
    repv last_cursor_pos;
    int last_cursor_change_count;
    Lisp_Buffer *last_cursor_tx;

    repv display_origin;

    /* 0=block marked, 1=start marked, 2=end marked, -1=none marked */
    int block_state;
    repv block_start, block_end;

    /* List of extents currently under the mouse in this view. */
    Lisp_Extent *pointer_extents[MAX_POINTER_EXTENTS];
    int pointer_extents_count;

    /* This pane of window starts at glyph (FirstX, FirstY), for
       (MaxX, MaxY) glyphs (not including status line) */
    int min_x, min_y;
    int width, height;

    /* List of buffers accessible in this window.  This is not used by the
       C code at all; access is via the `buffer-list' variable.  */
    repv buffer_list;

    int scroll_ratio_x, scroll_ratio_y;
    int scroll_step_x, scroll_step_y;
} Lisp_View;

/* mark rectangular blocks */
#define VWFF_RECTBLOCKS		(1 << (rep_CELL16_TYPE_BITS + 0))

/* view is of a minibuffer */
#define VWFF_MINIBUF		(1 << (rep_CELL16_TYPE_BITS + 1))

/* at last redisplay, the last line in the buffer was visible */
#define VWFF_AT_BOTTOM		(1 << (rep_CELL16_TYPE_BITS + 2))


/* Windows */

typedef uint8_t glyph_code;
typedef uint8_t glyph_attr;

enum Glyph_Attrs {
    GA_LastFace = 63,			/* max faces per window */
    GA_Garbage = 255,
};

typedef struct {
    int cols, rows;
    glyph_code **codes;			/* ROWS glyph codes */
    glyph_attr **attrs;			/* ROWS glyph attrs */
    uint32_t *hashes;			/* ROWS hash values */

    /* Note that attrs[i] follows immediately after codes[i], so that
       all data for a line can be copied by a single call to memcpy() */
} glyph_buf;

/* Each window is represented by one of these */

typedef struct lisp_window {
    repv car;
    struct lisp_window *next;

    Lisp_View *view_list;		/* List of views in top-down order */
    Lisp_View *current_view;		/* Active view in window */
    Lisp_View *mini_buffer_view;	/* Minibuffer view */
    int view_count;			/* Number of views in window */

    WindowSystem window_system;	/* Data for the window system */
    glyph_buf *content, *new_content;	/* Data for redisplay */
    struct visible_extent *visible_extents;	/* List of displayed extents */

    char *message;			/* non-null == msg in minibuffer */
    size_t message_length;

    int column_count, row_count;	/* COLS,ROWS in whole window */
    int pixel_width, pixel_height;	/* width,height in pixels of window */
    int pixel_left, pixel_right;	/* pixel position of top left corner */
    int pixel_top, pixel_bottom;	/*  "      "   of bottom right */

    repv font_name;
    int font_width, font_height;	/* pixel width and height of glyphs */

    repv displayed_name;		/* current ``name'' of window  */

    /* Merged faces in this window. If the `next' field in each
       face is non-zero the face is valid. */
    Merged_Face merged_faces[GA_LastFace+1];
} Lisp_Window;

/* refresh whole window */
#define WINFF_FORCE_REFRESH	(1 << (rep_CELL16_TYPE_BITS + 0))

/* window is iconified */
#define WINFF_SLEEPING		(1 << (rep_CELL16_TYPE_BITS + 1))

/* a message is currently displayed in the minibuffer */
#define WINFF_MESSAGE		(1 << (rep_CELL16_TYPE_BITS + 2))

/* using the 'new_content' field of the window to preserve the 'content'.
   Used by asynchronous input handling to save screen contents */
#define WINFF_PRESERVING	(1 << (rep_CELL16_TYPE_BITS + 3))

/* True when the minibuffer in WIN is in use. */
#define MINIBUFFER_ACTIVE_P(win) \
    ((win)->mini_buffer_view->tx != mb_unused_buffer)

#endif /* EDIT_H */
