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
#define VTX(v)		((TX *)rep_PTR(v))
#define VBUFFER(v)	VTX(v)
#define VWIN(v)		((WIN *)rep_PTR(v))
#define VVIEW(v)	((VW *)rep_PTR(v))
#define VGLYPHTAB(v)	((glyph_table_t *)rep_PTR(v))
#define VEXTENT(v)	((Lisp_Extent *)rep_PTR(v))	
#define VFACE(v)	((Lisp_Face *)rep_PTR(v))	
#define VCOLOR(v)	((Lisp_Color *)rep_PTR(v))	

#define BUFFERP(v)	rep_CELL16_TYPEP(v, buffer_type)
#define MARKP(v)	rep_CELL16_TYPEP(v, mark_type)
#define WINDOWP(v)	(rep_CELL16_TYPEP(v, window_type) && VWIN(v)->w_Window)
#define VIEWP(v)	(rep_CELL16_TYPEP(v, view_type) && VVIEW(v)->vw_Win)
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
    long row, col;
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


/* Line structure -- an array of these is in the TX->tx_Lines */
typedef struct LINE {
    u_char	   *ln_Line;
    long	    ln_Strlen;	/* includes '\0' */
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

    struct _TX *tx;
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
    u_long lru_clock;
};

/* Each window has a list of the extents that are in the current
   contents of the window, and their positions in the character
   grid of the window. */
struct visible_extent {
    struct visible_extent *next;
    Lisp_Extent *extent;
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


/* A buffer, strangely called `TX' */
typedef struct _TX {
    repv	    tx_Car;
#define tx_Flags tx_Car

    struct _TX	   *tx_Next;
    Lisp_Mark	   *tx_MarkChain;
    /* tx_Lines is allocated using r_alloc, thus it should *never*
       be cached since *any* call to the malloc library could move it */
    LINE	   *tx_Lines;
    long	    tx_NumLines, tx_TotalLines;	/* text-lines, array-length */

    /* line numbers of `narrowed' region */
    long	    tx_LogicalStart, tx_LogicalEnd;

    /* unique name of buffer */
    repv	    tx_BufferName;

    /* absolute name of the file in this buffer as the user sees it (or nil) */
    repv	    tx_FileName;

    /* name of the file in this buffer such that we can compare two
       files by comparing their canonical names (or nil). */
    repv	    tx_CanonicalFileName;

    /* Data for status line and window title */
    repv	    tx_StatusId;
    
    int		    tx_Changes;
    int		    tx_LastSaveChanges;	 /* changes at last save (any type) */
    int		    tx_ProperSaveChanges; /* changes at last `proper' save */
    int		    tx_AutoSaveInterval; /* seconds between saves */
    long	    tx_LastSaveTime;	 /* time at last save (auto or user) */

    int		    tx_TabSize;

    /* This is an extent covering the _whole_ buffer. */
    Lisp_Extent	   *tx_GlobalExtent;
    struct cached_extent tx_ExtentCache[EXTENT_CACHE_SIZE];

    /* Undo information */
    repv	    tx_UndoList;
    repv	    tx_ToUndoList;
    repv	    tx_UndoneList;

    /* Saved state for buffers which are not being displayed.  */
    repv	    tx_SavedCPos;
    repv	    tx_SavedWPos;
    repv	    tx_SavedBlockPos[2];
    char	    tx_SavedBlockStatus;

} TX;

/* No recording of undo information */
#define TXFF_NO_UNDO		(1 << (rep_CELL16_TYPE_BITS + 0))

/* don't wrap long lines */
#define TXFF_DONT_WRAP_LINES	(1 << (rep_CELL16_TYPE_BITS + 1))
#define TX_WRAP_LINES_P(tx)	(((tx)->tx_Flags & TXFF_DONT_WRAP_LINES) == 0)

/* Remnants from the old redisplay code */
#define flag_insertion(tx, start, end)		((tx)->tx_Changes++)
#define flag_deletion(tx, start, end)		((tx)->tx_Changes++)
#define flag_modification(tx, start, end)	((tx)->tx_Changes++)


/* Each view in a window is like this */
typedef struct _VW
{
    repv	    vw_Car;
#define vw_Flags vw_Car

    struct _VW	   *vw_Next;
    TX		   *vw_Tx;
    struct _WIN	   *vw_Win;
    struct _VW	   *vw_NextView;	/* for w_ViewList */

    /* Cursor positioning data.  */
    repv	    vw_CursorPos;
    u_long	    vw_LastCursorOffset; /* number of glyphs from col 0 */
    repv	    vw_LastCursorPos;
    u_long	    vw_LastCursorChanges;
    TX		   *vw_LastCursorTx;

    repv	    vw_DisplayOrigin;

    repv	    vw_BlockS, vw_BlockE;
    /* 0=block marked, 1=start marked, 2=end marked, -1=none marked */
    int		    vw_BlockStatus;

    /* This pane of vw_Win starts at glyph (FirstX, FirstY), for
       (MaxX, MaxY) glyphs (not including status line) */
    int		    vw_FirstX, vw_FirstY;
    int		    vw_MaxX, vw_MaxY;

    /* List of buffers accessible in this window.  This is not used by the
       C code at all; access is via the `buffer-list' variable.  */
    repv	    vw_BufferList;

    short	    vw_XStepRatio, vw_YStepRatio;
    short	    vw_XStep, vw_YStep;
} VW;

/* mark rectangular blocks */
#define VWFF_RECTBLOCKS		(1 << (rep_CELL16_TYPE_BITS + 0))

/* view is of a minibuffer */
#define VWFF_MINIBUF		(1 << (rep_CELL16_TYPE_BITS + 1))

/* at last redisplay, the last line in the buffer was visible */
#define VWFF_AT_BOTTOM		(1 << (rep_CELL16_TYPE_BITS + 2))


/* Windows */

typedef u_char glyph_code;
typedef u_char glyph_attr;

enum Glyph_Attrs {
    GA_LastFace = 63,			/* max faces per window */
    GA_Garbage = 255,
};

typedef struct {
    int cols, rows;
    glyph_code **codes;			/* ROWS glyph codes */
    glyph_attr **attrs;			/* ROWS glyph attrs */
    u_long *hashes;			/* ROWS hash values */

    /* Note that attrs[i] follows immediately after codes[i], so that
       all data for a line can be copied by a single call to memcpy() */
} glyph_buf;

/* Each window is represented by one of these */
typedef struct _WIN {
    repv w_Car;
#define w_Flags w_Car

    struct _WIN *w_Next;

    VW *w_ViewList;			/* List of views in top-down order */
    VW *w_CurrVW;			/* Active view in window */
    VW *w_MiniBuf;			/* Minibuffer view */
    int w_ViewCount;			/* Number of views in window */

    W_WindowSys w_WindowSys;		/* Data for the window system */
    glyph_buf *w_Content, *w_NewContent; /* Data for redisplay */
    struct visible_extent *w_VisibleExtents;
    Lisp_Extent *w_MouseExtent;

    u_long w_LastClickMics;		/* Last mouse click event */

    u_char *w_Message;			/* non-null == msg in minibuffer */
    u_long w_MessageLen;

    int w_MaxX, w_MaxY;			/* COLS,ROWS in whole window */
    int w_WidthPix, w_HeightPix;	/* width,height in pixels of window */
    int w_LeftPix, w_RightPix;		/* Pixel position of top left corner */
    int w_TopPix, w_BottomPix;		/*  "      "   of bottom right */

    repv w_FontName;
    short w_FontX, w_FontY;		/* pixel width and height of glyphs */

    repv w_DisplayedName;		/* current ``name'' of window  */

    /* Merged faces in this window. If the `next' field in each
       face is non-zero the face is valid. */
    Merged_Face w_MergedFaces[GA_LastFace+1];
} WIN;

/* refresh whole window */
#define WINFF_FORCE_REFRESH	(1 << (rep_CELL16_TYPE_BITS + 0))

/* window is iconified */
#define WINFF_SLEEPING		(1 << (rep_CELL16_TYPE_BITS + 1))

/* a message is currently displayed in the minibuffer */
#define WINFF_MESSAGE		(1 << (rep_CELL16_TYPE_BITS + 2))

/* using the w_NewContent field of the window to preserve the w_Content.
   Used by asynchronous input handling to save screen contents */
#define WINFF_PRESERVING	(1 << (rep_CELL16_TYPE_BITS + 3))

/* True when the minibuffer in WIN is in use. */
#define MINIBUFFER_ACTIVE_P(win) \
    ((win)->w_MiniBuf->vw_Tx != mb_unused_buffer)

#endif /* EDIT_H */
