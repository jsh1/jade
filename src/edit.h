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

#ifndef _EDIT_H
#define _EDIT_H


/* Line structure -- an array of these is in the TX->tx_Lines */
typedef struct LINE {
    u_char	   *ln_Line;
    long	    ln_Strlen;	/* includes '\0' */
} LINE;


/* Each bookmark has one of these */
typedef struct lisp_mark {
    VALUE car;

    /* When the file is resident this node is linked into its tx_Marks list,
       otherwise it's in a list of all non-resident marks.  */
    struct lisp_mark *next;

    /* Linked into the list of all allocated marks */
    struct lisp_mark *next_alloc;

    /* The position of the marked character. */
    VALUE pos;

    /* The file. Either a buffer, or a string canonically naming an
       unloaded file. */
    VALUE file;
} Lisp_Mark;

#define MARK_RESIDENT_P(m) BUFFERP((m)->file)


/* Extents -- plists for buffer regions */

typedef struct lisp_extent {
    VALUE car;
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
    VALUE plist;
    VALUE locals;			/* alist of (SYMBOL . VALUE) */

    /* The start and end positions of the fragment. Note that the ``row''
       components are relative to the row in which the parent of this
       fragment begins. */
    Pos start, end;

} Lisp_Extent;

#define EXTFF_OPEN_START	(1 << (CELL8_TYPE_BITS + 0))
#define EXTFF_OPEN_END		(1 << (CELL8_TYPE_BITS + 1))

#define EXTENT_CACHE_SIZE 4

struct cached_extent {
    Pos pos;
    Lisp_Extent *extent;
    u_long lru_clock;
};


/* faces */

typedef struct lisp_face {
    VALUE car;
    struct lisp_face *next;

    int id;
    VALUE name;
    VALUE foreground, background;
} Lisp_Face;

#define FACEFF_UNDERLINE	(1 << (CELL8_TYPE_BITS + 0))
#define FACEFF_BOLD		(1 << (CELL8_TYPE_BITS + 1))
#define FACEFF_ITALIC		(1 << (CELL8_TYPE_BITS + 2))


/* colours */

typedef struct lisp_color {
    VALUE car;
    struct lisp_color *next;

    VALUE name;

    /* System-local representation of the color */
    SYS_COLOR_TYPE color;
} Lisp_Color;


/* A buffer, strangely called `TX' */
typedef struct _TX {
    VALUE	    tx_Car;
#define tx_Flags tx_Car

    struct _TX	   *tx_Next;
    Lisp_Mark	   *tx_MarkChain;
    LINE	   *tx_Lines;
    long	    tx_NumLines, tx_TotalLines;	/* text-lines, array-length */

    StrMem	    tx_StringPool;

    /* line numbers of `narrowed' region */
    long	    tx_LogicalStart, tx_LogicalEnd;

    /* unique name of buffer */
    VALUE	    tx_BufferName;

    /* absolute name of the file in this buffer as the user sees it (or nil) */
    VALUE	    tx_FileName;

    /* name of the file in this buffer such that we can compare two
       files by comparing their canonical names (or nil). */
    VALUE	    tx_CanonicalFileName;

    /* Data for status line and window title */
    VALUE	    tx_StatusId;
    
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
    VALUE	    tx_UndoList;
    VALUE	    tx_ToUndoList;
    VALUE	    tx_UndoneList;

    /* Saved state for buffers which are not being displayed.  */
    VALUE	    tx_SavedCPos;
    VALUE	    tx_SavedWPos;
    VALUE	    tx_SavedBlockPos[2];
    char	    tx_SavedBlockStatus;

} TX;

/* No recording of undo information */
#define TXFF_NO_UNDO		(1 << (CELL8_TYPE_BITS + 0))

/* don't wrap long lines */
#define TXFF_DONT_WRAP_LINES	(1 << (CELL8_TYPE_BITS + 1))
#define TX_WRAP_LINES_P(tx)	(((tx)->tx_Flags & TXFF_DONT_WRAP_LINES) == 0)

/* Remnants from the old redisplay code */
#define flag_insertion(tx, start, end)		((tx)->tx_Changes++)
#define flag_deletion(tx, start, end)		((tx)->tx_Changes++)
#define flag_modification(tx, start, end)	((tx)->tx_Changes++)


/* Each view in a window is like this */
typedef struct _VW
{
    VALUE	    vw_Car;
#define vw_Flags vw_Car

    struct _VW	   *vw_Next;
    TX		   *vw_Tx;
    struct _WIN	   *vw_Win;
    struct _VW	   *vw_NextView;	/* for w_ViewList */

    /* Cursor positioning data.  */
    VALUE	    vw_CursorPos;
    u_long	    vw_LastCursorOffset; /* number of glyphs from col 0 */
    VALUE	    vw_LastCursorPos;
    u_long	    vw_LastCursorChanges;
    TX		   *vw_LastCursorTx;

    VALUE	    vw_DisplayOrigin;

    VALUE	    vw_BlockS, vw_BlockE;
    /* 0=block marked, 1=start marked, 2=end marked, -1=none marked */
    int		    vw_BlockStatus;

    /* This pane of vw_Win starts at glyph (FirstX, FirstY), for
       (MaxX, MaxY) glyphs (not including status line) */
    int		    vw_FirstX, vw_FirstY;
    int		    vw_MaxX, vw_MaxY;

    /* When non-null, a string to replace the normal status line. */
    VALUE	    vw_StatusOverride;

#ifndef NOSCRLBAR
    ScrollBar	    vw_SBar;
#endif

    /* List of buffers accessible in this window.  This is not used by the
       C code at all; access is via the `buffer-list' variable.  */
    VALUE	    vw_BufferList;

    short	    vw_XStepRatio, vw_YStepRatio;
    short	    vw_XStep, vw_YStep;
} VW;

/* mark rectangular blocks */
#define VWFF_RECTBLOCKS		(1 << (CELL8_TYPE_BITS + 0))

/* view is of a minibuffer */
#define VWFF_MINIBUF		(1 << (CELL8_TYPE_BITS + 1))

/* at last redisplay, the last line in the buffer was visible */
#define VWFF_AT_BOTTOM		(1 << (CELL8_TYPE_BITS + 2))


/* Windows */

typedef u_char glyph_code;
typedef u_char glyph_attr;

enum Glyph_Attrs {
    GA_Garbage = 0,			/* glyph was lost */
    GA_FirstFace = 1,
    GA_DefaultFace = GA_FirstFace,
    GA_BlockFace,
    GA_ModeLineFace,
    GA_LastFace = 127,
    GA_CursorFace = 128,		/* invert fg-bg */
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
    VALUE w_Car;
#define w_Flags w_Car

    struct _WIN *w_Next;

    VW *w_ViewList;			/* List of views in top-down order */
    VW *w_CurrVW;			/* Active view in window */
    VW *w_MiniBuf;			/* Minibuffer view */
    int w_ViewCount;			/* Number of views in window */

    W_WindowSys w_WindowSys;		/* Data for the window system */
    glyph_buf *w_Content, *w_NewContent; /* Data for redisplay */
    u_long w_LastClickMics;		/* Last mouse click event */

    u_char *w_Message;			/* non-null == msg in minibuffer */
    u_long w_MessageLen;

    int w_MaxX, w_MaxY;			/* COLS,ROWS in whole window */
    int w_WidthPix, w_HeightPix;	/* width,height in pixels of window */
    int w_LeftPix, w_RightPix;		/* Pixel position of top left corner */
    int w_TopPix, w_BottomPix;		/*  "      "   of bottom right */

    VALUE w_FontName;
    short w_FontX, w_FontY;		/* pixel width and height of glyphs */

    VALUE w_DisplayedName;		/* current ``name'' of window  */
} WIN;

/* refresh whole window */
#define WINFF_FORCE_REFRESH	(1 << (CELL8_TYPE_BITS + 0))

/* window is iconified */
#define WINFF_SLEEPING		(1 << (CELL8_TYPE_BITS + 1))

/* a message is currently displayed in the minibuffer */
#define WINFF_MESSAGE		(1 << (CELL8_TYPE_BITS + 2))

/* using the w_NewContent field of the window to preserve the w_Content.
   Used by asynchronous input handling to save screen contents */
#define WINFF_PRESERVING	(1 << (CELL8_TYPE_BITS + 3))

/* True when the minibuffer in WIN is in use. */
#define MINIBUFFER_ACTIVE_P(win) \
    ((win)->w_MiniBuf->vw_Tx != mb_unused_buffer)

#define CURS_ON	 1
#define CURS_OFF 0

#endif /* _EDIT_H */
