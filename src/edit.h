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

    /* absolute name of the file in this buffer as the user sees it
       (or a null string) */
    VALUE	    tx_FileName;

    /* name of the file in this buffer such that we can compare two
       files by comparing their canonical names. */
    VALUE	    tx_CanonicalFileName;

    VALUE	    tx_ModeName;
    VALUE	    tx_MinorModeNameList;
    VALUE	    tx_MinorModeNameString;

    int		    tx_Changes;
    int		    tx_LastSaveChanges;	 /* changes at last save (any type) */
    int		    tx_ProperSaveChanges; /* changes at last `proper' save */
    int		    tx_AutoSaveInterval; /* seconds between saves */
    long	    tx_LastSaveTime;	 /* time at last save (auto or user) */

    int		    tx_TabSize;

    VALUE	    tx_LocalVariables; /* alist of (SYMBOL . VALUE) */
    VALUE	    tx_GlyphTable;

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

/* No modifications to file */
#define TXFF_RDONLY		(1 << (CELL8_TYPE_BITS + 0))

/* No mod flag, buffer never killed. */
#define TXFF_SPECIAL		(1 << (CELL8_TYPE_BITS + 1))

/* No recording of undo information */
#define TXFF_NO_UNDO		(1 << (CELL8_TYPE_BITS + 2))

/* don't wrap long lines */
#define TXFF_DONT_WRAP_LINES	(1 << (CELL8_TYPE_BITS + 3))
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

    u_char	   *vw_StatusBuf;
#define STATUS_BUFSIZ 128

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

/* status text set by user */
#define VWFF_CUSTOM_STATUS	(1 << (CELL8_TYPE_BITS + 2))

/* at last redisplay, the last line in the buffer was visible */
#define VWFF_AT_BOTTOM		(1 << (CELL8_TYPE_BITS + 3))


/* Windows */

typedef u_char glyph_code;
typedef u_char glyph_attr;

enum Glyph_Attrs {
    GA_Text = P_TEXT,			/* foreground on background */
    GA_Text_RV = P_TEXT_RV,		/* bg on fg */
    GA_Block = P_BLOCK,			/* fg on highlight */
    GA_Block_RV = P_BLOCK_RV,		/* hl on fg */
    GA_MAX = P_MAX,
    GA_Garbage = 255,			/* glyph was lost */
};

typedef struct {
    int cols, rows;
    u_long hashes[0];			/* ROWS hash codes*/

    /* Following the ROWS hash codes, we have COLSxROWS glyph codes
       followed by COLSxROWS glyph attrs. Use the GLYPH_BUF_CODES and
       GLYPH_BUF_ATTRS to get pointers into them. */
} glyph_buf;

#define SIZEOF_GLYPH_BUF(cols, rows)		\
    (sizeof(glyph_buf)				\
     + ((cols) * (rows) * sizeof(glyph_code))	\
     + ((cols) * (rows) * sizeof(glyph_attr))	\
     + ((rows) * sizeof(u_long)))

/* Return the address of the code line ROW in glyph buffer G. */
#define GLYPH_BUF_CODES(g,row)						\
    ((glyph_code *)(((char *)g)						\
		    + sizeof(glyph_buf)					\
		    + (((g)->rows) * sizeof(u_long))			\
		    + (((g)->cols * (row)) * sizeof(glyph_code))))

/* Return the address of the attribute line ROW in glyph buffer G. */
#define GLYPH_BUF_ATTRS(g,row)						\
    ((glyph_attr *)(((char *)g)						\
		    + sizeof(glyph_buf)					\
		    + (((g)->rows) * sizeof(u_long))			\
		    + (((g)->cols * (g)->rows) * sizeof(glyph_code))	\
		    + (((g)->cols * (row)) * sizeof(glyph_attr))))

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
} WIN;

/* refresh whole window */
#define WINFF_FORCE_REFRESH	(1 << (CELL8_TYPE_BITS + 0))

/* window is iconified */
#define WINFF_SLEEPING		(1 << (CELL8_TYPE_BITS + 1))

/* a message is currently displayed in the minibuffer */
#define WINFF_MESSAGE		(1 << (CELL8_TYPE_BITS + 2))

/* True when the minibuffer in WIN is in use. */
#define MINIBUFFER_ACTIVE_P(win) \
    ((win)->w_MiniBuf->vw_Tx != mb_unused_buffer)

#define CURS_ON	 1
#define CURS_OFF 0

#endif /* _EDIT_H */
