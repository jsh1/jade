/* render.c -- System-independent rendering
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <assert.h>

#ifdef HAVE_AMIGA
# ifdef _DCC
#  define GfxBase_DECLARED
# endif
# include <clib/graphics_protos.h>
# include <graphics/gfxbase.h>
# include <graphics/gfxmacros.h>
extern struct GfxBase *GfxBase;
#endif

_PR void cursor(VW *, bool);
_PR void pen_to_line(VW *, long);
_PR void pen_to_pos(VW *, long, long);
_PR void pen_to_glyph_pos(VW *, long, long);
_PR void draw_bit(VW *, int, u_char *, int, long, long);
_PR void draw_line(VW *, LINE *, long);
_PR void redraw_all(VW *);
_PR void redraw_region(VW *, POS *, POS *);
_PR void redraw_lines(VW *, long, long);
_PR void redraw_lines_clr(VW *, long, long);
_PR void redraw_line_from(VW *, long, long);
_PR void redraw_rect(VW *, POS *, POS *, bool);
_PR void clear_lines(VW *, long, long);
_PR void cut_paste_lines(VW *, long, long);
_PR void scroll_vw(VW *vw, long lines);
_PR void redraw_message(WIN *w);
_PR void redraw_status_buffer(VW *vw);

/* I hope this is enough!? :-| */
static u_char glyph_buf[512];

static void
pen_to_cursor(VW *vw)
{
    register int x, y;
    calc_cursor_offset(vw);
    y = (vw->vw_CursorPos.pos_Line - vw->vw_StartLine) * vw->vw_Win->w_FontY
	+ FONT_ASCENT(vw);
    x = ((vw->vw_LastCursorOffset - vw->vw_StartCol) * vw->vw_Win->w_FontX);
    if(x < 0)
	x = 0;
    MOVE(vw, x, y);
}

void
pen_to_line(VW *vw, long line)
{
    MOVE(vw, 0,
	 (line - vw->vw_StartLine) * vw->vw_Win->w_FontY + FONT_ASCENT(vw));
}

void
pen_to_pos(VW *vw, long col, long line)
{
    TX *tx = vw->vw_Tx;
    int x, y;
    y = (line - vw->vw_StartLine) * vw->vw_Win->w_FontY + FONT_ASCENT(vw);
    assert(line < tx->tx_NumLines);
    x = (glyph_col(tx, col, line) - vw->vw_StartCol) * vw->vw_Win->w_FontX;
    if(x < 0)
	x = 0;
    MOVE(vw, x, y);
}

void
pen_to_glyph_pos(VW *vw, long gcol, long line)
{
    int x, y;
    y = (line - vw->vw_StartLine) * vw->vw_Win->w_FontY + FONT_ASCENT(vw);
    x = (gcol - vw->vw_StartCol) * vw->vw_Win->w_FontX;
    if(x < 0)
	x = 0;
    MOVE(vw, x, y);
}

void
cursor(VW *vw, bool status)
{
    if(!(vw->vw_Win->w_Flags & WINFF_SLEEPING)
       && (!(vw->vw_Flags & VWFF_MINIBUF)
	   || !(vw->vw_Win->w_Flags & WINFF_MESSAGE)))
    {
	if(vw->vw_CursorPos.pos_Line < vw->vw_StartLine + vw->vw_MaxY)
	{
	    int pencol;
	    LINE *line = vw->vw_Tx->tx_Lines + vw->vw_CursorPos.pos_Line;
	    long cursoff = vw->vw_CursorPos.pos_Col;
	    if(!cursor_in_block(vw))
		pencol = (status == CURS_ON) ? P_TEXT_RV : P_TEXT;
	    else
		pencol = (status == CURS_ON) ? P_BLOCK_RV : P_BLOCK;
	    pen_to_cursor(vw);
	    if((cursoff + 1) >= line->ln_Strlen)
		TEXT(vw, pencol, " ", 1);
	    else
		TEXT(vw, pencol,
		     char_glyphs(vw->vw_Tx, line->ln_Line[cursoff]), 1);
	}
    }
}

/*
 * draws a section of a line of bytes, beg and end are x ordinates
 */
void
draw_bit(VW *vw, int colour, u_char *str, int slen, long beg, long end)
{
    long length;
    if(beg < vw->vw_StartCol)
	beg = vw->vw_StartCol;
    if(end > vw->vw_StartCol + vw->vw_MaxX + 1)
	end = vw->vw_StartCol + vw->vw_MaxX + 1;
    length = end - beg;
    slen -= beg;
    if(slen > length)
	slen = length;
    if(end >= vw->vw_StartCol && slen > 0)
	TEXT(vw, colour, str + beg, slen);
}

static void
draw_block_line(VW *vw, long blockStartCol, long blockEndCol,
		long drawEndCol, bool useBEC, bool useBSC)
{
    int xend = (drawEndCol - vw->vw_StartCol) * vw->vw_Win->w_FontX;
    if(PEN_X(vw) < vw->vw_WidthPix)
    {
	int rectblocks = vw->vw_Flags & VWFF_RECTBLOCKS;
	if(!rectblocks && useBSC)
	{
	    int xblkstart = ((blockStartCol - vw->vw_StartCol)
			     * vw->vw_Win->w_FontX);
	    if(xblkstart < 0)
		xblkstart = 0;
	    if(xblkstart > vw->vw_WidthPix)
		return;
	    if(PEN_X(vw) < xblkstart)
		MOVE(vw, xblkstart, PEN_Y(vw));
	}
	if(useBEC || rectblocks)
	{
	    int xblkend = (blockEndCol - vw->vw_StartCol)
			  * vw->vw_Win->w_FontX;
	    if(rectblocks)
	    {
		if(((PEN_X(vw) / vw->vw_Win->w_FontX)
		    + vw->vw_StartCol) < blockStartCol)
		{
		    MOVE(vw, (blockStartCol - vw->vw_StartCol)
			 * vw->vw_Win->w_FontX,
			 PEN_Y(vw));
		}
	    }
	    if(xblkend >= xend)
		xblkend = xend;
	    if(xblkend > PEN_X(vw))
	    {
		SET_AREA(vw, P_BLOCK_RV, PEN_X(vw),
			 PEN_Y(vw) - FONT_ASCENT(vw),
			 xblkend - PEN_X(vw), vw->vw_Win->w_FontY);
	    }
	}
	else
	{
	    SET_AREA(vw, P_BLOCK_RV, PEN_X(vw), PEN_Y(vw) - FONT_ASCENT(vw),
			   xend - PEN_X(vw), vw->vw_Win->w_FontY);
	}
    }
}

/*
 * pen should be at start of line to draw (line should be cleared first)
 */
void
draw_line(VW *vw, LINE *line, long lineNum)
{
    long glyphs = make_glyph_array(vw->vw_Tx, line->ln_Line, 0,
				   line->ln_Strlen-1, glyph_buf,
				   vw->vw_StartCol, vw->vw_MaxX);
    if(vw->vw_BlockStatus != 0
       || (vw->vw_CursorPos.pos_Line < vw->vw_BlockS.pos_Line
           && vw->vw_CursorPos.pos_Line > vw->vw_BlockE.pos_Line))
	TEXT(vw, P_TEXT, glyph_buf, glyphs);
    else
    {
	long block0col = glyph_col(vw->vw_Tx, vw->vw_BlockS.pos_Col,
				   vw->vw_BlockS.pos_Line);
	long block1col = glyph_col(vw->vw_Tx, vw->vw_BlockE.pos_Col,
				   vw->vw_BlockE.pos_Line);
	u_char *glyph_line = glyph_buf - vw->vw_StartCol;
	long abs_glyphs = glyphs + vw->vw_StartCol;
	if(vw->vw_Flags & VWFF_RECTBLOCKS)
	{
	    if(block0col > block1col)
	    {
		long tmp;
		tmp = block0col;
		block0col = block1col;
		block1col = tmp;
	    }
	}
	switch(line_in_block(vw, lineNum))
	{
	case 0: /* none of line in block */
	    TEXT(vw, P_TEXT, glyph_buf, glyphs);
	    break;
	case 1: /* whole of line in block */
	    TEXT(vw, P_BLOCK, glyph_buf, glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_StartCol + vw->vw_MaxX, FALSE, FALSE);
	    break;
	case 2: /* start of line in block */
	    draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, 0, block1col);
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, block1col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_StartCol + vw->vw_MaxX, TRUE, FALSE);
	    break;
	case 3: /* end of line in block */
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, 0, block0col);
	    draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, block0col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_StartCol + vw->vw_MaxX, FALSE, TRUE);
	    break;
	case 4: /* middle of line in block */
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, 0, block0col);
	    draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, block0col,
		     block1col);
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, block1col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_StartCol + vw->vw_MaxX, TRUE, TRUE);
	    break;
	}
    }
}

/*
 * pen should be at first draw position, draws from character XSTART to the
 * end of the line.
 */
static void
draw_line_part(VW *vw, LINE *line, long lineNum, long xStart,
	       long glyph_xStart)
{
    u_char *src = line->ln_Line + xStart;
    u_long srclen = (line->ln_Strlen - 1) - xStart;
    long glyphs = make_glyph_array(vw->vw_Tx, src, glyph_xStart, srclen,
				   glyph_buf, 0,
				   (vw->vw_StartCol+vw->vw_MaxX)-glyph_xStart);
    if(vw->vw_BlockStatus != 0
       || (vw->vw_CursorPos.pos_Line < vw->vw_BlockS.pos_Line
           && vw->vw_CursorPos.pos_Line > vw->vw_BlockE.pos_Line))
	TEXT(vw, P_TEXT, glyph_buf, glyphs);
    else
    {
	long block0col = glyph_col(vw->vw_Tx, vw->vw_BlockS.pos_Col,
				   vw->vw_BlockS.pos_Line);
	long block1col = glyph_col(vw->vw_Tx, vw->vw_BlockE.pos_Col,
				   vw->vw_BlockE.pos_Line);
	u_char *glyph_line = glyph_buf - glyph_xStart;
	long abs_glyphs = glyphs + glyph_xStart;
	if(vw->vw_Flags & VWFF_RECTBLOCKS)
	{
	    if(block0col > block1col)
	    {
		long tmp;
		tmp = block0col;
		block0col = block1col;
		block1col = tmp;
	    }
	}
	switch(line_in_block(vw, lineNum))
	{
	case 0: /* none of line in block */
	    TEXT(vw, P_TEXT, glyph_buf, glyphs);
	    break;
	case 1: /* whole of line in block */
	    TEXT(vw, P_BLOCK, glyph_buf, glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_MaxX + vw->vw_StartCol, FALSE, FALSE);
	    break;
	case 2: /* start of line in block */
	    if(glyph_xStart < block1col)
		draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, glyph_xStart,
			 block1col);
	    else
		block1col = glyph_xStart;
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, block1col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_MaxX + vw->vw_StartCol, TRUE, FALSE);
	    break;
	case 3: /* end of line in block */
	    if(glyph_xStart < block0col)
		draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, glyph_xStart,
			 block0col);
	    else
		block0col = glyph_xStart;
	    draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, block0col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_MaxX + vw->vw_StartCol, FALSE, TRUE);
	    break;
	case 4: /* middle of line in block */
	    if(glyph_xStart < block0col)
		draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, glyph_xStart,
			 block0col);
	    else
		block0col = glyph_xStart;
	    if(block0col < block1col)
		draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, block0col,
			 block1col);
	    else
		block1col = block0col;
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, block1col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col,
			    vw->vw_MaxX + vw->vw_StartCol, TRUE, TRUE);
	    break;
	}
    }
}

#ifdef HAVE_X11
/*
 * pen should be at first draw position
 * The XSTART and XEND are *glyph* indexes -- the physical position on the
 * screen.
 */
static void
draw_line_glyph_length(VW *vw, LINE *line, long lineNum, long xStart, long xEnd)
{
    long glyphs;
    if(xStart < vw->vw_StartCol)
	xStart = vw->vw_StartCol;
    if(xEnd > (vw->vw_StartCol + vw->vw_MaxX))
	xEnd = (vw->vw_StartCol + vw->vw_MaxX);
    glyphs = make_glyph_array(vw->vw_Tx, line->ln_Line, 0, line->ln_Strlen-1,
			      glyph_buf, xStart, xEnd - xStart);
    if(vw->vw_BlockStatus != 0
       || (vw->vw_CursorPos.pos_Line < vw->vw_BlockS.pos_Line
           && vw->vw_CursorPos.pos_Line > vw->vw_BlockE.pos_Line))
	TEXT(vw, P_TEXT, glyph_buf, glyphs);
    else
    {
	long block0col = glyph_col(vw->vw_Tx, vw->vw_BlockS.pos_Col,
				   vw->vw_BlockS.pos_Line);
	long block1col = glyph_col(vw->vw_Tx, vw->vw_BlockE.pos_Col,
				   vw->vw_BlockE.pos_Line);
	u_char *glyph_line = glyph_buf - xStart;
	long abs_glyphs = glyphs + xStart;
	if(vw->vw_Flags & VWFF_RECTBLOCKS)
	{
	    if(block0col > block1col)
	    {
		long tmp;
		tmp = block0col;
		block0col = block1col;
		block1col = tmp;
	    }
	}
	switch(line_in_block(vw, lineNum))
	{
	case 0: /* none of line in block */
	    TEXT(vw, P_TEXT, glyph_buf, glyphs);
	    break;
	case 1: /* whole of line in block */
	    TEXT(vw, P_BLOCK, glyph_buf, glyphs);
	    draw_block_line(vw, block0col, block1col, xEnd, FALSE, FALSE);
	    break;
	case 2: /* start of line in block */
	    if(xStart < block1col)
		draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, xStart,
			 block1col);
	    else
		block1col = xStart;
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, block1col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col, xEnd, TRUE, FALSE);
	    break;
	case 3: /* end of line in block */
	    if(xStart < block0col)
		draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, xStart,
			 block0col);
	    else
		block0col = xStart;
	    draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, block0col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col, xEnd, FALSE, TRUE);
	    break;
	case 4: /* middle of line in block */
	    if(xStart < block0col)
		draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, xStart,
			 block0col);
	    else
		block0col = xStart;
	    if(block0col < block1col)
		draw_bit(vw, P_BLOCK, glyph_line, abs_glyphs, block0col,
			 block1col);
	    else
		block1col = block0col;
	    draw_bit(vw, P_TEXT, glyph_line, abs_glyphs, block1col,
		     abs_glyphs);
	    draw_block_line(vw, block0col, block1col, xEnd, TRUE, TRUE);
	    break;
	}
    }
}
#endif /* HAVE_X11 */

void
redraw_all(VW *vw)
{
    long linenum = vw->vw_StartLine;
    LINE *line = vw->vw_Tx->tx_Lines + linenum;
    short y = 0;
    CLR_AREA(vw, 0, 0, vw->vw_WidthPix, vw->vw_HeightPix);
    while((y < vw->vw_MaxY) && (linenum < vw->vw_Tx->tx_LogicalEnd))
    {
	pen_to_line(vw, linenum);
	draw_line(vw, line, linenum);
	y++;
	linenum++;
	line++;
    }
}

void
redraw_region(VW *vw, POS *start, POS *end)
{
    long linenum = start->pos_Line;
    LINE *line;
    long y, yend, yord;
    if(POS_EQUAL_P(start, end)
       || (end->pos_Line < vw->vw_StartLine)
       || (end->pos_Line < vw->vw_Tx->tx_LogicalStart)
       || (start->pos_Line > (vw->vw_StartLine + vw->vw_MaxY))
       || (start->pos_Line > (vw->vw_Tx->tx_LogicalEnd)))
	return;
    if(linenum < vw->vw_StartLine)
	linenum = vw->vw_StartLine;
    line = vw->vw_Tx->tx_Lines + linenum;
    y = linenum - vw->vw_StartLine;
    yend = end->pos_Line - vw->vw_StartLine + 1;
    if(yend > vw->vw_MaxY)
	yend = vw->vw_MaxY;
    yord = y * vw->vw_Win->w_FontY;
    if((y >= 0) && (y < yend))
    {
	long start_col = (linenum == start->pos_Line
			  ? start->pos_Col
			  : vw->vw_StartCol);
	long gcol = glyph_col(vw->vw_Tx, start_col, start->pos_Line);
	long tmp = gcol - vw->vw_StartCol;
	if(tmp < 0)
	{
	    gcol = vw->vw_StartCol;
	    start_col = char_col(vw->vw_Tx, gcol, start->pos_Line);
	    tmp = 0;
	}
	CLR_AREA(vw, tmp * vw->vw_Win->w_FontX, yord,
		 vw->vw_WidthPix - (tmp * vw->vw_Win->w_FontX),
		 vw->vw_Win->w_FontY);
	if(linenum < vw->vw_Tx->tx_LogicalEnd)
	{
	    pen_to_glyph_pos(vw, gcol, linenum);
	    draw_line_part(vw, line, linenum, start_col, gcol);
	}
	line++;
	linenum++;
	y++;
	yord += vw->vw_Win->w_FontY;
    }
    else
    {
	y = 0;
	yord = 0;
	linenum = vw->vw_StartLine;
	line = vw->vw_Tx->tx_Lines + linenum;
    }
    if(y < yend)
    {
	CLR_AREA(vw, 0, yord, vw->vw_WidthPix,
		 (yend * vw->vw_Win->w_FontY) - yord);
	while((y < yend) && (linenum < vw->vw_Tx->tx_LogicalEnd))
	{
	    pen_to_line(vw, linenum);
	    draw_line(vw, line, linenum);
	    y++;
	    linenum++;
	    line++;
	}
    }
}

/*
 * DOES NOT clear the drawing area
 */
void
redraw_lines(VW *vw, long startLine, long endLine)
{
    LINE *line = vw->vw_Tx->tx_Lines;
    short y;
    if(startLine < vw->vw_StartLine)
	startLine = vw->vw_StartLine;
    y = startLine - vw->vw_StartLine;
    line += startLine;
    if(endLine > vw->vw_Tx->tx_LogicalEnd)
	endLine = vw->vw_Tx->tx_LogicalEnd;
    endLine -= vw->vw_StartLine;
    if(endLine > vw->vw_MaxY)
	endLine = vw->vw_MaxY;
    while(y < endLine)
    {
	pen_to_line(vw, startLine);
	draw_line(vw, line, startLine);
	startLine++;
	y++;
	line++;
    }
}

void
redraw_lines_clr(VW *vw, long startLine, long endLine)
{
    LINE *line = vw->vw_Tx->tx_Lines;
    short y;
    endLine++;
    if((endLine <= vw->vw_StartLine)
       || (endLine < vw->vw_Tx->tx_LogicalStart)
       || (startLine > (vw->vw_StartLine + vw->vw_MaxY))
       || (startLine > (vw->vw_Tx->tx_LogicalEnd)))
    {
	return;
    }
    if(startLine < vw->vw_StartLine)
	startLine = vw->vw_StartLine;
    y = startLine - vw->vw_StartLine;
    line += startLine;
    if(endLine > vw->vw_Tx->tx_LogicalEnd)
	endLine = vw->vw_Tx->tx_LogicalEnd;
    endLine -= vw->vw_StartLine;
    if(endLine > vw->vw_MaxY)
	endLine = vw->vw_MaxY;
    CLR_AREA(vw, 0, y * vw->vw_Win->w_FontY, vw->vw_WidthPix,
	     (endLine - y) * vw->vw_Win->w_FontY);
    while(y < endLine)
    {
	pen_to_line(vw, startLine);
	draw_line(vw, line, startLine);
	startLine++;
	y++;
	line++;
    }
}

void
redraw_line_from(VW *vw, long col, long lineNum)
{
    if((lineNum >= vw->vw_StartLine)
       && (lineNum < vw->vw_StartLine + vw->vw_MaxY))
    {
	long gcol = glyph_col(vw->vw_Tx, col, lineNum);
	int yord = (lineNum - vw->vw_StartLine) * vw->vw_Win->w_FontY;
	long tmp = gcol - vw->vw_StartCol;
	if(tmp < 0)
	{
	    gcol = vw->vw_StartCol;
	    col = char_col(vw->vw_Tx, gcol, lineNum);
	    tmp = 0;
	}
	CLR_AREA(vw, tmp * vw->vw_Win->w_FontX, yord,
		 vw->vw_WidthPix - tmp * vw->vw_Win->w_FontX,
		 vw->vw_Win->w_FontY);
	pen_to_glyph_pos(vw, gcol, lineNum);
	draw_line_part(vw, vw->vw_Tx->tx_Lines + lineNum, lineNum, col, gcol);
    }
}

#ifdef HAVE_X11
/*
 * Assumes (start, end) is valid AND totally viewable (ie, no checks, no
 * clipping). Should be ok since this is meant for Expose type events
 * start is probably trashed
 * `gapBlank' says whether or not I need to clear the space I'm drawing into.
 * These coordinates are *GLYPH* coordinates.
 */
void
redraw_rect(VW *vw, POS *start, POS *end, bool gapBlank)
{
    TX *tx = vw->vw_Tx;
    LINE *line = tx->tx_Lines + start->pos_Line;
    int yord = (start->pos_Line - vw->vw_StartLine) * vw->vw_Win->w_FontY;
    end->pos_Col++;
    end->pos_Line++;
    if(!gapBlank)
    {
	CLR_RECT(vw, (start->pos_Col - vw->vw_StartCol) * vw->vw_Win->w_FontX,
		 yord,
		 ((end->pos_Col - vw->vw_StartCol) * vw->vw_Win->w_FontX),
		 ((end->pos_Line - vw->vw_StartLine) * vw->vw_Win->w_FontY));
    }
    if(end->pos_Line > tx->tx_LogicalEnd)
	end->pos_Line = tx->tx_LogicalEnd;
    while(end->pos_Line > start->pos_Line)
    {
	pen_to_glyph_pos(vw, start->pos_Col, start->pos_Line);
	draw_line_glyph_length(vw, line, start->pos_Line, start->pos_Col,
			       end->pos_Col /* + 1 */);
	line++;
	start->pos_Line++;
    }
}
#endif /* HAVE_X11 */

/* Clear the lines of text starting at FIRST-LINE and ending at the
   line LAST-LINE. */
void
clear_lines(VW *vw, long first_line, long last_line)
{
    if((first_line >= (vw->vw_StartLine + vw->vw_MaxY))
       || (last_line < vw->vw_StartLine)
       || (first_line >= last_line))
	return;
    if(first_line < vw->vw_StartLine)
	first_line = 0;
    else
	first_line -= vw->vw_StartLine;
    if(last_line > (vw->vw_StartLine + vw->vw_MaxY))
	last_line = vw->vw_MaxY;
    else
	last_line -= vw->vw_StartLine;
    CLR_RECT(vw, 0, first_line * vw->vw_Win->w_FontY,
	     vw->vw_WidthPix, last_line * vw->vw_Win->w_FontY);
}

/*
 * Copies from srcLine to bottom of screen to dstLine
 */
void
cut_paste_lines(VW *vw, long srcLine, long dstLine)
{
    int yht, ysrc, ydst;
    int ybottom = vw->vw_MaxY * vw->vw_Win->w_FontY;
    long lastline = vw->vw_StartLine + vw->vw_MaxY;
    /* number of lines which are not blank. */
    int lastdisp = (vw->vw_Tx->tx_LogicalEnd - vw->vw_Tx->tx_ModDelta)
		   - vw->vw_LastDisplayOrigin.pos_Line;
    if(srcLine < vw->vw_StartLine)
	srcLine = vw->vw_StartLine;
    if(dstLine < vw->vw_StartLine)
	dstLine = vw->vw_StartLine;
#if 0
    if(srcLine >= lastline)
	srcLine = lastline - 1;
#endif
    if(dstLine >= lastline)
	return;
    if(srcLine == dstLine)
	return;
    ysrc = (srcLine - vw->vw_StartLine) * vw->vw_Win->w_FontY;
    ydst = (dstLine - vw->vw_StartLine) * vw->vw_Win->w_FontY;
    if(ysrc > ydst)
    {
	if(lastdisp >= vw->vw_MaxY)
	    yht = ybottom - ysrc;
	else
	{
	    yht = (lastdisp * vw->vw_Win->w_FontY) - ydst;
	    /* I'm not sure about this? should `ysrc' be replaced by `ydst'? */
	    if((yht + ysrc) > ybottom)
		yht = ybottom - ysrc;
	}
    }
    else
    {
	if(lastdisp >= vw->vw_MaxY)
	    yht = ybottom - ydst;
	else
	{
	    yht = (lastdisp * vw->vw_Win->w_FontY) - ysrc;
	    if((yht + ydst) > ybottom)
		yht = ybottom - ydst;
	}
    }
    if(yht > 0)
	COPY_AREA(vw, 0, ysrc, vw->vw_WidthPix, yht, 0, ydst);
    if(ysrc > ydst)
    {
	/* stuff we weren't able to blit.  */
	long firstline = lastline - (srcLine - dstLine);
	if(firstline < vw->vw_Tx->tx_LogicalEnd)
	{
	    redraw_lines_clr(vw, firstline, lastline);
	    if(lastline > vw->vw_Tx->tx_LogicalEnd)
	    {
		CLR_RECT(vw, 0, (vw->vw_Win->w_FontY * (vw->vw_Tx->tx_NumLines
							- vw->vw_StartLine)),
			 vw->vw_WidthPix, ybottom);
	    }
	}
	else
	{
	    CLR_RECT(vw, 0, (vw->vw_Win->w_FontY * (firstline
						    - vw->vw_StartLine)),
		     vw->vw_WidthPix, ybottom);
	}
    }
}

/* Scrolls the view LINES towards row zero. LINES can be +ve or -ve */
void
scroll_vw(VW *vw, long lines)
{
    if(lines >= vw->vw_MaxY || lines <= -vw->vw_MaxY)
    {
	/* No point scrolling, just clear the whole view. */
	CLR_AREA(vw, 0, 0, vw->vw_WidthPix, vw->vw_HeightPix);
    }
    else if(lines > 0)
    {
	COPY_AREA(vw, 0, lines * vw->vw_Win->w_FontY,
		  vw->vw_WidthPix,
		  vw->vw_HeightPix - (lines * vw->vw_Win->w_FontY),
		  0, 0);
	CLR_AREA(vw, 0, vw->vw_HeightPix - (lines * vw->vw_Win->w_FontY),
		 vw->vw_WidthPix, lines * vw->vw_Win->w_FontY);
    }
    else if(lines < 0)
    {
	lines = 0 - lines;
	COPY_AREA(vw, 0, 0, vw->vw_WidthPix,
		  vw->vw_HeightPix - (lines * vw->vw_Win->w_FontY),
		  0, lines * vw->vw_Win->w_FontY);
	CLR_AREA(vw, 0, 0, vw->vw_WidthPix, lines * vw->vw_Win->w_FontY);
    }
}

void
redraw_message(WIN *w)
{
    VW *vw = w->w_MiniBuf;
    u_char *msg = w->w_Message;
    MOVE(vw, 0, FONT_ASCENT(vw));
    if(msg != 0)
    {
	int len = strlen(msg);
	if(len >= vw->vw_MaxX - 1)
	    len = vw->vw_MaxX - 1;
	TEXT(vw, P_TEXT, msg, len);
    }
    CLR_AREA(vw, PEN_X(vw), 0, vw->vw_WidthPix - PEN_X(vw), w->w_FontY);
}

void
redraw_status_buffer(VW *vw)
{
    int len;
    MOVE(vw, 0, vw->vw_MaxY * vw->vw_Win->w_FontY + FONT_ASCENT(vw));
    if(vw->vw_StatusBuf)
    {
	len = strlen(vw->vw_StatusBuf);
	if(len > vw->vw_MaxX)
	    len = vw->vw_MaxX;
	TEXT(vw, (vw->vw_Flags & VWFF_CUSTOM_STATUS) ? P_BLOCK :P_TEXT_RV,
	     vw->vw_StatusBuf, len);
	SET_AREA(vw, (vw->vw_Flags & VWFF_CUSTOM_STATUS) ? P_BLOCK_RV : P_TEXT,
		 PEN_X(vw), PEN_Y(vw) - FONT_ASCENT(vw),
		 vw->vw_WidthPix - PEN_X(vw), vw->vw_Win->w_FontY);
    }
    else
	SET_AREA(vw, (vw->vw_Flags & VWFF_CUSTOM_STATUS) ? P_BLOCK_RV : P_TEXT,
		 PEN_X(vw), PEN_Y(vw) - FONT_ASCENT(vw),
		 vw->vw_WidthPix - PEN_X(vw), vw->vw_Win->w_FontY);
}
