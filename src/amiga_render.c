/* amiga_render.c -- Rendering for AmigaDOS
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

#include "jade.h"
#include "jade_protos.h"

#ifdef _DCC
# define GfxBase_DECLARED
#endif
#include <clib/graphics_protos.h>
#include <graphics/gfxbase.h>
#include <graphics/gfxmacros.h>

_PR void sys_scroll_vw(VW *, long);

extern struct GfxBase *GfxBase;

/*
 * Scrolls the view `lines' towards line 0,
 */
void
sys_scroll_vw(VW *vw, long lines)
{
    struct RastPort *rp = vw->vw_WindowSys.ws_Rp;
    ScrollRaster(rp, 0, lines * vw->vw_FontY, vw->vw_XStartPix,
		 vw->vw_YStartPix, vw->vw_XEndPix,
		 vw->vw_YStartPix + (vw->vw_MaxY * vw->vw_FontY) - 1);
}
