/* keys.h -- Event structures
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

#define EV_TYPE_KEYBD	0x00000001
#define EV_TYPE_MOUSE	0x00000002
#define EV_TYPE_MASK	0x00000003

#define EV_MOD_SHIFT	0x00000004
#define EV_MOD_CTRL	0x00000008
#define EV_MOD_MOD1	0x00000010
#define EV_MOD_BUTTON1	0x00000020
#define EV_MOD_BUTTON2	0x00000040
#define EV_MOD_BUTTON3	0x00000080
#define EV_MOD_MOD2	0x00000100
#define EV_MOD_MOD3	0x00000200
#define EV_MOD_MOD4	0x00000400
#define EV_MOD_MOD5	0x00000800
#define EV_MOD_BUTTON4	0x00001000
#define EV_MOD_BUTTON5	0x00002000

/* Fake meta modifier. This is replaced by whichever of the above modifiers
   is the result of the WINDOW_META(W) macro. */
#define EV_MOD_META	0x00010000

#define EV_MOD_MASK	0xfffffffc
#define EV_MOD_BUTTON_MASK (EV_MOD_BUTTON1 | EV_MOD_BUTTON2 \
			    | EV_MOD_BUTTON3 | EV_MOD_BUTTON4 | EV_MOD_BUTTON5)

#define EV_MOD_LMB 	EV_MOD_BUTTON1
#define EV_MOD_MMB	EV_MOD_BUTTON2
#define EV_MOD_RMB	EV_MOD_BUTTON3

/* For EV_TYPE_KEYBD the code is like this,
   X11:
    Standard Keysym's
   Amiga:
    Normal *raw* keycodes  */

/* EV_CODE for EV_TYPE_MOUSE events */
#define EV_CODE_MOUSE_CLICK1 1
#define EV_CODE_MOUSE_CLICK2 2
#define EV_CODE_MOUSE_MOVE   3
#define EV_CODE_MOUSE_UP     4

/* An event object is (CODE . MODS) */
#define EVENTP(v)	(rep_CONSP(v) && rep_INTP(rep_CAR(v)) && rep_INTP(rep_CDR(v)))
#define MAKE_EVENT(c, m) Fcons(c, m)
#define EVENT_CODE(v)	rep_CAR(v)
#define EVENT_MODS(v)	rep_CDR(v)

/* A `key' is (COMMAND . EVENT) */
#define KEYP(v)		(rep_CONSP(v) && rep_CONSP(rep_CDR(v)))
#define KEY_COMMAND(v)	rep_CAR(v)
#define KEY_EVENT(v)	rep_CDR(v)
#define MAKE_KEY(e, c)	Fcons(c, e)
