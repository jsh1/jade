/* amiga_defs.h -- declarations for AmigaDOS
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

#ifndef _AMIGA_DEFS_H
#define _AMIGA_DEFS_H

#include <exec/lists.h>
#include <clib/exec_protos.h>
#include <dos/dosextens.h>
#include "amiga_windowsys.h"

/* Since the Amiga has static stacks and my Lisp goes deep into
   the stack, some checking is order...  */
#define MINSTACK    2048
#define GC_MINSTACK 256		/* give gc more chance of succeeding */
#define STK_SIZE    stksize()
#define STK_WARN(s) ami_ezreq("%s: Almost out of stack!", "Stop", (long)s)

#define AMIGA_V39 (AMIGA_INCLUDE_VER >= 39)

#if AMIGA_V39
# define SET_WRITE_MASK(rp,m) SafeSetWriteMask(rp,m)
#else
# define SET_WRITE_MASK(rp,m) SetWrMsk(rp,m)
#endif

/* default directory to look for scripts in */
#define LISP_LIB_DIR "JADE:lisp/"

/* file containing doc-strings */
#define DOC_FILE "JADE:DOC"

/* standard font */
#define DEFAULT_FONT "topaz.font"
#define DEFAULT_FONT_SIZE 8

/* Timeout length. This determines how much time with no events received
   is needed before looking for buffers which are due to be auto-saved.  */
#define EVENT_TIMEOUT_LENGTH 1

/* Interrupt testing.  Test for the signal bit inline, the interrupt
   handler knows to clear it properly.  */
#define TEST_INT \
    do { \
	if(ami_jade_process->pr_Task.tc_SigRecvd & SIGBREAKF_CTRL_C) \
	     ami_interrupt_handler(); \
    } while(0)

#endif /* _AMIGA_DEFS_H */
