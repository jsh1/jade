/* amiga_display.c -- Initialisation for Amiga window-system
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

#include <string.h>
#include <stdlib.h>
#include <exec/libraries.h>
#include <exec/tasks.h>

_PR void sys_usage(void);
_PR int sys_init(int, char **);
_PR int ami_interrupt_handler(void);

extern	struct Library *SysBase;
_PR bool ami_from_wb;
    bool ami_from_wb;
_PR short ami_os_version;
    short ami_os_version;

_PR VALUE def_font_str, ami_def_pub_screen;
VALUE def_font_str = MKSTR(DEFAULT_FONT), ami_def_pub_screen;
_PR int ami_def_font_size;
int ami_def_font_size = DEFAULT_FONT_SIZE;

_PR struct Process *ami_jade_process;
struct Process *ami_jade_process;

void
sys_usage(void)
{
    fputs("where SYSTEM-OPTIONS are,\n"
	"    -pubscreen SCREEN\n"
	"    -font FONT   FONT is something like `topaz.font-8'\n"
#if AMIGA_V39 && (!defined(NO_STACK_SWAP))
        "    -stack STACK-SIZE\n"
#endif
	, stderr);
}

int
sys_init(int argc, char **argv)
{
    /* This variable is static to ensure that if a new stack is created
       it's still accessible. */
    static int rc;
    int new_stack = 0;

    if(argc == 0)
	ami_from_wb = TRUE;
    else
    {
	argc--;
	argv++;
    }
    if((ami_os_version = SysBase->lib_Version) < 37)
    {
	doconmsg("error: need system version 2.04 or greater\n");
	return(10);
    }
    ami_jade_process = (struct Process *)FindTask(NULL);
    ami_def_pub_screen = null_string;
    while((argc >= 1) && (**argv == '-'))
    {
	if(argc >= 2)
	{
	    if(!strcmp("-pubscreen", *argv))
		ami_def_pub_screen = string_dup(argv[1]);
	    else if(!strcmp("-font", *argv))
	    {
		char *tmp = strrchr(argv[1], '-');
		if(!tmp)
		{
		    doconmsg("error: no size in `-font' arg\n");
		    return(5);
		}
		ami_def_font_size = atol(tmp+1);
		def_font_str = string_dupn(argv[1], tmp - argv[1]);
	    }
#if AMIGA_V39 && (!defined(NO_STACK_SWAP))
            else if(!strcmp("-stack", *argv))
	    {
		new_stack = atol(argv[1]);
		if(new_stack < 4096)
		{
		    doconmsg("error: stack size too small\n");
		    return(5);
		}
	    }
#endif
	    else
		break;
	    argc--; argv++;
	}
	else
	    break;
	argc--; argv++;
    }

    /* Install ^C handler to take stdio caught signals. */
#ifdef HAVE_ONBREAK
    onbreak(ami_interrupt_handler);
#endif

    rexx_init();
    ami_menus_init();

#if AMIGA_V39 && (!defined(NO_STACK_SWAP))
    /* The StackSwap() function only seems to be in my V39 includes even
       though it's in the V37 library??
         Note that the two calls of StackSwap() must be done inline! a stub
       would rely on the old stack!  */
    if(new_stack > 0)
    {
	char *stk_mem = mymalloc(new_stack);
	if(stk_mem == NULL)
	{
	    doconmsg("error: can't allocate new stack\n");
	    rc = 10;
	}
	else
	{
	    static struct StackSwapStruct ss;
	    static int argc_copy;
	    static char **argv_copy;

	    /* These are copied into non-stack memory. */
	    argc_copy = argc;
	    argv_copy = argv;

	    /* Setup SS and swap the stacks over. */
	    ss.stk_Lower = (APTR)stk_mem;
	    ss.stk_Upper = (ULONG)(stk_mem + new_stack);
	    ss.stk_Pointer = (APTR)(stk_mem + new_stack);
	    StackSwap(&ss);

	    /* Call the main loop */
	    rc = inner_main(argc_copy, argv_copy);

	    /* Reinstall the original stack. */
	    StackSwap(&ss);

	    myfree(stk_mem);
	}
    }
    else
#endif
        rc = inner_main(argc, argv);

    clip_kill();
    freq_kill();
    ami_menus_kill();
    rexx_kill();
    return(rc);
}

int
ami_interrupt_handler(void)
{
    /* Clear the outstanding signal */
    SetSignal(0, SIGBREAKF_CTRL_C);
    throw_value = int_cell;
    /* Ignore ^C if from onbreak(). */
    return(0);
}

#ifdef _DCC
/*
 * I think this is what DICE's wbmain() is like? All I want to
 * do is call main() with argc=0
 */
void
wbmain(void *unused)
{
    char *argv[1];
    argv[0] = NULL;
    main(0, argv);
}
#endif
