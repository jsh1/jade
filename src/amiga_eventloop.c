/* amiga_eventloop.c -- Eventloop for AmigaDOS
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

#define INTUI_V36_NAMES_ONLY
#include <clib/intuition_protos.h>
#include <intuition/gadgetclass.h>
#include <string.h>

_PR VALUE event_loop(void);

VALUE
handle_event(struct IntuiMessage *imsg)
{
    VW *oldvw = curr_vw, *ev_vw;
    VALUE result = sym_nil;
    ev_vw = (VW *)imsg->IDCMPWindow->UserData;
    if(((imsg->Class != IDCMP_RAWKEY) || (!(imsg->Code & IECODE_UP_PREFIX)))
       && ((imsg->Class != IDCMP_MENUPICK) || (imsg->Code != MENUNULL)))
    {
	switch(imsg->Class)
	{
	    u_long code, mods;
	case IDCMP_NEWSIZE:
	    if(ev_vw == oldvw)
		cursor(ev_vw, CURS_OFF);
	    sys_update_dimensions(ev_vw);
	    ev_vw->vw_Flags |= VWFF_FORCE_REFRESH;
	    refresh_window(ev_vw);
	    if(ev_vw == oldvw)
		cursor(ev_vw, CURS_ON);
	    break;
	case IDCMP_ACTIVEWINDOW:
	    if(ev_vw != oldvw)
	    {
		cursor(oldvw, CURS_OFF);
		cursor(ev_vw, CURS_ON);
		curr_vw = ev_vw;
	    }
	    break;
	case IDCMP_MOUSEBUTTONS:
	case IDCMP_MOUSEMOVE:
#ifndef NOSCRLBAR
	    if(ev_vw->vw_SBar.gad
	       && (ev_vw->vw_SBar.gad->Flags & GFLG_SELECTED))
	    {
		long newtop;
		GetAttr(PGA_Top, ev_vw->vw_SBar.gad, &newtop);
		if(ev_vw == oldvw)
		    cursor(ev_vw, CURS_OFF);
		set_start_line(ev_vw, newtop);
		refresh_window(ev_vw);
		if(ev_vw == oldvw)
		    cursor(ev_vw, CURS_ON);
		ev_vw->vw_SBar.top = newtop;
		break;
	    }
#endif
	    /* FALL THROUGH */
	case IDCMP_RAWKEY:
	    code = mods = 0;
	    translate_event(&code, &mods, imsg);
	    if(mods & EV_TYPE_MASK)
	    {
		curr_vw = ev_vw;
		if(oldvw != ev_vw)
		    cursor(oldvw, CURS_OFF);
		result = usekey(imsg, code, mods, (ev_vw == oldvw));
	    }
	    break;
	case IDCMP_CLOSEWINDOW:
	    curr_vw = ev_vw;
	    cursor(oldvw, CURS_OFF);
	    result = cmd_eval_hook2(MKSTR("window-closed-hook"), sym_nil);
	    if(curr_vw)
	    {
		refresh_world();
		cursor(curr_vw, CURS_ON);
	    }
	    break;
	case IDCMP_MENUPICK:
	    curr_vw = ev_vw;
	    reset_message(ev_vw);
	    if(ev_vw == oldvw)
		cursor(curr_vw, CURS_OFF);
	    result = evalmenu(imsg->Code, imsg->Qualifier);
	    break;
#ifndef NOSCRLBAR
	case IDCMP_GADGETUP:
	case IDCMP_GADGETDOWN:
	    if(imsg->IAddress == ev_vw->vw_SBar.gad)
	    {
		long newtop;
		GetAttr(PGA_Top, ev_vw->vw_SBar.gad, &newtop);
		if(ev_vw == oldvw)
		    cursor(ev_vw, CURS_OFF);
		set_start_line(ev_vw, newtop);
		refresh_window(ev_vw);
		if(ev_vw == oldvw)
		    cursor(ev_vw, CURS_ON);
		ev_vw->vw_SBar.top = newtop;
	    }
	    break;
#endif
	}
	undo_end_of_command();
    }
    return(result);
}

VALUE
event_loop(void)
{
    VALUE result = sym_nil;
    int ticks = 0;
    recurse_depth++;
    std_message(curr_vw);
    refresh_world_curs();
    while(curr_vw)
    {
	u_long signals;
	struct IntuiMessage *imsg;
	std_message(curr_vw);
	if(input_lock && rexx_sig)
	    signals = Wait(rexx_sig);
	else
	    signals = Wait((1 << curr_vw->vw_Window->UserPort->mp_SigBit)
			   | rexx_sig | server_sig | SIGBREAKF_CTRL_C);
	if(signals & SIGBREAKF_CTRL_C)
	    ami_interrupt_handler();
	else if(rexx_sig && (signals & rexx_sig))
	{
	    disp_rexx_port();
	    refresh_world_curs();
	}
	else if(server_sig && (signals & server_sig))
	    server_accept();
	while(INT_P
	      || (curr_vw
		  && (imsg = (struct IntuiMessage *)
		      GetMsg(curr_vw->vw_Window->UserPort))))
	{
	    if(INT_P)
		result = NULL;
	    else
	    {
		struct IntuiMessage imsgcopy = *imsg;
		ReplyMsg((struct Message *)imsg);
		if(imsg->Class == IDCMP_INTUITICKS)
		{
		    bool refreshp = FALSE;
		    /* This needs fixing. Intuiticks are only received
		       while one of our windows is active.  */
		    if(++ticks >= (EVENT_TIMEOUT_LENGTH * 10))
		    {
			ticks = 0;
			if(print_event_prefix() || auto_save_buffers())
			    refreshp = TRUE;
			else if(data_after_gc > idle_gc_threshold)
			{
			    /* nothing was saved so try a GC */
			    cmd_garbage_collect(sym_t);
			}
			else
			{
			    VALUE hook = cmd_symbol_value(sym_idle_hook,
							  sym_t);
			    if(!VOIDP(hook) && !NILP(hook))
			    {
				cmd_eval_hook2(sym_idle_hook, sym_nil);
				refreshp = TRUE;
			    }
			}
			if(refreshp)
			{
			    std_message(curr_vw);
			    refresh_world_curs();
			}
		    }
		}
		else
		{
		    ticks = 0;
		    result = handle_event(&imsgcopy);
		}
	    }
	    if(!result)
	    {
		if(throw_value)
		{
		    VALUE tv = throw_value;
		    VALUE car = VCAR(tv);
		    throw_value = NULL;
		    if(car == sym_exit)
		    {
			result = VCDR(tv);
			if(recurse_depth > 0)
			    goto end;
		    }
		    else if((car == sym_top_level) && (recurse_depth == 0))
			result = VCDR(tv);
		    else if(car == sym_quit)
			goto end;
		    else if(car == sym_user_interrupt)
		    {
			handle_error(car, sym_nil);
			result = sym_nil;
		    }
		    else if(car == sym_error)
		    {
			handle_error(VCAR(VCDR(tv)), VCDR(VCDR(tv)));
			result = sym_nil;
		    }
		    else if(recurse_depth == 0)
		    {
			result = sym_nil;
#if 0
			/* This is no good; we have to do the error NOW */
			cmd_signal(sym_no_catcher, LIST_1(car));
#else
			handle_error(sym_no_catcher, LIST_1(car));
#endif
		    }
		    else
		    {
			throw_value = tv;
			goto end;
		    }
		}
		else
		    result = sym_nil;
	    }
	}
	if(!curr_vw)
	    goto end;
#ifndef NOSCRLBAR
	if(((curr_vw->vw_Flags & VWFF_SLEEPING) == 0)
	   && ((curr_vw->vw_StartLine != curr_vw->vw_SBar.top)
	       || (curr_vw->vw_Tx->tx_NumLines != curr_vw->vw_SBar.total)))
	{
	    sys_update_scroller(curr_vw);
	}
#endif
	std_message(curr_vw);
	if(curr_vw->vw_Flags & VWFF_REFRESH_STATUS)
	{
	    refresh_message(curr_vw);
	    curr_vw->vw_Flags &= ~VWFF_REFRESH_STATUS;
	}
    }
end:
    recurse_depth--;
    return(result);
}
