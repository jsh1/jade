/* amiga_minrexx.c -- ARexx interface
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

/*
 * Originally by Radical Eye Software, modified by me.
 */

static char *blurb = "Radical Eye MinRexx 0.4.jsh";

#include "jade.h"
#include "jade_protos.h"

#include <clib/exec_protos.h>
#ifdef _DCC
# define RexxSysBase_DECLARED
#endif
#include <clib/rexxsyslib_protos.h>
#include <string.h>

_PR void rexx_init(void);
_PR void rexx_kill(void);

_PR VALUE do_sync_rexx(u_char *, bool);
_PR void disp_rexx_port(void);
static struct RexxMsg *send_rexx_cmd(u_char *, int(*)(), u_char *, u_char *, u_char *, long);
_PR struct RexxMsg *sync_rexx_cmd(u_char *, bool, struct RexxMsg *);
_PR struct RexxMsg *async_rexx_cmd(u_char *);
_PR struct RexxMsg *async_rexx_string(u_char *);
_PR void reply_rexx_cmd(struct RexxMsg *, long, long, u_char *);
static void close_rexx_lib(void);
static bool open_rexx_lib(void);
static void reply_to_it(struct RexxMsg *);

_PR u_long rexx_sig;
    u_long rexx_sig;

static struct MsgPort *rexx_port;
static bool bringer_down;
static long still_need_replies;
static struct RexxMsg *o_rexx_msg;
static VALUE rexx_name;

struct RxsLib *RexxSysBase;

#define RX_EXTEN "jade"

#define MAXRXOUTSTANDING (300)
#define RXERRORIMGONE (100)
#define RXERRORNOCMD (30)

_PR VALUE cmd_rexx_async_macro(VALUE name);
DEFUN("rexx-async-macro", cmd_rexx_async_macro, subr_rexx_async_macro, (VALUE name), V_Subr1, DOC_rexx_async_macro) /*
::doc:rexx_async_macro::
rexx-async-macro NAME
<AMIGA-ONLY>

Asynchronously invokes the ARexx macro called NAME.
::end:: */
{
    DECLARE1(name, STRINGP);
    if(async_rexx_cmd(VSTR(name)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_rexx_async_str(VALUE str);
DEFUN("rexx-async-str", cmd_rexx_async_str, subr_rexx_async_str, (VALUE str), V_Subr1, DOC_rexx_async_str) /*
::doc:rexx_async_str::
rexx-async-str STRING
<AMIGA-ONLY>

Gets ARexx to asynchronously interpret the string of REXX code STRING.
::end:: */
{
    DECLARE1(str, STRINGP);
    if(async_rexx_string(VSTR(str)))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_rexx_sync_macro(VALUE name);
DEFUN("rexx-sync-macro", cmd_rexx_sync_macro, subr_rexx_sync_macro, (VALUE name), V_Subr1, DOC_rexx_sync_macro) /*
::doc:rexx_sync_macro::
rexx-sync-macro NAME
<AMIGA-ONLY>

Invokes the ARexx macro NAME synchronously, returning its value when it
exits (this will either be a number or a string.
::end:: */
{
    DECLARE1(name, STRINGP);
    return(do_sync_rexx(VSTR(name), TRUE));
}

_PR VALUE cmd_rexx_sync_str(VALUE str);
DEFUN("rexx-sync-str", cmd_rexx_sync_str, subr_rexx_sync_str, (VALUE str), V_Subr1, DOC_rexx_sync_str) /*
::doc:rexx_sync_str::
rexx-sync-str STRING
<AMIGA-ONLY>

Synchronously executes the string of ARexx commands STRING, returning their
exit value.
::end:: */
{
    DECLARE1(str, STRINGP);
    return(do_sync_rexx(VSTR(str), FALSE));
}

_PR VALUE var_rexx_port_name(VALUE val);
DEFUN("rexx-port-name", var_rexx_port_name, subr_rexx_port_name, (VALUE val), V_Var, DOC_rexx_port_name) /*
::doc:rexx_port_name::
Variable holding the name of our ARexx port. (Amiga only).
::end:: */
{
    if(val)
	return(NULL);
    return(rexx_name ? rexx_name : sym_nil);
}

void
rexx_init(void)
{
    int i;
    static UBYTE name[32];
    name[0] = V_StaticString;
    Forbid();
    for(i = 1;; i++)
    {
	sprintf(VSTR(name), "JADE.%d", i);
	if(!FindPort(VSTR(name)))
	{
	    rexx_port = CreateMsgPort();
	    if(rexx_port)
	    {
		rexx_port->mp_Node.ln_Name = VSTR(name);
		rexx_port->mp_Node.ln_Pri = 1;
		AddPort(rexx_port);
		rexx_sig = 1 << rexx_port->mp_SigBit;
		rexx_name = name;
	    }
	}
	break;
    }
    Permit();
    ADD_SUBR(subr_rexx_async_macro);
    ADD_SUBR(subr_rexx_async_str);
    ADD_SUBR(subr_rexx_sync_macro);
    ADD_SUBR(subr_rexx_sync_str);
    ADD_SUBR(subr_rexx_port_name);
}

void
rexx_kill(void)
{
    if(rexx_port)
    {
	RemPort(rexx_port);
	bringer_down = TRUE;
	if(o_rexx_msg)
	{
	    o_rexx_msg->rm_Result1 = RXERRORIMGONE;
	    ReplyMsg((struct Message *)o_rexx_msg);
	    o_rexx_msg = NULL;
	}
	while(still_need_replies)
	{
	    WaitPort(rexx_port);
	    disp_rexx_port();
	}
	close_rexx_lib();
	DeleteMsgPort(rexx_port);
	rexx_port = NULL;
    }
    rexx_sig = 0;
}

/*
 * This function [hopefully] performs a synchronous REXX call, from
 * which we get the result :)
 */
VALUE
do_sync_rexx(u_char *arg, bool isMac)
{
    VALUE result = sym_nil;
    if(rexx_port && open_rexx_lib())
    {
	struct MsgPort *rp;
	if(rp = CreateMsgPort())
	{
	    struct RexxMsg *rm;
	    if(rm = CreateRexxMsg(rp, RX_EXTEN, rexx_port->mp_Node.ln_Name))
	    {
		rm->rm_Action |= RXFF_RESULT;
		if(send_rexx_cmd(arg, reply_to_it, (char *)rm, NULL, NULL, (isMac ? 0 : RXFF_STRING) | RXFF_RESULT))
		{
		    u_long rpsigmask = 1 << rp->mp_SigBit;
		    u_long sigs;

		    do {
			sigs = Wait(rpsigmask | rexx_sig);
			disp_rexx_port();
		    } while(!(sigs & rpsigmask));

		    if(GetMsg(rp))
		    {
			if(!rm->rm_Result1 && rm->rm_Result2)
			{
			    result = string_dup((u_char *)rm->rm_Result2);
			    if(open_rexx_lib())
				DeleteArgstring((u_char *)rm->rm_Result2);
			}
			else
			    result = make_number(rm->rm_Result1);
		    }
		}
		if(open_rexx_lib())
		    DeleteRexxMsg(rm);
	    }
	    DeleteMsgPort(rp);
	}
	close_rexx_lib();
    }
    return(result);
}

static void
close_rexx_lib(void)
{
    if((!still_need_replies) && RexxSysBase)
    {
	CloseLibrary((struct Library *)RexxSysBase);
	RexxSysBase = NULL;
    }
}

void
disp_rexx_port(void)
{
    struct RexxMsg *rexxmsg;
    u_char *cmd;

    if(!rexx_port)
	return;

    while(rexxmsg = (struct RexxMsg *)GetMsg(rexx_port))
    {
	if(rexxmsg->rm_Node.mn_Node.ln_Type == NT_REPLYMSG)
	{
	    if (rexxmsg->rm_Args[1])
	    {
		((int (*)(struct RexxMsg *))(rexxmsg->rm_Args[1]))(rexxmsg);
	    }
	    DeleteArgstring(rexxmsg->rm_Args[0]);
	    DeleteRexxMsg(rexxmsg);
	    still_need_replies--;
	    close_rexx_lib();
	}
	else
	{
	    cmd = (u_char *)rexxmsg->rm_Args[0];
	    while (*cmd > 0 && *cmd <= ' ')
		cmd++;
	    rexxmsg->rm_Result1 = 0;
	    rexxmsg->rm_Result2 = 0;

	    if(bringer_down)
		rexxmsg->rm_Result1 = RXERRORIMGONE;
	    else
	    {
		VALUE res;
		o_rexx_msg = rexxmsg ;
		cursor(curr_vw, CURS_OFF);
		res = eval_string(cmd, FALSE);
		if(res)
		{
		    if(STRINGP(res))
			reply_rexx_cmd(rexxmsg, 0, 0, VSTR(res));
		    else if(NUMBERP(res))
		    {
			u_char buf[32];
			sprintf(buf, "%d", VNUM(res));
			reply_rexx_cmd(rexxmsg, 0, 0, buf);
		    }
		    else if(res == sym_nil)
			reply_rexx_cmd(rexxmsg, 5, 0, NULL);
		    else
			reply_rexx_cmd(rexxmsg, 0, 0, NULL);
		}
		else
		    rexxmsg->rm_Result1 = RXERRORNOCMD;
		cursor(curr_vw, CURS_ON);
	    }
	    o_rexx_msg = NULL ;
	    ReplyMsg((struct Message *)rexxmsg);
	}
    }
}

static bool
open_rexx_lib(void)
{
    if(RexxSysBase || (RexxSysBase = (struct RxsLib *)OpenLibrary(RXSNAME, 0L)))
	return(TRUE);
    return(FALSE);
}

static struct RexxMsg *
send_rexx_cmd(u_char *s, int(*f)(), u_char *p1, u_char *p2, u_char *p3, long m)
{
    struct MsgPort *rexxport;
    struct RexxMsg *rexxmsg;

    if((!rexx_port) || (still_need_replies > MAXRXOUTSTANDING-1))
	return(FALSE);
    rexxmsg = NULL;
    if(open_rexx_lib()
       && (rexxmsg = CreateRexxMsg(rexx_port, RX_EXTEN, rexx_port->mp_Node.ln_Name))
       && (rexxmsg->rm_Args[0] = CreateArgstring(s, (long)strlen(s))))
    {
	rexxmsg->rm_Action = RXCOMM | m;
	rexxmsg->rm_Args[1] = (u_char *)f;
	rexxmsg->rm_Args[2] = p1;
	rexxmsg->rm_Args[3] = p2;
	rexxmsg->rm_Args[4] = p3;
	rexxmsg->rm_Node.mn_Node.ln_Name = RXSDIR;
	Forbid();
	if(rexxport = FindPort(RXSDIR))
	    PutMsg(rexxport, (struct Message *)rexxmsg);
	Permit();
	if(rexxport)
	{
	    still_need_replies++;
	    return(rexxmsg);
	}
	else
	    DeleteArgstring(rexxmsg->rm_Args[0]);
    }
    if(rexxmsg)
	DeleteRexxMsg(rexxmsg);
    close_rexx_lib();
    return(FALSE);
}

struct RexxMsg *
async_rexx_cmd(u_char *s)
{
    return(send_rexx_cmd(s, NULL, NULL, NULL, NULL, 0));
}

struct RexxMsg *
async_rexx_string(u_char *s)
{
    return(send_rexx_cmd(s, NULL, NULL, NULL, NULL, RXFF_STRING));
}

static void
reply_to_it(struct RexxMsg *msg)
{
    struct RexxMsg *omsg = (struct RexxMsg *)(msg->rm_Args[2]);
    reply_rexx_cmd(omsg, msg->rm_Result1, msg->rm_Result2,
		   (u_char *)msg->rm_Result2);
    ReplyMsg((struct Message *)omsg);
}

struct RexxMsg *
sync_rexx_cmd(u_char *s, bool isMac, struct RexxMsg *msg)
{
    return(send_rexx_cmd(s, reply_to_it, (u_char *)msg, NULL, NULL,
			 isMac ? 0 : RXFF_STRING));
}

void
reply_rexx_cmd(struct RexxMsg *msg, long primary, long secondary,
	       u_char *string)
{
    if(!primary && (msg->rm_Action & (1L << RXFB_RESULT)))
    {
	if(string && open_rexx_lib())
	    secondary = (long)CreateArgstring(string, (long)strlen(string));
	else
	    secondary = 0L;
    }
    msg->rm_Result1 = primary;
    msg->rm_Result2 = secondary;
    close_rexx_lib();
}
