/* amiga_server.c -- editor server for Amiga
   Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING. If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <clib/exec_protos.h>
#include <exec/ports.h>

#include "amiga_server.h"

_PR void server_accept(void);
_PR void server_init(void);
_PR void server_kill(void);

/* List of (FILE-NAME . CLIENT-MSG) where CLIENT-MSG is a pointer (number)
   to the struct clientmsg  */
static VALUE client_list;

static VALUE sym_server_open_file;

/* Signal mask for the server's message port. */
_PR u_long server_sig;
u_long server_sig;

/* The server's msgport */
static struct MsgPort *server_port;

/* Called when the eventloop receives server_sig */
void
server_accept(void)
{
    struct clientmsg *cm;
    while(cm = (struct clientmsg *)GetMsg(server_port))
    {
	VALUE file = string_dup(cm->cm_file);
	client_list = cmd_cons(cmd_cons(file, make_number((u_long)cm)), client_list);
	cursor(curr_vw, CURS_OFF);
	call_lisp2(sym_server_open_file, file, make_number(cm->cm_num - 1));
	std_message(curr_vw);
	refresh_world();
	cursor(curr_vw, CURS_ON);
    }
}

_PR VALUE cmd_server_open_p(void);
DEFUN("server-open-p", cmd_server_open_p, subr_server_open_p, (void), V_Subr0, DOC_server_open_p)
{
    if(server_port)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_server_open(void);
DEFUN_INT("server-open", cmd_server_open, subr_server_open, (void), V_Subr0, DOC_server_open, "")
{
    VALUE res = sym_t;
    if(!server_port)
    {
	Forbid();
	if(FindPort(JADE_PORT_NAME))
	{
	    message("A server is already open.");
	    res = sym_nil;
	}
	else
	{
	    server_port = CreateMsgPort();
	    if(server_port)
	    {
		server_port->mp_Node.ln_Name = JADE_PORT_NAME;
		server_port->mp_Node.ln_Pri = 1;
		AddPort(server_port);
		server_sig = 1 << server_port->mp_SigBit;
	    }
	    else
		res = cmd_signal(sym_error, LIST_1(MKSTR("Can't CreateMsgPort()")));
	}
	Permit();
    }
    return(res);
}

_PR VALUE cmd_server_close(void);
DEFUN_INT("server-close", cmd_server_close, subr_server_close, (void), V_Subr0, DOC_server_close, "")
{
    if(server_port)
    {
	RemPort(server_port);
	DeleteMsgPort(server_port);
	server_port = NULL;
	server_sig = 0;
    }
    return(sym_t);
}

_PR VALUE cmd_server_reply(VALUE file, VALUE rc);
DEFUN("server-reply", cmd_server_reply, subr_server_reply, (VALUE file, VALUE rc), V_Subr2, DOC_server_reply)
{
    VALUE res = sym_nil;
    VALUE tmp = client_list;
    if(BUFFERP(file))
	file = VTX(file)->tx_FileName;
    else if(!STRINGP(file))
	file = curr_vw->vw_Tx->tx_FileName;
    client_list = sym_nil;
    while(CONSP(tmp))
    {
	register VALUE car = VCAR(tmp);
	VALUE next = VCDR(tmp);
	if(!VALUE_CMP(file, VCAR(car)))
	{
	    /* Send the result to our client. */
	    struct clientmsg *cm = (struct clientmsg *)VNUM(VCDR(car));
	    cm->cm_num = NUMBERP(rc) ? VNUM(rc) : 0;
	    ReplyMsg(&cm->cm_msg);
	    res = sym_t;
	}
	else
	{
	    VCDR(tmp) = client_list;
	    client_list = tmp;
	}
	tmp = next;
    }
    return(res);
}

void
server_init(void)
{
    client_list = sym_nil;
    mark_static(&client_list);
    INTERN(sym_server_open_file, "server-open-file");
    ADD_SUBR(subr_server_open_p);
    ADD_SUBR(subr_server_open);
    ADD_SUBR(subr_server_close);
    ADD_SUBR(subr_server_reply);
}

void
server_kill(void)
{
    VALUE tmp = client_list;
    while(CONSP(tmp))
    {
	/* Any client-opened files still around are replied to with
	   a result of 5 (fail).  */
	struct clientmsg *cm = (struct clientmsg *)VNUM(VCDR(VCAR(tmp)));
	cm->cm_num = 5;
	ReplyMsg(&cm->cm_msg);
	tmp = VCDR(tmp);
    }
    client_list = sym_nil;
    cmd_server_close();
}
