/* unix_server.c -- client/server file handling
   Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING. If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

_PR void server_init(void);
_PR void server_kill(void);

/* List of (FILE-NAME . SOCK-FD) */
static VALUE client_list;

_PR VALUE sym_server_find_file;
DEFSYM(server_find_file, "server-find-file");

/* fd of the socket which clients connect to, or zero. */
static int socket_fd = -1;

/* pathname of the socket. */
static VALUE socket_name;

DEFSTRING(io_error, "server_make_connection:io");
DEFSTRING(val_fmt, "%S");

static void
server_handle_request(int fd)
{
    char req;
    if(read(fd, &req, 1) != 1)
	goto disconnect;
    switch(req)
    {
	u_long len, tem;
	VALUE val;

    case req_find_file:
	/* 1. read length field
	   2. read LENGTH bytes for the filename
	   3. read the line number
	   4. add (FILE . SOCK-FD) to list of client files
	   5. call server-find-file with FILE and LINE */
	if(read(fd, &len, sizeof(u_long)) != sizeof(u_long)
	   || (val = make_string(len + 1)) == LISP_NULL
	   || read(fd, VSTR(val), len) != len
	   || read(fd, &tem, sizeof(u_long)) != sizeof(u_long))
	    goto io_error;
	VSTR(val)[len] = 0;
	client_list = cmd_cons(cmd_cons(val, MAKE_INT(fd)), client_list);
	call_lisp2(sym_server_find_file, val, MAKE_INT(tem));
	/* Block any more input on this fd until we've replied to
	   the original message. */
	sys_deregister_input_fd(fd);
	break;

    case req_eval:
	/* 1. read length field
	   2. read LENGTH bytes of FORM
	   3. eval and print FORM
	   4. write length of result-string
	   5. write LENGTH bytes of result string */
	if(read(fd, &len, sizeof(u_long)) != sizeof(u_long)
	   || (val = make_string(len + 1)) == LISP_NULL
	   || read(fd, VSTR(val), len) != len)
	    goto io_error;
	VSTR(val)[len] = 0;
	val = cmd_read(cmd_cons(MAKE_INT(0), val));
	if(val != LISP_NULL)
	    val = cmd_eval(val);
	if(val != LISP_NULL)
	    val = cmd_format(LIST_3(sym_nil, VAL(&val_fmt), val));
	if(val != LISP_NULL && STRINGP(val))
	{
	    len = STRING_LEN(val);
	    if(write(fd, &len, sizeof(u_long)) != sizeof(u_long)
	       || write(fd, VSTR(val), len) != len)
		goto io_error;
	}
	else
	{
	    len = 0;
	    if(write(fd, &len, sizeof(u_long)) != sizeof(u_long))
		goto io_error;
	}
	break;

    io_error:
	cmd_signal(sym_error, LIST_1(VAL(&io_error)));
	return;

    case req_end_of_session:
    disconnect:
	sys_deregister_input_fd(fd);
	close(fd);
    }
}

static void
server_accept_connection(int unused_fd)
{
    int confd = accept(socket_fd, NULL, NULL);
    if(confd >= 0)
    {
	/* Once upon a time, I started reading commands here. I think
	   it's cleaner to just register CONFD as an input source */
	sys_register_input_fd(confd, server_handle_request);
    }
}

_PR VALUE cmd_server_open_p(void);
DEFUN("server-open-p", cmd_server_open_p, subr_server_open_p, (void), V_Subr0, DOC_server_open_p) /*
::doc:server_open_p::
server-open-p

t if the edit-server is open.
::end:: */
{
    if(socket_fd >= 0)
	return(sym_t);
    return(sym_nil);
}

DEFSTRING(unexp_name, "~/" JADE_SOCK_NAME);
DEFSTRING(no_name, "Can't make socket name");

_PR VALUE cmd_server_open(void);
DEFUN_INT("server-open", cmd_server_open, subr_server_open, (void), V_Subr0, DOC_server_open, "") /*
::doc:server_open::
server-open

Creates the socket (or whatever) so that the editor's client program can
send us messages.
::end:: */
{
    VALUE name;
    GC_root gc_name;
    if(socket_fd >= 0)
	return(sym_t);
    name = cmd_local_file_name(VAL(&unexp_name));
    if(name && STRINGP(name))
    {
	VALUE tmp;
	PUSHGC(gc_name, name);
	tmp = cmd_file_exists_p(name);
	POPGC;
	if(!tmp || !NILP(tmp))
	{
	    message("A server is already open.");
	    return(sym_nil);
	}
	socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if(socket_fd >= 0)
	{
	    struct sockaddr_un addr;
	    addr.sun_family = AF_UNIX;
	    strcpy(addr.sun_path, VSTR(name));
	    if(bind(socket_fd, (struct sockaddr *)&addr,
		    sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
	    {
		if(listen(socket_fd, 5) == 0)
		{
		    unix_set_fd_nonblocking(socket_fd);
		    sys_register_input_fd(socket_fd, server_accept_connection);

		    socket_name = name;
		    return(sym_t);
		}
	    }
		signal_file_error(sym_nil);
	}
	else
	{
	    cmd_signal(sym_error, LIST_1(VAL(&no_name)));
	}
	close(socket_fd);
	socket_fd = -1;
    }
    else
	signal_file_error(sym_nil);
    return LISP_NULL;
}

_PR VALUE cmd_server_close(void);
DEFUN_INT("server-close", cmd_server_close, subr_server_close, (void), V_Subr0, DOC_server_close, "") /*
::doc:server_close::
server-close

Stops listening for client messages.
::end:: */
{
    if(socket_fd > 0)
    {
	sys_deregister_input_fd(socket_fd);
	close(socket_fd);
	socket_fd = -1;
	unlink(VSTR(socket_name));
	socket_name = LISP_NULL;
    }
    return(sym_t);
}

_PR VALUE cmd_server_reply(VALUE file, VALUE rc);
DEFUN("server-reply", cmd_server_reply, subr_server_reply, (VALUE file, VALUE rc), V_Subr2, DOC_server_reply) /*
::doc:server_reply::
server-reply [FILE-NAME] [RETURN-CODE]

Replies to the editor client which asked us to edit the file FILE-NAME.
RETURN-CODE is the optional result for the client, by default it is zero
which denotes no errors. Returns nil if the file doesn't have a client.
::end:: */
{
    VALUE res = sym_nil, tmp;

    if(BUFFERP(file))
	file = VTX(file)->tx_CanonicalFileName;
    else if(!STRINGP(file))
	file = curr_vw->vw_Tx->tx_CanonicalFileName;
    else
    {
	GC_root gc_rc;
	PUSHGC(gc_rc, rc);
	file = cmd_canonical_file_name(file);
	POPGC;
	if(!file || !STRINGP(file))
	    return LISP_NULL;
    }

    tmp = client_list;
    client_list = sym_nil;
    while(CONSP(tmp))
    {
	register VALUE car = VCAR(tmp);
	VALUE next = VCDR(tmp);
	if(STRINGP(VCAR(car))
	   && strcmp(VSTR(file), VSTR(VCAR(car))) == 0)
	{
	    /* Send the result to our client. */
	    int con_fd = VINT(VCDR(car));
	    u_long result = INTP(rc) ? VINT(rc) : 0;
	    if(write(con_fd, &result, sizeof(result)) != sizeof(result))
		res = signal_file_error(file);
	    else
		res = sym_t;
	    /* We can handle input on this fd again now. */
	    sys_register_input_fd(con_fd, server_handle_request);
	}
	else
	{
	    VCDR(tmp) = client_list;
	    client_list = tmp;
	}
	tmp = next;
    }

    return res;
}

void
server_init(void)
{
    client_list = sym_nil;
    mark_static(&client_list);
    mark_static(&socket_name);
    INTERN(server_find_file);
    ADD_SUBR(subr_server_open_p);
    ADD_SUBR_INT(subr_server_open);
    ADD_SUBR_INT(subr_server_close);
    ADD_SUBR(subr_server_reply);
}

void
server_kill(void)
{
    /* clean up */
    VALUE tmp = client_list;
    while(CONSP(tmp))
    {
	/* Any client-opened files still around are replied to with
	   a result of 5 (fail).  */
	static u_long failrc = 5;
	int fd = VINT(VCDR(VCAR(tmp)));
	write(fd, &failrc, sizeof(u_long));
	close(fd);
	tmp = VCDR(tmp);
    }
    client_list = sym_nil;
    cmd_server_close();
}
