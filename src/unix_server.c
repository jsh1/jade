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
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>

_PR void server_init(void);
_PR void server_kill(void);

/* List of (FILE-NAME . SOCK-FD) */
static VALUE client_list;

static DEFSYM(server_open_file, "server-open-file");

/* fd of the socket which clients connect to, or zero. */
static int socket_fd = -1;

/* pathname of the socket. */
static VALUE socket_name;

static void
server_accept_connection(int unused_fd)
{
    int confd = accept(socket_fd, NULL, NULL);
    if(confd >= 0)
    {
	/* 1. read length field
	   2. read LENGTH bytes for the filename
	   3. read the line number
	   4. add (FILE . SOCK-FD) to list of client files
	   5. call client-open-file with FILE and LINE */
	u_short filenamelen;
	u_long tmp;
	VALUE filename, linenum;
	if(read(confd, &filenamelen, sizeof(u_short)) != sizeof(u_short))
	    goto readerror;
	filename = make_string(filenamelen + 1);
	if(read(confd, VSTR(filename), filenamelen) != filenamelen)
	    goto readerror;
	VSTR(filename)[filenamelen] = 0;
	if(read(confd, &tmp, sizeof(u_long)) != sizeof(u_long))
	{
	    static DEFSTRING(err, "server_make_connection:read");
	readerror:
	    cmd_signal(sym_error, LIST_1(VAL(err)));
	    return;
	}
	linenum = MAKE_INT(tmp - 1);
	client_list = cmd_cons(cmd_cons(filename, MAKE_INT(confd)),
			       client_list);
	/* lose this on exec() */
	fcntl(confd, F_SETFD, 1);
	cursor(curr_vw, CURS_OFF);
	call_lisp2(sym_server_open_file, filename, linenum);
	cursor(curr_vw, CURS_ON);
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

_PR VALUE cmd_server_open(void);
DEFUN_INT("server-open", cmd_server_open, subr_server_open, (void), V_Subr0, DOC_server_open, "") /*
::doc:server_open::
server-open

Creates the socket (or whatever) so that the editor's client program can
send us messages.
::end:: */
{
    static DEFSTRING(unexp_name, "~/" JADE_SOCK_NAME);
    VALUE name;
    if(socket_fd >= 0)
	return(sym_t);
    name = cmd_expand_file_name(VAL(unexp_name), sym_nil);
    if(name && STRINGP(name))
    {
	VALUE tmp = cmd_file_exists_p(name);
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
		    /* lose this on exec() */
		    fcntl(socket_fd, F_SETFD, 1);
		    fcntl(socket_fd, F_SETFL, O_NONBLOCK);

		    register_input_fd(socket_fd, server_accept_connection);

		    socket_name = name;
		    return(sym_t);
		}
	    }
		signal_file_error(sym_nil);
	}
	else
	{
	    static DEFSTRING(no_name, "Can't make socket name");
	    cmd_signal(sym_error, LIST_1(VAL(no_name)));
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
	deregister_input_fd(socket_fd);
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
	if(STRINGP(VCAR(car)) && same_files(VSTR(file), VSTR(VCAR(car))))
	{
	    /* Send the result to our client. */
	    int con_fd = VINT(VCDR(car));
	    u_long result = INTP(rc) ? VINT(rc) : 0;
	    if(write(con_fd, &result, sizeof(result)) != sizeof(result))
		res = signal_file_error(file);
	    else
		res = sym_t;
	    close(con_fd);
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
    mark_static(&socket_name);
    INTERN(server_open_file);
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
