/* server.c -- client/server file handling
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
#include "server.h"

#ifdef HAVE_UNIX

#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdarg.h>
#include <errno.h>

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

/* List of (FILE-NAME . SOCK-FD) */
static repv client_list;

DEFSYM(server_find_file, "server-find-file");

/* fd of the socket which clients connect to, or zero. */
static int socket_fd = -1;

/* pathname of the socket. */
static repv socket_name;

DEFSTRING(io_error, "server_make_connection:io");
DEFSTRING(val_fmt, "%S");

static void
server_handle_request(int fd)
{
    u_char req;
    if(read(fd, &req, 1) != 1)
	goto disconnect;
    switch(req)
    {
	u_long len, tem;
	repv val;

    case req_find_file:
    case req_find_file_async:
	/* 1. read length field
	   2. read LENGTH bytes for the filename
	   3. read the line number
	   4. if !async add (FILE . SOCK-FD) to list of client files
	   5. call server-find-file with FILE and LINE */
	if(read(fd, &len, sizeof(u_long)) != sizeof(u_long)
	   || (val = rep_make_string(len + 1)) == rep_NULL
	   || read(fd, rep_STR(val), len) != len
	   || read(fd, &tem, sizeof(u_long)) != sizeof(u_long))
	    goto io_error;
	rep_STR(val)[len] = 0;
	if (req != req_find_file_async)
	    client_list = Fcons(Fcons(val, rep_MAKE_INT(fd)), client_list);
	rep_call_lisp2(Fsymbol_value(Qserver_find_file, Qt),
		       val, rep_MAKE_INT(tem));
	if (req != req_find_file_async)
	{
	    /* Block any more input on this fd until we've replied to
	       the original message. */
	    rep_deregister_input_fd(fd);
	}
	break;

    case req_eval:
    case req_eval_async:
	/* 1. read length field
	   2. read LENGTH bytes of FORM
	   3. eval and print FORM
	   4. write length of result-string
	   5. write LENGTH bytes of result string */
	if(read(fd, &len, sizeof(u_long)) != sizeof(u_long)
	   || (val = rep_make_string(len + 1)) == rep_NULL
	   || read(fd, rep_STR(val), len) != len)
	    goto io_error;
	rep_STR(val)[len] = 0;
	val = Fread(Fcons(rep_MAKE_INT(0), val));
	if(val != rep_NULL)
	    val = Feval(val);
	if (req != req_eval_async)
	{
	    if(val != rep_NULL)
		val = Fformat(rep_LIST_3(Qnil, rep_VAL(&val_fmt), val));
	    if(val != rep_NULL && rep_STRINGP(val))
	    {
		len = rep_STRING_LEN(val);
		if(write(fd, &len, sizeof(u_long)) != sizeof(u_long)
		   || write(fd, rep_STR(val), len) != len)
		    goto io_error;
	    }
	    else
	    {
		len = 0;
		if(write(fd, &len, sizeof(u_long)) != sizeof(u_long))
		    goto io_error;
	    }
	}
	break;

    io_error:
	Fsignal(Qerror, rep_LIST_1(rep_VAL(&io_error)));
	return;

    case req_end_of_session:
    disconnect:
	rep_deregister_input_fd(fd);
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
	rep_register_input_fd(confd, server_handle_request);

	/* CONFD will inherit the properties of SOCKET-FD, i.e. non-
	   blocking. Make it block.. */
	rep_unix_set_fd_blocking(confd);
    }
}

DEFUN("server-open-p", Fserver_open_p, Sserver_open_p, (void), rep_Subr0) /*
::doc:server-open-p::
server-open-p

t if the edit-server is open.
::end:: */
{
    if(socket_fd >= 0)
	return(Qt);
    return(Qnil);
}

DEFSTRING(no_name, "Can't make socket name");

DEFUN_INT("server-open", Fserver_open, Sserver_open, (void), rep_Subr0, "") /*
::doc:server-open::
server-open

Creates the socket (or whatever) so that the editor's client program can
send us messages.
::end:: */
{
    char namebuf[256];
    repv name;
    if(socket_fd >= 0)
	return(Qt);
    name = Fsystem_name();
    if(!name || !rep_STRINGP(name))
	return rep_NULL;
#ifdef HAVE_SNPRINTF
    snprintf(namebuf, sizeof(namebuf), "~/" JADE_SOCK_NAME, rep_STR(name));
#else
    sprintf(namebuf, "~/" JADE_SOCK_NAME, rep_STR(name));
#endif
    name = Flocal_file_name(rep_string_dup(namebuf));
    if(name && rep_STRINGP(name))
    {
	if(access(rep_STR(name), F_OK) == 0)
	{
	    /* Socket already exists. See if it's live */
	    struct sockaddr_un addr;
	    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
	    if(sock >= 0)
	    {
		strcpy(addr.sun_path, rep_STR(name));
		addr.sun_family = AF_UNIX;
		if(connect(sock, (struct sockaddr *)&addr,
                   sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
		{
		    close(sock);
		    (*rep_message_fun)(rep_message,
				       "A server is already open.");
		    return(Qnil);
		}
		close(sock);
		/* Socket is probably parentless; delete it */
		(*rep_message_fun)(rep_message, "Deleted stale socket.");
		unlink(rep_STR(name));
	    }
	}
	socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if(socket_fd >= 0)
	{
	    struct sockaddr_un addr;
	    addr.sun_family = AF_UNIX;
	    strcpy(addr.sun_path, rep_STR(name));
	    if(bind(socket_fd, (struct sockaddr *)&addr,
		    sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
	    {
		if(listen(socket_fd, 5) == 0)
		{
		    rep_unix_set_fd_nonblocking(socket_fd);
		    rep_register_input_fd(socket_fd, server_accept_connection);

		    socket_name = name;
		    return(Qt);
		}
	    }
	    rep_signal_file_error(Qnil);
	}
	else
	{
	    Fsignal(Qerror, rep_LIST_1(rep_VAL(&no_name)));
	}
	close(socket_fd);
	socket_fd = -1;
    }
    else
	rep_signal_file_error(Qnil);
    return rep_NULL;
}

DEFUN_INT("server-close", Fserver_close, Sserver_close, (void), rep_Subr0, "") /*
::doc:server-close::
server-close

Stops listening for client messages.
::end:: */
{
    if(socket_fd > 0)
    {
	rep_deregister_input_fd(socket_fd);
	close(socket_fd);
	socket_fd = -1;
	unlink(rep_STR(socket_name));
	socket_name = rep_NULL;
    }
    return(Qt);
}

DEFUN("server-reply", Fserver_reply, Sserver_reply,
      (repv file, repv rc), rep_Subr2) /*
::doc:server-reply::
server-reply [FILE-NAME] [RETURN-CODE]

Replies to the editor client which asked us to edit the file FILE-NAME.
RETURN-CODE is the optional result for the client, by default it is zero
which denotes no errors. Returns nil if the file doesn't have a client.
::end:: */
{
    repv res = Qnil, tmp;

    if(BUFFERP(file))
	file = VTX(file)->tx_CanonicalFileName;
    else if(!rep_STRINGP(file))
	file = curr_vw->vw_Tx->tx_CanonicalFileName;
    else
    {
	rep_GC_root gc_rc;
	rep_PUSHGC(gc_rc, rc);
	file = Fcanonical_file_name(file);
	rep_POPGC;
	if(!file || !rep_STRINGP(file))
	    return rep_NULL;
    }

    tmp = client_list;
    client_list = Qnil;
    while(rep_CONSP(tmp))
    {
	register repv car = rep_CAR(tmp);
	repv next = rep_CDR(tmp);
	if(rep_STRINGP(rep_CAR(car))
	   && strcmp(rep_STR(file), rep_STR(rep_CAR(car))) == 0)
	{
	    /* Send the result to our client. */
	    int con_fd = rep_INT(rep_CDR(car));
	    u_long result = rep_INTP(rc) ? rep_INT(rc) : 0;
	    if(write(con_fd, &result, sizeof(result)) != sizeof(result))
		res = rep_signal_file_error(file);
	    else
		res = Qt;
	    /* We can handle input on this fd again now. */
	    rep_register_input_fd(con_fd, server_handle_request);
	}
	else
	{
	    rep_CDR(tmp) = client_list;
	    client_list = tmp;
	}
	tmp = next;
    }

    return res;
}


/* dl hooks */

repv
rep_dl_init(repv file_name)
{
    client_list = Qnil;
    rep_mark_static(&client_list);
    rep_mark_static(&socket_name);
    rep_INTERN(server_find_file);
    rep_ADD_SUBR (Sserver_open_p);
    rep_ADD_SUBR (Sserver_open);
    rep_ADD_SUBR (Sserver_close);
    rep_ADD_SUBR (Sserver_reply);
    return Qt;
}

void
rep_dl_kill(void)
{
    /* clean up */
    repv tmp = client_list;
    while(rep_CONSP(tmp))
    {
	/* Any client-opened files still around are replied to with
	   a result of 5 (fail).  */
	static u_long failrc = 5;
	int fd = rep_INT(rep_CDR(rep_CAR(tmp)));
	write(fd, &failrc, sizeof(u_long));
	close(fd);
	tmp = rep_CDR(tmp);
    }
    client_list = Qnil;
    Fserver_close();
}

#endif /* HAVE_UNIX */

/*
;;;###autoload (autoload 'server-open-p "server")
;;;###autoload (autoload 'server-open "server" t)
;;;###autoload (autoload 'server-reply "server")
*/
