/* unix_client.c -- client program to communicate with unix_server.c
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pwd.h>

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

static int opt_quiet = 0;		/* dont't print results */

/* Return a file descriptor of a connection to the server, or -1 if
   an error occurred. */
static int
connect_to_jade(void)
{
    int sock_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if(sock_fd >= 0)
    {
	struct sockaddr_un addr;
	struct passwd *pwd = getpwuid(getuid());
	if(pwd && pwd->pw_dir)
	{
	    char *end;
	    strcpy(addr.sun_path, pwd->pw_dir);
	    end = addr.sun_path + strlen(addr.sun_path);
	    if(end[-1] != '/')
		*end++ = '/';
	    strcpy(end, JADE_SOCK_NAME);
	    addr.sun_family = AF_UNIX;
	    if(access(addr.sun_path, F_OK) != 0)
	    {
		/* Jade isn't running yet. hang around 'til it is... */
		fprintf(stderr, "Jade not running, waiting...");
		fflush(stderr);
		do {
		    sleep(1);
		} while(access(addr.sun_path, F_OK) != 0);
		fprintf(stderr, "okay\n");
	    }
	    if(connect(sock_fd, (struct sockaddr *)&addr,
		       sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
	    {
		return sock_fd;
	    }
	    else
		perror("jadeclient:connect()");
	}
	else
	    fprintf(stderr, "jadeclient: can't find your home dir\n");
    }
    else
	perror("jadeclient:socket()");
    return -1;
}

static void
disconnect_from_jade(int sock_fd)
{
    /* Overkill really. */
    char req = req_end_of_session;
    write(sock_fd, &req, 1);
    close(sock_fd);
}

static u_long
find_file(int sock_fd, char *arg, u_long linenum)
{
    char buf[PATH_MAX];
    char *filename;
    u_long filename_len;
    char req = req_find_file;
    u_long result;

    /* Make sure the filename is absolute; the server could have
       a different working directory to us.  */
    if(*arg != '/' && *arg != '~')
    {
	char *end;
#ifdef HAVE_GETCWD
	if(!getcwd(buf, PATH_MAX))
#else
	if(!getwd(buf))
#endif
	{
	    perror("getcwd");
	    exit(5);
	}
	end = buf + strlen(buf);
	if(end[-1] != '/')
	    *end++ = '/';
	strcpy(end, arg);
	filename = buf;
    }
    else
	filename = arg;

    /* Protocol is;
		>req_find_file:1, >FILE-NAME-LEN:4,
		>FILE-NAME:?, >LINE:4, <RES:4
       all in the local byte-order. */

    filename_len = strlen(filename);

    if(write(sock_fd, &req, 1) != 1
       || write(sock_fd, &filename_len, sizeof(u_long)) != sizeof(u_long)
       || write(sock_fd, filename, filename_len) != filename_len
       || write(sock_fd, &linenum, sizeof(u_long)) != sizeof(u_long)
       || read(sock_fd, &result, sizeof(u_long)) != sizeof(u_long))
    {
	perror("find_file_req");
	return 10;
    }
    return result;
}

static u_long
eval_lisp_form(int sock_fd, char *form)
{
    /* Protocol is; >req_eval:1, >FORM-LEN:4, >FORM:?, <RES-LEN:4, <RES:?
       in the local byte-order. */
    char req = req_eval;
    u_long len = strlen(form);
    char *result;

    if(write(sock_fd, &req, 1) != 1
       || write(sock_fd, &len, sizeof(u_long)) != sizeof(u_long)
       || write(sock_fd, form, len) != len
       || read(sock_fd, &len, sizeof(u_long)) != sizeof(u_long)
       || (result = malloc(len + 1)) == 0
       || read(sock_fd, result, len) != len)
    {
	perror("eval_req");
	return 10;
    }
    result[len] = 0;
    if(!opt_quiet)
	printf("%s\n => %s\n", form, result);
    return 0;
}

static void
usage(char *prog_name)
{
    fprintf(stderr, "usage: %s OPTIONS...\n
where OPTIONS are any of:\n
	-q		Be quiet
	-f FUNCTION	Call Lisp function FUNCTION on the server
	-e FORM		Evaluate Lisp form FORM on the server
	[+LINE] FILE	Edit file FILE on the server, with the cursor
			 at line number LINE optionally.\n", prog_name);
}
		
int
main(int argc, char *argv[])
{
    char *prog_name = argv[0];
    int sock_fd;
    u_long linenum = 1;
    u_long result = 0;

    argc--; argv++;
    if(argc == 0)
	return 0;

    sock_fd = connect_to_jade();
    if(sock_fd == -1)
	return 10;

    while(result == 0 && argc > 0)
    {
	result = 5;
	if(**argv == '-')
	{
	    switch((*argv)[1])
	    {
		char buf[512];

	    case 'q':
		opt_quiet = 1;
		result = 0;
		break;

	    case 'f':			/* -f FUNCTION */
		if(argc < 2)
		    goto opt_error;
		buf[0] = '(';
		strcpy(buf + 1, argv[1]);
		strcat(buf, ")");
		result = eval_lisp_form(sock_fd, buf);
		argc--; argv++;
		break;

	    case 'e':			/* -e FORM */
		if(argc < 2)
		    goto opt_error;
		result = eval_lisp_form(sock_fd, argv[1]);
		argc--; argv++;
		break;

	    case '?': case 'h':
		usage(prog_name);
		break;

	    default:
	    opt_error:
		fprintf(stderr, "unknown option `%s'; try `%s -h'\n",
			*argv, prog_name);
		result =  5;
	    }
	    argc--; argv++;
	}
	else if(argc >= 1 && **argv == '+')	/* +LINE-NUMBER */
	{
#ifdef HAVE_STRTOL
	    linenum = strtol(argv[0], NULL, 0);
#else
	    linenum = atol(argv[0]);
#endif
	    if(linenum <= 0)
		linenum = 1;
	    argc--; argv++;
	}
	else if(argc > 0)
	{
	    result = find_file(sock_fd, *argv, linenum);
	    linenum = 1;
	    /* a short pause is nice. */
	    if(argc > 0)
		sleep(1);
	    argc--; argv++;
	}
    }

    disconnect_from_jade(sock_fd);
    return result;
}
