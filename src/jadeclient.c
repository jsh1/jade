/* jadeclient.c -- client program to communicate with server-dl.c
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
#include "server.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#ifdef HAVE_UNIX

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pwd.h>
#include <netdb.h>

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_UTSNAME_H
# include <sys/utsname.h>
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

static int opt_quiet = 0;		/* dont't print results */
static int opt_nowait = 0;

/* copied from src/unix_main.c */
static char *
system_name(void)
{
    char buf[256];
    struct hostent *h;

    static char *system_name;
    if(system_name)
	return system_name;

#ifdef HAVE_GETHOSTNAME
    if(gethostname(buf, 256))
	return rep_NULL;
#else
    {
	struct utsname uts;
	uname(&uts);
	strncpy(buf, uts.nodename, 256);
    }
#endif
    h = gethostbyname(buf);
    if(h)
    {
	if(!strchr(h->h_name, '.'))
	{
	    /* The official name is not fully qualified. Try looking
	       through the list of alternatives. */
	    char **aliases = h->h_aliases;
	    while(*aliases && !strchr(*aliases, '.'))
		aliases++;
	    system_name = strdup(*aliases ? *aliases : h->h_name);
	}
	else
	    system_name = strdup((char *)h->h_name);
    }
    else
	system_name = strdup(buf);
    return system_name;
}

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
	    sprintf(end, JADE_SOCK_NAME, system_name());
	    addr.sun_family = AF_UNIX;
	again:
	    if(access(addr.sun_path, F_OK) != 0)
	    {
		if (opt_nowait)
		{
		    fprintf(stderr, "server not running\n");
		    exit (1);
		}

		/* Jade isn't running yet. hang around 'til it is... */
		fprintf(stderr, "server not running, waiting...");
		fflush(stderr);
		do {
		    sleep(1);
		} while(access(addr.sun_path, F_OK) != 0);
		fprintf(stderr, "okay\n");
	    }
	    if(connect(sock_fd, (struct sockaddr *)&addr,
		       sizeof(addr.sun_family) + strlen(addr.sun_path) + 1) == 0)
	    {
		return sock_fd;
	    }
	    else
	    {
		/* Assume we've found a stale socket. */
		if (unlink(addr.sun_path) == 0)
		    goto again;
		perror ("unlink socket");
		exit (10);
	    }
	}
	else
	    fprintf(stderr, "can't find your home dir\n");
    }
    else
	perror("socket");
    return -1;
}

static void
disconnect_from_jade(int sock_fd)
{
    /* Overkill really. */
    u_char req = req_end_of_session;
    write(sock_fd, &req, 1);
    close(sock_fd);
}

static u_long
find_file(int sock_fd, char *arg, u_long linenum)
{
    char buf[PATH_MAX];
    char *filename;
    u_long filename_len;
    u_char req = !opt_quiet ? req_find_file : req_find_file_async;
    u_long result = 0;

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
       || (req != req_find_file_async
	   && read(sock_fd, &result, sizeof(u_long)) != sizeof(u_long)))
    {
	perror("find_file");
	return 10;
    }
    return result;
}

static u_long
eval_lisp_form(int sock_fd, char *form)
{
    /* Protocol is; >req_eval:1, >FORM-LEN:4, >FORM:?, <RES-LEN:4, <RES:?
       in the local byte-order. */
    u_char req = !opt_quiet ? req_eval : req_eval_async;
    u_long len = strlen(form);
    char *result;

    if(write(sock_fd, &req, 1) != 1
       || write(sock_fd, &len, sizeof(u_long)) != sizeof(u_long)
       || write(sock_fd, form, len) != len
       || (req != req_eval_async
	   && read(sock_fd, &len, sizeof(u_long)) != sizeof(u_long)))
    {
	perror("eval_req");
	return 10;
    }
    if(req != req_eval_async)
    {
	if(len > 0)
	{
	    result = malloc(len + 1);
	    if(result == 0 || read(sock_fd, result, len) != len)
	    {
		perror("eval_req");
		return 10;
	    }
	    result[len] = 0;
	    if(!opt_quiet)
		puts(result);
	}
	else
	    printf("%s\n---> error\n", form);
    }
    return 0;
}

#endif /* HAVE_UNIX */

static void
usage(char *prog_name)
{
    fprintf(stderr, "usage: %s OPTIONS...\n\n\
where OPTIONS are any of:\n\n\
	-w		Don't wait for server if not already running,\n\
			 return with exit code 1\n\
	[+LINE] FILE	Edit file FILE on the server, with the cursor\n\
			 at line number LINE optionally.\n\
	-q		Be quiet (perform commands asynchronously)\n\
	-f FUNCTION	Call Lisp function FUNCTION on the server\n\
	-e FORM		Evaluate Lisp form FORM on the server\n\
	-x DISPLAY	Connect the server to X11 display DISPLAY\n\
	-X		Connect to $DISPLAY\n\
	-		Read lines of input until EOF, evaluating each\n\
			 one as it is read\n\
	--		Read forms from standard input until EOF, evaluating\n\
			 the whole lot in one go (inside a progn)\n",
	    prog_name);
}
		
int
main(int argc, char *argv[])
{
    char *prog_name = argv[0];
    int sock_fd;
    u_long result = 0;

    argc--; argv++;

    if (argc > 0 && strcmp(argv[0], "-w") == 0)
    {
	opt_nowait = 1;
	argc--; argv++;
    }

    if (argc > 0 && strcmp(argv[0], "-?") == 0)
    {
	usage(prog_name);
	argc--; argv++;
    }

    if(argc == 0)
	return 0;

    sock_fd = connect_to_jade();
    if(sock_fd == -1)
	return 1;

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

	    case 'x':
		if(argc < 2)
		    goto opt_error;
		sprintf(buf, "(make-window-on-display \"%s\")", argv[1]);
		result = eval_lisp_form(sock_fd, buf);
		argc--; argv++;
		break;

	    case 'X':
		{
		    char *dpy = getenv("DISPLAY");
		    if(dpy == 0)
			fprintf(stderr, "No DISPLAY environment variable\n");
		    else
		    {
			sprintf(buf, "(make-window-on-display \"%s\")", dpy);
			result = eval_lisp_form(sock_fd, buf);
		    }
		    break;
		}

	    case 0:
		do {
		    if(isatty(0))
			printf("jade%% "), fflush(stdout);
		    if(fgets(buf, sizeof(buf), stdin) == 0)
			result = 10;
		    else
			result = eval_lisp_form(sock_fd, buf);
		} while(result == 0);
		argc--; argv++;
		break;

	    case '-':
		{
		    int bufsiz = 1024, bufuse = 0;
		    char *input_buf = malloc(bufsiz);
		    if(input_buf == 0)
		    {
			perror("malloc");
			result = 10;
			break;
		    }
		    strcpy(input_buf, "(progn ");
		    bufuse = 7;

		    while(fgets(buf, sizeof(buf), stdin) != 0)
		    {
			int len = strlen(buf);
			if(bufuse + len + 1 >= bufsiz)
			{
			    bufsiz *= 2;
			    input_buf = realloc(buf, bufsiz);
			    if(input_buf == 0)
			    {
				perror("realloc");
				result = 10;
				break;
			    }
			}
			memcpy(input_buf + bufuse, buf, len);
			bufuse += len;
		    }
		    if(input_buf != 0)
		    {
			input_buf[bufuse] = ')';
			input_buf[bufuse+1] = 0;
			result = eval_lisp_form(sock_fd, input_buf);
			free(input_buf);
		    }
		}
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
	else if(argc > 0)
	{
	    u_long linenum = 0;
	    if(argc >= 1 && **argv == '+')	/* +LINE-NUMBER */
	    {
#ifdef HAVE_STRTOL
		linenum = strtol(*argv + 1, NULL, 0) - 1;
#else
		linenum = atol(*argv + 1) - 1;
#endif
		if(linenum <= 0)
		    linenum = 1;
		argc--; argv++;
	    }
	    result = find_file(sock_fd, *argv, linenum);
	    argc--; argv++;
	}
    }

    disconnect_from_jade(sock_fd);
    return result;
}
