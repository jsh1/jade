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

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pwd.h>
#include "unix_defs.h"

int
main(int argc, char *argv[])
{
    u_long linenum;
    u_long result = 0;
    argc--; argv++;
    if(argc == 0)
    {
	fprintf(stderr, "usage: jadeclient { [+LINE-NUMBER] FILE-NAME }...\n");
	return(0);
    }
    while(result == 0 && argc > 0)
    {
	result = 5;
	if(argc >= 1 && **argv == '+')
	{
	    linenum = strtol(argv[0], NULL, 0);
	    if(linenum <= 0)
		linenum = 1;
	    argc--; argv++;
	}
	else
	    linenum = 1;
	if(argc > 0)
	{
	    char buf[512];
	    char *filename;
	    int sock_fd;

	    /* Make sure the filename is absolute; the server could have
	       a different working directory to us.  */
	    if(**argv != '/' && **argv != '~')
	    {
		char *end;
		if(!getcwd(buf, 511))
		{
		    perror("getcwd");
		    exit(5);
		}
		end = buf + strlen(buf);
		if(end[-1] != '/')
		    *end++ = '/';
		strcpy(end, *argv);
		filename = buf;
	    }
	    else
		filename = *argv;

	    sock_fd = socket(AF_UNIX, SOCK_STREAM, 0);
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
			u_short filenamelen = strlen(filename);
			if(write(sock_fd, &filenamelen, sizeof(u_short)) != sizeof(u_short))
			    perror("jadeclient:write():filenamelen");
			else if(write(sock_fd, filename, filenamelen) != filenamelen)
			    perror("jadeclient:write():filename");
			else if(write(sock_fd, &linenum, sizeof(u_long)) != sizeof(u_long))
			    perror("jadeclient:write():linenum");
			else if(read(sock_fd, &result, sizeof(u_long)) != sizeof(u_long))
			    perror("jadeclient:read():result");
		    }
		    else
			perror("jadeclient:connect()");
		}
		else
		    fprintf(stderr, "jadeclient: can't find your home dir\n");
	    }
	    else
		perror("jadeclient:socket()");
	}
	argc--; argv++;
	/* a short pause is nice. */
	if(argc > 0)
	    sleep(1);
    }
    return(result);
}
