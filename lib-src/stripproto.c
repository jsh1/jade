/* stripproto.c -- Program to strip `_PR' lines from C source
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

#include <stdio.h>
#include <string.h>

extern void exit(int);

static void
usage(void)
{
    fputs("usage: stripproto [-o dest-file] [src-files...]\n", stderr);
    exit(1);
}

static void
scanfile(FILE *file)
{
    char buf[512];
    while(fgets(buf, 512, file))
    {
	if((buf[0] == '_') && (buf[1] == 'P') && (buf[2] == 'R'))
	    fputs(buf, stdout);
    }
}

int
main(int ac, char **av)
{
    ac--;
    av++;
    while(ac && (**av == '-'))
    {
	switch((*av)[1])
	{
	case 'o':
	    ac--;
	    av++;
	    if(!ac)
		usage();
	    if(!freopen(*av, "w", stdout))
	    {
		perror("freopen");
		exit(5);
	    }
	    break;
	case '-':
	    ac--;
	    av++;
	    goto endopts;
	default:
	    usage();
	}
	ac--;
	av++;
    }
endopts:
    if(!ac)
	scanfile(stdin);
    else
    {
	while(ac)
	{
	    FILE *file = fopen(*av, "r");
	    if(file)
	    {
		scanfile(file);
		fclose(file);
	    }
	    ac--;
	    av++;
	}
    }
    exit(0);
}
