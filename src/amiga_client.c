/* amiga_client.c -- client program to communicate with amiga_server.c
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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <clib/exec_protos.h>
#include <exec/ports.h>
#include <clib/dos_protos.h>

#include "amiga_server.h"

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
	    /* this should be in public mem really */
	    char buf[512];
	    if(getcwd(buf, 511))
	    {
		if(AddPart(buf, *argv, 511))
		{
		    struct MsgPort *mp;
		    mp = CreateMsgPort();
		    if(mp)
		    {
			char domsg = 0;
			/* this should be in public mem really */
			struct clientmsg cm;
			struct MsgPort *servport;
			cm.cm_msg.mn_Length = sizeof(cm);
			cm.cm_msg.mn_ReplyPort = mp;
			cm.cm_file = buf;
			cm.cm_num = linenum;
			Forbid();
			servport = FindPort(JADE_PORT_NAME);
			if(!servport)
			{
			    /* Have to be careful with the forbid/permit, must
			       ensure that no other tasks are running when we
			       locate the port.  */
			    Permit();
			    fprintf(stderr, "Jade not running, waiting...");
			    fflush(stderr);
			    Forbid();
			    do {
				Permit();
				sleep(1);
				Forbid();
			    } while(!(servport = FindPort(JADE_PORT_NAME)));
			    domsg = 1;
			}
			PutMsg(servport, &cm.cm_msg);
			Permit();
			if(domsg)
			    fprintf(stderr, "okay\n");
			WaitPort(mp);
			GetMsg(mp);	    /* is this needed? */
			result = cm.cm_num;
			DeleteMsgPort(mp);
		    }
		    else
			fprintf(stderr, "jadeclient: can't CreateMsgPort()\n");
		}
		else
		    fprintf(stderr, "jadeclient: internal buffer overflow\n");
	    }
	    else
		fprintf(stderr, "jadeclient: internal buffer overflow\n");
	}
	argc--; argv++;
	/* a short pause is nice. */
	if(argc > 0)
	    sleep(1);
    }
    return(result);
}
