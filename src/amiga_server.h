/* amiga_server.h -- protocol between client and server
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

#ifndef _AMIGA_SERVER_H
#define _AMIGA_SERVER_H

#define JADE_PORT_NAME "Jade_rendezvous"

struct clientmsg {
    struct Message cm_msg;
    u_char	  *cm_file;
    /* linenumber on outward journey, result code on return. */
    u_long	   cm_num;
};

#endif /* _AMIGA_SERVER_H */
