/* server.h -- Definitions for client/server
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

#ifndef JADE_SERVER_H
#define JADE_SERVER_H

/* Name of the unix domain socket. It's stored in the user's home directory,
   but this is the basename. %s is fully-qualified host name */
#define JADE_SOCK_NAME ".jade-%s"

/* Types of request packet. A byte with one of these values is sent to
   initiate a command. */
enum server_request {
    req_find_file = 0,
    req_find_file_async,
    req_eval,
    req_eval_async,
    req_end_of_session = 255
};

#endif /* JADE_SERVER_H */
