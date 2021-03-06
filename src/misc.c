/* misc.c -- Miscellaneous functions
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
#include "build.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

DEFSTRING(jade_vers_string, JADE_VERSION);
DEFSTRING(jade_build_id_string,
	  BUILD_DATE " by " BUILD_USER "@" BUILD_HOST ", for " HOST_TYPE ".");

DEFSYM(window_system, "window-system");
DEFSYM(jade_build_id, "jade-build-id");
DEFSYM(jade_major_version, "jade-major-version");
DEFSYM(jade_minor_version, "jade-minor-version");
DEFSYM(jade_version, "jade-version"); /*
::doc:window-system::
A symbol defining the window system that Jade is running under. The only
current possibilities are `x11' or `gtk'.
::end::
::doc:jade-build-id::
A string describing when, where, and by who the running version of the
editor was built.
::end::
::doc:jade-major-version::
The major version number of the editor.
::end::
::doc:jade-minor-version::
The minor version number of the editor.
::end::
::doc:jade-version::
A string naming the editor version.
::end:: */

#if defined (HAVE_GTK)
DEFSYM(gtk, "gtk");
#elif defined (HAVE_MAC)
DEFSYM(mac, "mac");
#elif defined (HAVE_X11)
DEFSYM(x11, "x11");
#endif

#ifndef HAVE_STPCPY
#ifndef __APPLE__
/*
 * copy src to dst, returning pointer to terminating '\0' of dst.
 * Although this has a prototype in my <string.h> it doesn't seem to be
 * in the actual library??
 */
char *
stpcpy(char *dst, const char *src)
{
    while((*dst++ = *src++) != 0)
	;
    return(dst - 1);
}
#endif /* !__APPLE__ */
#endif /* !HAVE_STPCPY */

#ifndef HAVE_MEMCHR
void *
memchr(const void *mem, int c, size_t len)
{
    register char *tmp = (char *)mem;
    while(len-- > 0)
    {
	if(*tmp++ != c)
	    continue;
	return((void *)(tmp - 1));
    }
    return(NULL);
}
#endif /* !HAVE_MEMCHR */

void
misc_init(void)
{
    rep_INTERN_SPECIAL(window_system);
#if defined (HAVE_GTK)
    rep_INTERN(gtk);
    Fset (Qwindow_system, Qgtk);
#elif defined (HAVE_MAC)
    rep_INTERN(mac);
    Fset (Qwindow_system, Qmac);
#elif defined (HAVE_X11)
    rep_INTERN(x11);
    Fset (Qwindow_system, Qx11);
#endif

    rep_INTERN_SPECIAL(jade_build_id);
    Fset (Qjade_build_id, rep_VAL(&jade_build_id_string));
    rep_INTERN_SPECIAL(jade_major_version);
    Fset (Qjade_major_version, rep_MAKE_INT(JADE_MAJOR));
    rep_INTERN_SPECIAL(jade_minor_version);
    Fset (Qjade_major_version, rep_MAKE_INT(JADE_MINOR));
    rep_INTERN_SPECIAL(jade_version);
    Fset (Qjade_version, rep_VAL(&jade_vers_string));
}
