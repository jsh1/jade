/*
 * regsub @(#)regsub.c	1.3 of 2 April 86
 *
 * Copyright (c) 1986 by University of Toronto. Written by Henry Spencer.  Not
 * derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any purpose on any
 * computer system, and to redistribute it freely, subject to the following
 * restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 * software, no matter how awful, even if they arise from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either by explicit
 * claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 */

#include "jade.h"
#include <rep_regexp.h>
#include <stdio.h>

#ifndef CHARBITS
#define UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

#include <string.h>

/*
 * - regsub - perform substitutions after a regexp match
 *
 * data is null if the last match was a string, or the TX if the last
 * match was on a buffer.
 */
void
jade_regsub(lasttype, matches, source, dest, data)
    int		    lasttype;
    rep_regsubs	   *matches;
    char	   *source;
    char	   *dest;
    void	   *data;
{
    register u_char  *src;
    register u_char  *dst;
    register u_char   c;
    register int    no;
    register int    len;

    if (matches == NULL || source == NULL || dest == NULL) {
	rep_regerror("NULL parm to regsub");
	return;
    }

    if ((lasttype == rep_reg_string && !rep_STRINGP(rep_VAL(data)))
	|| (lasttype == rep_reg_obj && !BUFFERP(rep_VAL(data))))
    {
	rep_regerror("Bad type of data to regsub");
	return;
    }

    src = source;
    dst = dest;
    while ((c = *src++) != '\0')
    {
	if (c == '&')
	    no = 0;
	else if (c == '\\' && '0' <= *src && *src <= '9')
	    no = *src++ - '0';
	else
	    no = -1;

	if (no < 0) {		/* Ordinary character. */
	    if (c == '\\' && (*src == '\\' || *src == '&'))
		c = *src++;
	    *dst++ = c;
	} else {
	    if(lasttype == rep_reg_string)
	    {
		if (matches->string.startp[no] != NULL
		    && matches->string.endp[no] != NULL)
		{
		    len = matches->string.endp[no]
			  - matches->string.startp[no];
		    (void) strncpy(dst, matches->string.startp[no], len);
		    dst += len;
		    if (len != 0 && *(dst - 1) == '\0')
		    {
			/* strncpy hit NUL. */
			rep_regerror("damaged match string");
			return;
		    }
		}
	    }
	    else if(lasttype == rep_reg_obj)
	    {
		TX *tx = data;
		if(matches->obj.startp[no] != rep_NULL)
		{
		    if(check_section(tx, &matches->obj.startp[no],
				     &matches->obj.endp[no]))
		    {
			long len = section_length(tx, matches->obj.startp[no],
						  matches->obj.endp[no]);
			copy_section(tx, matches->obj.startp[no],
				     matches->obj.endp[no], dst);
			dst += len;
		    }
		}
	    }
	}
    }
    *dst++ = '\0';
}

/*
 * - regsublen - dummy regsub() returning length of contructed string,
 * including terminating '\0'
 */
int
jade_regsublen(lasttype, matches, source, data)
    int		    lasttype;
    rep_regsubs	   *matches;
    char	   *source;
    void	   *data;
{
    register u_char  *src;
    register u_char   c;
    register int    no;
    register int    dstlen = 1;

    if (matches == NULL || source == NULL) {
	rep_regerror("NULL parm to regsublen");
	return(0);
    }
    if ((lasttype == rep_reg_string && !rep_STRINGP(rep_VAL(data)))
	|| (lasttype == rep_reg_obj && !BUFFERP(rep_VAL(data))))
    {
	rep_regerror("Bad type of data to regsublen");
	return (0);
    }

    src = source;
    while ((c = *src++) != '\0') {
	if (c == '&')
	    no = 0;
	else if (c == '\\' && '0' <= *src && *src <= '9')
	    no = *src++ - '0';
	else
	    no = -1;

	if (no < 0) {		/* Ordinary character. */
	    if (c == '\\' && (*src == '\\' || *src == '&'))
		c = *src++;
	    dstlen++;
	} else {
	    if(lasttype == rep_reg_string)
	    {
		if (matches->string.startp[no] != NULL
		    && matches->string.endp[no] != NULL)
		{
		    dstlen += matches->string.endp[no]
			      - matches->string.startp[no];
		}
	    }
	    else if(lasttype == rep_reg_obj)
	    {
		TX *tx = data;
		if(matches->obj.startp[no] != rep_NULL)
		{
		    if(check_section(tx, &matches->obj.startp[no],
				     &matches->obj.endp[no]))
		    {
			dstlen += section_length(tx, matches->obj.startp[no],
						 matches->obj.endp[no]);
		    }
		}
	    }
	}
    }
    return(dstlen);
}
