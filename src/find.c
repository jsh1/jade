/* find.c -- Searching and replacing
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

#include "jade.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>


/* Basic buffer utility functions. These are also used by the regjade.c
   regexp code.

   All of these break the rule about not modifying the contents of
   position objects. They all do! This is to reduce allocation when
   executing regular expressions. */

/* Set POS to the position of the first char in TX from POS that is in
   the set of characters CHARS. Returns true if such a character was
   found, false if not (POS undefined in this case). */
bool
buffer_strpbrk(Lisp_Buffer *tx, Pos *pos, const char *chars)
{
    LINE *line = tx->lines + PROW(pos);
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }

    while(PROW(pos) < tx->logical_end)
    {
	char *ptr = strpbrk(line->ln_Line + PCOL(pos), chars);
	if(ptr != NULL)
	{
	    PCOL(pos) = ptr - line->ln_Line;
	    return true;
	}
	else if(chars_has_newline)
	{
	    PCOL(pos) = line->ln_Strlen - 1;
	    return true;
	}
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }
    return false;
}

/* Same as buffer_strpbrk() but searches backwards. */
bool
buffer_reverse_strpbrk(Lisp_Buffer *tx, Pos *pos, const char *chars)
{
    LINE *line = tx->lines + PROW(pos);
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = line->ln_Strlen - 1;
	if(chars_has_newline)
	    return true;
    }
    while(1)
    {
	char *match = line->ln_Line + PCOL(pos);
	while(match >= line->ln_Line)
	{
	    if(strchr(chars, *match) != NULL)
	    {
		PCOL(pos) = match - line->ln_Line;
		return true;
	    }
	    match--;
	}
	line--;
	if(--PROW(pos) < tx->logical_start)
	    return false;
	PCOL(pos) = line->ln_Strlen - 1;
	if(chars_has_newline)
	    return true;
    }
}

/* Set POS to the first character C in TX from POS. */
bool
buffer_strchr(Lisp_Buffer *tx, Pos *pos, char c)
{
    LINE *line = tx->lines + PROW(pos);
    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }

    if(c == '\n')
    {
	if(PROW(pos) < tx->logical_end - 1)
	{
	    PCOL(pos) = tx->lines[PROW(pos)].ln_Strlen - 1;
	    return true;
	}
	else
	{
	    /* no newline at end of buffer. */
	    return false;
	}
    }
    else
    {
	while(PROW(pos) < tx->logical_end)
	{
	    char *match = strchr(line->ln_Line + PCOL(pos), c);
	    if(match)
	    {
		PCOL(pos) = match - line->ln_Line;
		return true;
	    }
	    PROW(pos)++;
	    PCOL(pos) = 0;
	    line++;
	}
	return false;
    }
}

/* Same as buffer_strchr() but searches backwards. */
bool
buffer_reverse_strchr(Lisp_Buffer *tx, Pos *pos, char c)
{
    LINE *line = tx->lines + PROW(pos);

    if(PCOL(pos) >= line->ln_Strlen)
	PCOL(pos) = line->ln_Strlen - 1;

    if(c == '\n')
    {
	if(PCOL(pos) == line->ln_Strlen - 1)
	    return true;
	if(PROW(pos) == tx->logical_start)
	    return false;
	else
	{
	    line--;
	    PROW(pos)--;
	    PCOL(pos) = line->ln_Strlen - 1;
	    return true;
	}
    }
    else
    {
	while(1)
	{
	    char *match = line->ln_Line + PCOL(pos);
	    while(match >= line->ln_Line)
	    {
		if(*match == c)
		{
		    PCOL(pos) = match - line->ln_Line;
		    return true;
		}
		match--;
	    }
	    if(PROW(pos) == tx->logical_start)
		return false;
	    line--;
	    PROW(pos)--;
	    PCOL(pos) = line->ln_Strlen - 1;
	}
    }
}

/* Compares the string STR to the contents of TX at POS using the function
   CMPFN to do the comparison (must be a pointer to either strncmp() or
   strncasecmp(), or something with the same interface).
   Leaves POS at the end of the match if it returns true. */
/* The argument CMPFN is declared void * to hopefully avoid irksome
   compiler warnings. */
#define CMPFN ((int (*)(const char *, const char *, size_t))cmpfn)
bool
buffer_compare_n(Lisp_Buffer *tx, Pos *pos,
		 const char *str, int n, void *cmpfn)
{
    LINE *line = tx->lines + PROW(pos);

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }

    while(n > 0 && PROW(pos) < tx->logical_end)
    {
	char *chunk = strchr(str, '\n');
	int len;
	if(chunk == NULL)
	    len = strlen(str);
	else
	    len = chunk - str;
	if(len > n)
	    len = n;
	if(len > 0)
	{
	    if(CMPFN(line->ln_Line + PCOL(pos), str, len) != 0)
		return false;
	    PCOL(pos) += len;
	    n -= len;
	    if(len == 0 || chunk == NULL)
		return true;	/* looked at all n */
	}
	if(PCOL(pos) != line->ln_Strlen - 1)
	    return false;
	n--;
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
	str = chunk + 1;
    }
    return n == 0;
}

/* Move POS forward COUNT characters in TX. Returns true if the end
   of the buffer wasn't reached. */
bool
forward_char(intptr_t count, Lisp_Buffer *tx, Pos *pos)
{
    LINE *line = tx->lines + PROW(pos);
    if(PCOL(pos) >= line->ln_Strlen)
	PCOL(pos) = line->ln_Strlen - 1;
    while(count > 0)
    {
	if(count < (line->ln_Strlen - PCOL(pos)))
	{
	    PCOL(pos) += count;
	    count = 0;
	}
	else
	{
	    count -= line->ln_Strlen - PCOL(pos);
	    PROW(pos)++;
	    if(PROW(pos) >= tx->logical_end)
		return false;
	    line++;
	    PCOL(pos) = 0;
	}
    }
    return true;
}

/* Move POS backward COUNT characters in TX. Returns true if the start
   of the buffer wasn't reached. */
bool
backward_char(intptr_t count, Lisp_Buffer *tx, Pos *pos)
{
    LINE *line = tx->lines + PROW(pos);
    while(count > 0)
    {
	if(count <= PCOL(pos))
	{
	    PCOL(pos) -= count;
	    count = 0;
	}
	else
	{
	    count -= PCOL(pos) + 1; /* `+ 1' for the assumed '\n' */
	    PROW(pos)--;
	    if(PROW(pos) < tx->logical_start)
		return false;
	    line--;
	    PCOL(pos) = line->ln_Strlen - 1;
	}
    }
    return true;
}


/* Matching and searching */

DEFUN("re-search-forward", Fre_search_forward, Sre_search_forward, (repv re, repv pos, repv tx, repv nocase_p), rep_Subr4) /*
::doc:re-search-forward::
re-search-forward REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with REGEXP. Returns the position of the next match or nil. Updates the
match data.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    rep_DECLARE1(re, rep_STRINGP);
    if(!POSP(pos))
	pos = curr_vw->cursor_pos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(check_line(VBUFFER(tx), pos))
    {
	repv ret = Qnil;
	rep_regexp *prog;
	prog = rep_compile_regexp(re);
	if(prog != NULL)
	{
	    if(regexec_buffer(prog, VBUFFER(tx), pos,
			  rep_NILP(nocase_p) ? 0 : rep_REG_NOCASE))
	    {
		rep_update_last_match(tx, prog);
		ret = prog->matches.obj.startp[0];
	    }
	    else
		ret = Qnil;
	}
	return(ret);
    }
    return 0;
}

DEFUN("re-search-backward", Fre_search_backward, Sre_search_backward, (repv re, repv pos, repv tx, repv nocase_p), rep_Subr4) /*
::doc:re-search-backward::
re-search-backward REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with REGEXP. Returns the position of the next match or nil. Updates the
match data.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    rep_DECLARE1(re, rep_STRINGP);
    if(!POSP(pos))
	pos = curr_vw->cursor_pos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(check_line(VBUFFER(tx), pos))
    {
	repv ret = Qnil;
	rep_regexp *prog = rep_compile_regexp(re);
	if(prog != NULL)
	{
	    if(regexec_reverse_buffer(prog, VBUFFER(tx), pos,
				  rep_NILP(nocase_p) ? 0 : rep_REG_NOCASE))
	    {
		rep_update_last_match(tx, prog);
		ret = prog->matches.obj.startp[0];
	    }
	    else
		ret = Qnil;
	}
	return(ret);
    }
    return 0;
}

DEFUN("search-forward", Fsearch_forward, Ssearch_forward, (repv str, repv pos, repv tx, repv nocasep), rep_Subr4) /*
::doc:search-forward::
search-forward STRING [POS] [BUFFER] [IGNORE-CASE-P]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with STRING. Returns the position of the next match or nil. Updates the
match data.
::end:: */
{
    rep_DECLARE1(str, rep_STRINGP);
    if(!POSP(pos))
	pos = curr_vw->cursor_pos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(check_line(VBUFFER(tx), pos))
    {
	char first[3];
	intptr_t len = rep_STRING_LEN(str);
	Pos start;
	COPY_VPOS(&start, pos);
	if(!rep_NILP(nocasep))
	{
	    first[0] = tolower(rep_STR(str)[0]);
	    first[1] = toupper(rep_STR(str)[0]);
	    first[2] = 0;
	}
	else
	{
	    first[0] = rep_STR(str)[0];
	    first[1] = 0;
	}
	while(buffer_strpbrk(VBUFFER(tx), &start, first))
	{
	    Pos end = start;
	    if(buffer_compare_n(VBUFFER(tx), &end, rep_STR(str), len,
				rep_NILP(nocasep) ? strncmp : strncasecmp))
	    {
		repv vstart = make_pos(PCOL(&start), PROW(&start));
		rep_set_string_match(tx, vstart,
				     make_pos(PCOL(&end), PROW(&end)));
		return vstart;
	    }
	    if(!forward_char(1, VBUFFER(tx), &start))
		break;
	}
	return(Qnil);
    }
    return 0;
}

DEFUN("search-backward", Fsearch_backward, Ssearch_backward, (repv str, repv pos, repv tx, repv nocasep), rep_Subr4) /*
::doc:search-backward::
search-backward STRING [POS] [BUFFER] [IGNORE-CASE-P]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with STRING. Returns the position of the next match or nil. Updates the
match data.
::end:: */
{
    rep_DECLARE1(str, rep_STRINGP);
    if(!POSP(pos))
	pos = curr_vw->cursor_pos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(check_line(VBUFFER(tx), pos))
    {
	char first[3];
	intptr_t len = rep_STRING_LEN(str);
	Pos start;
	COPY_VPOS(&start, pos);
	if(!rep_NILP(nocasep))
	{
	    first[0] = tolower(rep_STR(str)[0]);
	    first[1] = toupper(rep_STR(str)[0]);
	    first[2] = 0;
	}
	else
	{
	    first[0] = rep_STR(str)[0];
	    first[1] = 0;
	}
	while(buffer_reverse_strpbrk(VBUFFER(tx), &start, first))
	{
	    Pos end = start;
	    if(buffer_compare_n(VBUFFER(tx), &end, rep_STR(str), len,
				rep_NILP(nocasep) ? strncmp : strncasecmp))
	    {
		repv vstart = make_pos(PCOL(&start), PROW(&start));
		rep_set_string_match(tx, vstart,
				     make_pos(PCOL(&end), PROW(&end)));
		return vstart;
	    }
	    if(!backward_char(1, VBUFFER(tx), &start))
		break;
	}
	return(Qnil);
    }
    return 0;
}

DEFUN("char-search-forward", Fchar_search_forward, Schar_search_forward, (repv ch, repv pos, repv tx), rep_Subr3) /*
::doc:char-search-forward::
char-search-forward CHAR [POS] [BUFFER]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    rep_DECLARE1(ch, rep_CHAR_8BIT_P);
    if(!POSP(pos))
	pos = curr_vw->cursor_pos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(check_line(VBUFFER(tx), pos))
    {
	Pos tem;
	COPY_VPOS(&tem, pos);
	if(buffer_strchr(VBUFFER(tx), &tem, rep_CHAR_VALUE(ch)))
	    return make_pos(PCOL(&tem), PROW(&tem));
	return(Qnil);
    }
    return 0;
}

DEFUN("char-search-backward", Fchar_search_backward, Schar_search_backward, (repv ch, repv pos, repv tx), rep_Subr3) /*
::doc:char-search-backward::
char-search-backward CHAR [POS] [BUFFER]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    rep_DECLARE1(ch, rep_CHAR_8BIT_P);
    if(!POSP(pos))
	pos = curr_vw->cursor_pos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(check_line(VBUFFER(tx), pos))
    {
	Pos tem;
	COPY_VPOS(&tem, pos);
	if(buffer_reverse_strchr(VBUFFER(tx), &tem, rep_CHAR_VALUE(ch)))
	    return make_pos(PCOL(&tem), PROW(&tem));
	return(Qnil);
    }
    return 0;
}

DEFUN("looking-at", Flooking_at, Slooking_at, (repv re, repv pos, repv tx, repv nocase_p), rep_Subr4) /*
::doc:looking-at::
looking-at REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Returns t if REGEXP matches the text at POS. Updates the match data.
::end:: */
{
    rep_DECLARE1(re, rep_STRINGP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->tx);
    if(!POSP(pos))
	pos = get_buffer_cursor(VBUFFER(tx));
    if(check_line(VBUFFER(tx), pos))
    {
	rep_regexp *prog = rep_compile_regexp(re);
	if(prog != NULL)
	{
	    repv res;
	    if(regmatch_buffer(prog, VBUFFER(tx), pos,
			   rep_NILP(nocase_p) ? 0 : rep_REG_NOCASE))
	    {
		rep_update_last_match(tx, prog);
		res = prog->matches.obj.startp[0];
	    }
	    else
		res = Qnil;
	    return res;
	}
    }
    return 0;
}

DEFUN("buffer-compare-string", Fbuffer_compare_string, Sbuffer_compare_string,
      (repv string, repv pos, repv casep, repv len), rep_Subr4) /*
::doc:buffer-compar_string::
buffer-compare-string STRING [POSITION] [IGNORE-CASE] [LENGTH] 

Compare the characters in STRING with characters in the current buffer.
If LENGTH is defined, at most LENGTH characters are matched. When defined,
POSITION gives the first character in the buffer to match with. Unless
IGNORE-CASE is t, the match is case-significant.

If STRING matches the buffer, then the position of the character following
the last compared character is returned, otherwise nil is returned. Updates
the match data.
::end:: */
{
    intptr_t length;
    Lisp_Buffer *tx = curr_vw->tx;
    rep_DECLARE1(string, rep_STRINGP);
    length = rep_STRING_LEN(string);
    if(rep_INTP(len))
    {
	if(rep_INT(len) <= length)
	    length = rep_INT(len);
	else
	    return rep_signal_arg_error(len, 2);
    }
    if(!POSP(pos))
	pos = get_buffer_cursor(tx);
    if(check_line(tx, pos))
    {
	Pos ppos;
	COPY_VPOS(&ppos, pos);
	if(buffer_compare_n(tx, &ppos, rep_STR(string), length,
			    rep_NILP(casep) ? strncmp : strncasecmp))
	{
	    repv end = COPY_POS(&ppos);
	    rep_set_string_match(rep_VAL(tx), pos, end);
	    return end;
	}
	else
	    return Qnil;
    }
    else
	return 0;
}

void
find_init(void)
{
    rep_ADD_SUBR(Sre_search_forward);
    rep_ADD_SUBR(Sre_search_backward);
    rep_ADD_SUBR(Ssearch_forward);
    rep_ADD_SUBR(Ssearch_backward);
    rep_ADD_SUBR(Schar_search_forward);
    rep_ADD_SUBR(Schar_search_backward);
    rep_ADD_SUBR(Slooking_at);
    rep_ADD_SUBR(Sbuffer_compare_string);
}
