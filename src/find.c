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
   the set of characters CHARS. Returns non-zero if such a character was
   found, zero if not (POS undefined in this case). */
int
buffer_strpbrk(TX *tx, Pos *pos, const char *chars)
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
	    return 1;
	}
	else if(chars_has_newline)
	{
	    PCOL(pos) = line->ln_Strlen - 1;
	    return 1;
	}
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }
    return 0;
}

/* Same as buffer_strpbrk() but searches backwards. */
int
buffer_reverse_strpbrk(TX *tx, Pos *pos, const char *chars)
{
    LINE *line = tx->lines + PROW(pos);
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = line->ln_Strlen - 1;
	if(chars_has_newline)
	    return 1;
    }
    while(1)
    {
	char *match = line->ln_Line + PCOL(pos);
	while(match >= line->ln_Line)
	{
	    if(strchr(chars, *match) != NULL)
	    {
		PCOL(pos) = match - line->ln_Line;
		return 1;
	    }
	    match--;
	}
	line--;
	if(--PROW(pos) < tx->logical_start)
	    return 0;
	PCOL(pos) = line->ln_Strlen - 1;
	if(chars_has_newline)
	    return 1;
    }
}

/* Set POS to the first character C in TX from POS. Returns non-zero for
   success. */
int
buffer_strchr(TX *tx, Pos *pos, char c)
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
	    return 1;
	}
	else
	{
	    /* no newline at end of buffer. */
	    return 0;
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
		return 1;
	    }
	    PROW(pos)++;
	    PCOL(pos) = 0;
	    line++;
	}
	return 0;
    }
}

/* Same as buffer_strchr() but searches backwards. */
int
buffer_reverse_strchr(TX *tx, Pos *pos, char c)
{
    LINE *line = tx->lines + PROW(pos);

    if(PCOL(pos) >= line->ln_Strlen)
	PCOL(pos) = line->ln_Strlen - 1;

    if(c == '\n')
    {
	if(PCOL(pos) == line->ln_Strlen - 1)
	    return 1;
	if(PROW(pos) == tx->logical_start)
	    return 0;
	else
	{
	    line--;
	    PROW(pos)--;
	    PCOL(pos) = line->ln_Strlen - 1;
	    return 1;
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
		    return 1;
		}
		match--;
	    }
	    if(PROW(pos) == tx->logical_start)
		return 0;
	    line--;
	    PROW(pos)--;
	    PCOL(pos) = line->ln_Strlen - 1;
	}
    }
}

/* Compares the string STR to the contents of TX at POS using the function
   CMPFN to do the comparison (must be a pointer to either strncmp() or
   strncasecmp(), or something with the same interface).
   Returns 0 for false, 1 for true. Leaves POS at the end of the match
   if it returns true. */
/* The argument CMPFN is declared void * to hopefully avoid irksome
   compiler warnings. */
#define CMPFN ((int (*)(const char *, const char *, size_t))cmpfn)
int
buffer_compare_n(TX *tx, Pos *pos, const char *str, int n, void *cmpfn)
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
		return 0;
	    PCOL(pos) += len;
	    n -= len;
	    if(len == 0 || chunk == NULL)
		return 1;	/* looked at all n */
	}
	if(PCOL(pos) != line->ln_Strlen - 1)
	    return 0;
	n--;
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
	str = chunk + 1;
    }
    return n == 0 ? 1 : 0;
}

/* Move POS forward COUNT characters in TX. Returns non-zero if the end
   of the buffer wasn't reached. */
int
forward_char(long count, TX *tx, Pos *pos)
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
		return 0;
	    line++;
	    PCOL(pos) = 0;
	}
    }
    return 1;
}

/* Move POS backward COUNT characters in TX. Returns non-zero if the start
   of the buffer wasn't reached. */
int
backward_char(long count, TX *tx, Pos *pos)
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
		return 0;
	    line--;
	    PCOL(pos) = line->ln_Strlen - 1;
	}
    }
    return 1;
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
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	repv ret = Qnil;
	rep_regexp *prog;
	prog = rep_compile_regexp(re);
	if(prog != NULL)
	{
	    if(regexec_tx(prog, VTX(tx), pos,
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
    return rep_NULL;
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
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	repv ret = Qnil;
	rep_regexp *prog = rep_compile_regexp(re);
	if(prog != NULL)
	{
	    if(regexec_reverse_tx(prog, VTX(tx), pos,
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
    return rep_NULL;
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
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	char first[3];
	long len = rep_STRING_LEN(str);
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
	while(buffer_strpbrk(VTX(tx), &start, first))
	{
	    Pos end = start;
	    if(buffer_compare_n(VTX(tx), &end, rep_STR(str), len,
				rep_NILP(nocasep) ? strncmp : strncasecmp))
	    {
		repv vstart = make_pos(PCOL(&start), PROW(&start));
		rep_set_string_match(tx, vstart,
				     make_pos(PCOL(&end), PROW(&end)));
		return vstart;
	    }
	    if(!forward_char(1, VTX(tx), &start))
		break;
	}
	return(Qnil);
    }
    return rep_NULL;
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
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	char first[3];
	long len = rep_STRING_LEN(str);
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
	while(buffer_reverse_strpbrk(VTX(tx), &start, first))
	{
	    Pos end = start;
	    if(buffer_compare_n(VTX(tx), &end, rep_STR(str), len,
				rep_NILP(nocasep) ? strncmp : strncasecmp))
	    {
		repv vstart = make_pos(PCOL(&start), PROW(&start));
		rep_set_string_match(tx, vstart,
				     make_pos(PCOL(&end), PROW(&end)));
		return vstart;
	    }
	    if(!backward_char(1, VTX(tx), &start))
		break;
	}
	return(Qnil);
    }
    return rep_NULL;
}

DEFUN("char-search-forward", Fchar_search_forward, Schar_search_forward, (repv ch, repv pos, repv tx), rep_Subr3) /*
::doc:char-search-forward::
char-search-forward CHAR [POS] [BUFFER]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    rep_DECLARE1(ch, rep_INTP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	Pos tem;
	COPY_VPOS(&tem, pos);
	if(buffer_strchr(VTX(tx), &tem, rep_INT(ch)))
	    return make_pos(PCOL(&tem), PROW(&tem));
	return(Qnil);
    }
    return rep_NULL;
}

DEFUN("char-search-backward", Fchar_search_backward, Schar_search_backward, (repv ch, repv pos, repv tx), rep_Subr3) /*
::doc:char-search-backward::
char-search-backward CHAR [POS] [BUFFER]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    rep_DECLARE1(ch, rep_INTP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	Pos tem;
	COPY_VPOS(&tem, pos);
	if(buffer_reverse_strchr(VTX(tx), &tem, rep_INT(ch)))
	    return make_pos(PCOL(&tem), PROW(&tem));
	return(Qnil);
    }
    return rep_NULL;
}

DEFUN("looking-at", Flooking_at, Slooking_at, (repv re, repv pos, repv tx, repv nocase_p), rep_Subr4) /*
::doc:looking-at::
looking-at REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Returns t if REGEXP matches the text at POS. Updates the match data.
::end:: */
{
    rep_DECLARE1(re, rep_STRINGP);
    if(!BUFFERP(tx))
	tx = rep_VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(check_line(VTX(tx), pos))
    {
	rep_regexp *prog = rep_compile_regexp(re);
	if(prog != NULL)
	{
	    repv res;
	    if(regmatch_tx(prog, VTX(tx), pos,
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
    return rep_NULL;
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
    long length;
    TX *tx = curr_vw->vw_Tx;
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
	pos = get_tx_cursor(tx);
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
	return rep_NULL;
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
