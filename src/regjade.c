/* regjade.c -- implementation/port of regexec that works in a buffer.
   $Id$ */

/*
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
 * 2. The origin of this software must not be misrepresented, either by
 * explicit claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator precedence
 * is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 */

#define rep_NEED_REGEXP_INTERNALS
#include "jade.h"
#include <rep_regexp.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/*
 * Utility definitions.
 */
#ifndef CHARBITS
#define UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

/* Global work variables for regexec(). */
static TX      *regtx;		/* buffer */
static Pos      reginput;	/* String-input pointer. */
static char	regnocase;	/* Ignore case when string-matching. */
static repv   *regstartp;	/* Pointer to startp array. */
static repv   *regendp;	/* Ditto for endp. */
static int	regnest;

/* Forwards. */
static int	regtry(TX *tx, rep_regexp *, Pos *);
static int	regmatch(char *);
static int	regrepeat(char *);
static char    *regnext(char *);

#ifdef DEBUG
extern int regnarrate;
extern char *regprop (char *);
#endif

/* Expands to the current input character at position P. This should
   not be called when P is past the end of the buffer. */
#define INPUT_CHAR(p)						\
    ((PCOL(p) >= regtx->tx_Lines[PROW(p)].ln_Strlen - 1)	\
     ? '\n'							\
     : regtx->tx_Lines[PROW(p)].ln_Line[PCOL(p)])

#define TOUPPER_INPUT_CHAR(p)					\
    ((PCOL(p) >= regtx->tx_Lines[PROW(p)].ln_Strlen - 1)	\
     ? '\n'							\
     : toupper(regtx->tx_Lines[PROW(p)].ln_Line[PCOL(p)]))

/* Non-zero when position P is past the last character in the buffer. */
#define END_OF_INPUT(p)						\
    (PROW(p) >= regtx->tx_LogicalEnd				\
     || (PROW(p) == regtx->tx_LogicalEnd - 1			\
	 && PCOL(p) >= regtx->tx_Lines[PROW(p)].ln_Strlen - 1))

#define START_OF_INPUT(p)					\
    (PROW(p) < regtx->tx_LogicalStart				\
     || (PROW(p) == regtx->tx_LogicalStart && PCOL(p) == 0))

/*
 * - regexec_tx - search forwards for a regexp in a buffer sub-string
 *    START is preserved whatever.
 */
int
regexec_tx(prog, tx, start, eflags)
    register rep_regexp *prog;
    TX *tx;
    repv start;
    int eflags;
{
    Pos s;

    /* For REG_NOCASE and strpbrk()  */
    static char mat[3] = "xX";

    /* Be paranoid... */
    if (prog == NULL || tx == NULL || start == rep_NULL || !POSP(start)) {
	rep_regerror("NULL parameter");
	return (0);
    }
    /* Check validity of program. */
    if (UCHARAT(prog->program) != MAGIC) {
	rep_regerror("corrupted program");
	return (0);
    }

    /* jsh -- Check for REG_NOCASE, means ignore case in string matches.  */
    regnocase = ((eflags & rep_REG_NOCASE) != 0);

    /* If there is a "must appear" string, look for it. */
    if (prog->regmust != NULL)
    {
	int found = 0;
	COPY_VPOS(&s, start);
	if(regnocase)
	{
	    mat[0] = tolower(prog->regmust[0]);
	    mat[1] = toupper(prog->regmust[0]);
	    while (buffer_strpbrk(tx, &s, mat))
	    {
		if(buffer_compare_n(tx, &s, prog->regmust,
				    prog->regmlen, strncasecmp))
		{
		    found = 1;
		    break;	    /* Found it. */
		}
		if(!forward_char(1, tx, &s)) break;
	    }
	}
	else
	{
	    while (buffer_strchr(tx, &s, prog->regmust[0]))
	    {
		if(buffer_compare_n(tx, &s, prog->regmust,
				    prog->regmlen, strncmp))
		{
		    found = 1;
		    break;	    /* Found it. */
		}
		if(!forward_char(1, tx, &s)) break;
	    }
	}
	if (!found)		/* Not present. */
	    return (0);
    }

    /* Simplest case:  anchored match need be tried only once.
       For buffers, this means that it only needs to be tried
       at the start of each line after position START */
    if (prog->reganch)
    {
	COPY_VPOS(&s, start);
	if(PCOL(&s) > 0)
	{
	    PCOL(&s) = 0;
	    PROW(&s)++;
	}
	while(PROW(&s) < tx->tx_LogicalEnd)
	{
	    if(regtry(tx, prog, &s))
		return (1);
	    PROW(&s)++;
	}
	return (0);
    }

    /* Messy cases:  unanchored match. */
    COPY_VPOS(&s, start);
    if (prog->regstart != '\0')
    {
	/* We know what char it must start with. */
	if(regnocase)
	{
	    mat[0] = tolower(prog->regstart);
	    mat[1] = toupper(prog->regstart);
	    while(buffer_strpbrk(tx, &s, mat))
	    {
		if(regtry(tx, prog, &s))
		    return (1);
		if(!forward_char(1, tx, &s)) break;
	    }
	}
	else
	{
	    while(buffer_strchr(tx, &s, prog->regstart))
	    {
		if(regtry(tx, prog, &s))
		    return (1);
		if(!forward_char(1, tx, &s)) break;
	    }
	}
    }
    else
	/* We don't -- general case. */
	do {
	    if (regtry(tx, prog, &s))
		return (1);
	} while (forward_char(1, tx, &s));

    /* Failure. */
    return (0);
}

/* regexec_reverse_tx - search backwards for a regexp in a buffer.
   START is preserved whatever.
  
   There's a slight issue here. We obviously want to find the largest
   possible match; this means that just working our way back through
   the string to the first character that regtry() accepts won't
   work. Matching from a previous character could also succeed,
   possibly giving a longer match.

   My approach is this: find the first match as usual, then scan
   backwards a character at a time, until the start of the line,
   finding the match nearest the start of the line, with the proviso
   that the new match must have the same end position as the
   original. Obviously the longest-match rule doesn't hold across line
   boundaries; I think this is acceptable. */
int
regexec_reverse_tx(prog, tx, start, eflags)
    register rep_regexp *prog;
    TX *tx;
    repv start;
    int eflags;
{
    Pos s;

    /* For REG_NOCASE and strpbrk()  */
    static char mat[3] = "xX";

    /* Be paranoid... */
    if (prog == NULL || tx == NULL || start == rep_NULL || !POS(start)) {
	rep_regerror("NULL parameter");
	return (0);
    }
    /* Check validity of program. */
    if (UCHARAT(prog->program) != MAGIC) {
	rep_regerror("corrupted program");
	return (0);
    }

    /* jsh -- Check for REG_NOCASE, means ignore case in string matches.  */
    regnocase = ((eflags & rep_REG_NOCASE) != 0);

    /* If there is a "must appear" string, look for it. */
    if (prog->regmust != NULL)
    {
	int found = 0;
	COPY_VPOS(&s, start);
	if(regnocase)
	{
	    mat[0] = tolower(prog->regmust[0]);
	    mat[1] = toupper(prog->regmust[0]);
	    while (buffer_reverse_strpbrk(tx, &s, mat))
	    {
		if(buffer_compare_n(tx, &s, prog->regmust,
				    prog->regmlen, strncasecmp))
		{
		    found = 1;
		    break;	    /* Found it. */
		}
		if(!backward_char(1, tx, &s)) break;
	    }
	}
	else
	{
	    while (buffer_reverse_strchr(tx, &s, prog->regmust[0]))
	    {
		if(buffer_compare_n(tx, &s, prog->regmust,
				    prog->regmlen, strncmp))
		{
		    found = 1;
		    break;	    /* Found it. */
		}
		if(!backward_char(1, tx, &s)) break;
	    }
	}
	if (!found)		/* Not present. */
	    return (0);
    }

    /* Simplest case:  anchored match need be tried only once.
       For buffers, this means that it only needs to be tried
       at the start of each line after position START. Also
       the longest-match issue doesn't apply since we're always
       matching from the start of a line. */
    if (prog->reganch)
    {
	COPY_VPOS(&s, start);
	PCOL(&s) = 0;
	while(PROW(&s) >= tx->tx_LogicalStart)
	{
	    if(regtry(tx, prog, &s))
		return (1);
	    PROW(&s)--;
	}
	return (0);
    }

    /* Messy cases:  unanchored match. */
    COPY_VPOS(&s, start);
    if (prog->regstart != '\0')
    {
	/* We know what char it must start with. */
	if(regnocase)
	{
	    mat[0] = tolower(prog->regstart);
	    mat[1] = toupper(prog->regstart);
	    while(buffer_reverse_strpbrk(tx, &s, mat))
	    {
		if(regtry(tx, prog, &s))
		{
		    /* Try for a longer match. */
		    rep_regsubs leftmost = prog->matches;
		    while(PCOL(&s)-- > 0)
		    {
			char c = INPUT_CHAR(&s);
			if(toupper(c) == toupper(prog->regstart)
			   && regtry(tx, prog, &s)
			   && POS_EQUAL_P(prog->matches.obj.endp[0],
					  leftmost.obj.endp[0]))
			{
			    /* found an equivalent match to the left,
			       replace the original */
			    leftmost = prog->matches;
			}
		    }
		    prog->matches = leftmost;
		    return (1);
		}
		if(!backward_char(1, tx, &s)) break;
	    }
	}
	else
	{
	    while(buffer_reverse_strchr(tx, &s, prog->regstart))
	    {
		if(regtry(tx, prog, &s))
		{
		    /* Try for a longer match. */
		    rep_regsubs leftmost = prog->matches;
		    while(PCOL(&s)-- > 0)
		    {
			if(INPUT_CHAR(&s) == prog->regstart
			   && regtry(tx, prog, &s)
			   && POS_EQUAL_P(prog->matches.obj.endp[0],
					  leftmost.obj.endp[0]))
			{
			    /* found an equivalent match to the left,
			       replace the original */
			    leftmost = prog->matches;
			}
		    }
		    prog->matches = leftmost;
		    return (1);
		}
		if(!backward_char(1, tx, &s)) break;
	    }
	}
    }
    else
    {
	/* We don't -- general case. */
	do {
	    if (regtry(tx, prog, &s))
	    {
		/* Try for a longer match. */
		rep_regsubs leftmost = prog->matches;
		while(PCOL(&s)-- > 0)
		{
		    if(regtry(tx, prog, &s)
		       && POS_EQUAL_P(prog->matches.obj.endp[0],
				      leftmost.obj.endp[0]))
		    {
			/* found an equivalent match to the left,
			   replace the original */
			leftmost = prog->matches;
		    }
		}
		prog->matches = leftmost;
		return (1);
	    }
	} while (backward_char(1, tx, &s));
    }

    /* Failure. */
    return (0);
}

/*
 * - regmatch_tx - match a regexp against the string starting at
 *		   START. No searching. START is preserved.
 */
int
regmatch_tx(prog, tx, start, eflags)
    register rep_regexp *prog;
    TX *tx;
    repv start;
    int eflags;
{
    Pos s;
    COPY_VPOS(&s, start);

    /* Check for REG_NOCASE, means ignore case in string matches.  */
    regnocase = ((eflags & rep_REG_NOCASE) != 0);
    return regtry(tx, prog, &s);
}

/*
 * - regtry - try match at specific point
 */
static int			/* 0 failure, 1 success */
regtry(tx, prog, matchpos)
    TX		   *tx;
    rep_regexp	   *prog;
    Pos            *matchpos;
{
    register int    i;

    regtx = tx;
    reginput = *matchpos;
    regstartp = prog->matches.obj.startp;
    regendp = prog->matches.obj.endp;
    regnest = 0;

    for (i = 0; i < rep_NSUBEXP; i++) {
	regstartp[i] = rep_NULL;
	regendp[i] = rep_NULL;
    }
    if (regmatch(prog->program + 1)) {
	regstartp[0] = make_pos(PCOL(matchpos), PROW(matchpos));
	regendp[0] = make_pos(PCOL(&reginput), PROW(&reginput));
	prog->lasttype = rep_reg_obj;
	return (1);
    } else
	return (0);
}

/* get around the insane number of return statements in regmatch () */
static inline int
nested_regmatch (char *prog)
{
    int ret;
    regnest++;
    ret = regmatch (prog);
    regnest--;
    return ret;
}

/*
 * - regmatch - main matching routine
 *
 * Conceptually the strategy is simple:	 check to see whether the current node
 * matches, call self recursively to see whether the rest matches, and then
 * act accordingly.  In practice we make some effort to avoid recursion, in
 * particular by going through "ordinary" nodes (that don't need to know
 * whether the rest of the match failed) by a loop instead of by recursion.
 */
static int			/* 0 failure, 1 success */
regmatch(prog)
    char	   *prog;
{
    register char  *scan;	/* Current node. */
    char	   *next;	/* Next node. */

    if (regnest >= rep_regexp_max_depth)
    {
	/* recursion overload, bail out */
	rep_regerror ("stack overflow");
	return 0;
    }

    scan = prog;
#ifdef DEBUG
    if (scan != NULL && regnarrate)
	fprintf(stderr, "%s(\n", regprop(scan));
#endif
    while (scan != NULL) {
#ifdef DEBUG
	if (regnarrate)
	    fprintf(stderr, "%s...\n", regprop(scan));
#endif
	next = regnext(scan);

	switch (OP(scan)) {
	    char c;
	case BOL:
	    if (PCOL(&reginput) > 0)
		return (0);
	    break;
	case EOL:
	    if (PCOL(&reginput)
		< regtx->tx_Lines[PROW(&reginput)].ln_Strlen - 1)
		return (0);
	    break;
	case ANY:
	    /* Don't match newlines for . */
	    if(PCOL(&reginput)
	       == regtx->tx_Lines[PROW(&reginput)].ln_Strlen - 1)
		return (0);
	    forward_char(1, regtx, &reginput);
	    break;
	case EXACTLY:{
		register int	len;
		register u_char *opnd;
		opnd = OPERAND(scan);
		if(regnocase)
		{
		    /* Inline the first character, for speed. */
		    if(END_OF_INPUT(&reginput))
			return (0);
		    c = INPUT_CHAR(&reginput);
		    if(toupper(*opnd) != toupper(c))
			return (0);
		    len = strlen(opnd);
		    if(len == 1)
			forward_char(1, regtx, &reginput);
		    else
		    {
			/* Advances reginput to end of match */
			if(!buffer_compare_n(regtx, &reginput, opnd,
					     len, strncasecmp))
			    return (0);
		    }
		}
		else
		{
		    /* Inline the first character, for speed. */
		    if(END_OF_INPUT(&reginput)
		       || *opnd != INPUT_CHAR(&reginput))
			return (0);
		    len = strlen(opnd);
		    if(len == 1)
			forward_char(1, regtx, &reginput);
		    else
		    {
			/* Advances reginput to end of match */
			if(!buffer_compare_n(regtx, &reginput, opnd,
					     len, strncmp))
			    return (0);
		    }
		}
	    }
	    break;
	case ANYOF: {
		if (END_OF_INPUT(&reginput)
		    || strchr(OPERAND(scan),
			      INPUT_CHAR(&reginput)) == NULL)
		    return (0);
		forward_char(1, regtx, &reginput);
	    }
	    break;
	case ANYBUT: {
		if (END_OF_INPUT(&reginput)
		    || strchr(OPERAND(scan),
			      INPUT_CHAR(&reginput)) != NULL)
		    return (0);
		forward_char(1, regtx, &reginput);
	    }
	    break;
	case NOTHING:
	    break;
	case BACK:
	    break;
	case OPEN + 1:
	case OPEN + 2:
	case OPEN + 3:
	case OPEN + 4:
	case OPEN + 5:
	case OPEN + 6:
	case OPEN + 7:
	case OPEN + 8:
	case OPEN + 9:{
		register int	no;
		Pos save;

		no = OP(scan) - OPEN;
		save = reginput;

		if (nested_regmatch(next)) {
		    /*
		     * Don't set startp if some later invocation of the same
		     * parentheses already has.
		     */
		    if (regstartp[no] == rep_NULL)
			regstartp[no] = make_pos(PCOL(&save), PROW(&save));
		    return (1);
		} else
		    return (0);
	    }
	    break;
	case CLOSE + 1:
	case CLOSE + 2:
	case CLOSE + 3:
	case CLOSE + 4:
	case CLOSE + 5:
	case CLOSE + 6:
	case CLOSE + 7:
	case CLOSE + 8:
	case CLOSE + 9:{
		register int	no;
		Pos save;

		no = OP(scan) - CLOSE;
		save = reginput;

		if (nested_regmatch(next)) {
		    /*
		     * Don't set endp if some later invocation of the same
		     * parentheses already has.
		     */
		    if (regendp[no] == rep_NULL)
			regendp[no] = make_pos(PCOL(&save), PROW(&save));
		    return (1);
		} else
		    return (0);
	    }
	    break;
	case BRANCH:{
		Pos save;

		if (OP(next) != BRANCH) /* No choice. */
		    next = OPERAND(scan);	/* Avoid recursion. */
		else {
		    do {
			save = reginput;
			if (nested_regmatch(OPERAND(scan)))
			    return (1);
			reginput = save;
			scan = regnext(scan);
		    } while (scan != NULL && OP(scan) == BRANCH);
		    return (0);
		    /* NOTREACHED */
		}
	    }
	    break;
	case STAR:
	case PLUS:{
		register u_char	nextch;
		register int	no;
		Pos save;
		register int	min;

		/*
		 * Lookahead to avoid useless match attempts when we know
		 * what character comes next.
		 */
		nextch = '\0';
		if (OP(next) == EXACTLY)
		    nextch = *OPERAND(next);
		if(regnocase)
		    nextch = toupper(nextch);
		min = (OP(scan) == STAR) ? 0 : 1;
		save = reginput;
		no = regrepeat(OPERAND(scan));
		while (no >= min) {
		    /* If it could work, try it. */
		    if (nextch == '\0'
			|| (!END_OF_INPUT(&reginput)
			    && (regnocase ? TOUPPER_INPUT_CHAR(&reginput)
				: INPUT_CHAR(&reginput)) == nextch))
			if (nested_regmatch(next))
			    return (1);
		    /* Couldn't or didn't -- back up. */
		    no--;
		    reginput = save;
		    forward_char(no, regtx, &reginput);
		}
		return (0);
	    }
	    break;
	case NGSTAR:
	case NGPLUS:{
		register u_char	nextch;
		register int	no;
		Pos save;
		register int	max;

		/*
		 * Lookahead to avoid useless match attempts when we know
		 * what character comes next.
		 */
		nextch = '\0';
		if (OP(next) == EXACTLY)
		    nextch = *OPERAND(next);
		if(regnocase)
		    nextch = toupper(nextch);
		no = (OP(scan) == NGSTAR) ? 0 : 1;
		save = reginput;
		max = regrepeat(OPERAND(scan));
		while (no <= max) {
		    reginput = save;
		    forward_char(no, regtx, &reginput);
		    /* If it could work, try it. */
		    if (nextch == '\0'
			|| (!END_OF_INPUT(&reginput)
			    && (regnocase ? TOUPPER_INPUT_CHAR(&reginput)
				: INPUT_CHAR(&reginput)) == nextch))
			if (nested_regmatch(next))
			    return (1);
		    /* Couldn't or didn't -- move up. */
		    no++;
		}
		return (0);
	    }
	    break;
	case WORD:
	    if (END_OF_INPUT (&reginput))
		return 0;
	    c = INPUT_CHAR(&reginput);
	    if (c != '_' && !isalnum ((int)c))
		return 0;
	    forward_char (1, regtx, &reginput);
	    break;
	case NWORD:
	    if (END_OF_INPUT (&reginput))
		return 0;
	    c = INPUT_CHAR(&reginput);
	    if (c == '_' || isalnum ((int)c))
		return 0;
	    forward_char (1, regtx, &reginput);
	    break;
	case WSPC:
	    if (END_OF_INPUT (&reginput))
		return 0;
	    c = INPUT_CHAR(&reginput);
	    if (!isspace ((int)c))
		return 0;
	    forward_char (1, regtx, &reginput);
	    break;
	case NWSPC:
	    if (END_OF_INPUT (&reginput))
		return 0;
	    c = INPUT_CHAR(&reginput);
	    if (isspace ((int)c))
		return 0;
	    forward_char (1, regtx, &reginput);
	    break;
	case DIGI:
	    if (END_OF_INPUT (&reginput))
		return 0;
	    c = INPUT_CHAR(&reginput);
	    if (!isdigit ((int)c))
		return 0;
	    forward_char (1, regtx, &reginput);
	    break;
	case NDIGI:
	    if (END_OF_INPUT (&reginput))
		return 0;
	    c = INPUT_CHAR(&reginput);
	    if (isdigit ((int)c))
		return 0;
	    forward_char (1, regtx, &reginput);
	    break;
	case WEDGE:
	    if (START_OF_INPUT (&reginput) || END_OF_INPUT (&reginput))
		break;
	    else
	    {
		Pos tem = reginput;
		u_char c0, c1;
		backward_char (1, regtx, &tem);
		c0 = INPUT_CHAR (&reginput);
		c1 = INPUT_CHAR (&tem);
		if (((c0 == '_' || isalnum ((int)c0))
		     && (c1 != '_' && !isalnum ((int)c1)))
		    || ((c0 != '_' && !isalnum ((int)c0))
			&& (c1 == '_' || isalnum ((int)c1))))
		    break;
	    }
	    return 0;
	case NWEDGE:
	    if (START_OF_INPUT (&reginput) || END_OF_INPUT (&reginput))
		c = 0;
	    else
	    {
		Pos tem = reginput;
		u_char c0, c1;
		backward_char (1, regtx, &tem);
		c0 = INPUT_CHAR (&reginput);
		c1 = INPUT_CHAR (&tem);
		if (((c0 == '_' || isalnum ((int)c0))
		     && (c1 != '_' && !isalnum ((int)c1)))
		    || ((c0 != '_' && !isalnum ((int)c0))
			&& (c1 == '_' || isalnum ((int)c1))))
		    c = 0;
		else
		    c = 1;
	    }
	    if (c == 0)
		return 0;
	    break;
	case END:
	    return (1);		/* Success! */
	    break;
	default:
	    rep_regerror("memory corruption");
	    return (0);
	    break;
	}

	scan = next;
    }

    /*
     * We get here only if there's trouble -- normally "case END" is the
     * terminating point.
     */
    rep_regerror("corrupted pointers");
    return (0);
}

/*
 * - regrepeat - repeatedly match something simple, report how many
 */
static int
regrepeat(p)
    char *p;
{
    register int count = 0;
    Pos scan;
    register u_char *opnd;
    char c;

    scan = reginput;
    opnd = OPERAND(p);
    switch (OP(p)) {
    case ANY:
	/* TODO: what to do here? How about matching up to the
	   end of the buffer? No, this could be something like .*
	   what we want is to match up to the end of a line. */
	count = (regtx->tx_Lines[PROW(&scan)].ln_Strlen - 1) - PCOL(&scan);
	PCOL(&scan) += count;
	break;
    case EXACTLY:
	if(regnocase)
	{
	    char uo = toupper(*opnd);
	    while(!END_OF_INPUT(&scan))
	    {
		c = INPUT_CHAR(&scan);
		if(uo != toupper(c))
		    break;
		count++;
		forward_char(1, regtx, &scan);
	    }
	}
	else
	{
	    while(!END_OF_INPUT(&scan))
	    {
		c = INPUT_CHAR(&scan);
		if(*opnd != c)
		    break;
		count++;
		forward_char(1, regtx, &scan);
	    }
	}
	break;
    case ANYOF:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR(&scan);
	    if(strchr(opnd, c) == NULL)
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case ANYBUT:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR(&scan);
	    if(strchr(opnd, c) != NULL)
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case WORD:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR (&scan);
	    if (c != '_' && !isalnum ((int)c))
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case NWORD:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR (&scan);
	    if (c == '_' || isalnum ((int)c))
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case WSPC:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR (&scan);
	    if (!isspace ((int)c))
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case NWSPC:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR (&scan);
	    if (isspace ((int)c))
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case DIGI:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR (&scan);
	    if (!isdigit ((int)c))
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    case NDIGI:
	while (!END_OF_INPUT(&scan))
	{
	    c = INPUT_CHAR (&scan);
	    if (isdigit ((int)c))
		break;
	    count++;
	    forward_char(1, regtx, &scan);
	}
	break;
    default:			/* Oh dear.  Called inappropriately. */
	rep_regerror("internal foulup");
	count = 0;		/* Best compromise. */
	break;
    }

    reginput = scan;
    return (count);
}

/*
 * - regnext - dig the "next" pointer out of a node 
 */
static char    *
regnext(p)
    register char  *p;
{
    register int    offset;

    offset = NEXT(p);
    if (offset == 0)
	return (NULL);

    if (OP(p) == BACK)
	return (p - offset);
    else
	return (p + offset);
}
