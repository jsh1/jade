
/*
 * CHANGED, 14-Jan-93, by J.Harper,
 * added #ifdef __STDC__ prototype sections so I can use registerized
 * arguments
 */

#include <stdio.h>
#ifdef __STDC__
void	exit(int);
#ifdef ERRAVAIL
void	error(char *, char *);
#endif
#endif

void
regerror(s)
    char	   *s;
{
#ifdef ERRAVAIL
    error("regexp: %s", s);
#else
    fprintf(stderr, "regexp(3): %s", s);
    exit(1);
#endif
    /* NOTREACHED */
}
