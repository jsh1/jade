/* foomail.c -- simple program to put in place of sendmail to examine
		how it's being called. */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define META "\"\\\n\t"
int
main(int argc, char **argv)
{
    char buf[256];

    argc--; argv++;
    fputs("Arguments: ", stdout);
    while(argc > 0)
    {
	char *start, *chunk;
	putc('"', stdout);
	start = *argv;
	while((chunk = strpbrk(start, META)) != 0)
	{
	    fwrite(start, chunk - start, 1, stdout);
	    switch(*chunk)
	    {
	    case '"':
		fputs("\\\"", stdout);
		break;

	    case '\\':
		fputs("\\\\", stdout);
		break;

	    case '\n':
		fputs("\\n", stdout);
		break;

	    case '\t':
		fputs("\\t", stdout);
		break;

	    default:
		printf("\\%03d", *chunk);
	    }
	    start = chunk + 1;
	}
	fputs(start, stdout);
	fputs("\" ", stdout);
	argc--; argv++;
    }
    fputs("\n-----message-----\n", stdout);

    while(fgets(buf, 256, stdin))
	fputs(buf, stdout);
    return 0;
}
