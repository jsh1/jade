/* test-dl.c -- Test of dynamic loading */

#include "jade.h"
#include "jade_protos.h"

DEFUN("dl-test", cmd_dl_test, subr_dl_test, (VALUE arg), V_Subr1, 0)
{
    DECLARE1(arg, INTP);
    return MAKE_INT(VINT(arg) + 42);
}

Lisp_XSubr *jade_subrs[] = { &subr_dl_test, 0 };
