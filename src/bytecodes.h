/* bytecodes.h -- Constant definitions of lispmach byte-codes
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef BYTECODES_H
#define BYTECODES_H

#define BYTECODE_MAJOR_VERSION 2
#define BYTECODE_MINOR_VERSION 1

/* Number of bits encoded in each extra opcode forming the argument. */
#define ARG_SHIFT    8

/* The bits in the opcode used to encode the argument. */
#define OP_ARG_MASK  0x07

/* The inverse of the above. */
#define OP_OP_MASK   0xf8

/* Special arg specifying that the next opcode is actually an 8-bit
   argument. */
#define OP_ARG_1BYTE 6

/* Special arg meaning following two opcodes are a 16-bit argument. The
   first opcode is the high bits, the second the low bits. */
#define OP_ARG_2BYTE 7


/* Opcodes which have an argument encoded in them */

/* Call function on top of stack with following ARG parameters. Leave
   result on stack. */
#define OP_CALL 0x08

/* Push const[ARG] onto the stack. */
#define OP_PUSH 0x10

/* Push the symbol-value of the symbol const[ARG] onto the stack. */
#define OP_REFQ 0x18

/* Set the symbol-value of symbol const[ARG] to the value on the
   stack. Leaves value on stack. */
#define OP_SETQ 0x20

/* Push the list formed from the top ARG values on the stack. */
#define OP_LIST 0x28

/* Bind the value on the top of the stack to the symbol const[ARG]. */
#define OP_BIND 0x30

#define OP_LAST_WITH_ARGS 0x38


/* Opcodes without arguments. */

#define OP_REF 0x40			/* push (symbol-value pop) */
#define OP_SET 0x41			/* push (set pop[1] pop[2]) */
#define OP_FREF 0x42			/* push (symbol-function pop) */
#define OP_FSET 0x43			/* (fset pop[1] pop[2]) */
#define OP_INIT_BIND 0x44		/* new-binding-set */
#define OP_UNBIND 0x45			/* rewind-binding-set */
#define OP_DUP	0x46			/* push stk[0] */
#define OP_SWAP 0x47			/* stk[0] = stk[1], stk[1] = stk[0] */
#define OP_POP	0x48			/* pop[1] */

#define OP_NIL 0x49			/* push nil */
#define OP_T 0x4a			/* push t */
#define OP_CONS 0x4b			/* push (cons pop[1] pop[2]) */
#define OP_CAR 0x4c			/* push (car pop[1]) */
#define OP_CDR 0x4d			/* push (cdr pop[2])  */
#define OP_RPLACA 0x4e			/* call-2 rplaca */
#define OP_RPLACD 0x4f			/* call-2 rplaca */
#define OP_NTH 0x50			/* call-2 nth */
#define OP_NTHCDR 0x51			/* call-2 nthcdr */
#define OP_ASET 0x52			/* call-3 aset */
#define OP_AREF 0x53			/* call-2 aref */
#define OP_LENGTH 0x54			/* call-1 length */
#define OP_EVAL 0x55			/* call-1 eval */
#define OP_ADD 0x56			/* push (+ pop[1] pop[2]) */
#define OP_NEG 0x57			/* push (- pop[1]) */
#define OP_SUB 0x58			/* push (- pop[1] pop[2]) */
#define OP_MUL 0x59			/* push (* pop[1] pop[2]) */
#define OP_DIV 0x5a			/* push (/ pop[1] pop[2]) */
#define OP_MOD 0x5b			/* push (% pop[1] pop[2]) */
#define OP_LNOT 0x5c			/* push (lognot pop[1]) */
#define OP_NOT 0x5d			/* push (not pop[1]) */
#define OP_LOR 0x5e			/* push (logior pop[1] pop[2]) */
#define OP_LAND 0x5f			/* push (logand pop[1] pop[2]) */
#define OP_EQUAL 0x60			/* push (equal pop[1] pop[2]) */
#define OP_EQ 0x61			/* push (eq pop[1] pop[2]) */
#define OP_NUM_EQ 0x62			/* push (= pop[1] pop[2]) */
#define OP_NUM_NOTEQ 0x63		/* push (/= pop[1] pop[2]) */
#define OP_GT 0x64			/* push (> pop[1] pop[2]) */
#define OP_GE 0x65			/* push (>= pop[1] pop[2]) */
#define OP_LT 0x66			/* push (< pop[1] pop[2]) */
#define OP_LE 0x67			/* push (<= pop[1] pop[2]) */
#define OP_INC 0x68			/* push (1+ pop[1]) */
#define OP_DEC 0x69			/* push (1- pop[1]) */
#define OP_LSH 0x6a			/* push (lsh pop[1] pop[2]) */
#define OP_ZEROP 0x6b			/* push (zerop pop[1]) */
#define OP_NULL 0x6c			/* push (null pop[1]) */
#define OP_ATOM 0x6d			/* push (atom pop[1]) */
#define OP_CONSP 0x6e			/* push (consp pop[1]) */
#define OP_LISTP 0x6f			/* push (listp pop[1]) */
#define OP_NUMBERP 0x70			/* push (numberp pop[1]) */
#define OP_STRINGP 0x71			/* push (stringp pop[1]) */
#define OP_VECTORP 0x72			/* push (vectorp pop[1]) */
#define OP_CATCH_KLUDGE 0x73		/* push (catch pop[1] pop[2]) */
#define OP_THROW 0x74			/* push (throw pop[1]) */
#define OP_UNWIND_PRO 0x75		/* magic */
#define OP_UN_UNWIND_PRO obsolete	/* more magic */
#define OP_FBOUNDP 0x77			/* call-1 fboundp */
#define OP_BOUNDP 0x78			/* call-1 boundp */
#define OP_SYMBOLP 0x79			/* push (symbolp pop[1]) */
#define OP_GET 0x7a			/* call-2 get */
#define OP_PUT 0x7b			/* call-3 put */
#define OP_ERROR_PRO 0x7c		/* push (error-protect pop[2] pop[1]) */
#define OP_SIGNAL 0x7d			/* call-2 signal */
#define OP_RETURN 0x7e			/* (throw 'defun pop[0]) */

/* new 12/7/94 */
#define OP_REVERSE 0x7f			/* call-1 reverse */
#define OP_NREVERSE 0x80		/* call-1 nreverse */
#define OP_ASSOC 0x81			/* call-2 assoc */
#define OP_ASSQ 0x82			/* call-2 assq */
#define OP_RASSOC 0x83			/* call-2 rassoc */
#define OP_RASSQ 0x84			/* call-2 rassq */
#define OP_LAST 0x85			/* call-1 last */
#define OP_MAPCAR 0x86			/* call-2 mapcar */
#define OP_MAPC 0x87			/* call-1 mapc */
#define OP_MEMBER 0x88			/* call-2 member */
#define OP_MEMQ 0x89			/* call-2 memq */
#define OP_DELETE 0x8a			/* call-2 delete */
#define OP_DELQ 0x8b			/* call-2 delq */
#define OP_DELETE_IF 0x8c		/* call-2 delete-if */
#define OP_DELETE_IF_NOT 0x8d		/* call-2 delete-if-not */
#define OP_COPY_SEQUENCE 0x8e		/* call-1 copy-sequence */
#define OP_SEQUENCEP 0x8f		/* call-1 sequencep */
#define OP_FUNCTIONP 0x90		/* call-1 functionp */
#define OP_SPECIAL_FORM_P 0x91		/* call-1 special-form-p */
#define OP_SUBRP 0x92			/* call-1 subrp */
#define OP_EQL 0x93			/* push (eql pop[1] pop[2]) */

/* new 23-8-94 */
#define OP_LXOR 0x94			/* push (logxor pop[1] pop[2] */

#define OP_SET_CURRENT_BUFFER 0xb0	/* call-2 set-current-buffer */
#define OP_BIND_BUFFER 0xb1
#define OP_CURRENT_BUFFER 0xb2		/* call-1 current-buffer */
#define OP_BUFFERP 0xb3			/* call-1 bufferp */
#define OP_MARKP 0xb4			/* call-1 markp */
#define OP_WINDOWP 0xb5			/* call-1 windowp */
#define OP_BIND_WINDOW 0xb6

/* new 26-8-97 */
#define OP_VIEWP 0xb7			/* call-1 viewp */
#define OP_BIND_VIEW 0xb8
#define OP_CURRENT_VIEW 0xb9		/* call-1 current-view */
#define OP_SWAP2 0xba			/* stk[0] = stk[1], stk[1] = stk[2],
					   stk[2] = stk[0]. */


#define OP_LAST_BEFORE_JMPS 0xfa

#define OP_JMP 0xfb			/* jmp pc[0,1] */
#define OP_JN 0xfc			/* if (not pop[1]) jmp pc[0,1] */
#define OP_JT 0xfd			/* if pop[1] jmp pc[0,1] */
#define OP_JNP 0xfe			/* if (not stk[0]) pop else jmp */
#define OP_JTP 0xff			/* if stk[0] pop else jmp */

#endif /* BYTECODES_H */
