/*
 * Structure for regexp "program".  This is essentially a linear encoding of
 * a nondeterministic finite-state machine (aka syntax charts or "railroad
 * normal form" in parsing technology).  Each node is an opcode plus a "next"
 * pointer, possibly plus an operand.  "Next" pointers of all nodes except
 * BRANCH implement concatenation; a "next" pointer with a BRANCH on both
 * ends of it is connecting two alternatives.	(Here we have one of the
 * subtle syntax dependencies:	an individual BRANCH (as opposed to a
 * collection of them) is never concatenated with anything because of
 * operator precedence.)  The operand of some types of node is a literal
 * string; for others, it is a node leading into a sub-FSM.  In particular,
 * the operand of a BRANCH node is the first node of the branch. (NB this is
 * *not* a tree structure:  the tail of the branch connects to the thing
 * following the set of BRANCHes.)  The opcodes are:
 */

/* definition	number	opnd?	meaning */
#define END	0		/* no	End of program. */
#define BOL	1		/* no	Match "" at beginning of line. */
#define EOL	2		/* no	Match "" at end of line. */
#define ANY	3		/* no	Match any one character. */
#define ANYOF	4		/* str	Match any character in this string. */
#define ANYBUT	5		/* str	Match any character not in this
				 * string. */
#define BRANCH	6		/* node Match this alternative, or the
				 * next... */
#define BACK	7		/* no	Match "", "next" ptr points backward. */
#define EXACTLY 8		/* str	Match this string. */
#define NOTHING 9		/* no	Match empty string. */
#define STAR	10		/* node Match this (simple) thing 0 or more
				 * times. */
#define PLUS	11		/* node Match this (simple) thing 1 or more
				 * times. */
#define OPEN	20		/* no	Mark this point in input as start of
				 * #n. */
/* OPEN+1 is number 1, etc. */
#define CLOSE	30		/* no	Analogous to OPEN. */

/*
 * Opcode notes:
 *
 * BRANCH	The set of branches constituting a single choice are hooked together
 * with their "next" pointers, since precedence prevents anything being
 * concatenated to any individual branch.  The "next" pointer of the last
 * BRANCH in a choice points to the thing following the whole choice.  This
 * is also where the final "next" pointer of each individual branch points;
 * each branch starts with the operand node of a BRANCH node.
 *
 * BACK		Normal "next" pointers all implicitly point forward; BACK exists to
 * make loop structures possible.
 *
 * STAR,PLUS	'?', and complex '*' and '+', are implemented as circular
 * BRANCH structures using BACK.  Simple cases (one character per match) are
 * implemented with STAR and PLUS for speed and to minimize recursive
 * plunges.
 *
 * OPEN,CLOSE	...are numbered at compile time.
 */

/*
 * A node is one char of opcode followed by two chars of "next" pointer.
 * "Next" pointers are stored as two 8-bit pieces, high order first.  The
 * value is a positive offset from the opcode of the node containing it. An
 * operand, if any, simply follows the node.  (Note that much of the code
 * generation knows about this implicit relationship.)
 *
 * Using two bytes for the "next" pointer is vast overkill for most things, but
 * allows patterns to get big without disasters.
 */
#define OP(p)	(*(p))
#define NEXT(p) (((*((p)+1)&0377)<<8) + (*((p)+2)&0377))
#define OPERAND(p)	((p) + 3)

/*
 * See regmagic.h for one further detail of program structure.
 */


