/* commands.c -- Interactive calling of commands/functions
   Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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
   along with Jade; see the file COPYING. If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <string.h>


/* Symbols of the Lisp functions called. */

DEFSYM(prompt_for_function, "prompt-for-function");
DEFSYM(prompt_for_buffer, "prompt-for-buffer");
DEFSYM(prompt_for_char, "prompt-for-char");
DEFSYM(prompt_for_command, "prompt-for-command");
DEFSYM(prompt_for_directory, "prompt-for-directory");
DEFSYM(prompt_for_file, "prompt-for-file");
DEFSYM(prompt_for_number, "prompt-for-number");
DEFSYM(prompt_for_string, "prompt-for-string");
DEFSYM(prompt_for_symbol, "prompt-for-symbol");
DEFSYM(prompt_for_variable, "prompt-for-variable");
DEFSYM(prompt_for_lisp, "prompt-for-lisp");
DEFSYM(read_event, "read-event");
DEFSYM(set_auto_mark, "set-auto-mark");

DEFSYM(interactive, "interactive");
DEFSTRING(err_interactive, "Bad interactive specification");

/* Prefix argument for the next command and the current command. */
static repv prefix_arg, current_prefix_arg;

/* Command being executed and command last executed. */
repv this_command, last_command;

/* hooks.
::doc:Vpre-command-hook::
Hook called before evaluating each command.
::end::
::doc:Vpost-command-hook::
Hook called after evaluating each command.
::end:: */

DEFSYM(pre_command_hook, "pre-command-hook");
DEFSYM(post_command_hook, "post-command-hook");


DEFUN("this-command", var_this_command, Sthis_command, (repv val), rep_Var) /*
::doc:Vthis-command::
This variable holds the command currently being evaluated, or nil if no
command is active. The `command' is whatever is being evaluated; it could
be a function, a form or even a list of forms (from a menu).
::end:: */
{
    if(val)
	this_command = val;
    return(this_command);
}

DEFUN("last-command", var_last_command, Slast_command, (repv val), rep_Var) /*
::doc:Vlast-command::
This variable holds the last interactive command evaluated. This will either
be from a keybinding or a menu. Setting the value of the `next-keymap-path'
variable is not considered a command. After a command finishes this variable
takes the value of `this-command'.
::end:: */
{
    if(val)
	last_command = val;
    return(last_command);
}

DEFUN("prefix-arg", var_prefix_arg, Sprefix_arg, (repv val), rep_Var) /*
::doc:Vprefix-arg::
Value of the prefix argument for the next command.
::end:: */
{
    if(val)
	prefix_arg = val;
    return(prefix_arg);
}

DEFUN("current-prefix-arg", var_current_prefix_arg, Scurrent_prefix_arg, (repv val), rep_Var) /*
::doc:Vcurrent-prefix-arg::
Value of the prefix argument for the current command.
::end:: */
{
    if(val)
	current_prefix_arg = val;
    return(current_prefix_arg);
}

/* Search the definition of the command CMD for an interactive calling
   spec. Return it or NULL. */
static repv
interactive_spec(repv cmd)
{
    repv fun, spec = rep_NULL;
    if(rep_SYMBOLP(cmd))
	cmd = Fsymbol_value(cmd, Qt);
again:
    if (rep_FUNARGP(cmd))
	fun = rep_FUNARG(cmd)->fun;
    else
	fun = cmd;
    if(!rep_VOIDP(fun) && !rep_NILP(fun))
    {
	if((rep_TYPE(fun) >= rep_Subr0) && (rep_TYPE(fun) <= rep_SubrN))
	    spec = rep_SUBR(fun)->int_spec;
	else if(rep_COMPILEDP(fun))
	    spec = rep_COMPILED_INTERACTIVE(fun);
	else if(rep_CONSP(fun))
	{
	    if(rep_CAR(fun) == Qlambda)
	    {
		/* A lambda expression, test its first proper form. */
		fun = Fnthcdr(rep_MAKE_INT(2), fun);
		if(fun == rep_NULL)
		    return rep_NULL;
		if(rep_CONSP(fun)
		   && (rep_STRINGP(rep_CAR(fun)) || rep_INTP(rep_CAR(fun)))
		   && rep_CONSP(rep_CDR(fun)))
		{
		    /* A doc-string */
		    fun = rep_CDR(fun);
		}
		if(fun && rep_CONSP(fun))
		{
		    fun = rep_CAR(fun);
		    if(rep_CONSP(fun)
		       && (rep_CAR(fun) == Qinteractive))
		    {
			/* got it. */
			spec = rep_CONSP(rep_CDR(fun)) ? rep_CAR(rep_CDR(fun)) : Qnil;
		    }
		}
	    }
	    else if(rep_CAR(fun) == Qautoload && rep_FUNARGP(cmd))
	    {
		/* An autoload, load it then try again. */
		struct rep_Call lc;
		lc.fun = lc.args = lc.args_evalled_p = Qnil;
		rep_PUSH_CALL(lc);
		rep_USE_FUNARG(cmd);
		cmd = rep_load_autoload(cmd);
		rep_POP_CALL(lc);
		if(cmd != rep_NULL)
		    goto again;
	    }
	}
    }
    return(spec);
}

DEFSTRING(no_block, "No block marked");
DEFSTRING(nil_arg, "Nil argument to command");
DEFSTRING(not_command, "Not a command");
DEFUN_INT("call-command", Fcall_command, Scall_command, (repv cmd, repv Farg), rep_Subr2, "CEnter command:" rep_DS_NL "P") /*
::doc:Scall-command::
call-command COMMAND [PREFIX-ARG]

Invoke the command COMMAND. This can be one of,
 1. A symbol whose function value is to be called, the symbol must be of
    type `commandp'; any interactive calling specification will be
    used to find arguments to give to the function. (see `interactive')
 2. A lambda-expression to call as a function name
 3. A single Lisp form to be evaluated by eval

If PREFIX-ARG is non-nil it specifies the value of the COMMAND's
current-prefix-arg. This is used in call-command's interactive spec so that
any entered arg is given to the invoked COMMAND.
::end:: */
{
    repv res = rep_NULL;
    this_command = cmd;

    /* Move the prefix arg. */
    if(rep_NILP(Farg))
	Farg = prefix_arg;
    prefix_arg = Qnil;
    current_prefix_arg = Farg;

    Fcall_hook(Qpre_command_hook, Qnil, Qnil);

    if(rep_SYMBOLP(cmd) || (rep_CONSP(cmd) && rep_CAR(cmd) == Qlambda))
    {
	/* A named command; call it properly taking note of any interactive
	   declaration. */
	repv int_spec = interactive_spec(cmd);
	repv args = Qnil;
	repv *argsp = &args;
	rep_GC_root gc_cmd;
	bool clear_block = FALSE, goto_result = FALSE, set_before_goto = FALSE;

	if(int_spec == rep_NULL)
	{
	    Fsignal(Qerror, rep_list_2(rep_VAL(&not_command), cmd));
	    goto exit;
	}

	rep_PUSHGC(gc_cmd, cmd);
	if(rep_STRINGP(int_spec))
	{
	    u_char *spec_str = rep_STR(int_spec);
	    u_char c;
	    rep_GC_root gc_args;

	    /* Handle leading flags */
	    while(1)
	    {
		c = *spec_str;
		if(c == '*')
		{
		    /* Abort if buffer is read-only */
		    if(read_only_pos(curr_vw->vw_Tx, curr_vw->vw_CursorPos))
		    {
			rep_POPGC;
			goto exit;
		    }
		}
		else if(c == '-')
		    /* Clear block after building argument list */
		    clear_block = TRUE;
		else if(c == '@')
		    /* If result of command is a position, move the
		       cursor to it. */
		    goto_result = TRUE;
		else if(c == '!')
		    /* Set auto mark */
		    set_before_goto = TRUE;
		else
		    break;
		spec_str++;
	    }

	    rep_PUSHGC(gc_args, args);
	    while((c = *spec_str++) != 0)
	    {
		repv prompt, arg = Qnil;
		if(c != '\n')
		{
		    /* Non-null code. */
		    bool can_be_nil = FALSE;
		    if(*spec_str == '\n')
		    {
			/* no prompt */
			prompt = Qnil;
			spec_str++;
		    }
		    else
		    {
			/* copy the prompt */
			u_char *end = memchr(spec_str, '\n',
					     rep_STRING_LEN(int_spec) -
					     (spec_str - rep_STR(int_spec)));
			if(!end)
			    end = rep_STR(int_spec) + rep_STRING_LEN(int_spec);
			prompt = rep_string_dupn(spec_str, end - spec_str);
			if(memchr(spec_str, '%', end - spec_str))
			{
			    /* Format characters; format it. */
			    prompt = Fformat(Fcons(Qnil,
							 Fcons(prompt, args)));
			    if(!prompt || !rep_STRINGP(prompt))
				prompt = rep_string_dupn(spec_str, end - spec_str);
			}
			spec_str = *end ? end + 1 : end;
		    }
		    switch(c)
		    {
		    case 'a':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_function, Qt), prompt);
			break;
		    case 'b':
			arg = rep_call_lisp2(Fsymbol_value(Qprompt_for_buffer, Qt), prompt, Qt);
			break;
		    case 'B':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_buffer, Qt), prompt);
			break;
		    case 'c':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_char, Qt), prompt);
			break;
		    case 'C':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_command, Qt), prompt);
			break;
		    case 'd':
			arg = Fcursor_pos();
			break;
		    case 'D':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_directory, Qt), prompt);
			break;
		    case 'e':
			arg = Fcurrent_event();
			break;
		    case 'E':
			arg = Fcurrent_event_string();
			break;
		    case 'f':
			arg = rep_call_lisp2(Fsymbol_value(Qprompt_for_file, Qt), prompt, Qt);
			break;
		    case 'F':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_file, Qt), prompt);
			break;
		    case 'k':
			arg = rep_call_lisp1(Fsymbol_value(Qread_event, Qt), prompt);
			break;
		    case 'm':
		    case 'M':
			arg = (c == 'm') ? Fblock_start(Qnil)
			                 : Fblock_end(Qnil);
			if(!arg || rep_NILP(arg))
			{
			    arg = rep_NULL;
			    Fsignal(Qerror, rep_LIST_1(rep_VAL(&no_block)));
			}
			break;
		    case 'n':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_number, Qt), prompt);
			break;
		    case 'N':
			if(rep_NILP(Farg))
			    arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_number, Qt), prompt);
			else
			    arg = Fprefix_numeric_argument(Farg);
			break;
		    case 'p':
			arg = Fprefix_numeric_argument(Farg);
			break;
		    case 'P':
			arg = Farg;
			can_be_nil = TRUE;
			break;
		    case 's':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_string, Qt), prompt);
			break;
		    case 'S':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_symbol, Qt), prompt);
			can_be_nil = TRUE;
			break;
		    case 't':
			arg = Qt;
			break;
		    case 'v':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_variable, Qt), prompt);
			break;
		    case 'x':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_lisp, Qt), prompt);
			can_be_nil = TRUE;
			break;
		    case 'X':
			arg = rep_call_lisp1(Fsymbol_value(Qprompt_for_lisp, Qt), prompt);
			if(arg)
			    arg = Feval(arg);
			can_be_nil = TRUE;
			break;
		    default:
			arg = rep_NULL;
			Fsignal(Qinteractive, rep_list_2(cmd, int_spec));
		    }
		    if(!arg)
		    {
			args = rep_NULL;
			break;
		    }
		    if(!can_be_nil && rep_NILP(arg))
		    {
			Fsignal(Qerror, rep_list_2(rep_VAL(&nil_arg), cmd));
			args = rep_NULL;
			break;
		    }
		}
		/* Tack on this argument. */
		*argsp = Fcons(arg, Qnil);
		argsp = &rep_CDR(*argsp);
	    }
	    rep_POPGC;
	}
	else if(!rep_NILP(int_spec) && int_spec != Qt)
	    args = Feval(int_spec);
	if(clear_block)
	    Fblock_kill();
	/* Reinitialise current-prefix-arg, in case any functions called
	   to build the list of arguments overwrote it. */
	current_prefix_arg = Farg;
	if(args)
	{
	    if (rep_SYMBOLP(cmd))
		cmd = Fsymbol_value (cmd, Qt);
	    res = rep_funcall(cmd, args, FALSE);
	}
	rep_POPGC;

	if(goto_result && res != rep_NULL && POSP(res))
	{
	    if(set_before_goto)
	    {
		rep_GC_root gc_res;
		rep_PUSHGC(gc_res, res);
		rep_call_lisp0(Fsymbol_value(Qset_auto_mark, Qt));
		rep_POPGC;
	    }
	    Fgoto(res);
	}
    }
    else
    {
	/* Assume it's just an arbitrary Lisp form. */
	res = Feval(cmd);
    }
exit:
    Fcall_hook(Qpost_command_hook, Qnil, Qnil);

    last_command = this_command;
    this_command = Qnil;
    current_prefix_arg = Qnil;
    return(res);
}

DEFUN("prefix-numeric-argument", Fprefix_numeric_argument, Sprefix_numeric_argument, (repv arg), rep_Subr1) /*
::doc:Sprefix-numeric-argument::
prefix-numeric-argument ARG

Returns the numeric value of the raw prefix argument ARG.
::end:: */
{
    switch(rep_TYPE(arg))
    {
    case rep_Symbol:
	arg = rep_MAKE_INT(rep_NILP(arg) ? 1 : -1);
	break;
    case rep_Int:
	break;
    case rep_Cons:
	arg = rep_CAR(arg);
	break;
    default:
	arg = rep_MAKE_INT(1);
    }
    return(arg);
}

DEFUN("interactive", Finteractive, Sinteractive, (repv arg_list), rep_SF) /*
::doc:Sinteractive::
interactive CALLING-SPEC

This is a declaration used by the `call-command' function. For each Lisp
function which may be invoked as a command (interactively by the user) the
first *actual* form of the function (after the arguments and optional doc
string) must be an `interactive' declaration. For example,

(defun foo (bar)
  "An illustration"
  (interactive ...)
  ...

When called, the interactive special form just returns nil.

The CALLING-SPEC defines the arguments which are given to the command, it
can be either,

 1. nil -- no arguments are given to the function, this is just used to show
    that this function may be called as a command.

 2. A Lisp form -- it is evaluated and expected to provide a *list* of
    arguments which will be given to the function

 3. A string -- zero or more lines (separated by `\n'); each line tells
    how to get one argument. The first character of each line is a code
    letter, the rest of the line is an optional prompt-string which the
    user will see when entering the argument's value.

    The code letters available are,
	a	A function
	b	An existing buffer
	B	A buffer, it will be created if it doesn't exist
	c	A character
	C	A command
	d	The position of the cursor
	D	The name of a directory
	e	The event which caused this command
	E	The event which caused this command as a string
	f	The name of an existing file
	F	The name of a file
	k	An event
	m	The start position of the currently-marked block
	M	The end of the block
	n	A number
	N	The numeric prefix arg, or an entered number
	p	The numeric prefix arg
	P	The raw prefix arg
	s	A string
	S	A symbol
	t	The symbol `t'
	v	A variable
	x	A Lisp object
	X	A Lisp object, read then evaluated

    A null line produces an argument of nil.

    Any non-alphabetic characters at the beginning of the CALLING-SPEC
    are used as flags, the currently recognised flags are,

	*	If the active buffer is read-only an error will be signalled
	-	After building the argument list the block marked in the
		current window will be unmarked.
	@	If the result of the command-function is a position object,
		move the cursor to that position.
	!	Used in conjunction with the @ flag. When set, call the
		Lisp function `set-auto-mark' before moving to the new
		position.

Example usage,

    (interactive)			-- No arguments but function may
					   be called as a command
    (interactive "bBuffer to kill:")	-- One arg, an existing buffer
    (interactive "*\nxLisp form:\nt")	-- If not read-only, three arguments;
					   `nil', a lisp form and `t'.
::end:: */
{
    return(Qnil);
}

DEFUN("commandp", Fcommandp, Scommandp, (repv cmd), rep_Subr1) /*
::doc:Scommandp::
commandp COMMAND

Returns t if COMMAND may be called interactively.
::end:: */
{
    if(rep_SYMBOLP(cmd))
	cmd = Fsymbol_value(cmd, Qt);
    if (rep_FUNARGP(cmd))
	cmd = rep_FUNARG(cmd)->fun;
    if(!rep_VOIDP(cmd) && !rep_NILP(cmd))
    {
	if((((rep_TYPE(cmd) >= rep_Subr0) && (rep_TYPE(cmd) <= rep_SubrN))
	    && (rep_SUBR(cmd)->int_spec != rep_NULL))
	   || (rep_COMPILEDP(cmd) && !rep_NILP(rep_COMPILED_INTERACTIVE(cmd))))
	    return(Qt);
	else if(rep_CONSP(cmd))
	{
	    if(rep_CAR(cmd) == Qautoload)
	    {
		cmd = Fnth(rep_MAKE_INT(2), cmd);
		if(cmd != rep_NULL && !rep_NILP(cmd))
		    return(Qt);
	    }
	    else if(rep_CAR(cmd) == Qlambda)
	    {
		/* A lambda expression, test its first proper form. */
		cmd = Fnthcdr(rep_MAKE_INT(2), cmd);
		if(cmd == rep_NULL)
		    return rep_NULL;
		if(rep_CONSP(cmd)
		   && (rep_STRINGP(rep_CAR(cmd)) || rep_INTP(rep_CAR(cmd)))
		   && rep_CONSP(rep_CDR(cmd)))
		{
		    /* A doc-string */
		    cmd = rep_CDR(cmd);
		}
		if(rep_CONSP(cmd))
		{
		    cmd = rep_CAR(cmd);
		    if(rep_CONSP(cmd)
		       && (rep_CAR(cmd) == Qinteractive))
		    {
			return(Qt);
		    }
		}
	    }
	}
    }
    return(Qnil);
}
    
void
commands_init(void)
{
    /* Create the function symbols. */
    rep_INTERN(prompt_for_function);
    rep_INTERN(prompt_for_buffer);
    rep_INTERN(prompt_for_char);
    rep_INTERN(prompt_for_command);
    rep_INTERN(prompt_for_directory);
    rep_INTERN(prompt_for_file);
    rep_INTERN(prompt_for_number);
    rep_INTERN(prompt_for_string);
    rep_INTERN(prompt_for_symbol);
    rep_INTERN(prompt_for_variable);
    rep_INTERN(prompt_for_lisp);
    rep_INTERN(read_event);
    rep_INTERN(set_auto_mark);

    rep_INTERN(interactive); rep_ERROR(interactive);

    prefix_arg = current_prefix_arg = Qnil;
    rep_mark_static(&prefix_arg);
    rep_mark_static(&current_prefix_arg);

    this_command = last_command = Qnil;
    rep_mark_static(&this_command);
    rep_mark_static(&last_command);

    rep_INTERN_SPECIAL(pre_command_hook);
    rep_INTERN_SPECIAL(post_command_hook);

    rep_ADD_SUBR(Sthis_command);
    rep_ADD_SUBR(Slast_command);
    rep_ADD_SUBR(Sprefix_arg);
    rep_ADD_SUBR(Scurrent_prefix_arg);
    rep_ADD_SUBR_INT(Scall_command);
    rep_ADD_SUBR(Sprefix_numeric_argument);
    rep_ADD_SUBR(Sinteractive);
    rep_ADD_SUBR(Scommandp);
}
