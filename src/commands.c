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
#include "jade_protos.h"

#include <string.h>

_PR void commands_init(void);

/* Symbols of the Lisp functions called to get input. */
static VALUE sym_prompt_for_function, sym_prompt_for_buffer,
    sym_prompt_for_char, sym_prompt_for_command, sym_prompt_for_directory,
    sym_prompt_for_file, sym_prompt_for_number, sym_prompt_for_string,
    sym_prompt_for_symbol, sym_prompt_for_variable, sym_prompt_for_lisp,
    sym_read_event;

static VALUE sym_interactive;

/* Prefix argument for the next command and the current command. */
static VALUE prefix_arg, current_prefix_arg;

/* Command being executed and command last executed. */
_PR VALUE this_command, last_command;
VALUE this_command, last_command;

/* hooks.
::doc:pre_command_hook::
Hook called before evaluating each command.
::end::
::doc:post_command_hook::
Hook callled after evaluating each command.
::end:: */
_PR VALUE sym_pre_command_hook, sym_post_command_hook;
VALUE sym_pre_command_hook, sym_post_command_hook;

_PR VALUE var_this_command(VALUE val);
DEFUN("this-command", var_this_command, subr_this_command, (VALUE val), V_Var, DOC_this_command) /*
::doc:this_command::
This variable holds the command currently being evaluated, or nil if no
command is active. The `command' is whatever is being evaluated; it could
be a function, a form or even a list of forms (from a menu).
::end:: */
{
    if(val)
	this_command = val;
    return(this_command);
}

_PR VALUE var_last_command(VALUE val);
DEFUN("last-command", var_last_command, subr_last_command, (VALUE val), V_Var, DOC_last_command) /*
::doc:last_command::
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

_PR VALUE var_prefix_arg(VALUE val);
DEFUN("prefix-arg", var_prefix_arg, subr_prefix_arg, (VALUE val), V_Var, DOC_prefix_arg) /*
::doc:prefix_arg::
Value of the prefix argument for the next command.
::end:: */
{
    if(val)
	prefix_arg = val;
    return(prefix_arg);
}

_PR VALUE var_current_prefix_arg(VALUE val);
DEFUN("current-prefix-arg", var_current_prefix_arg, subr_current_prefix_arg, (VALUE val), V_Var, DOC_current_prefix_arg) /*
::doc:current_prefix_arg::
Value of the prefix argument for the current command.
::end:: */
{
    if(val)
	current_prefix_arg = val;
    return(current_prefix_arg);
}

/* Search the definition of the command CMD for an interactive calling
   spec. Return it or NULL. */
static VALUE
interactive_spec(VALUE cmd)
{
    VALUE fun, spec = NULL;
    if(SYMBOLP(cmd))
	fun = cmd_symbol_function(cmd, sym_t);
    else
	fun = cmd;
    if(!VOIDP(fun) && !NILP(fun))
    {
	if((VTYPE(fun) >= V_Subr0) && (VTYPE(fun) <= V_SubrN))
	    spec = VSUBR(fun)->subr_IntSpec;
	else if(CONSP(fun))
	{
	    if(VCAR(fun) == sym_autoload)
	    {
		VALUE tmp = move_down_list(fun, 2);
		if(CONSP(tmp) && !NILP(VCAR(tmp)))
		{
		    GCVAL gcv_cmd;
		    PUSHGC(gcv_cmd, cmd);
		    fun = load_autoload(cmd, fun);
		    POPGC;
		    if(!fun || !CONSP(fun))
			return(NULL);
		}
		else
		    return(NULL);
	    }
	    if(VCAR(fun) == sym_lambda)
	    {
		/* A lambda expression, test its first proper form. */
		fun = move_down_list(fun, 2);
		if(fun && CONSP(fun)
		   && (STRINGP(VCAR(fun)) || NUMBERP(VCAR(fun)))
		   && CONSP(VCDR(fun)))
		{
		    /* A doc-string */
		    fun = VCDR(fun);
		}
		if(fun && CONSP(fun))
		{
		    fun = VCAR(fun);
		    if(CONSP(fun)
		       && (VCAR(fun) == sym_interactive))
		    {
			/* got it. */
			spec = CONSP(VCDR(fun)) ? VCAR(VCDR(fun)) : sym_nil;
		    }
		}
	    }
	}
    }
    return(spec);
}

_PR VALUE cmd_call_command(VALUE cmd, VALUE arg);
DEFUN_INT("call-command", cmd_call_command, subr_call_command, (VALUE cmd, VALUE cmd_arg), V_Subr2, DOC_call_command, "CEnter command:\nP") /*
::doc:call_command::
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
    VALUE res = NULL;
    this_command = cmd;
    if(last_command == sym_t)
	undo_distinct();		/* last was an insertion */
    undo_new_group();

    /* Move the prefix arg. */
    if(NILP(cmd_arg))
	cmd_arg = prefix_arg;
    prefix_arg = sym_nil;
    current_prefix_arg = cmd_arg;

    cmd_eval_hook2(sym_pre_command_hook, cmd);

    if(SYMBOLP(cmd) || (CONSP(cmd) && VCAR(cmd) == sym_lambda))
    {
	/* A named command; call it properly taking note of any interactive
	   declaration. */
	VALUE int_spec = interactive_spec(cmd);
	VALUE args = sym_nil;
	VALUE *argsp = &args;
	GCVAL gcv_cmd;
	bool clear_block = FALSE;
	if(int_spec == NULL)
	{
	    cmd_signal(sym_error, list_2(MKSTR("Not a command"), cmd));
	    goto exit;
	}
	PUSHGC(gcv_cmd, cmd);
	if(STRINGP(int_spec))
	{
	    u_char *spec_str = VSTR(int_spec);
	    u_char c;
	    GCVAL gcv_args;
	    while(1)
	    {
		/* check for read-only flag */
		if(*spec_str == '*')
		{
		    if(read_only(curr_vw->vw_Tx))
		    {
			POPGC;
			goto exit;
		    }
		    else
			spec_str++;
		}
		else if(*spec_str == '-')
		{
		    /* clear block after building args. */
		    clear_block = TRUE;
		    spec_str++;
		}
		else
		    break;
	    }
	    PUSHGC(gcv_args, args);
	    while((c = *spec_str++) != 0)
	    {
		VALUE prompt, arg = sym_nil;
		if(c != '\n')
		{
		    /* Non-null code. */
		    bool can_be_nil = FALSE;
		    if(*spec_str == '\n')
		    {
			/* no prompt */
			prompt = sym_nil;
			spec_str++;
		    }
		    else
		    {
			/* copy the prompt */
			u_char *end = memchr(spec_str, '\n',
					     STRING_LEN(int_spec) -
					     (spec_str - VSTR(int_spec)));
			if(!end)
			    end = VSTR(int_spec) + STRING_LEN(int_spec);
			prompt = string_dupn(spec_str, end - spec_str);
			if(memchr(spec_str, '%', end - spec_str))
			{
			    /* Format characters; format it. */
			    prompt = cmd_format(cmd_cons(sym_nil,
							 cmd_cons(prompt, args)));
			    if(!prompt || !STRINGP(prompt))
				prompt = string_dupn(spec_str, end - spec_str);
			}
			spec_str = *end ? end + 1 : end;
		    }
		    switch(c)
		    {
		    case 'a':
			arg = call_lisp1(sym_prompt_for_function, prompt);
			break;
		    case 'b':
			arg = call_lisp2(sym_prompt_for_buffer, prompt, sym_t);
			break;
		    case 'B':
			arg = call_lisp1(sym_prompt_for_buffer, prompt);
			break;
		    case 'c':
			arg = call_lisp1(sym_prompt_for_char, prompt);
			break;
		    case 'C':
			arg = call_lisp1(sym_prompt_for_command, prompt);
			break;
		    case 'd':
			arg = cmd_cursor_pos();
			break;
		    case 'D':
			arg = call_lisp1(sym_prompt_for_directory, prompt);
			break;
		    case 'e':
			arg = cmd_current_event();
			break;
		    case 'E':
			arg = cmd_current_event_string();
			break;
		    case 'f':
			arg = call_lisp2(sym_prompt_for_file, prompt, sym_t);
			break;
		    case 'F':
			arg = call_lisp1(sym_prompt_for_file, prompt);
			break;
		    case 'k':
			arg = call_lisp1(sym_read_event, prompt);
			break;
		    case 'm':
		    case 'M':
			arg = (c == 'm') ? cmd_block_start(sym_nil)
			                 : cmd_block_end(sym_nil);
			if(!arg || NILP(arg))
			{
			    arg = NULL;
			    cmd_signal(sym_error,
				       LIST_1(MKSTR("No block marked")));
			}
			break;
		    case 'n':
			arg = call_lisp1(sym_prompt_for_number, prompt);
			break;
		    case 'N':
			if(NILP(cmd_arg))
			    arg = call_lisp1(sym_prompt_for_number, prompt);
			else
			    arg = cmd_prefix_numeric_argument(cmd_arg);
			break;
		    case 'p':
			arg = cmd_prefix_numeric_argument(cmd_arg);
			break;
		    case 'P':
			arg = cmd_arg;
			can_be_nil = TRUE;
			break;
		    case 's':
			arg = call_lisp1(sym_prompt_for_string, prompt);
			break;
		    case 'S':
			arg = call_lisp1(sym_prompt_for_symbol, prompt);
			can_be_nil = TRUE;
			break;
		    case 't':
			arg = sym_t;
			break;
		    case 'v':
			arg = call_lisp1(sym_prompt_for_variable, prompt);
			break;
		    case 'x':
			arg = call_lisp1(sym_prompt_for_lisp, prompt);
			can_be_nil = TRUE;
			break;
		    case 'X':
			arg = call_lisp1(sym_prompt_for_lisp, prompt);
			if(arg)
			    arg = cmd_eval(arg);
			can_be_nil = TRUE;
			break;
		    default:
			arg = NULL;
			cmd_signal(sym_interactive, list_2(cmd, int_spec));
		    }
		    if(!arg)
		    {
			args = NULL;
			break;
		    }
		    if(!can_be_nil && NILP(arg))
		    {
			cmd_signal(sym_error,
				   list_2(MKSTR("Nil argument to command"),
					  cmd));
			args = NULL;
			break;
		    }
		}
		/* Tack on this argument. */
		*argsp = cmd_cons(arg, sym_nil);
		argsp = &VCDR(*argsp);
	    }
	    POPGC;
	}
	else if(int_spec != sym_t)
	    args = cmd_eval(int_spec);
	if(clear_block)
	    cmd_block_kill();
	if(args)
	    res = funcall(cmd, args);
	POPGC;
    }
    else
	res = cmd_eval(cmd);
 exit:
    cmd_eval_hook2(sym_post_command_hook, cmd);

    last_command = this_command;
    /* This is in here so it can tell if the last binding was actually
       a command. */
    undo_distinct();
    this_command = sym_nil;
    current_prefix_arg = sym_nil;
    return(res);
}

_PR VALUE cmd_prefix_numeric_argument(VALUE arg);
DEFUN("prefix-numeric-argument", cmd_prefix_numeric_argument, subr_prefix_numeric_argument, (VALUE arg), V_Subr1, DOC_prefix_numeric_argument) /*
::doc:prefix_numeric_argument::
prefix-numeric-argument ARG

Returns the numeric value of the raw prefix argument ARG.
::end:: */
{
    switch(VTYPE(arg))
    {
    case V_Symbol:
	arg = make_number(NILP(arg) ? 1 : -1);
	break;
    case V_Number:
	break;
    case V_Cons:
	arg = VCAR(arg);
	break;
    default:
	arg = make_number(1);
    }
    return(arg);
}

_PR VALUE cmd_interactive(VALUE spec);
DEFUN("interactive", cmd_interactive, subr_interactive, (VALUE arg_list), V_SF, DOC_interactive) /*
::doc:interactive::
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

Example usage,

    (interactive)			-- No arguments but function may
					   be called as a command
    (interactive "bBuffer to kill:")	-- One arg, an existing buffer
    (interactive "*\nxLisp form:\nt")	-- If not read-only, three arguments;
					   `nil', a lisp form and `t'.
::end:: */
{
    return(sym_nil);
}

_PR VALUE cmd_commandp(VALUE cmd);
DEFUN("commandp", cmd_commandp, subr_commandp, (VALUE cmd), V_Subr1, DOC_commandp) /*
::doc:commandp::
commandp COMMAND

Returns t if COMMAND may be called interactively.
::end:: */
{
    if(SYMBOLP(cmd))
	cmd = cmd_symbol_function(cmd, sym_t);
    if(!VOIDP(cmd) && !NILP(cmd))
    {
	if(((VTYPE(cmd) >= V_Subr0) && (VTYPE(cmd) <= V_SubrN))
	   && (VSUBR(cmd)->subr_IntSpec != NULL))
	    return(sym_t);
	else if(CONSP(cmd))
	{
	    if(VCAR(cmd) == sym_autoload)
	    {
		cmd = find_member_by_index(cmd, 3);
		if(!NILP(cmd))
		    return(sym_t);
	    }
	    else if(VCAR(cmd) == sym_lambda)
	    {
		/* A lambda expression, test its first proper form. */
		cmd = move_down_list(cmd, 2);
		if(CONSP(cmd)
		   && (STRINGP(VCAR(cmd)) || NUMBERP(VCAR(cmd)))
		   && CONSP(VCDR(cmd)))
		{
		    /* A doc-string */
		    cmd = VCDR(cmd);
		}
		if(CONSP(cmd))
		{
		    cmd = VCAR(cmd);
		    if(CONSP(cmd)
		       && (VCAR(cmd) == sym_interactive))
		    {
			return(sym_t);
		    }
		}
	    }
	}
    }
    return(sym_nil);
}
    
void
commands_init(void)
{
    /* Create the function symbols. */
    INTERN(sym_prompt_for_function, "prompt-for-function");
    INTERN(sym_prompt_for_buffer, "prompt-for-buffer");
    INTERN(sym_prompt_for_char, "prompt-for-char");
    INTERN(sym_prompt_for_command, "prompt-for-command");
    INTERN(sym_prompt_for_directory, "prompt-for-directory");
    INTERN(sym_prompt_for_file, "prompt-for-file");
    INTERN(sym_prompt_for_number, "prompt-for-number");
    INTERN(sym_prompt_for_string, "prompt-for-string");
    INTERN(sym_prompt_for_symbol, "prompt-for-symbol");
    INTERN(sym_prompt_for_variable, "prompt-for-variable");
    INTERN(sym_prompt_for_lisp, "prompt-for-lisp");
    INTERN(sym_read_event, "read-event");

    INTERN(sym_interactive, "interactive");
    cmd_put(sym_interactive, sym_error_message,
	    MKSTR("Bad interactive specification"));

    prefix_arg = current_prefix_arg = sym_nil;
    mark_static(&prefix_arg);
    mark_static(&current_prefix_arg);

    this_command = last_command = sym_nil;
    mark_static(&this_command);
    mark_static(&last_command);

    INTERN(sym_pre_command_hook, "pre-command-hook");
    DOC_VAR(sym_pre_command_hook, DOC_pre_command_hook);
    INTERN(sym_post_command_hook, "post-command-hook");
    DOC_VAR(sym_post_command_hook, DOC_post_command_hook);

    ADD_SUBR(subr_this_command);
    ADD_SUBR(subr_last_command);
    ADD_SUBR(subr_prefix_arg);
    ADD_SUBR(subr_current_prefix_arg);
    ADD_SUBR(subr_call_command);
    ADD_SUBR(subr_prefix_numeric_argument);
    ADD_SUBR(subr_interactive);
    ADD_SUBR(subr_commandp);
}
