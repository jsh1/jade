/* mac_runloop.m -- event loop integration for Mac OS X
   Copyright (C) 1999-2007 John Harper <jsh@unfactored.org>

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
#include "mac_internal.h"
#include <pthread.h>

/* adapted from rep-gtk.c. */

struct input_data {
    struct input_data *next;
    int fd;
    void (*func)(int);
    CFRunLoopSourceRef source;
};

struct timeout_data {
    struct timeout_data *next;
    int timed_out;
    int idle_counter;
    u_long this_timeout_msecs;
    u_long actual_timeout_msecs;
    CFRunLoopTimerRef timer;
};

static int input_pipe[2];
static fd_set input_set;
static struct input_data *inputs;
static pthread_mutex_t input_mutex = PTHREAD_MUTEX_INITIALIZER;

static struct timeout_data *context;

static void *
input_thread (void *arg)
{
    while (1)
    {
	fd_set copy;
	int err;
	struct input_data *d;

	FD_COPY (&copy, &input_set);

	err = select (FD_SETSIZE, &copy, NULL, NULL, NULL);
	if (err == -1)
	    return NULL;

	if (err > 0 && FD_ISSET (input_pipe[0], &copy))
	{
	    char c;
	    read (input_pipe[0], &c, 1);
	    err--;
	}

	pthread_mutex_lock (&input_mutex);

	for (d = inputs; err > 0 && d != NULL; d = d->next)
	{
	    if (FD_ISSET (d->fd, &copy))
	    {
		CFRunLoopSourceSignal (d->source);
		err--;
	    }
	}

	pthread_mutex_unlock (&input_mutex);
    }
}

static repv
inner_input_callback (repv data_)
{
    struct input_data *data = (struct input_data *) rep_PTR (data_);
    (*data->func) (data->fd);
    return Qnil;
}

static void
mac_source_perform (void *info)
{
    struct input_data *d = info;

    rep_call_with_barrier (inner_input_callback, rep_VAL(d), rep_TRUE, 0, 0, 0);

    mac_callback_postfix ();
}

static void
mac_register_input_fd (int fd, void (*callback)(int fd))
{
    struct input_data *d;
    CFRunLoopSourceContext ctx;
    pthread_attr_t attr;
    pthread_t tid;
    char c = 1;

    if (callback == 0)
	return;

    d = calloc (1, sizeof (*d));

    memset (&ctx, 0, sizeof (ctx));
    ctx.version = 0;
    ctx.info = d;
    ctx.perform = mac_source_perform;

    d->fd = fd;
    d->func = callback;
    d->source = CFRunLoopSourceCreate (NULL, 0, &ctx);

    FD_SET (fd, &input_set);

    pthread_mutex_lock (&input_mutex);

    d->next = inputs;
    inputs = d;

    if (input_pipe[0] == 0)
    {
	pipe (input_pipe);
	pthread_attr_init (&attr);
	pthread_attr_setscope (&attr, PTHREAD_SCOPE_SYSTEM);
	pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
	pthread_create (&tid, &attr, input_thread, NULL);
	pthread_attr_destroy (&attr);
    }

    write (input_pipe[1], &c, 1);

    pthread_mutex_unlock (&input_mutex);

    CFRunLoopAddSource (CFRunLoopGetCurrent (), d->source,
			kCFRunLoopCommonModes);
}

static void
mac_deregister_input_fd (int fd)
{
    struct input_data **ptr, *d;
    char c = 1;

    pthread_mutex_lock (&input_mutex);

    for (ptr = &inputs; (d = *ptr) != 0; ptr = &d->next)
    {
	if (d->fd != fd)
	    continue;
	*ptr = d->next;
	CFRunLoopRemoveSource (CFRunLoopGetCurrent (), d->source,
			       kCFRunLoopCommonModes);
	free (d);
	FD_CLR (fd, &input_set);
	write (input_pipe[1], &c, 1);
	break;
    }

    pthread_mutex_unlock (&input_mutex);
}

static void
timer_callback (CFRunLoopTimerRef timer, void *info)
{
    struct timeout_data *d = info;

    d->timed_out = 1;

    /* Only quit if we'd return to the correct event loop */
    if (context == d)
	[NSApp stop:nil];
}

static void
unset_timeout (void)
{
    if (context != 0 && context->timer != 0)
    {
	CFRunLoopTimerSetNextFireDate (context->timer,
				       CFAbsoluteTimeGetCurrent ()
				       + 100. * 365. * 24. * 60. * 60);
    }
}

static void
remove_timeout (void)
{
    if (context != 0 && context->timer != 0)
    {
	CFRunLoopRemoveTimer (CFRunLoopGetCurrent (), context->timer,
			      kCFRunLoopCommonModes);
	CFRelease (context->timer);
	context->timer = 0;
    }
}

static void
set_timeout (void)
{
    if (context != 0 && !context->timed_out)
    {
	u_long max_sleep = rep_max_sleep_for ();

	context->this_timeout_msecs = rep_input_timeout_secs * 1000;
	context->actual_timeout_msecs = MIN (context->this_timeout_msecs,
					     max_sleep);

	CFAbsoluteTime abs_t = (CFAbsoluteTimeGetCurrent ()
				+ context->actual_timeout_msecs * 1000);

	if (context->timer == 0)
	{
	    CFRunLoopTimerContext ctx;
	    memset (&ctx, 0, sizeof (ctx));
	    ctx.info = context;
	    context->timer = CFRunLoopTimerCreate (NULL, abs_t, 100. * 365.
						   * 24. * 60. * 60, 0, 0,
						   timer_callback, &ctx);
	}
	else
	    CFRunLoopTimerSetNextFireDate (context->timer, abs_t);
    }
}

/* Call this after executing any callbacks that could invoke Lisp code */

void
mac_callback_postfix (void)
{
    unset_timeout ();

    if (rep_INTERRUPTP && context != 0)
	[NSApp stop:nil];
    else if (rep_redisplay_fun != 0)
	(*rep_redisplay_fun)();

    if (context != 0)
    {
	context->timed_out = 0;
	set_timeout ();
	context->idle_counter = 0;
    }
}

/* This function replaces the standard rep event loop. */

static repv
mac_event_loop (void)
{
    struct timeout_data data;

    memset (&data, 0, sizeof (data));
    data.next = context;
    context = &data;

    while (1)
    {
	if (rep_redisplay_fun != 0)
	    (*rep_redisplay_fun)();

	data.timed_out = 0;
	set_timeout ();

	[NSApp run];

	unset_timeout ();
	if (data.timed_out)
	{
	    if (data.actual_timeout_msecs < data.this_timeout_msecs)
	    {
		Fthread_suspend (Qnil, rep_MAKE_INT (data.this_timeout_msecs
						     - data.actual_timeout_msecs));
	    }
	    else
		rep_on_idle (data.idle_counter++);
	}

	rep_proc_periodically ();

	/* Check for exceptional conditions. */
	if(rep_throw_value != rep_NULL)
	{
	    repv result;
	    if(rep_handle_input_exception (&result))
	    {
		remove_timeout ();
		context = data.next;
		/* reset the timeout for any containing event loop */
		set_timeout ();
		return result;
	    }
	}

#ifdef C_ALLOCA
	/* Using the C implementation of alloca. So garbage collect
	   anything below the current stack depth. */
	alloca(0);
#endif
    }
}

/* Called by librep/src/unix_processes.c whenever SIGCHLD is received
   (from the signal handler) */

static void
mac_sigchld_callback (void)
{
#if FIXME
    /* Set up another pipe for this, we can't call [NSApp stop] from a
       signal handler. */

    if (context != 0)
	[NSApp stop];
#endif
}

void
mac_runloop_init (void)
{
  rep_register_input_fd_fun = mac_register_input_fd;
  rep_deregister_input_fd_fun = mac_deregister_input_fd;
  rep_map_inputs (mac_register_input_fd);
  rep_event_loop_fun = mac_event_loop;
  rep_sigchld_fun = mac_sigchld_callback;
}
