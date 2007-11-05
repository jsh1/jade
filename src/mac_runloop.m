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
    bool pending :1;
};

struct timeout_data {
    struct timeout_data *next;
    int timed_out;
    int idle_counter;
    u_long this_timeout_msecs;
    u_long actual_timeout_msecs;
    CFRunLoopTimerRef timer;
};

CFRunLoopObserverRef observer;

static int input_pipe[2];
static fd_set input_set;
static struct input_data *inputs;
static pthread_mutex_t input_mutex = PTHREAD_MUTEX_INITIALIZER;

static int sigchld_pipe[2];
static CFRunLoopSourceRef sigchld_source;

static int waiting_for_input;
static NSEvent *pending_event;
static JadeView *pending_event_view;

static struct timeout_data *context;

static void mac_deliver_events (void);

static void *
input_thread (void *arg)
{
    pthread_mutex_lock (&input_mutex);

    while (1)
    {
	fd_set copy;
	int err;
	struct input_data *d;
	char c;

	FD_COPY (&input_set, &copy);
	FD_SET (input_pipe[0], &copy);
	FD_SET (sigchld_pipe[0], &copy);

	pthread_mutex_unlock (&input_mutex);

	err = select (FD_SETSIZE, &copy, NULL, NULL, NULL);

	/* We may get EBADF errors if the fd-set changed while we were
	   blocked, but that's okay, we'll be in sync with the main
	   thread next time we wait. All the other possible errors
	   should be harmless also. */

	pthread_mutex_lock (&input_mutex);

	if (err > 0 && FD_ISSET (input_pipe[0], &copy))
	{
	    read (input_pipe[0], &c, 1);
	    err--;
	}

	if (err > 0 && FD_ISSET (sigchld_pipe[0], &copy))
	{
	    read (sigchld_pipe[0], &c, 1);
	    CFRunLoopSourceSignal (sigchld_source);
	    err--;
	}

	for (d = inputs; err > 0 && d != NULL; d = d->next)
	{
	    if (FD_ISSET (d->fd, &copy))
	    {
		/* FIXME: we should really remove this from our fd set
		   until we know the main thread has serviced the
		   runloop source. Otherwise we'll keep spinning and
		   signalling.. */

		CFRunLoopSourceSignal (d->source);
		err--;
	    }
	}
    }

    /* not reached */
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

    if (waiting_for_input != 0)
    {
	d->pending = true;
	[NSApp stop:nil];
    }
    else
    {
	d->pending = false;
	rep_call_with_barrier (inner_input_callback,
			       rep_VAL(d), rep_TRUE, 0, 0, 0);
    }
}

static void
mac_register_input_fd (int fd, void (*callback)(int fd))
{
    struct input_data *d;
    CFRunLoopSourceContext ctx;
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

    pthread_mutex_lock (&input_mutex);

    d->next = inputs;
    inputs = d;

    FD_SET (fd, &input_set);

    pthread_mutex_unlock (&input_mutex);

    CFRunLoopAddSource (CFRunLoopGetCurrent (), d->source,
			kCFRunLoopCommonModes);

    write (input_pipe[1], &c, 1);
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
	CFRelease (d->source);
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

    /* Only quit if we'd return to the correct event loop.
       FIXME: this doesn't do anything until an event arrives? */

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
set_timeout (u_long timeout_msecs)
{
    if (context != 0 && !context->timed_out)
    {
	u_long max_sleep = rep_max_sleep_for ();

	context->this_timeout_msecs = timeout_msecs;
	context->actual_timeout_msecs = MIN (context->this_timeout_msecs,
					     max_sleep);

	CFAbsoluteTime abs_t = (CFAbsoluteTimeGetCurrent ()
				+ context->actual_timeout_msecs / 1000.);

	if (context->timer == 0)
	{
	    CFRunLoopTimerContext ctx;
	    memset (&ctx, 0, sizeof (ctx));
	    ctx.info = context;
	    context->timer = CFRunLoopTimerCreate (NULL, abs_t, 100. * 365.
						   * 24. * 60. * 60, 0, 0,
						   timer_callback, &ctx);
	    CFRunLoopAddTimer (CFRunLoopGetCurrent (), context->timer,
			       kCFRunLoopCommonModes);
	}
	else
	    CFRunLoopTimerSetNextFireDate (context->timer, abs_t);
    }
}

/* This function replaces the standard rep event loop. */

static repv
mac_event_loop (void)
{
    struct timeout_data data;
    struct input_data *d;

    memset (&data, 0, sizeof (data));
    data.next = context;
    context = &data;

    while (1)
    {
	mac_deliver_events ();

	for (d = inputs; d != NULL; d = d->next)
	{
	    if (d->pending)
	    {
		d->pending = false;
		rep_call_with_barrier (inner_input_callback,
				       rep_VAL(d), rep_TRUE, 0, 0, 0);
	    }
	}

	if (rep_redisplay_fun != 0)
	    (*rep_redisplay_fun) ();

	data.timed_out = 0;
	set_timeout (rep_input_timeout_secs * 1000);
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
		set_timeout (rep_input_timeout_secs * 1000);
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

/* called by the view when it receives events. Makes sure that cancels
   any calls to wait_for_input. */

bool
mac_defer_event (void *view, void *ns_event)
{
    if (waiting_for_input == 0)
	return false;

    [pending_event release];
    [pending_event_view release];

    pending_event = [(id)ns_event copy];
    pending_event_view = [(id)view retain];

    [NSApp stop:nil];

    return true;
}

static void
mac_deliver_events (void)
{
    if (pending_event != nil)
    {
	NSEvent *ev = pending_event;
	JadeView *view = pending_event_view;

	pending_event = nil;
	pending_event_view = nil;

	[view handleEvent:ev];
	[ev release];
	[view release];
    }
}

static int
mac_wait_for_input (fd_set *fds, u_long timeout_msecs)
{
    struct timeout_data data;
    struct input_data *d;
    int count;

    ++waiting_for_input;

    memset (&data, 0, sizeof (data));
    data.next = context;
    context = &data;

    set_timeout (timeout_msecs);
    [NSApp run];
    remove_timeout ();

    context = data.next;

    --waiting_for_input;

    count = 0;

    if (!data.timed_out)
    {
	for (d = inputs; d != NULL; d = d->next)
	{
	    if (!FD_ISSET (d->fd, fds))
		continue;
	    if (d->pending)
		count++;
	    else
		FD_CLR (d->fd, fds);
	}
    }

    return count;
}

static void
observer_callback (CFRunLoopObserverRef observer,
		   CFRunLoopActivity activity, void *info)
{
    if (rep_INTERRUPTP && context != 0)
    {
	[NSApp stop:nil];
	return;
    }

    Fredisplay (Qnil);

    if (context != 0)
    {
	context->timed_out = 0;
	set_timeout (rep_input_timeout_secs * 1000);
	context->idle_counter = 0;
    }
}

static void
mac_sigchld_handler (void *info)
{
    if (context != 0)
	[NSApp stop:nil];
}

/* Called by librep/src/unix_processes.c whenever SIGCHLD is received
   (from the signal handler) */

static void
mac_sigchld_callback (void)
{
    char c = 1;
    write (sigchld_pipe[1], &c, 1);
}

void
mac_runloop_init (void)
{
    CFRunLoopSourceContext ctx;
    pthread_attr_t attr;
    pthread_t tid;

    observer = CFRunLoopObserverCreate (NULL, kCFRunLoopBeforeWaiting
					| kCFRunLoopExit, true, 2000000,
					observer_callback, NULL);
    CFRunLoopAddObserver (CFRunLoopGetCurrent (),
			  observer, kCFRunLoopCommonModes);

    pipe (input_pipe);
    pipe (sigchld_pipe);

    memset (&ctx, 0, sizeof (ctx));
    ctx.perform = mac_sigchld_handler;
    sigchld_source = CFRunLoopSourceCreate (NULL, 0, &ctx);
    CFRunLoopAddSource (CFRunLoopGetCurrent (),
			sigchld_source, kCFRunLoopCommonModes);

    pthread_attr_init (&attr);
    pthread_attr_setscope (&attr, PTHREAD_SCOPE_SYSTEM);
    pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
    pthread_create (&tid, &attr, input_thread, NULL);
    pthread_attr_destroy (&attr);

    rep_register_input_fd_fun = mac_register_input_fd;
    rep_deregister_input_fd_fun = mac_deregister_input_fd;
    rep_map_inputs (mac_register_input_fd);
    rep_event_loop_fun = mac_event_loop;
    rep_wait_for_input_fun = mac_wait_for_input;
    rep_sigchld_fun = mac_sigchld_callback;
}
