/* -*- mode: c; c-basic-offset: 4; -*- */

/* todo: doxygenize */
/* todo: nail down what a task is and a thread. I have a hunch these
 * are just tasks. confer with Hershal and also consider renaming this
 * to libtask. */

#ifndef __THREAD__
#define __THREAD__

/* These are the suffixes (prefixes are task names) that guarantee a
 * task can be considered a task */
#define THREAD_SPAWN      _spawn
#define THREAD_REPRESENT  _represent
#define THREAD_KILL       _kill

#define THREAD_MAKE_TASK

#endif
