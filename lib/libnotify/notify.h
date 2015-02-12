/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __LIBNOTIFY__
#define __LIBNOTIFY__

/* Currently responsible for notification data structures, not the act
 * of notification. */

/* TODO: doxygenize */

typedef enum  {

    NOTIFY_INT,
    NOTIFY_DOUBLE,
    NOTIFY_CHAR,
    NOTIFY_STRING,
} HW_NOTIFICATION_TYPE;

typedef union  {

    int    _int;
    double _double;
    char   _char;
    char   _string[16];
} hw_notification;

#endif
