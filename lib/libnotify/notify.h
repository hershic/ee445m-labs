/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __LIBNOTIFY__
#define __LIBNOTIFY__

/** Type of the relevant data inside of a NVIC-invoked
 * hardware-triggered ISR. This is used to help unwrap the data from
 * inside of a \hw_notification union. */
typedef enum  {

    NOTIFY_INT,
    NOTIFY_DOUBLE,
    NOTIFY_CHAR,
    NOTIFY_STRING,
} HW_NOTIFICATION_TYPE;

/** Capable of wrapping any type of incoming external information
 * (delivered in an interrupt) that libhw will notify running
 * processes about. This allows the processes' pseudo-ISRs to have a
 * consistent protytype, and a simplified notification-passing
 * system. */
typedef union  {

    int    _int;
    double _double;
    char   _char;
    char   _string[16];
} hw_notification;

#endif
