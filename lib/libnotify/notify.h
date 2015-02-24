/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __LIBNOTIFY__
#define __LIBNOTIFY__

#include "libstd/nexus.h"

/*! Type of the relevant data inside of a NVIC-invoked
 * hardware-triggered ISR. This is used to help unwrap the data from
 * inside of a \notification union. */
typedef enum {
    NOTIFY_INT,
    NOTIFY_DOUBLE,
    NOTIFY_CHAR,
    NOTIFY_STRING,
} NOTIFICATION_TYPE;

/*! Capable of wrapping any type of incoming external information
 * (delivered in an interrupt) that libhw will notify running
 * processes about. This allows the processes' pseudo-ISRs to have a
 * consistent protytype, and a simplified notification-passing
 * system. */
typedef union {
    int    _int;
    double _double;
    char   _char;
    char   _string[16];
} notification;

/*! Metadata associated with a notification from a general thread to
 *  libhw for transmission to a libhw-managed hardware device. */
typedef struct {
    memory_address_t base;
    memory_address_t pin;
    NOTIFICATION_TYPE type;
    notification** data;
} notification_metadata;

#endif
