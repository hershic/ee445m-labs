/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __LIBNOTIFY__
#define __LIBNOTIFY__

#include "libstd/nexus.h"

/*! Type of the relevant data inside of a NVIC-invoked
 * hardware-triggered ISR. This is used to help unwrap the data from
 * inside of a \notification union. */
typedef enum {
    NOTIFY_INT,
    NOTIFY_UINT,
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
    int32_t  _int;
    uint32_t _uint;
    char     _char;
    char     _string[16];
} notification;

/*! Metadata associated with a notification from a general thread to
 *  libhw for transmission to a libhw-managed hardware device. */
typedef struct {
    memory_address_t base;
    memory_address_t pin;
    NOTIFICATION_TYPE type;
    notification** data;
} notification_metadata;

/*! Create a notification with specified name, type and value. */
#define notification_init_(_name, _type, _value) \
    notification _name;                          \
    _name._##_type = _value

/*! Create a notification with specified type and value of name
 *  `note'. */
#define notification_init(_type, _value)        \
    notification_init_(note, _type, _value)

#endif
