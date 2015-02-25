/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __LIBNOTIFY__
#define __LIBNOTIFY__

#include "libstd/nexus.h"

/*! Capable of wrapping any type of incoming external information
 * (delivered in an interrupt) that libhw will notify running
 * processes about. This allows the processes' pseudo-ISRs to have a
 * consistent protytype, and a simplified notification-passing
 * system. */
typedef struct {

    /* for legacy compatibility */
    int32_t message;

    uint32_t* raw_data;

} notification;

/*! Create a notification with specified name, type and value. */
#define notification_init_(_name, _value, _raw_data) \
    notification _name;                                     \
    _name.message = _value;                                   \
    _name.raw_data = _raw_data                              \

/*! Create a notification with specified type and value of name
 *  `note'. */
#define notification_init(_value, _raw_data)         \
    notification_init_(note, _value, _raw_data)      \

#endif
