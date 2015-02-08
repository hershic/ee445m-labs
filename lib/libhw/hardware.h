/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __HARDWARE__
#define __HARDWARE__

#include <stdbool.h>
#include "libnotify/notify.h"

typedef unsigned char hw_iterator;
#define HW_DRIVER_MAX_CHANNELS 8
#define HW_DRIVER_MAX_SUBSCRIPTIONS 8

/* TODO: document */

/* A driver consists of individual channels */
/* TODO: this needs to be generalized to satisfy the requirements of
 * each peripheral, but for dev mode simplicity wins! */
typedef struct _isr_subscription {

    bool valid;
    bool single_shot_subscription;
    void (*slot)(hw_notification, HW_NOTIFICATION_TYPE);
} _isr_subscription;

typedef struct hw_channel {

    _isr_subscription isr_subscriptions[HW_DRIVER_MAX_SUBSCRIPTIONS];
} hw_channel;

/* Why is this a struct? http://stackoverflow.com/a/4523537 */
typedef struct hw_driver {

    hw_channel channels[HW_DRIVER_MAX_CHANNELS];
} hw_driver;

typedef enum  {
    HW_UART,
    HW_LCD,
} HW_DEVICES;

void hw_driver_init(HW_DEVICES);
void hw_channel_init(hw_channel);

/* TODO: add single-shot dis/connect functions */
/* Connect a signal to a slot */
void hw_connect(HW_DEVICES, long, const void*);
void hw_disconnect(HW_DEVICES, long, const void*);


long _hw_channel_to_index(long channel, HW_DEVICES hw_group);
hw_driver hw_driver_singleton(HW_DEVICES hw_group);

void hw_notify(HW_DEVICES hw_group,
	       long channel,
	       hw_notification notification,
	       HW_NOTIFICATION_TYPE notification_type);

#endif
