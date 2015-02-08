/* -*- mode: c; c-basic-offset: 4; -*- */

#include <stdbool.h>
#include "libnotify/libnotify.h"

#define HW_DRIVER_MAX_CHANNELS 8
#define HW_DRIVER_MAX_SUBSCRIPTIONS 8

/* TODO: document */
/* TODO: consider uthash http://troydhanson.github.io/uthash/index.html */

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

typedef struct hw_driver {

    hw_channel channels[HW_DRIVER_MAX_CHANNELS];
} hw_driver;

typedef enum  {
    HW_UART,
    HW_LCD,
} HW_DEVICES;

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
