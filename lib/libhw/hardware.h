/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __HARDWARE__
#define __HARDWARE__

#include <stdbool.h>
#include "libnotify/notify.h"

/** An iterator ensured to be of an optimized size according to the
 * relevant #defines below it.  */
typedef uint8_t hw_iterator;

/* TODO: device-specific metadata like this */
/** Maximum number of channels supported by each hardware peripheral  */
#define HW_DRIVER_MAX_CHANNELS 8

/** Maximum number of subscribed threads each hardware driver is
 * responsible for responding to */
#define HW_DRIVER_MAX_SUBSCRIPTIONS 8

typedef struct _isr_subscription {

    bool valid;
    bool single_shot_subscription;
    void (*slot)(hw_notification);
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
    HW_TIMER,
    HW_ADC,
} HW_DEVICES;

/** This function is responsible for enabling the peripherals and
 * internal data strutures used by the specified \hw_group.
 * \param hw_group The hardware group driver to initialize
 */
void hw_driver_init(HW_DEVICES);

/** For internal use only. Initialize (aka reset) a hardware
 *  channel's internal data structures.
 * \param channel The hardware channel to initialize
 */
void _hw_channel_init(hw_channel);

/* pass it device and channel, have it call respective uart_init for
 * channel or whatever */
/* void hw_channel_init(); */

/* TODO: add single-shot dis/connect functions */
/* Connect a signal to a slot */
bool hw_connect(HW_DEVICES, long, const void*);
bool hw_disconnect(HW_DEVICES, long, const void*);

bool hw_connect_single_shot(HW_DEVICES hw_group, long channel_id, const void* isr);


long _hw_channel_to_index(long channel, HW_DEVICES hw_group);
hw_driver* hw_driver_singleton(HW_DEVICES hw_group);
hw_channel* hw_get_channel(HW_DEVICES group, long channel_id);

void hw_notify(HW_DEVICES hw_group,
	       long channel,
	       hw_notification notification,
	       HW_NOTIFICATION_TYPE notification_type);

#endif
