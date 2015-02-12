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

/** Used to represent #defines memory-mapped addresses before they are
 * converted into hw_driver channel indices by libhw */
typedef long raw_hw_channel;

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

/** This function is responsible for enabling a specific channel on
 * the specified hardware device. Internal libhw datastructures will
 * also be reset.
 */
void hw_channel_init(HW_DEVICES, raw_hw_channel);

/** Subscribe to a hardware interrupt. This is also called connecting
 * a signal (the isr to be notified) to a slot (the hw-triggered
 * driver isr).
 * \param hw_group The hardware group in question
 * \param raw_channel \hw_group's channel (memory-mapped address)
 * \param isr The pseudo-isr to be notified by the hardware driver
 */
bool hw_connect(HW_DEVICES, raw_hw_channel, const void*);
/** Unsubscribt from a hardware interrupt */
bool hw_disconnect(HW_DEVICES, raw_hw_channel, const void*);

bool hw_connect_single_shot(HW_DEVICES, raw_hw_channel, const void*);
bool hw_connect_single_shot(HW_DEVICES, raw_hw_channel, const void*);

hw_iterator hw_first_available_subscription(hw_channel*);

raw_hw_channel _hw_channel_to_index(raw_hw_channel channel, HW_DEVICES hw_group);
hw_driver* hw_driver_singleton(HW_DEVICES hw_group);
hw_channel* _hw_get_channel(HW_DEVICES group, raw_hw_channel channel_id);

void hw_notify(HW_DEVICES hw_group,
	       raw_hw_channel channel,
	       hw_notification notification,
	       HW_NOTIFICATION_TYPE notification_type);

#endif
