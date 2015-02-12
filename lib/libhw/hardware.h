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

typedef struct hw_uart_metadata {

    uint32_t UART_BAUD_RATE;
} hw_uart_metadata;

typedef struct hw_timer_metadata {

    uint32_t TIMER_FREQUENCY;
} hw_timer_metadata;

typedef union {

    hw_uart_metadata uart;
    hw_timer_metadata timer;
} hw_metadata;

/** This function is responsible for enabling the peripherals and
 * internal data strutures used by the specified \hw_group.
 * \param hw_group The hardware group driver to initialize
 */
void hw_driver_init(HW_DEVICES);

/** This function is responsible for enabling a specific channel on
 * the specified hardware device. Internal libhw datastructures will
 * also be reset.
 * \param hw_group
 * \param raw_channel
 * \param metadata
 */
void hw_channel_init(HW_DEVICES, raw_hw_channel, hw_metadata);

/** Subscribe to a hardware interrupt. This is also called connecting
 * a signal (the isr to be notified) to a slot (the hw-triggered
 * driver isr).
 * \param hw_group The hardware group in question
 * \param raw_channel \hw_group's channel (memory-mapped address)
 * \param isr The pseudo-isr to be notified by the hardware driver
 */
bool hw_connect(HW_DEVICES, raw_hw_channel, const void*);
/** Unsubscribe from a hardware interrupt. This is also called
 * disconnecting a signal (the isr in question) from a slot (the
 * hw-triggered driver isr).
 * \param hw_group The hardware group in question
 * \param raw_channel \hw_group's channel (memory-mapped address)
 * \param isr The pseudo-isr to be not notified anymore by the hardware driver
 */
bool hw_disconnect(HW_DEVICES, raw_hw_channel, const void*);

/** Subscribe to a one-time notification from a hardware
 * interrupt. This is analagous to connecting to a hardware signal and
 * disconnecting from said signal in the slot that is called. Note
 * however that the hardware driver will do the disconnecting for you.
 * \param hw_group The hardware group in question
 * \param raw_channel \hw_group's channel (memory-mapped address)
 * \param isr The pseudo-isr to be not notified anymore by the hardware driver
 * \note When the hardware driver fires off a signal to a single-shot
 * slot, the hardware driver will disconnect said slot from future
 * notifications. In other words, the developer doesn't need to
 * disconnect an isr manually if the one-shot slot is called.
 * \note To disconnect from a one-shot timer, simply use \hw_disconnect.
 */
bool hw_connect_single_shot(HW_DEVICES, raw_hw_channel, const void*);

/** For internal use only. This function returns the index of the
 * first \_isr_subscription in \channel's internal data structures.
 * \param channel The channel to query for an open subscription slot
 * \returns The index of the first available subscription bucket of \channel
 */
hw_iterator _hw_first_available_subscription(hw_channel*);

/* TODO: for extra cleanliness, determine hw_group automatically from
 * channel's raw address! */
/** For internal use only. Convert a memory-mapped address
 * (raw_hw_channel) into an index for hw_driver's internal data
 * structures.
 * \param hw_group The hardware group in question
 * \param channel The specified channel of \hw_group
 * \returns Internal index of channel's first open subscription bucket
 */
raw_hw_channel _hw_channel_to_index(raw_hw_channel, HW_DEVICES);

/** Return the one hw_driver per hardware device group in use by the
 * TM4C.
 * \param hw_group The hardware group to grab a pointer of
 * \returns Pointer to the hw_driver data structure managing \hw_group
 */
hw_driver* hw_driver_singleton(HW_DEVICES hw_group);

/**
 */
hw_channel* _hw_get_channel(HW_DEVICES group, raw_hw_channel channel_id);

/**
 */
void hw_notify(HW_DEVICES hw_group,
	       raw_hw_channel channel,
	       hw_notification notification,
	       HW_NOTIFICATION_TYPE notification_type);

#endif
