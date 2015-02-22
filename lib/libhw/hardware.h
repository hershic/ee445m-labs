/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __HARDWARE__
#define __HARDWARE__

#include <stdbool.h>
#include "libnotify/notify.h"

/* Note to developers:
 *
 * Why did the authors create structs that contain only an array?
 * Wouldn't it be easier to simply refer to the array instead of
 * unwrapping it from the context of the struct each time it is
 * invoked? You could even use a typedef to further shorten each invocation!
 *
 * Wisdom, thrown down from the mountain:
 * http://stackoverflow.com/a/4523537 [quoted below]
 *
 * [An unwrapped array with a typedef for convenience] is probably a
 * very bad idea, because the resulting type is an array type, but
 * users of it won't see that it's an array type. If used as a
 * function argument, it will be passed by reference, not by value,
 * and the sizeof for it will then be wrong.
 */

/* A note about this library:
 *
 * This hardware driver only manages peripherals that many tasks would
 * theoritecally like to subscribe to at one time. Hardware devices
 * such as GPIO pins should be manipulated with TI's driverlib. */


/* \bug TODO for extra cleanliness, determine hw_group automatically from
 * channel's raw address! */

/** An iterator ensured to be of an optimized size according to the
 * relevant #defines below it.  */
typedef uint8_t hw_iterator;

/* TODO: device-specific metadata like this */
/** Maximum number of channels supported by each hardware peripheral  */
#define HW_DRIVER_MAX_CHANNELS 8

/** Maximum number of subscribed threads each hardware driver is
 * responsible for responding to */
#define HW_DRIVER_MAX_SUBSCRIPTIONS 8

/** Used to represent #defines memory-mapped addresses before they are
 * converted into hw_driver channel indices by libhw */
typedef long raw_hw_channel;

/** Each peripheral channel needs to be able to notify
 * \HW_DRIVER_MAX_SUBSCRIPTIONS subscribers about incoming hardware
 * events */
typedef struct _isr_subscription {

    bool valid;
    bool single_shot_subscription;
    void (*slot)(hw_notification);
} _isr_subscription;

/** A hardware driver manages multiple peripheral channels */
typedef struct hw_channel {

    _isr_subscription isr_subscriptions[HW_DRIVER_MAX_SUBSCRIPTIONS];
} hw_channel;

/** Top level container for hardware driver data */
typedef struct hw_driver {

    hw_channel channels[HW_DRIVER_MAX_CHANNELS];
} hw_driver;

/** Represents each of the hardware groups supported by libhw */
typedef enum  {

    HW_UART,
    HW_LCD,
    HW_TIMER,
    HW_ADC,
    HW_SSI,
} HW_DEVICES;
/* OPTIONAL: consider I2C, CAN, USB integration with this module */

/* UART properties */
typedef struct hw_uart_metadata {

    uint32_t UART_BAUD_RATE;
} hw_uart_metadata;

/** Timer properties */
typedef struct hw_timer_metadata {

    uint32_t TIMER_FREQUENCY;
} hw_timer_metadata;

/** Initialization information comes in many shapes and sizes. Here is
 * one convenient container. */
typedef union {

    hw_uart_metadata uart;
    hw_timer_metadata timer;
} hw_metadata;


/*--------------------------------------------------------------*
 * Begin producer API -- libhw notifies subscribers             *
 *--------------------------------------------------------------*/

/** This function is responsible for enabling the peripherals and
 * internal data strutures used by the specified \hw_group.
 * \param hw_group The hardware group driver to initialize
 */
void hw_driver_init(HW_DEVICES);

/** This function is responsible for enabling a specific channel on
 * the specified hardware device. Internal libhw datastructures will
 * also be reset.
 * \param hw_group The hardware group in question
 * \param raw_channel The memory-mapped address of the device channel to initialize
 * \param metadata Information about how to initialize the device
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

/** For internal use only. Take care of the conversion between a
 * memory-mapped (raw) hw address and the index of said peripheral
 * channel in the memory banks of libhw's device-specific data
 * structures.
 * \param hw_group The hardware group in question
 * \param raw_hw_channel The specific channel of \hw_group
 * \returns Index of \raw_channel'channel in \hw_group's data structures
 */
hw_channel* _hw_get_channel(HW_DEVICES, raw_hw_channel);

/* hershal's idealistic notify TODO:
 *
 * hershal envisions a system where notifications in both directions
 * occur through one \hw_notify function instead of two functions
 * \hw_notify_subscriber and \hw_notify_driver. Sounds hefty, do it in
 * a branch.
 */

/** Notify threads subscribed to \channel about an incoming hardware
 * event.
 * \param hw_group The hardwawre group that received the incoming
 * hardware event
 * \param channel The channel of \hw_group that received the incoming
 * hardware event
 * \param notifications The notification (prepared by NVIC-invoked
 * ISR) containing information regarding the recently-occurring
 * hardware interrupt event
 */
/* TODO: rename hw_notify_subscriber */
void hw_notify_subscriber(HW_DEVICES hw_group,
			  raw_hw_channel channel,
			  hw_notification notification);

/*--------------------------------------------------------------*
 * Begin consumer API -- libhw notifies hw for threads          *
 *--------------------------------------------------------------*/

/* TODO: doxygenize */
void hw_notify_driver(hw_notification_metadata, hw_notification);

#endif
