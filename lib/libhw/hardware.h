/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __HARDWARE__
#define __HARDWARE__

#include <stdbool.h>
#include "libnotify/notify.h"
#include "libstd/nexus.h"

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
 * This hardware driver interfaces between userspace programs and
 * peripheral drivers. Users should never touch drivers directly but
 * instead rely on libhw's notification system. */


/* TODO: integrate ADC lib */

typedef enum {
    HW_UART,
    HW_TIMER,
    HW_BUTTON
} HW_TYPE;

/*! UART properties */
typedef struct {
    uint32_t baud_rate;
    memory_address_t channel;
} hw_uart_metadata;

/* todo: add union for timer_frequency or timer_period */
/*! timer properties */
typedef struct {
    memory_address_t base;
    uint32_t frequency;
    uint32_t interrupt;
    uint32_t periodic; /* TIMER_CFG_PERIODIC or TIMER_CFG_ONE_SHOT */
} hw_timer_metadata;

/*! button properties */
typedef struct {
    /* NOTE: base will be used in the future when we have buttons on
       different ports */
    memory_address_t base;
    memory_address_t pin;
    bool int_enable;
    uint32_t int_type;
} hw_button_metadata;

/*! Initialization information comes in many shapes and sizes. Here is
 * one convenient container. */
typedef union {
    hw_uart_metadata uart;
    hw_timer_metadata timer;
    hw_button_metadata button;
} hw_metadata;

/*! An iterator ensured to be of an optimized size according to the
 * relevant #defines below it.  */
typedef uint8_t hw_iterator;

/*! Typedef for an interrupt service routine, which can never accept
 *  arguments or return a value. */
/* typedef void (*isr_t)(notification note); */

/* TODO: device-specific metadata like this */
/*! Maximum number of channels supported by each hardware peripheral  */
#define HW_DRIVER_MAX_CHANNELS 8

/*! Maximum number of subscribed threads each hardware driver is
 * responsible for responding to */
#define HW_DRIVER_MAX_SUBSCRIPTIONS 8

/*! Each peripheral channel needs to be able to notify
 * \HW_DRIVER_MAX_SUBSCRIPTIONS subscribers about incoming hardware
 * events */
typedef struct _isr_subscription _isr_subscription;
struct _isr_subscription {
    bool single_shot_subscription;
    void (*slot)(notification);
    _isr_subscription* next;
    _isr_subscription* prev;
};

/*! A hardware driver manages multiple peripheral channels */
typedef struct {
    _isr_subscription isr_subscriptions[HW_DRIVER_MAX_SUBSCRIPTIONS];
    _isr_subscription* free_slots;
    _isr_subscription* full_slots;
} hw_channel;

/*! Top level container for hardware driver data */
typedef struct {
    hw_channel channels[HW_DRIVER_MAX_CHANNELS];
} hw_driver;

#define hw_init(type, metadata)	    \
    hw_driver_init(type, metadata); \
    hw_channel_init(type, metadata)

/*! Return the one hw_driver per hardware device group in use by the
 * TM4C.
 * \param The hardware type to grab a pointer of
 * \returns Pointer to the hw_driver data structure managing \type
 */
hw_driver* hw_driver_singleton(HW_TYPE);

/*! This function is responsible for enabling the peripherals and
 * internal data strutures used by the specified \hw_group.
 * \param hw_group The hardware group driver to initialize
 */
void hw_driver_init(HW_TYPE, hw_metadata);

/*! This function is responsible for enabling a specific channel on
 * the specified hardware device. Internal libhw datastructures will
 * also be reset.
 * \param hw_group The hardware group in question
 * \param raw_channel The memory-mapped address of the device channel to initialize
 * \param metadata Information about how to initialize the device
 */
void hw_channel_init(HW_TYPE, hw_metadata);

/* TODO: doxygenize */
#define hw_subscribe(type, metadata, isr) \
    _hw_subscribe(type, metadata, isr, false)

#define hw_subscribe_single_shot(type, metadata, isr) \
    _hw_subscribe(type, metadata, isr, true)

/*! Subscribe to a hardware interrupt. This is also called connecting
 * a signal (the isr to be notified) to a slot (the hw-triggered
 * driver isr).
 * \param hw_group The hardware group in question
 * \param raw_channel \hw_group's channel (memory-mapped address)
 * \param isr The pseudo-isr to be notified by the hardware driver
 */
void _hw_subscribe(HW_TYPE, hw_metadata, void (*isr)(notification note), bool);

/*! Unsubscribe from a hardware interrupt. This is also called
 * disconnecting a signal (the isr in question) from a slot (the
 * hw-triggered driver isr).
 * \param hw_group The hardware group in question
 * \param raw_channel \hw_group's channel (memory-mapped address)
 * \param isr The pseudo-isr to be not notified anymore by the hardware driver
 */
void hw_unsubscribe(HW_TYPE, hw_metadata, void (*isr)(notification note));

/*! For internal use only. Convert a memory-mapped address
 * (raw_hw_channel) into an index for hw_driver's internal data
 * structures.
 * \param hw_group The hardware group in question
 * \param channel The specified channel of \hw_group
 * \returns Internal index of channel's first open subscription bucket
 */
int32_t _hw_channel_to_index();

/*! For internal use only. Take care of the conversion between a
 * memory-mapped (raw) hw address and the index of said peripheral
 * channel in the memory banks of libhw's device-specific data
 * structures.
 * \param hw_group The hardware group in question
 * \param raw_hw_channel The specific channel of \hw_group
 * \returns Index of \raw_channel'channel in \hw_group's data structures
 */
hw_channel* _hw_get_channel(HW_TYPE, hw_metadata);

/*! Notify threads subscribed to \channel about an incoming hardware
 * event.
 * \param hw_group The hardwawre group that received the incoming
 * hardware event
 * \param channel The channel of \hw_group that received the incoming
 * hardware event
 * \param notifications The notification (prepared by NVIC-invoked
 * ISR) containing information regarding the recently-occurring
 * hardware interrupt event
 */
void hw_notify(HW_TYPE, hw_metadata, notification);

#endif
