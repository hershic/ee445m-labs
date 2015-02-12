/* -*- mode: c; c-basic-offset: 4; -*- */
/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libhw/hardware.h"
#include "libstd/defines.h"
#include "inc/hw_memmap.h"

hw_driver HW_TIMER_DRIVER;
hw_driver HW_UART_DRIVER;
hw_notification HW_UART_NOTIFICATION;

/* Note; this still requires dev to run uart_init() */
void hw_driver_init(HW_DEVICES hw_group) {

    hw_iterator i;
    hw_channel channel;
    hw_driver* driver = hw_driver_singleton(hw_group);

    /* Enable the peripherals this driver is responsible for */
    /* TODO: allow flexibility with channel */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);
    /* TODO: allow other devices (than uart) to use the HW_DRIVER API */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA);

    /* Initialize driver data structures */
    for(i=0; i<HW_DRIVER_MAX_CHANNELS; ++i) {
	_hw_channel_init(driver->channels[i]);
    }

    /* Set the active uart channel -- dev convenience */
    uart_set_active_channel(UART0_BASE);
}

/* TODO: consider returning false if the scoreboard (which is a todo)
 * indicates in-use */
void _hw_channel_init(hw_channel channel) {

    hw_iterator i;
    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
	channel.isr_subscriptions[i].valid = false;
    }
}

bool hw_connect(HW_DEVICES hw_group, long channel_id, const void* isr) {

    hw_iterator i = 0;
    hw_channel* channel = hw_get_channel(hw_group, channel_id);

    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel->isr_subscriptions[i].valid) {
	++i;
    }
    if(channel->isr_subscriptions[i].valid) {
	/* There are no empty slots for a new subscriber */
	return false;
    }
    channel->isr_subscriptions[i].valid = true;
    channel->isr_subscriptions[i].slot = isr;
    return true;
}

/* TODO: try to extract /something/ between this and hw_connect */
bool hw_connect_single_shot(HW_DEVICES hw_group, long channel_id, const void* isr) {

    hw_iterator i = 0;
    hw_channel* channel = hw_get_channel(hw_group, channel_id);

    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel->isr_subscriptions[i].valid) {
	++i;
    }
    if(channel->isr_subscriptions[i].valid) {
	/* There are no empty slots for a new subscriber */
	return false;
    }
    channel->isr_subscriptions[i].valid = true;
    channel->isr_subscriptions[i].slot = isr;
    return true;
}

/* TODO: should this be private? */
hw_channel* hw_get_channel(HW_DEVICES group, long channel_id) {

    long channel_index = _hw_channel_to_index(channel_id, group);
    return &(hw_driver_singleton(group)->channels[channel_index]);
}

/* Note: there is no check to see if a signal is even connected before
 * a disconnect is attempted. This would be great to add but it's not
 * the time right now. Comment created Saturday February 7, 2015 15:46 */
bool hw_disconnect(HW_DEVICES hw_group, long channel_id, const void* isr) {

    hw_iterator i;
    hw_channel* channel = hw_get_channel(hw_group, channel_id);

    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel->isr_subscriptions[i].slot != isr) {
	++i;
    }
    channel->isr_subscriptions[i].valid = false;
    channel->isr_subscriptions[i].slot = NULL;
    return true;
}

/* immaculate hashing function, much fast */
long _hw_channel_to_index(long channel, HW_DEVICES hw_group) {
    switch(hw_group){
    case HW_UART:
	switch(channel) {
	case UART0_BASE: return 0;
	case UART1_BASE: return 1;
	case UART2_BASE: return 2;
	case UART3_BASE: return 3;
	case UART4_BASE: return 4;
	case UART5_BASE: return 5;
	case UART6_BASE: return 6;
	case UART7_BASE: return 7;
	}
	break;
    case HW_LCD:
	break;
    default:
	/* TODO: do something horrific here */
	break;
    }
}

hw_driver* hw_driver_singleton(HW_DEVICES hw_group) {

    /* TODO: add drivers */
    return &HW_UART_DRIVER;
}

void hw_notify(HW_DEVICES           hw_group,
	       long                 channel_id,
	       hw_notification      notification,
	       HW_NOTIFICATION_TYPE notification_type) {

    hw_iterator i=0;
    hw_channel* channel = hw_get_channel(hw_group, channel_id);

    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
	if (channel->isr_subscriptions[i].valid) {
	    /* TODO: schedule a task to enable quick return from this ISR */
	    channel->isr_subscriptions[i].slot(notification);
	}
    }
}

/* TODO: document */
void UART0_Handler(void) {

    unsigned short i;

    while(UARTCharsAvail(UART0_BASE)) {

	/* Notify every subscribed task of each incoming character
	 * (but schedule them for later so we can return from this ISR
	 * asap). */
	hw_notification notification;
	notification._char = uart_get_char();

	/* TODO: schedule this thread instead of running it immediately */
	hw_notify(HW_UART, UART0_BASE, notification, NOTIFY_CHAR);
    }
}
