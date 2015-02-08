/* -*- mode: c; c-basic-offset: 4; -*- */
#include "libhw/libhw.h"
#include "libstd/defines.h"
#include "inc/hw_memmap.h"

hw_driver HW_UART_DRIVER;
hw_notification HW_UART_NOTIFICATION;

void hw_connect(HW_DEVICES hw_group, long channel_id, const void* isr) {

    unsigned short i;
    long channel_index = _hw_channel_to_index(channel_id, hw_group);
    hw_channel channel = hw_driver_singleton(hw_group).channels[channel_index];

    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel.isr_subscriptions[i].valid) {
	++i;
    }
    if(channel.isr_subscriptions[i].valid) {
	/* There are no empty slots for a new subscriber */
	/* TODO: determine something horrific to do! */
    }
    channel.isr_subscriptions[i].valid = true;
    channel.isr_subscriptions[i].slot = isr;
}

/* Note: there is no check to see if a signal is even connected before
 * a disconnect is attempted. This would be great to add but it's not
 * the time right now. Comment created Saturday February 7, 2015 15:46 */
void hw_disconnect(HW_DEVICES hw_group, long channel_id, const void* isr) {

    unsigned short i;
    long channel_index = _hw_channel_to_index(channel_id, hw_group);
    hw_channel channel = hw_driver_singleton(hw_group).channels[channel_index];

    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel.isr_subscriptions[i].slot != isr) {
	++i;
    }
    channel.isr_subscriptions[i].valid = false;
    channel.isr_subscriptions[i].slot = NULL;
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

hw_driver hw_driver_singleton(HW_DEVICES hw_group) {

    switch(hw_group) {
    case HW_UART:
	return HW_UART_DRIVER;
	break;
    case HW_LCD:
	break;
    default:
	/* TODO: do something horrific here */
	break;
    }
}

void hw_notify(HW_DEVICES           hw_group,
	       long                 channel_id,
	       hw_notification      notification,
	       HW_NOTIFICATION_TYPE notification_type) {

    unsigned short i;
    long channel_index = _hw_channel_to_index(channel_id, hw_group);
    hw_channel channel = hw_driver_singleton(hw_group).channels[channel_index];

    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
	if (channel.isr_subscriptions[i].valid) {
	    channel.isr_subscriptions[i].slot(notification, notification_type);    break;
	}
    }
}
