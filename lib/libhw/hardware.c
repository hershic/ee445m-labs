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
#include "driverlib/timer.h"
#include "driverlib/rom.h"

#include "libhw/hardware.h"
#include "libstd/nexus.h"
#include "inc/hw_memmap.h"

/******************************************************************************
 * Ye Royale List of TODOs -- keep in mind the One Goal: speed
 *
 * TODO: Determine why this builds without #include "libuart/uart.h"
 * TODO: Determine how much room to allocate for notifications
 * TODO: integrate with libscoreboard (nonexistent)
 * TODO: document ISRs (they aren't in the header)
 * TODO: place all relevant ISRs in this document
 * 
 * Hershal's 
 * TODO: decouple SSI from DisplayDriver (hershal)
 *******************************************************************************/

/* Each driver is statically allocated */
hw_driver HW_UART_DRIVER;
hw_driver HW_LCD_DRIVER;
hw_driver HW_TIMER_DRIVER;
hw_driver HW_ADC_DRIVER;
hw_driver HW_SSI_DRIVER;

hw_notification HW_UART_NOTIFICATION;

void hw_driver_init(HW_DEVICES hw_group) {

    hw_iterator i;
    hw_channel channel;
    hw_driver* driver = hw_driver_singleton(hw_group);

    /* Enable the peripherals this driver is responsible for */
    switch(hw_group){
    case HW_UART:
	/* TODO: allow flexibility with channel */
	SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);
	SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA);
	break;

    case HW_LCD:   /* TODO: handle  */
    case HW_TIMER: /* TODO: handle  */
    case HW_ADC:   /* TODO: handle  */
    case HW_SSI:   /* TODO: handle  */
    default: postpone_death();
    }
}

/* TODO: consider returning false if scoreboard indicates in-use */
void hw_channel_init(HW_DEVICES     hw_group,
		     raw_hw_channel raw_channel,
		     hw_metadata    metadata) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(hw_group, raw_channel);

    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
	channel->isr_subscriptions[i].valid = false;
    }

    switch(hw_group) {
    case HW_UART:
	uart_set_active_channel(raw_channel);
	uart_init();
	break;
    case HW_LCD:   /* TODO: handle  */
    case HW_TIMER:
        timer_add_periodic_interrupt(metadata.timer.TIMER_FREQUENCY, raw_channel);
        break;
    case HW_ADC:   /* TODO: handle  */
    case HW_SSI:   /* TODO: handle  */
    default: postpone_death();
    }
}

bool hw_connect(HW_DEVICES hw_group, raw_hw_channel raw_channel, const void* isr) {

    hw_iterator i = 0;
    hw_channel* channel = _hw_get_channel(hw_group, raw_channel);
    i = _hw_first_available_subscription(channel);

    channel->isr_subscriptions[i].valid = true;
    channel->isr_subscriptions[i].single_shot_subscription = false;
    channel->isr_subscriptions[i].slot = isr;
    return true;
}

bool hw_connect_single_shot(HW_DEVICES     hw_group,
                            raw_hw_channel raw_channel,
			    const void*    isr) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(hw_group, raw_channel);
    i = _hw_first_available_subscription(channel);

    channel->isr_subscriptions[i].valid = true;
    channel->isr_subscriptions[i].single_shot_subscription = true;
    channel->isr_subscriptions[i].slot = isr;
    return true;
}

/* Note: there is no check to see if a signal is even connected before
 * a disconnect is attempted. This would be great to add but it's not
 * the time right now. Comment created Saturday February 7, 2015 15:46
 * OPTIONAL TODO
 */
bool hw_disconnect(HW_DEVICES hw_group, raw_hw_channel raw_channel, const void* isr) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(hw_group, raw_channel);
    
    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel->isr_subscriptions[i].slot != isr) {
	++i;
    }
    channel->isr_subscriptions[i].valid = false;
    channel->isr_subscriptions[i].single_shot_subscription = false;
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
    case HW_LCD:   /* TODO: handle  */
    case HW_TIMER: 
	switch(channel) {
	case TIMER0_BASE: return 0;
	case TIMER1_BASE: return 1;
	case TIMER2_BASE: return 2;
	case TIMER3_BASE: return 3;
	case TIMER4_BASE: return 4;
	}
    case HW_ADC:   /* TODO: handle channels having channels  */
	/* spitball'd idea - rename our 'channel' to 'base' (matching
	 * hw_memmap.h) */
	switch(channel) {
	case ADC0_BASE: return 0;
	case ADC1_BASE: return 1;
	}
    case HW_SSI:
	switch(channel) {
	case SSI0_BASE: return 0;
	case SSI1_BASE: return 1;
	case SSI2_BASE: return 2;
	case SSI3_BASE: return 3;
	}
    default: postpone_death();
    }
    /* TODO: ensure software doesn't try to access nonexistent hardware
     * (mind the board model this is currently running on. current idea:
     * compile flags limiting the furthest indices) */
}

/* OPTIMIZE: inline */
hw_driver* hw_driver_singleton(HW_DEVICES hw_group) {

    switch(hw_group) {
    case HW_UART:  return &HW_UART_DRIVER;
    case HW_LCD:   return &HW_LCD_DRIVER;
    case HW_TIMER: return &HW_TIMER_DRIVER;
    case HW_ADC:   return &HW_ADC_DRIVER;
    default:       postpone_death();
    }
    return NULL;
}

/* OPTIMIZE: optimize, this will be called a shit ton */
void hw_notify(HW_DEVICES           hw_group,
	       long                 raw_channel,
	       hw_notification      notification) {

    hw_iterator i=0;
    hw_channel* channel = _hw_get_channel(hw_group, raw_channel);

    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
	if (channel->isr_subscriptions[i].valid) {
	    /* TODO: schedule a task to enable quick return from this ISR */
	    channel->isr_subscriptions[i].slot(notification);
	    if (channel->isr_subscriptions[i].single_shot_subscription) {
		/* Cease fire! cease fire! */
		channel->isr_subscriptions[i].single_shot_subscription = false;
	    }
	}
    }
}

/* OPTIMIZE: inline, this will be called unbelievably often */
hw_channel* _hw_get_channel(HW_DEVICES hw_group, raw_hw_channel raw_channel) {

    long channel_index = _hw_channel_to_index(raw_channel, hw_group);
    return &(hw_driver_singleton(hw_group)->channels[channel_index]);
}

/* OPTIMIZE: inline, this will be called unbelievably often */
hw_iterator _hw_first_available_subscription(hw_channel* channel) {

    hw_iterator i = 0;
    while(i<HW_DRIVER_MAX_SUBSCRIPTIONS && channel->isr_subscriptions[i].valid) {
	++i;
    }
    if(channel->isr_subscriptions[i].valid) {
	/* There are no empty slots for a new subscriber */
	/* TODO: determine how serious this is.  */
	/* plan a: threat level midnight (TLM) */
	postpone_death();
	/* plan b: business as usual */
	return false;
    }
    return i;
}

/******************************************************************************/
/************************* Interrupt Service Routines *************************/
/******************************************************************************/

/* man oh man, how cool would it be if these were automatically
 * generated... oh, to be using a higher-order language than C. */

/************************* UART ISRs *************************/
void UART0_Handler(void) {

    unsigned short i;
    hw_notification notification;
    /* TODO: determine which bit to clear:  */
    unsigned long look_at_me = UARTIntStatus();
    /* UARTIntClear(UART0_BASE, ); */

    while(UARTCharsAvail(UART0_BASE)) {

	/* Notify every subscribed task of each incoming character
	 * (but schedule them for later so we can return from this ISR
	 * asap). */
	notification._char = uart_get_char();

	/* TODO: schedule this thread instead of running it immediately */
	hw_notify(HW_UART, UART0_BASE, notification);
    }
}

/************************* Timer ISRs *************************/
void Timer0A_Handler(void) {
    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
    hw_notify(HW_TIMER, 0, notification);
}

void Timer1A_Handler(void) {
    TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
    hw_notify(HW_TIMER, 1, notification);
}

void Timer2A_Handler(void) {
    TimerIntClear(TIMER2_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
    hw_notify(HW_TIMER, 2, notification);
}
