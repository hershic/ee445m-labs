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
#include "libbutton/button.h"
#include "libut/utlist.h"
#include "libtimer/timer.h"

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
hw_driver HW_BUTTON_DRIVER;

/* To satisfy our need for speed, we must avoid the branches and
 * memory ready necessary for lazy initialization; that is to say the
 * \hw_driver_init also executing \hw_channel_init or vice versa. */
/* TODO: Accept which periph to enable here */
void hw_driver_init(HW_TYPE type, hw_metadata metadata) {

    switch(type) {
    case HW_UART:
        SysCtlPeripheralEnable(metadata.uart.channel); /* such as SYSCTL_PERIPH_UART0 */
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA); /* todo: parametrize - are they all on A? */
        break;

    case HW_TIMER:
	SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0 +
			       (metadata.timer.base - 0x40030000) / 0x1000);
	break;

    case HW_BUTTON:
	/* TODO: parametrize to allow other buttons to be driven */
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
	/* #need4speed: Buttons on the board are only used for input,
	 * we know how they're going to be used. Break the libhw
	 * convention and initialize these buttons so this code never
	 * has to be run again */
	button_init(metadata);
        break;

    default: postpone_death();
    }
}

/* Initialize internal data structures and call driver functions to
 * initialize hardware relating to the specified channel in
 * \metadata. */
void hw_channel_init(HW_TYPE type, hw_metadata metadata) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(type, metadata);
    channel->full_slots = NULL;
    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
	CDL_PREPEND(channel->free_slots, &channel->isr_subscriptions[i]);
    }

    switch(type) {
    case HW_UART:
        uart_set_active_channel(metadata.uart.channel);
        uart_init();
        break;

    case HW_TIMER:
	timer_add_interrupt(metadata);
        break;

    case HW_BUTTON:
	/* TODO: parametrize */
        button_set_interrupt(metadata, BUTTONS_BOTH);
        break;

    default: postpone_death();
    }
}

void _hw_subscribe(HW_TYPE     type,
		   hw_metadata metadata,
		   void (*isr)(notification note),
		   bool        single_shot) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(type, metadata);
    _isr_subscription* new_subscrip = &channel->free_slots[0];

    CDL_DELETE(channel->free_slots, new_subscrip);
    CDL_PREPEND(channel->full_slots, new_subscrip);

    new_subscrip->single_shot_subscription = single_shot;
    new_subscrip->slot = isr;
}

/* Note: there is no check to see if a signal is even connected before
 * a disconnect is attempted. This would be great to add but it's not
 * the time right now. Comment created Saturday February 7, 2015 15:46
 * OPTIONAL because: consider the need 4 speed
 */
void hw_unsubscribe(HW_TYPE type,
		    hw_metadata metadata,
		    void (*isr)(notification note)) {

    hw_channel* channel = _hw_get_channel(type, metadata);
    _isr_subscription* remove = &channel->full_slots[0];

    while(remove->slot != isr) {++remove;}
    CDL_DELETE(channel->full_slots, remove);
    CDL_PREPEND(channel->free_slots, remove);
}

/* OPTIMIZE: optimize, this will be called a shit ton */
void hw_notify(HW_TYPE type, hw_metadata metadata, notification note) {

    hw_iterator i=0;
    hw_channel* channel = _hw_get_channel(type, metadata);
    _isr_subscription* subscrip = &channel->full_slots[0];
    _isr_subscription* new_subscrip = NULL;

    /* TODO: make this a doubly linked list, not circular */
    /* consider similar changes to os lib also */
    while(subscrip && subscrip != new_subscrip) {
	subscrip->slot(note);
	new_subscrip = subscrip->next;
	if (subscrip->single_shot_subscription) {
	    /* Cease fire! cease fire! */
	    subscrip->single_shot_subscription = false;
	    CDL_DELETE(channel->full_slots, subscrip);
	    CDL_PREPEND(channel->free_slots, subscrip);
	}
	subscrip = new_subscrip;
    }
}

/* immaculate hashing function, much fast */
/* OPTIMIZE: inline, this will be called unbelievably often */
hw_channel* _hw_get_channel(HW_TYPE type, hw_metadata metadata) {

    memory_address_t idx;
    switch(type) {
    case HW_UART:   idx = metadata.uart.channel; break;
    case HW_TIMER:  idx = metadata.timer.base;   break;
    case HW_BUTTON: idx = metadata.button.pin;   break;
    default: postpone_death();
    }

    /* TODO: is masking with an offset faster? Might take up less space */
    switch(type){
    case HW_UART:
        switch(idx) {
        case UART0_BASE: idx = 0; break;
        case UART1_BASE: idx = 1; break;
        case UART2_BASE: idx = 2; break;
        case UART3_BASE: idx = 3; break;
        case UART4_BASE: idx = 4; break;
        case UART5_BASE: idx = 5; break;
        case UART6_BASE: idx = 6; break;
        case UART7_BASE: idx = 7; break;
	default: postpone_death();
        }
        break;

    case HW_TIMER:
        switch(idx) {
        case TIMER0_BASE: idx = 0; break;
        case TIMER1_BASE: idx = 1; break;
        case TIMER2_BASE: idx = 2; break;
        case TIMER3_BASE: idx = 3; break;
        case TIMER4_BASE: idx = 4; break;
	default: postpone_death();
        }
        break;

    case HW_BUTTON:
        switch(idx) {
        case GPIO_PORTF_BASE: idx = 0; break;
	default: postpone_death();
        }
    default: postpone_death();
    }

    /* TODO: ensure software doesn't try to access nonexistent hardware
     * (mind the board model this is currently running on. current idea:
     * compile flags limiting the furthest indices) */
    return &(hw_driver_singleton(type)->channels[idx]);
}

inline
hw_driver* hw_driver_singleton(HW_TYPE type) {

    switch(type) {
    case HW_UART:   return &HW_UART_DRIVER;
    case HW_TIMER:  return &HW_TIMER_DRIVER;
    case HW_BUTTON: return &HW_BUTTON_DRIVER;
    default:        postpone_death();
    }
    return NULL;
}

/*----------------------------------------------------------------------------*
 *                        Interrupt Service Routines                          *
 *----------------------------------------------------------------------------*/

/*! GPIO PortF (includes on-board buttons) isr responsible for
 *  notifying all subscriptions with information describing the
 *  interrupt. */
void GPIOPortF_Handler(void) {

    notification note;
    hw_metadata metadata;
    metadata.button.base = GPIO_PORTF_BASE;
    metadata.button.pin = BUTTONS_BOTH;

    GPIOIntClear(GPIO_PORTF_BASE, BUTTONS_BOTH);
    note._int = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    hw_notify(HW_BUTTON, metadata, note);
}


/* These ISRs were generated programatically -- see
 * /bin/lisp/rtos-interrupt-generator.el */

/*! UART0 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void UART0_Handler(void) {

  unsigned short i;
  notification note;
  hw_metadata metadata;

  /* TODO: determine which bit to clear:  */
  unsigned long look_at_me = UARTIntStatus();
  /* UARTIntClear(UART0_BASE, ); */

  metadata.uart.channel = UART0_BASE;

  while(UARTCharsAvail(UART0_BASE)) {

    /* Notify every subscribed task of each incoming character
     * (but schedule them for later so we can return from this ISR
     * asap). */
    note._char = uart_get_char();

    /* TODO: schedule this thread instead of running it immediately */
    hw_notify(HW_UART, metadata, note);
  }
}

/*! UART1 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void UART1_Handler(void) {

  unsigned short i;
  notification note;
  hw_metadata metadata;

  /* TODO: determine which bit to clear:  */
  unsigned long look_at_me = UARTIntStatus();
  /* UARTIntClear(UART1_BASE, ); */

  metadata.uart.channel = UART1_BASE;

  while(UARTCharsAvail(UART1_BASE)) {

    /* Notify every subscribed task of each incoming character
     * (but schedule them for later so we can return from this ISR
     * asap). */
    note._char = uart_get_char();

    /* TODO: schedule this thread instead of running it immediately */
    hw_notify(HW_UART, metadata, note);
  }
}

/*! UART2 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void UART2_Handler(void) {

  unsigned short i;
  notification note;
  hw_metadata metadata;

  /* TODO: determine which bit to clear:  */
  unsigned long look_at_me = UARTIntStatus();
  /* UARTIntClear(UART2_BASE, ); */

  metadata.uart.channel = UART2_BASE;

  while(UARTCharsAvail(UART2_BASE)) {

    /* Notify every subscribed task of each incoming character
     * (but schedule them for later so we can return from this ISR
     * asap). */
    note._char = uart_get_char();

    /* TODO: schedule this thread instead of running it immediately */
    hw_notify(HW_UART, metadata, note);
  }
}

/*! TIMER0A isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void TIMER0A_Handler(void) {

  TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(TIMER0_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}

/*! TIMER1A isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void TIMER1A_Handler(void) {

  TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(TIMER1_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}

/*! TIMER2A isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void TIMER2A_Handler(void) {

  TimerIntClear(TIMER2_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(TIMER2_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}
