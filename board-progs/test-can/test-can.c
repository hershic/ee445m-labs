/* Created by Hershal Bhave and Eric Crosson on 2015-01-25 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"
#include "inc/hw_can.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/timer.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/can.h"

#include "libos/os.h"
#include "libhw/hardware.h"
#include "libstd/nexus.h"
#include "libuart/uart.h"
#include "libstd/nexus.h"
#include "libuart/uart.h"

#include "libtimer/timer.h"
#include "libbutton/button.h"
#include "libos/thread_structures.h"

/* CAN Control */
#define CAN_SEND 0
#define CAN_RECV 1
#define signal_length 512

uint32_t adc_producer_index = 0;
int32_t adc_data[signal_length];
semaphore_t ADC_BUFFER_FILLED = 0;

inline void increment_ptr(uint32_t* ptr, uint32_t wrap_len) {
    *ptr = (*ptr + 1) % wrap_len;
}

#define led_toggle(port, pin)                                   \
    GPIOPinWrite(port, pin, pin ^ GPIOPinRead(port, pin))

#define counter_delay(time, counter)                \
    counter = 0;                                    \
    while(counter < time){counter++;}

/*! Ping))) Control */
volatile bool g_bErrFlag = 0;
volatile semaphore_t sem_ping;
volatile semaphore_t sem_ping_do_avg;
uint32_t ping_idx = 0;
/* bounded by 2^16 */
#define ping_samples_to_avg 10
bool ping_sample_ready = false;
uint32_t ping_avg;
uint32_t ping_time[ping_samples_to_avg];
bool ping_cluster_sample;

uint32_t timer_overflow;
uint32_t timer_signal_value;
/* for clarity */
uint32_t timer_response_value;

typedef enum ping_status {
    ping_not_active,
    ping_signal,
    ping_response,
} ping_status_t;

ping_status_t ping_status;

inline
int schedule_sample() {
    sem_signal(sem_ping);
}

tCANMsgObject sCANMessage;
uint8_t *pui8MsgData;

#define CAN_ROLE CAN_SEND
int init_can(void) {

    uint32_t can_base = CAN0_BASE;
    CANInit(can_base);

    /* configure the controller for 1 Mbit operation -- http://bit.ly/1CW7hUO */
    tCANBitClkParms psClkParms;
    psClkParms.ui32SyncPropPhase1Seg = 5; /* from 2 to 16   */
    psClkParms.ui32Phase2Seg = 2;         /* from 1 to 8    */
    psClkParms.ui32QuantumPrescaler = 1;  /* from 1 to 1023 */
    psClkParms.ui32SJW = 2;               /* from 1 to 4    */
    CANBitTimingSet(can_base, &psClkParms);

    /* After CANInit() and CANBitTimingSet() we may CANEnable() */
    CANEnable(can_base);    /* can CANDisable(); which does not re-init */
    CANIntEnable(can_base, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);

    /* Begin CAN transmission init */
    uint32_t ui32Base;
    uint32_t ui32ObjID = 0;
    tCANMsgObject *psMsgObject;
    tMsgObjType eMsgType;
    uint32_t num_data_frame_bytes = 8;

    uint32_t ui32MsgData;

    if (CAN_ROLE == CAN_SEND) {

        pui8MsgData = (uint8_t *)&ui32MsgData;

        SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                       SYSCTL_XTAL_16MHZ);

        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);
        GPIOPinConfigure(GPIO_PB4_CAN0RX);
        GPIOPinConfigure(GPIO_PB5_CAN0TX);
        GPIOPinTypeCAN(GPIO_PORTB_BASE, GPIO_PIN_4 | GPIO_PIN_5);
        SysCtlPeripheralEnable(SYSCTL_PERIPH_CAN0);
        CANInit(CAN0_BASE);
        CANBitRateSet(CAN0_BASE, SysCtlClockGet(), 500000);
        CANIntEnable(CAN0_BASE, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);
        IntEnable(INT_CAN0);
        CANEnable(CAN0_BASE);
        ui32MsgData = 0;

    } else if (CAN_ROLE == CAN_RECV) {

        /* Example: To receive a specific data frame, take the following steps: */

        /* 1. Set eMsgObjType to MSG_OBJ_TYPE_RX. */
        /* 2. Set psMsgObject->ui32MsgID to the full message ID, or a partial */
        /*    mask to use partial ID matching. */
        /* 3. Set psMsgObject->ui32MsgIDMask bits that are used for masking */
        /*    during comparison. */
        /* 4. Set psMsgObject->ui32Flags as follows: */
        /*    - Set MSG_OBJ_RX_INT_ENABLE flag to be interrupted when the data */
        /*      frame is received. */
        /*    - Set MSG_OBJ_USE_ID_FILTER flag to enable identifier-based */
        /*      filtering. */
        /* 5. Set psMsgObject->ui32MsgLen to the number of bytes in the */
        /*    expected data frame. */
        /* 6. The buffer pointed to by psMsgObject->pui8MsgData is not used */
        /*    by this call as no data is present at the time of the call. */
        /* 7. Call this function with ui32ObjID set to one of the 32 object */
        /*    buffers.  If you specify a message object buffer that already */
        /*    contains a message definition, it is overwrit- ten. */

        /* 1. */ eMsgType = MSG_OBJ_TYPE_RX;
        /* 2. */ psMsgObject->ui32MsgID = 0; /* initial message id */
        /* 3. */ psMsgObject->ui32MsgIDMask = 0xFFFFFFFF;
        /* 4. */ psMsgObject->ui32Flags = MSG_OBJ_RX_INT_ENABLE;
        /* 5. */ psMsgObject->ui32MsgLen = num_data_frame_bytes;
        /* 6. */
    }
}

/*! Transmit ping data via CAN */
int can_transmit(uint32_t data) {

    sCANMessage.ui32MsgID = 1;
    sCANMessage.ui32MsgIDMask = 0;
    sCANMessage.ui32Flags = MSG_OBJ_TX_INT_ENABLE;
    sCANMessage.ui32MsgLen = sizeof(uint32_t);
    sCANMessage.pui8MsgData = (int8_t*)(&data);

    // Print a message to the console showing the message count and the
    // contents of the message being sent.
    /* UARTprintf("Sending msg: 0x%02X %02X %02X %02X", */
    /*            pui8MsgData[0], pui8MsgData[1], pui8MsgData[2], */
    /*            pui8MsgData[3]); */

    // Send the CAN message using object number 1 (not the same thing as
    // CAN ID, which is also 1 in this example).  This function will cause
    // the message to be transmitted right away.
    CANMessageSet(CAN0_BASE, 1, &sCANMessage, MSG_OBJ_TYPE_TX);

    // Now wait 1 second before continuing
    // SimpleDelay();

    // Check the error flag to see if errors occurred
    /* if(g_bErrFlag) { */
    /*     UARTprintf(" error - cable connected?\n"); */
    /* } */
    /* else { */
    /*     // If no errors then print the count of message sent */
    /*     UARTprintf(" total count = %u\n", g_ui32MsgCount); */
    /* } */
}


/*! Sample the Ping))) Sensor. If \ping_cluster_sample is true,
 *  ping_samples_to_avg samples will be averaged together to make
 *  one data point. */
int sample_ping(void) {

    uint32_t counter;
    ping_status = ping_not_active;
    ping_cluster_sample = true;

    while(true) {
        sem_guard(sem_ping) {
            sem_take(sem_ping);
            IntMasterDisable();

            /* Set Ping))) SIG to output */
            GPIOIntDisable(GPIO_PORTB_BASE, GPIO_INT_PIN_0);
            IntDisable(INT_GPIOB_TM4C123);
            IntDisable(INT_GPIOB);
            GPIOPinTypeGPIOOutput(GPIO_PORTB_BASE, GPIO_PIN_0);

            /* Set SIG high for 5usec */
            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 1);
            /* Delay1us(5); */
            counter_delay(4, counter);

            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 0);

            /* Set Ping))) SIG to input */
            GPIOPinTypeGPIOInput(GPIO_PORTB_BASE, GPIO_PIN_0);
            GPIOIntTypeSet(GPIO_PORTB_BASE, GPIO_PIN_0, GPIO_BOTH_EDGES);

            counter_delay(200, counter);

            GPIOIntClear(GPIO_PORTB_BASE, GPIO_PIN_0);
            GPIOIntEnable(GPIO_PORTB_BASE, GPIO_PIN_0);
            IntEnable(INT_GPIOB_TM4C123);
            IntEnable(INT_GPIOB);

            IntMasterEnable();
        }
        os_surrender_context();
    }
}

int send_adc(void) {
    can_transmit(adc_data[0]);
    uart_send_udec(adc_data[0]);
}

void TIMER1A_Handler() {
    TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
    uint32_t test = TimerValueGet(TIMER1_BASE, TIMER_A);
    ++timer_overflow;
}

/* Record how long the Ping))) took to respond */
int GPIOPortB_Handler() {

    GPIOIntClear(GPIO_PORTB_BASE, GPIO_PIN_0);

    ++ping_status;

    if (ping_status == ping_signal) {
        /* begin timer init */
        timer_overflow = 0;
        timer_signal_value = TimerValueGet(TIMER1_BASE, TIMER_A);
        timer_metadata_init(TIMER1_BASE, 0, INT_TIMER1A, TIMER_CFG_PERIODIC_UP);
        timer_metadata.timer.subtimer = TIMER_A;
        timer_add_interrupt(timer_metadata);
        TimerLoadSet(TIMER1_BASE, TIMER_A, 0x0fffffe);
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3);
        /* end timer init */
    } else if (ping_status == ping_response) {
        timer_response_value = TimerValueGet(TIMER1_BASE, TIMER_A);
        ping_time[ping_idx++] = timer_response_value - timer_signal_value;
        ping_status = ping_not_active;
        TimerDisable(TIMER1_BASE, TIMER_A);
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, 0);
        GPIOIntDisable(GPIO_PORTB_BASE, GPIO_INT_PIN_0);
        sem_post(sem_ping_do_avg);
    }
}

/* Exists to decouple work form the GPIOPortB_Handler ISR */
void ping_average_samples() {

    int32_t counter;
    ping_idx = 0;

    while(1) {
        sem_guard(sem_ping_do_avg) {
            sem_take(sem_ping_do_avg);
            if (ping_cluster_sample) {
                /* Each sample of the Ping))) triggers \ping_samples_to_avg
                 * samples and averages the results */
                if (ping_idx >= ping_samples_to_avg) {
                    uint32_t sample_sum = 0;
                    for(ping_idx = 0; ping_idx <= ping_samples_to_avg; ++ping_idx) {
                        sample_sum += ping_time[ping_idx];
                    }
                    ping_idx = 0;
                    ping_avg = sample_sum/ping_samples_to_avg;
                    uart_send_string("averaged sample))) ");
                    uart_send_udec(ping_avg);
                    uart_send_string("\n\r");
                    ping_sample_ready = true;
                    can_transmit(ping_avg);
                } else {
                    uart_send_udec(ping_time[ping_idx]);
                    uart_send_string("\n\r");
                    counter_delay(2000, counter);
                    schedule_sample();
                }
            } else {
                uart_send_udec(ping_time[--ping_idx]);
                uart_send_string("\n\r");
            }
        }
        os_surrender_context();
    }
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts */
    IntMasterDisable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);
    GPIOPinConfigure(GPIO_PB4_CAN0RX);
    GPIOPinConfigure(GPIO_PB5_CAN0TX);
    GPIOPinTypeCAN(GPIO_PORTB_BASE, GPIO_PIN_4 | GPIO_PIN_5);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_CAN0);

    init_can();

    /* Enable processor interrupts */
    IntMasterDisable();

    /* begin initialize ping))) */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);
    sem_init(sem_ping);
    sem_init(sem_ping_do_avg);
    /* end initialize ping))) */

    /* begin hearts init */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);
    /* end hearts init */

    /* begin uart init */
    uart_anon_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    /* end uart init */

    /* begin timer init */
    timer_anon_init(TIMER1_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_driver_init(HW_TIMER, timer_metadata);
    /* end timer init */

    /* begin shell init */
    system_init();
    system_register_command((const char*) "s", schedule_sample);
    system_register_command((const char*) "a", send_adc);
    shell_spawn();
    /* end shell init */

    /* begin adc init */
    hw_metadata metadata;
    /* Activate the ADC on PE1, 2, and 3 (AIN0-2). */
    metadata.adc.base = ADC0_BASE;
    metadata.adc.trigger_source = ADC_TRIGGER_TIMER;
    metadata.adc.sample_sequence = 2;
    metadata.adc.channel = 0;
    metadata.adc.channel_configuration = ADC_CTL_CH0 | ADC_CTL_IE | ADC_CTL_END;
    metadata.adc.trigger_metadata.timer.base = TIMER2_BASE;
    metadata.adc.trigger_metadata.timer.subtimer = TIMER_A;
    metadata.adc.trigger_metadata.timer.frequency = 15000 Hz;
    metadata.adc.trigger_metadata.timer.interrupt = INT_TIMER2A;
    metadata.adc.trigger_metadata.timer.periodic = TIMER_CFG_PERIODIC;

    adc_init(metadata);
    adc_channel_init(metadata);
    adc_interrupt_init(metadata);
    /* end adc init */

    /* begin os init */
    os_threading_init();
    sched(hw_daemon);
    sched(sample_ping);
    sched(ping_average_samples);
    os_launch();
    /* end os init */

    /* main never terminates */
    while (1);
}

void
CAN0_Handler(void)
{
    uint32_t ui32Status;

    //
    // Read the CAN interrupt status to find the cause of the interrupt
    //
    ui32Status = CANIntStatus(CAN0_BASE, CAN_INT_STS_CAUSE);

    //
    // If the cause is a controller status interrupt, then get the status
    //
    if(ui32Status == CAN_INT_INTID_STATUS)
    {
        //
        // Read the controller status.  This will return a field of status
        // error bits that can indicate various errors.  Error processing
        // is not done in this example for simplicity.  Refer to the
        // API documentation for details about the error status bits.
        // The act of reading this status will clear the interrupt.  If the
        // CAN peripheral is not connected to a CAN bus with other CAN devices
        // present, then errors will occur and will be indicated in the
        // controller status.
        //
        ui32Status = CANStatusGet(CAN0_BASE, CAN_STS_CONTROL);

        //
        // Set a flag to indicate some errors may have occurred.
        //
        g_bErrFlag = 1;
    }

    //
    // Check if the cause is message object 1, which what we are using for
    // sending messages.
    //
    else if(ui32Status == 1)
    {
        //
        // Getting to this point means that the TX interrupt occurred on
        // message object 1, and the message TX is complete.  Clear the
        // message object interrupt.
        //
        CANIntClear(CAN0_BASE, 1);

        //
        // Increment a counter to keep track of how many messages have been
        // sent.  In a real application this could be used to set flags to
        // indicate when a message is sent.
        //
        /* g_ui32MsgCount++; */

        //
        // Since the message was sent, clear any error flags.
        //
        g_bErrFlag = 0;
    }

    //
    // Otherwise, something unexpected caused the interrupt.  This should
    // never happen.
    //
    else
    {
        //
        // Spurious interrupt handling can go here.
        //
    }
}

void ADC0Seq2_Handler(void) {

    ADCIntClear(ADC0_BASE, 2);
    ADCSequenceDataGet(ADC0_BASE, 2, &adc_data[adc_producer_index]);
    ADC_BUFFER_FILLED += adc_producer_index / (signal_length-1);
    increment_ptr(&adc_producer_index, signal_length);
}

/* for adc */
void TIMER2A_Handler(void) {

  TimerIntClear(TIMER2_BASE, TIMER_TIMA_TIMEOUT);
}
