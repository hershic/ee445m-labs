/* Created by Hershal Bhave and Eric Crosson on 2015-01-25 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"
#include "driverlib/can.h"

#include "libadc/adc.h"
#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libstd/nexus.h"
#include "libdisplay/ST7735.h"
#include "libshell/shell.h"
#include "libuart/uart.h"

#define HEARTBEAT_MODAL

#include "libheart/heartbeat.h"

#include "arm_math.h"

#include "sine.h"

#define signal_length 512
#define fft_length signal_length*2
#define filter_length 128
#define disp_length 128

typedef enum {RAW, FFT, FILT} plot_mode_type;

/* Low Pass Filter v2 */
const int32_t h[filter_length]= {-2, -1, -1, -1, -1, -1, 0, 0, 0, 1, 1, 2, 3, 4,
                                 4, 5, 6, 7, 7, 7, 7, 7, 6, 5, 4, 2, 0, -3, -5,
                                 -9, -12, -16, -19, -22, -25, -28, -30, -31,
                                 -31, -31, -29, -25, -21, -14, -7, 2, 13, 25,
                                 38, 52, 67, 83, 99, 115, 130, 146, 160, 174,
                                 186, 196, 205, 211, 216, 218, 218, 216, 211,
                                 205, 196, 186, 174, 160, 146, 130, 115, 99, 83,
                                 67, 52, 38, 25, 13, 2, -7, -14, -21, -25, -29,
                                 -31, -31, -31, -30, -28, -25, -22, -19, -16,
                                 -12, -9, -5, -3, 0, 2, 4, 5, 6, 7, 7, 7, 7, 7,
                                 6, 5, 4, 4, 3, 2, 1, 1, 0, 0, 0, -1, -1, -1,
                                 -1, -1, -2};

arm_rfft_instance_q31 S;
arm_cfft_radix4_instance_q31 S_CFFT;

uint32_t adc_producer_index = 0;
uint32_t adc_consumer_index = 0;

int32_t adc_data[signal_length];
int32_t adc_processing_data[signal_length];
int32_t adc_data2[signal_length];
int32_t adc_fft_data[fft_length];
int32_t adc_filtered_data[filter_length + signal_length - 1];
int32_t disp_data[disp_length];

extern semaphore_t HW_BUTTON_RAW_SEM;
semaphore_t FILTERED_DATA_AVAIL = 0;
semaphore_t FFT_DATA_AVAIL = 0;

semaphore_t ADC_BUFFER_FILLED = 0;

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;

volatile semaphore_t button_debounced_new_data;

int8_t plot_en;
int8_t plot_mode;
uint32_t sim_freq;

void convolve(int32_t *x, const int32_t *h, int32_t *y, int32_t filter_len, int32_t signal_len) {
    uint32_t i, j;
    uint32_t jmin, jmax;
    for (i=0; i<(signal_len + filter_len-1); ++i) {
        y[i] = 0;

        jmin = (i >= signal_len - 1) ? i - (signal_len - 1) : 0;
        jmax = (i < filter_len - 1) ? i : filter_len - 1;

        for(j = jmin; j<=jmax; ++j) {
            y[i]+= (h[j] * x[i-j]);
        }
        y[i] /= 4096;
    }
}

inline void increment_ptr(uint32_t* ptr, uint32_t wrap_len) {
    *ptr = (*ptr + 1) % wrap_len;
}

/* This function assumes that there is enough space in the string
   buffer to store the digits of the stringified integer. */
char* fixed_4_digit_i2s(char* string_buf, int32_t data_12bit) {
    uint8_t tmp;
    uint16_t i, j, k, l;
    uint32_t num;

    num = data_12bit & 0x0FFF;
    for (j=0; j<4; ++j) {
        string_buf[3-j] = (num % 10) + 0x30;
        num /= 10;
    }

    string_buf[j] = 0;
    return string_buf;
}

void poor_mans_uart_send_string(char* text) {
    uint32_t cnt = ustrlen(text);
    char* ptr = (char*)text;

    while(cnt--) {
        UARTCharPut(UART0_BASE, *(ptr++));
    }
}

inline int32_t sine_at(uint32_t freq, uint32_t idx) {
    return sine[(idx * freq) % sine_length];
}

void generate_sine(uint32_t freq, int32_t* output, uint32_t output_length) {
    uint32_t i;

    if (freq > (sine_length/2)) {
        /* This is unreliable! */
        while (1) {}
    }

    for (i=0; i<output_length; i+=1) {
        output[i] = sine_at(freq, i);
    }
}

/*! Draw a pro graph plot -- not the data. */
inline void graph_draw(char title[5], char adc_itos[5]) {

    /* Draw the graph title and value of point */
    ST7735_DrawString(1, 1, title, ST7735_YELLOW);
    ST7735_DrawString(7, 1, adc_itos, ST7735_YELLOW);
}

/*! Graph one point on our pro graph. If a graph carriage-return is
 *  detected, clear the graph and prepare for displaying a new line
 *  from the left side of the screen. */
inline void graph_point(int32_t data, int32_t lower_limit, int32_t upper_limit) {

    ST7735_PlotLine(data);
    if (ST7735_PlotNext()) {
        ST7735_PlotClear(lower_limit, upper_limit);
    }
}

/*! Graph one point on our pro graph. Do not clear the screen (no
 *  carriage-return).  */
inline void graph_point_nocr(int32_t data) {

    ST7735_PlotLine(data);
    ST7735_PlotNext();
}

void display_all_adc_data() {

    int16_t i;
    int16_t j;
    char adc_itos[5];
    int32_t delta;
    int32_t tmp;
    ST7735_PlotClear(0, 4096);

    plot_en = 1;
    plot_mode = FILT;
    FILTERED_DATA_AVAIL = 0;
    FFT_DATA_AVAIL = 0;

    while (1) {
        if (!plot_en) {continue;}

        if (plot_mode == FFT) {
            sem_guard(FFT_DATA_AVAIL) {
                sem_take(FFT_DATA_AVAIL);
                ST7735_PlotClear(0, 4096);

                delta = signal_length/disp_length;
                for (i=0; i<signal_length; i+=delta) {
                    for (tmp=0, j=0; j<delta; ++j) {
                        tmp += adc_fft_data[2*(i+j)];
                    }
                    disp_data[i/delta] = tmp/delta;
                }
                graph_draw("fft ", "    ");
                for (i=0; i<disp_length; ++i) {
                    graph_point_nocr(disp_data[i]);
                }
            }
        } else if (plot_mode == FILT) {
            sem_guard(FILTERED_DATA_AVAIL) {
                sem_take(FILTERED_DATA_AVAIL);
                ST7735_PlotClear(-2048, 2048);

                delta = (signal_length)/disp_length;
                for (i=0; i<(signal_length); i+=delta) {
                    for (tmp=0, j=0; j<delta; ++j) {
                        tmp+= adc_filtered_data[i+j];
                    }
                    disp_data[i/delta] = tmp/delta;
                }
                graph_draw("filt", "    ");
                for (i=0; i<disp_length; ++i) {
                    graph_point_nocr(disp_data[i]);
                }
            }
        } else {
                ST7735_PlotClear(0, 4096);

                delta = (signal_length)/disp_length;
                while(adc_producer_index != 0) {}
                for (i=0; i<(signal_length); i+=delta) {
                    while(i > adc_producer_index) {}
                    for (tmp=0, j=0; j<delta; ++j) {
                        tmp+= adc_data[i+j];
                    }
                    disp_data[i/delta] = tmp/delta;
                }
                graph_draw("raw ", "    ");
                for (i=0; i<disp_length; ++i) {
                    graph_point_nocr(disp_data[i]);
                }
        }
        os_surrender_context();
    }
}

void mag(int32_t* in_complex, int32_t* out_real, uint32_t out_len) {
    int i;
    for(i=0; i<out_len; ++i) {
        arm_sqrt_q31(in_complex[2*i]*in_complex[2*i+1], &out_real[i]);
    }
}

void fft() {

    int32_t i;

    while (1) {
        if (FFT_DATA_AVAIL == 0 && plot_mode == FFT) {
            sem_guard(ADC_BUFFER_FILLED) {
                /* sem_take(ADC_BUFFER_FILLED); */

                /* This takes a snapshot of the adc data after the
                   next pass, making sure that there is absolutely no
                   aliasing. */
                while(adc_producer_index != 0) {}
                for (i=0; i<signal_length; ++i) {
                    while(i > adc_producer_index) {}
                    adc_processing_data[i] = adc_data[i];
                }

                /* Process the data through the RFFT/RIFFT module */
                arm_rfft_q31(&S, adc_processing_data, adc_fft_data);

                /* Process the data through the Complex Magnitude Model for
                 * calculating the magnitude at each bin */
                /* arm_cmplx_mag_q31(adc_data, adc_freq_data, signal_length); */
                /* mag(adc_data, adc_freq_data, signal_length); */
                for(i=0; i<signal_length; ++i) {
                    /* arm_sqrt_q31(adc_data[2*i]*adc_data[2*i+1], &adc_freq_data[i]); */
                    adc_fft_data[i] = adc_fft_data[2*i]*adc_fft_data[2*i] + adc_fft_data[2*i+1]*adc_fft_data[2*i+1];
                }

                sem_post(FFT_DATA_AVAIL);
                ADC_BUFFER_FILLED = 0;
            }
        }
        os_surrender_context();
    }
}

void filter() {

    uint32_t i;

    while (1) {
        if (FILTERED_DATA_AVAIL == 0 && plot_mode == FILT) {
            sem_guard(ADC_BUFFER_FILLED) {

                /* This takes a snapshot of the adc data after the
                   next pass, making sure that there is absolutely no
                   aliasing. */
                while(adc_producer_index != 0) {}
                for (i=0; i<signal_length; ++i) {
                    while(i > adc_producer_index) {}
                    adc_processing_data[i] = adc_data[i] - 2048;
                }

                convolve(adc_processing_data, h, adc_filtered_data, filter_length, signal_length);
                sem_post(FILTERED_DATA_AVAIL);
                ADC_BUFFER_FILLED = 0;
            }
        }
        os_surrender_context();
    }
}

void button_debounce_daemon() {

    int32_t button_raw_data = 0xff;

    while (1) {
        sem_guard(HW_BUTTON_RAW_SEM) {
            sem_take(HW_BUTTON_RAW_SEM);
            button_raw_data = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);

            if (~button_raw_data & BUTTON_LEFT) {
                GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                             GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
                /* ++button_left_pressed; */
                /* ST7735_DrawString(1, 2, "1", ST7735_YELLOW); */
                plot_mode = (plot_mode+1) % 3;
            } else {
                /* ST7735_DrawString(1, 2, "0", ST7735_YELLOW); */
            }
            if (~button_raw_data & BUTTON_RIGHT) {
                GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                             GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
                /* ++button_right_pressed; */
                /* ST7735_DrawString(2, 2, "1", ST7735_YELLOW); */
            } else {
                /* ST7735_DrawString(2, 2, "0", ST7735_YELLOW); */
            }
        }
        os_surrender_context();
    }
}

int plot_on() {
    plot_en = 1;
    poor_mans_uart_send_string("ok");
}

int plot_off() {
    plot_en = 0;
    poor_mans_uart_send_string("ok");
}

int plot_fft() {
    plot_mode = FFT;
    poor_mans_uart_send_string("ok");
}

int plot_filt() {
    plot_mode = FILT;
    poor_mans_uart_send_string("ok");
}

int plot_raw() {
    plot_mode = RAW;
    poor_mans_uart_send_string("ok");
}

int sim_20() {
    sim_freq = 20;
}

int sim_10() {
    sim_freq = 10;
}

int sim_1() {
    sim_freq = 1;
}

int sim(char* args) {
    /* TODO: implement */
    return -1;
}

void simulate_adc() {
    int8_t j;
    int32_t i;
    sim_freq = 20;

    while (1) {
        ++j;

        if (j > 1) {
            adc_data[adc_producer_index] = sine_at(sim_freq, i);
            ADC_BUFFER_FILLED += adc_producer_index / (signal_length-1);
            increment_ptr(&adc_producer_index, signal_length);
            ++i;
            j = 0;
        }
        os_suspend();
    }
}

/************************************/
/* Begin Interrupt Service Routines */
/************************************/

void TIMER1A_Handler(void) {

  TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
}

void ADC0Seq2_Handler(void) {

    ADCIntClear(ADC0_BASE, 2);
    ADCSequenceDataGet(ADC0_BASE, 2, &adc_data[adc_producer_index]);
    ADC_BUFFER_FILLED += adc_producer_index / (signal_length-1);
    increment_ptr(&adc_producer_index, signal_length);
}

/**********************************/
/* End Interrupt Service Routines */
/**********************************/

int main(void) {

    hw_metadata metadata;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts */
    IntMasterDisable();

    /* Initialize the output screen */
    Output_On();

    /* button init */
    button_left_pressed = 0;
    button_right_pressed = 0;
    button_debounced_mailbox = 0xff;
    sem_init(button_debounced_new_data);

    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, button_metadata);
    /* hw_subscribe(HW_BUTTON, button_metadata, button_debounce_start); */
    /* end button init */

    os_threading_init();
    schedule(display_all_adc_data, 200 Hz, DL_SOFT);
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    schedule(button_debounce_daemon, 100 Hz, DL_SOFT);
    schedule(fft, 100 Hz, DL_SOFT);
    schedule(filter, 100 Hz, DL_SOFT);
    const bool USE_SIMULATED_ADC = false;
    if (USE_SIMULATED_ADC) {
        schedule(simulate_adc, 100 Hz, DL_SOFT);
    }
    else {
        /* Activate the ADC on PE1, 2, and 3 (AIN0-2). */
        /* start adc init */
        metadata.adc.base = ADC0_BASE;
        metadata.adc.trigger_source = ADC_TRIGGER_TIMER;
        metadata.adc.sample_sequence = 2;
        metadata.adc.channel = 0;
        metadata.adc.channel_configuration =
            ADC_CTL_CH0 | ADC_CTL_IE | ADC_CTL_END;
        metadata.adc.trigger_metadata.timer.base = TIMER1_BASE;
        metadata.adc.trigger_metadata.timer.frequency = 15000 Hz;
        metadata.adc.trigger_metadata.timer.interrupt = INT_TIMER1A;
        metadata.adc.trigger_metadata.timer.periodic = TIMER_CFG_PERIODIC;

        adc_init(metadata);
        adc_channel_init(metadata);
        adc_interrupt_init(metadata);
        /* end adc init */
    }

    system_init();
    system_register_command((const char*) "plot_on", plot_on);
    system_register_command((const char*) "plot_off", plot_off);
    system_register_command((const char*) "plot_raw", plot_raw);
    system_register_command((const char*) "plot_fft", plot_fft);
    system_register_command((const char*) "plot_filt", plot_filt);
    system_register_command((const char*) "sim_20", sim_20);
    system_register_command((const char*) "sim_10", sim_10);
    system_register_command((const char*) "sim_1", sim_1);

    /* Initialize hardware devices */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();

    /* fft code influenced by http://bit.ly/1GZaHXo */
    uint32_t ifftFlag = 0;
    uint32_t doBitReverse = 0;  /* not sure what this does */

    arm_status status = ARM_MATH_SUCCESS;

    /* Initialize the CFFT/CIFFT module */
    status = arm_rfft_init_q31(&S, &S_CFFT, signal_length, ifftFlag, doBitReverse);

    /**********************/
    /* CAN Initialization */
    /**********************/
    /* ctags _CANBaseValid(uint32_t ui32Base) for more info */
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

    /* Begin CAN transmission init */
    uint32_t ui32Base;
    uint32_t ui32ObjID = 0;
    tCANMsgObject *psMsgObject;
    tMsgObjType eMsgType;
    uint32_t num_data_frame_bytes = 8;
    uint8_t data_frame[] = {'T','h','i','s',' ','i','s',0};

#define CAN_SEND 0
#define CAN_RECV 1
#define CAN_ROLE CAN_SEND

    if (CAN_ROLE == CAN_SEND) {
        /* To send a data frame or remote frame (in response to a remote */
        /* request), take the following steps: */

        /* 1. Set eMsgType to MSG_OBJ_TYPE_TX. */
        /* 2. Set psMsgObject->ui32MsgID to the message ID. */
        /* 3. Set psMsgObject->ui32Flags. Make sure to set */
        /*    MSG_OBJ_TX_INT_ENABLE to allow an interrupt to be generated */
        /*    when the message is sent. */
        /* 4. Set psMsgObject->ui32MsgLen to the number of bytes in the data */
        /*    frame. */
        /* 5. Set psMsgObject->pui8MsgData to point to an array containing */
        /*    the bytes to send in the message. */
        /* 6. Call this function with ui32ObjID set to one of the 32 object buffers. */

        /* 1. */ eMsgType = MSG_OBJ_TYPE_TX;
        /* 2. */ psMsgObject->ui32MsgID = 0; /* initial message id */
        /* 3. */ psMsgObject->ui32Flags = MSG_OBJ_TX_INT_ENABLE; /* generate interrupt when message is sent */
        /* 4. */ psMsgObject->ui32MsgLen = num_data_frame_bytes;
        /* 5. */ psMsgObject->pui8MsgData = data_frame;

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
    /* Fin. */ CANMessageSet(ui32Base, ui32ObjID, psMsgObject, eMsgType);
    /**************************/
    /* End CAN Initialization */
    /**************************/

    /* If execution enters this loop there is a problem that needs to be addressed */
    if (status != ARM_MATH_SUCCESS) {
        while (1) ;
    }

    os_launch();

    /* main never terminates */
    while (1);
}
