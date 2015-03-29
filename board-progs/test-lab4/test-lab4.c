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

#define signal_length 1024
#define fft_length signal_length*2
#define filter_length 51
#define disp_length 128

#define plot_mode_raw 0
#define plot_mode_fft 1
#define plot_mode_filt 2

const int32_t h[filter_length]={4,-1,-8,-14,-16,-10,-1,6,5,-3,-13,
                                -15,-8,3,5,-5,-20,-25,-8,25,46,26,-49,-159,-257, 984,
                                -257,-159,-49,26,46,25,-8,-25,-20,-5,5,3,-8,
                                -15,-13,-3,5,6,-1,-10,-16,-14,-8,-1,4};

arm_cfft_radix4_instance_q31 S;

int32_t adc_data[fft_length];
int32_t adc_freq_data[signal_length];
int32_t adc_filtered_data[filter_length];
int32_t disp_data[disp_length];

extern semaphore_t HW_ADC_SEQ2_SEM;
extern semaphore_t HW_BUTTON_RAW_SEM;
semaphore_t FILTERED_DATA_AVAIL;
semaphore_t FFT_DATA_AVAIL;
extern int32_t ADC0_SEQ2_SAMPLES[4];

uint32_t red_work = 0;
uint32_t blue_work = 0;
uint32_t green_work = 0;

uint32_t ADC_DATA_IDX = 0;
uint32_t ADC_DATA_FIFO[51];
uint32_t ADC_DATA_SEM = 0;

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;

volatile semaphore_t button_debounced_new_data;

int8_t plot_en;
int8_t plot_mode;

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
        y[i] /= 256;
    }
}

void led_blink_red() {
    while (1) {
        ++red_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                     GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
        os_surrender_context();
    }
}

void led_blink_green() {
    while (1) {
        sem_guard(HW_ADC_SEQ2_SEM) {
            sem_take(HW_ADC_SEQ2_SEM);
            ++green_work;
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3,
                         GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
            os_surrender_context();
        }
    }
}

void led_blink_blue() {
    while (1) {
        ++blue_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        os_surrender_context();
    }
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

void expand_data(int32_t* input, int32_t* outupt, uint32_t input_length, uint32_t output_length) {

}

void fill_buffer(int32_t* input, int32_t* outupt, uint32_t input_length, uint32_t output_length) {

}

void display_all_adc_data() {

    int16_t i;
    int16_t j;
    char string_buf[5];
    int32_t delta;
    int32_t tmp;
    ST7735_PlotClear(0, 4095);

    plot_en = 1;
    plot_mode = plot_mode_fft;

    while (1) {

        if (plot_mode == plot_mode_fft) {
            sem_guard(FFT_DATA_AVAIL) {
                sem_take(FFT_DATA_AVAIL);
                delta = signal_length/disp_length;
                for (i=0; i<signal_length; i+=delta) {
                    tmp = 0;
                    for (j=0; j<delta; ++j) {
                        tmp+= adc_data[2*(i+j)];
                    }
                    disp_data[i/delta] = tmp/delta;
                }
                ST7735_DrawString(1, 1, "freq", ST7735_YELLOW);
                ST7735_PlotClear(-512, 2047);
                for (i=0; i<disp_length; ++i) {
                    ST7735_PlotLine(disp_data[i]);
                    tmp = ST7735_PlotNext();
                }
            }
        } else if (plot_mode == plot_mode_filt) {
            sem_guard(FILTERED_DATA_AVAIL) {
                sem_take(FILTERED_DATA_AVAIL);
                delta = filter_length/disp_length;
                for (i=0; i<filter_length; i+=delta) {
                    tmp = 0;
                    for (j=0; j<delta; ++j) {
                        tmp+= adc_filtered_data[2*(i+j)];
                    }
                    disp_data[i/delta] = tmp/delta;
                }
                ST7735_DrawString(1, 1, "filt", ST7735_YELLOW);
                ST7735_PlotClear(-512, 2047);
                for (i=0; i<disp_length; ++i) {
                    ST7735_PlotLine(disp_data[i]);
                    tmp = ST7735_PlotNext();
                }
            }
        } else {
            sem_guard(HW_ADC_SEQ2_SEM) {
                sem_take(HW_ADC_SEQ2_SEM);

                fixed_4_digit_i2s(string_buf, ADC0_SEQ2_SAMPLES[0]);

                delta = signal_length/disp_length;
                tmp+= ADC0_SEQ2_SAMPLES[0];
                if (j >= delta) {
                    ST7735_DrawString(1, 1, string_buf, ST7735_YELLOW);
                    ST7735_PlotLine(tmp/delta);
                    if (ST7735_PlotNext()) {
                        ST7735_PlotClear(0, 4096);
                    }
                    j = 0;
                    ++i;
                    tmp = 0;
                }
                ++j;

                /* ST7735_PlotLine(ADC0_SEQ2_SAMPLES[0]); */
                /* if (ST7735_PlotNext()) { */
                /*     ST7735_PlotClear(0, 4095); */
                /* } */
            }
        }

        os_surrender_context();
    }
}

void fft() {

    int32_t i=0;

    while (1) {
        if (FFT_DATA_AVAIL == 0 && plot_mode == plot_mode_fft) {
            sem_guard(HW_ADC_SEQ2_SEM) {
                sem_take(HW_ADC_SEQ2_SEM);
                adc_data[i] = ADC0_SEQ2_SAMPLES[0];
                i+=2;

                if (i >= fft_length) {
                    i = 0;
                    /* Process the data through the CFFT/CIFFT modulke */
                    arm_cfft_radix4_q31(&S, adc_data);

                    /* Process the data through the Complex Magnitude Model for
                     * calculating the magnitude at each bin */
                    arm_cmplx_mag_q31(adc_data, adc_freq_data, signal_length);

                    sem_post(FFT_DATA_AVAIL);
                }
            }
        }
        os_surrender_context();
    }
}

void filter() {

    int32_t i=0;

    while (1) {
        if (FILTERED_DATA_AVAIL == 0 && plot_mode == plot_mode_filt) {
            sem_guard(HW_ADC_SEQ2_SEM) {
                sem_take(HW_ADC_SEQ2_SEM);
                adc_data[i] = ADC0_SEQ2_SAMPLES[0];
                ++i;
                if (i >= filter_length) {
                    i = 0;
                    convolve(adc_data, h, adc_filtered_data, filter_length, filter_length);
                    sem_post(FILTERED_DATA_AVAIL);
                }
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
                ST7735_DrawString(1, 2, "1", ST7735_YELLOW);
            } else {
                ST7735_DrawString(1, 2, "0", ST7735_YELLOW);
            }
            if (~button_raw_data & BUTTON_RIGHT) {
                GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                             GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
                /* ++button_right_pressed; */
                ST7735_DrawString(2, 2, "1", ST7735_YELLOW);
            } else {
                ST7735_DrawString(2, 2, "0", ST7735_YELLOW);
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
    plot_mode = plot_mode_fft;
    poor_mans_uart_send_string("ok");
}

int plot_filt() {
    plot_mode = plot_mode_filt;
    poor_mans_uart_send_string("ok");
}

int plot_raw() {
    plot_mode = plot_mode_raw;
    poor_mans_uart_send_string("ok");
}

int sample_raw() {
    poor_mans_uart_send_string("no");
}

int sample_filtered() {
    poor_mans_uart_send_string("no");
}

void simulate_adc() {
    int8_t j;
    int32_t i;

    while (1) {
        ++j;

        if (j > 10) {
            sem_post(HW_ADC_SEQ2_SEM);
            ADC0_SEQ2_SAMPLES[0] = sine_at(10, i);
            ++i;
            j = 0;
        }
        os_suspend();
    }
}

int main(void) {

    hw_metadata metadata;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts */
    IntMasterDisable();

    /* Initialize the output screen */
    Output_On();

    /* initialize the led gpio pins */
    /* heart_init(); */
    /* heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1); */
    /* heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2); */
    /* heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3); */

    /* Activate the ADC on PE1, 2, and 3 (AIN0-2). */
    /* start adc init */
    metadata.adc.base = ADC0_BASE;
    metadata.adc.trigger_source = ADC_TRIGGER_TIMER;
    metadata.adc.sample_sequence = 2;
    metadata.adc.channel = 0;
    metadata.adc.channel_configuration =
        ADC_CTL_CH0 | ADC_CTL_IE | ADC_CTL_END;
    metadata.adc.trigger_metadata.timer.base = TIMER1_BASE;
    metadata.adc.trigger_metadata.timer.frequency = 500 Hz;
    metadata.adc.trigger_metadata.timer.interrupt = INT_TIMER1A;
    metadata.adc.trigger_metadata.timer.periodic = TIMER_CFG_PERIODIC;

    /* adc_init(metadata); */
    /* adc_channel_init(metadata); */
    /* adc_interrupt_init(metadata); */
    /* end adc init */

    /* begin timer init for button debouncer */
    /* timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT); */
    /* hw_driver_init(HW_TIMER, timer_metadata); */
    /* end timer init for button debouncer */

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
    /* schedule(led_blink_red, 100 Hz, DL_SOFT); */
    schedule(display_all_adc_data, 200 Hz, DL_SOFT);
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    schedule(button_debounce_daemon, 100 Hz, DL_SOFT);
    schedule(fft, 100 Hz, DL_SOFT);
    /* schedule(filter, 100 Hz, DL_SOFT); */
    schedule(simulate_adc, 100 Hz, DL_SOFT);

    system_init();
    system_register_command((const char*) "plot_on", plot_on);
    system_register_command((const char*) "plot_off", plot_off);
    system_register_command((const char*) "sample_raw", sample_raw);
    system_register_command((const char*) "sample_filtered", sample_filtered);

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
    status = arm_cfft_radix4_init_q31(&S, signal_length, ifftFlag, doBitReverse);

    /* If execution enters this loop there is a problem that needs to be addressed */
    if (status != ARM_MATH_SUCCESS) {
        while (1) ;
    }

    os_launch();

    /* main never terminates */
    while (1);
}
