/*!
 * \brief Global values and typedef definitions
 * \author    Hershal Bhave
 * \author    Eric Crosson
 * \version   0.1
 * \date      2014
 * \copyright GNU Public License.
 * \addtogroup Globals Global variables and typedefs
 */

/* TODO: Doxygenize better */

#ifndef __DEFINES__
#define __DEFINES__

#include <stdint.h>

/* os/task defines */
/*! Type declaration of a task. \warning This is not a thread. We need
 *  real threads. */
typedef void (*task_t)();

typedef void (*isr_t)();        /* isr capable of hw_notifying */
typedef uint32_t frequency_t;

/*** Custom data types ***/
/* swiss army knife data types */
#ifndef null
#define null 0x00
#endif
#ifndef NULL
#define NULL 0x00
#endif
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned long ulong;

#include <stdbool.h>

/* shape.h data types */
#define shade_t           unsigned char
#define mu_fields_t       unsigned char
#define mu_input_event_t  unsigned char

/* OLED data types */
#define pixel_t    unsigned char
#define x_pixel_t  pixel_t
#define y_pixel_t  pixel_t

/* Graphlib data types */
#define y_range_t
#define x_range_t

/* adc data types */
#define adc_t          unsigned short
#define temperature_t  long
/*** End custom data types ***/

/*** Syntactic sugar ***/
#define when(cond) if(true == (cond))
#define private
#define public

/* colorized output through gcc */
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

/* mnemonics for framebuffer shades */
#define FB_SHADE_ERASE  ((shade_t) 0)
#define FB_SHADE_MIN    ((shade_t) 1)
#define FB_SHADE_MINOR  ((shade_t) 4)
#define FB_SHADE_MID    ((shade_t) 7)
#define FB_SHADE_MUCH   ((shade_t) 11)
#define FB_SHADE_MAX    ((shade_t) 15)
/* whoops..  backwards compatability */
#define FB_COLOR_ERASE  ((shade_t) 0)
#define FB_COLOR_MIN    ((shade_t) 1)
#define FB_COLOR_MINOR  ((shade_t) 4)
#define FB_COLOR_MID    ((shade_t) 7)
#define FB_COLOR_MUCH   ((shade_t) 11)
#define FB_COLOR_MAX    ((shade_t) 15)

/* custom shading through terminal */
#define SHADE_0 '.'
#define SHADE_1 '-'
#define SHADE_2 '*'
#define SHADE_3 '#'
#define SHADE_SIZE 4 /* bits necessary to represent different shades */

/* would be in framebuffer.h but we had scoping issues... */
#define FONT_VALVANO_HEIGHT   ((pixel_t) 8)
#define FONT_VALVANO_WIDTH    ((pixel_t) 5)
#define FONT_VALVANO_KERNING  ((pixel_t) 2)

/* vertical space between lines      */
#define FONT_VALVANO_LINE_SPACING  ((pixel_t) 2)
/* vertical space between paragraphs */
#define FONT_VALVANO_PARAGRAPH_SPACING  (FONT_VALVANO_LINE_SPACING+3)

/* OLED screen dimensions */
#define OLED_HEIGHT    ((pixel_t) 96)
#define OLED_WIDTH     ((pixel_t) 128)
/* synonyms used in different contexts */
#define RIT_FB_HEIGHT  ((pixel_t) OLED_HEIGHT)
#define RIT_FB_WIDTH   ((pixel_t) OLED_WIDTH/2)
/* synonyms make for happy developers */
#define FB_HEIGHT      ((pixel_t) 96)
#define FB_WIDTH       ((pixel_t) 128)

/* framebuffer constants */
#define FB_CLOCK_SET_TEXT_ROW      ((pixel_t) 45)
#define FB_CLOCK_SET_DIGITS_ROW    ((pixel_t) 55)
#define FB_CLOCK_SET_HOUR_COLUMN   ((pixel_t) 47)
#define FB_CLOCK_SET_COLON_COLUMN  ((pixel_t) 61)
#define FB_CLOCK_SET_MIN_COLUMN    ((pixel_t) 66)

#endif
