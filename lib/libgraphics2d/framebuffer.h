/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __FRAMEBUFFER__
#define __FRAMEBUFFER__

/*!
 * \brief Frame bufer representation in memory
 * \details Represent and transform one frame's buffer in memory.
 * \author    Hershal Bhave
 * \author    Eric Crosson
 * \version   0.1
 * \date      2014
 * \copyright GNU Public License.
 * \addtogroup Graphics Graphial manipulation framework
 * \warning Private functions are helper functions, and not to be accessed
   (though possible) outside of the scope of this class. The purpose of
   these functions is to reduce duplicated code.
 * \bug This library is not modal.
 */

/* TODO: mark the privates */

#include <stdarg.h>
#include "g2d_defines.h"
#include "shape.h"

/* TODO: Optimize this so we don't waste memory */
typedef char** framebuffer;

/* Note that all unenglishifyable chars are represented by a space. */
static const unsigned char valvanoFont[129][5] = {
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // NUL
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // Space
    { 0x00, 0x00, 0x4f, 0x00, 0x00 }, // !
    { 0x00, 0x07, 0x00, 0x07, 0x00 }, // "
    { 0x14, 0x7f, 0x14, 0x7f, 0x14 }, // #
    { 0x24, 0x2a, 0x7f, 0x2a, 0x12 }, // $
    { 0x23, 0x13, 0x08, 0x64, 0x62 }, // %
    { 0x36, 0x49, 0x55, 0x22, 0x50 }, // &
    { 0x00, 0x05, 0x03, 0x00, 0x00 }, // '
    { 0x00, 0x1c, 0x22, 0x41, 0x00 }, // (
    { 0x00, 0x41, 0x22, 0x1c, 0x00 }, // )
    { 0x14, 0x08, 0x3e, 0x08, 0x14 }, // *
    { 0x08, 0x08, 0x3e, 0x08, 0x08 }, // +
    { 0x00, 0x50, 0x30, 0x00, 0x00 }, // ,
    { 0x08, 0x08, 0x08, 0x08, 0x08 }, // -
    { 0x00, 0x60, 0x60, 0x00, 0x00 }, // .
    { 0x20, 0x10, 0x08, 0x04, 0x02 }, // /
    { 0x3e, 0x51, 0x49, 0x45, 0x3e }, // 0
    { 0x00, 0x42, 0x7f, 0x40, 0x00 }, // 1
    { 0x42, 0x61, 0x51, 0x49, 0x46 }, // 2
    { 0x21, 0x41, 0x45, 0x4b, 0x31 }, // 3
    { 0x18, 0x14, 0x12, 0x7f, 0x10 }, // 4
    { 0x27, 0x45, 0x45, 0x45, 0x39 }, // 5
    { 0x3c, 0x4a, 0x49, 0x49, 0x30 }, // 6
    { 0x01, 0x71, 0x09, 0x05, 0x03 }, // 7
    { 0x36, 0x49, 0x49, 0x49, 0x36 }, // 8
    { 0x06, 0x49, 0x49, 0x29, 0x1e }, // 9
    { 0x00, 0x36, 0x36, 0x00, 0x00 }, // :
    { 0x00, 0x56, 0x36, 0x00, 0x00 }, // ;
    { 0x08, 0x14, 0x22, 0x41, 0x00 }, // <
    { 0x14, 0x14, 0x14, 0x14, 0x14 }, // =
    { 0x00, 0x41, 0x22, 0x14, 0x08 }, // >
    { 0x02, 0x01, 0x51, 0x09, 0x06 }, // ?
    { 0x32, 0x49, 0x79, 0x41, 0x3e }, // @
    { 0x7e, 0x11, 0x11, 0x11, 0x7e }, // A
    { 0x7f, 0x49, 0x49, 0x49, 0x36 }, // B
    { 0x3e, 0x41, 0x41, 0x41, 0x22 }, // C
    { 0x7f, 0x41, 0x41, 0x22, 0x1c }, // D
    { 0x7f, 0x49, 0x49, 0x49, 0x41 }, // E
    { 0x7f, 0x09, 0x09, 0x09, 0x01 }, // F
    { 0x3e, 0x41, 0x49, 0x49, 0x7a }, // G
    { 0x7f, 0x08, 0x08, 0x08, 0x7f }, // H
    { 0x00, 0x41, 0x7f, 0x41, 0x00 }, // I
    { 0x20, 0x40, 0x41, 0x3f, 0x01 }, // J
    { 0x7f, 0x08, 0x14, 0x22, 0x41 }, // K
    { 0x7f, 0x40, 0x40, 0x40, 0x40 }, // L
    { 0x7f, 0x02, 0x0c, 0x02, 0x7f }, // M
    { 0x7f, 0x04, 0x08, 0x10, 0x7f }, // N
    { 0x3e, 0x41, 0x41, 0x41, 0x3e }, // O
    { 0x7f, 0x09, 0x09, 0x09, 0x06 }, // P
    { 0x3e, 0x41, 0x51, 0x21, 0x5e }, // Q
    { 0x7f, 0x09, 0x19, 0x29, 0x46 }, // R
    { 0x46, 0x49, 0x49, 0x49, 0x31 }, // S
    { 0x01, 0x01, 0x7f, 0x01, 0x01 }, // T
    { 0x3f, 0x40, 0x40, 0x40, 0x3f }, // U
    { 0x1f, 0x20, 0x40, 0x20, 0x1f }, // V
    { 0x3f, 0x40, 0x38, 0x40, 0x3f }, // W
    { 0x63, 0x14, 0x08, 0x14, 0x63 }, // X
    { 0x07, 0x08, 0x70, 0x08, 0x07 }, // Y
    { 0x61, 0x51, 0x49, 0x45, 0x43 }, // Z
    { 0x00, 0x7f, 0x41, 0x41, 0x00 }, // [
    { 0x02, 0x04, 0x08, 0x10, 0x20 }, // \
    { 0x00, 0x41, 0x41, 0x7f, 0x00 }, // ]
    { 0x04, 0x02, 0x01, 0x02, 0x04 }, // ^
    { 0x00, 0x01, 0x02, 0x04, 0x00 }, // `
    { 0x40, 0x40, 0x40, 0x40, 0x40 }, // _
    { 0x00, 0x00, 0x00, 0x00, 0x00 }, // CANNOT BE CALLED NATURALLY -- why is this here?
    { 0x20, 0x54, 0x54, 0x54, 0x78 }, // a
    { 0x7f, 0x48, 0x44, 0x44, 0x38 }, // b
    { 0x38, 0x44, 0x44, 0x44, 0x20 }, // c
    { 0x38, 0x44, 0x44, 0x48, 0x7f }, // d
    { 0x38, 0x54, 0x54, 0x54, 0x18 }, // e
    { 0x08, 0x7e, 0x09, 0x01, 0x02 }, // f
    { 0x0c, 0x52, 0x52, 0x52, 0x3e }, // g
    { 0x7f, 0x08, 0x04, 0x04, 0x78 }, // h
    { 0x00, 0x44, 0x7d, 0x40, 0x00 }, // i
    { 0x20, 0x40, 0x44, 0x3d, 0x00 }, // j
    { 0x7f, 0x10, 0x28, 0x44, 0x00 }, // k
    { 0x00, 0x41, 0x7f, 0x40, 0x00 }, // l
    { 0x7c, 0x04, 0x18, 0x04, 0x78 }, // m
    { 0x7c, 0x08, 0x04, 0x04, 0x78 }, // n
    { 0x38, 0x44, 0x44, 0x44, 0x38 }, // o
    { 0x7c, 0x14, 0x14, 0x14, 0x08 }, // p
    { 0x08, 0x14, 0x14, 0x18, 0x7c }, // q
    { 0x7c, 0x08, 0x04, 0x04, 0x08 }, // r
    { 0x48, 0x54, 0x54, 0x54, 0x20 }, // s
    { 0x04, 0x3f, 0x44, 0x40, 0x20 }, // t
    { 0x3c, 0x40, 0x40, 0x20, 0x7c }, // u
    { 0x1c, 0x20, 0x40, 0x20, 0x1c }, // v
    { 0x3c, 0x40, 0x30, 0x40, 0x3c }, // w
    { 0x44, 0x28, 0x10, 0x28, 0x44 }, // x
    { 0x0c, 0x50, 0x50, 0x50, 0x3c }, // y
    { 0x44, 0x64, 0x54, 0x4c, 0x44 }, // z
    { 0x00, 0x08, 0x36, 0x41, 0x00 }, // {
    { 0x00, 0x00, 0x7f, 0x00, 0x00 }, // |
    { 0x00, 0x41, 0x36, 0x08, 0x00 }, // }
    { 0x02, 0x01, 0x02, 0x04, 0x02 }, // ~
    { 0x00, 0x00, 0x00, 0x00, 0x00 }  // DEL
};

/* TODO: convert into inline functions and document */
#define max_pixel_width_of_long(x, y)  (max_uc(_fb_pixel_width_of_long(x), _fb_pixel_width_of_long(y)))
#define max_pixel_height_of_long(x, y) (max_uc(_fb_pixel_height_of_long(x),_fb_pixel_height_of_long(y)))

/* TODO: add all to group Graphics */

/*! Determine the max of two unsigned chars.
 *  \brief Determine the max of two unsigned chars.
 *  \param one
 *  \param two
 *  \returns "unsigned char" The larger value of one and two
 */
unsigned char max_uc(unsigned char, unsigned char);

/*! Convert an intever to a string.
 *  \brief Convert an integer to a string.
 *  \param i Int to convert into a string
 *  \param buffer Buffer for string contents
 *  \param length Length of allocated buffer
 *  \returns char* Buffer containing i represented as a string
 */
char* itoa(int, char*, uchar);

/*! Create a framebuffer object and return the handle.
 *  \brief Create a framebuffer object and return the handle.
 *  \returns framebuffer Newly malloc'd framebuffer
 */
framebuffer fb_init(void);

/*! Destroy a framebuffer object.
 *  \brief Destroy a framebuffer object.
 *  \param fb Framebuffer to destroy
 */
void fb_destroy(framebuffer);

/*! Draw string on fb starting at top_left_corner.
 *  \brief Draw string on fb starting at top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string Character array to draw
 *  \returns void
 */
void fb_draw_string(framebuffer, point*, char*);

/*! Call fb_draw_string and destroy top_left_corner.
 *  \brief Call fb_draw_string and destroy top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb. Will be destroyed.
 *  \param string Character array to draw
 *  \returns void
 */
void fb_draw_string_anon(framebuffer, point*, char*);

/*! Erase string on fb starting at top_left_corner.
 *  \brief Erase string on fb starting at top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string String to erase from fb
 *  \returns void
 *  \Note In order to erase a string successfully the fonts, char
 *  arrays and coordinates must be identical.
 *  \Warning This will clear the values of the affected pixels; it is
 *  a destructive action and will not 'undo' changes to the canvas.
 */
void fb_erase_string(framebuffer, point*, char*);

/*! Call \fb_erase_string_anon and free top_left_corner.
 *  \brief Erase string on fb starting at top_left_corner, then free top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string String to erase from fb
 *  \returns void
 *  \Note In order to erase a string successfully the fonts, char
 *  arrays and coordinates must be identical.
 *  \Warning This will clear the values of the affected pixels; it is
 *  a destructive action and will not 'undo' changes to the canvas.
 */
void fb_erase_string_anon(framebuffer, point*, char*);

/*! Erase char on fb starting at top_left_corner.
 *  \brief Erase char on fb starting at top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner on char on fb
 *  \param c Char to erase from fb
 *  \returns void
 *  \Note In order to erase a char successfully the fonts, char and
 *  coordinates must be identical.
 *  \Warning This will clear the values of the affected pixels; it is
 *  a destructive action and will not 'undo' changes to the canvas.
 */
void fb_erase_char(framebuffer, point*, char);

/*! Call \fb_erase_char and free top_left_corner.
 *  \brief Call \fb_erase_char and free top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner on char on fb. Will be destroyed.
 *  \param c Char to erase from fb
 *  \returns void
 *  \Note In order to erase a char successfully the fonts, char and
 *  coordinates must be identical.
 *  \Warning This will clear the values of the affected pixels; it is
 *  a destructive action and will not 'undo' changes to the canvas.
 */
void fb_erase_char_anon(framebuffer, point*, char);

/*! Draw a string on the framebuffer starting square against top_left_corner.
 * \brief Draw a string on the framebuffer starting square against top_left_corner.
 * \param fb Framebuffer to use as canvas
 * \param top_left_corner Coordinate of top left corner of string on fb
 * \param string String to draw
 * \returns void
 */
private void _fb_draw_string(framebuffer fb, point* top_left_corner, char* string);

/*! Peforms the base work of all framebuffer string drawing requests.
 * \brief Peforms the base work of all framebuffer string drawing requests.
 * \param fb Framebuffer to use as canvas
 * \param pen Coordinate of top left corner of char on fb
 * \param c Character to draw
 * \returns void
 */
private void _fb_draw_char(framebuffer, point*, char);

/*!
  Using fb as a canvas, draw sh.
  Input:  fb       framebuffer to use as canvas
  sh       shape to draw
  Output: none

  ** NOTICE **
  Note that this function can also draw points and lines, i.e. a shape
  with only one or two points respectively.
*/
void fb_draw_shape(framebuffer fb, shape* sh);

/*!
  Using fb as a canvas, draw sh.
  Input:  fb       framebuffer to use as canvas
  sh       shape to draw
  Output: none

  ** NOTICE **
  Note that this function can also draw points and lines, i.e. a shape
  with only one or two points respectively.
*/
void fb_erase_shape(framebuffer fb, shape* sh);

/*!
  Method to draw multiple shapes on fb. Provide the number of shapes
  to draw for va_args.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to draw before returning
  Output: void
*/
void fb_draw_multiple_shapes(framebuffer, ushort, ...);

/*!
  Method to draw an array of shapes on fb.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to draw before returning
  shape_arr   array containing shapes to draw
  Output: void
*/
void fb_draw_shape_arr(framebuffer fb, ushort numShapes, shape** shape_arr);

/*!
  Method to erase an array of shapes from fb.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to erase before returning
  shape_arr   array containing shapes to erase
  Output: void
*/
void fb_erase_shape_arr(framebuffer fb, ushort numShapes, shape** shape_arr);

/*!
  Internal helper function that performs the mechanics of drawing a
  shape. The purpose of this function is to reduce duplicated code in
  memory.
  Input:  fb        framebuffer to use as canvas
  sh        shape to draw
  shade     color of shape
  Output: void
*/
private void _fb_draw_shape(framebuffer fb, shape* sh, shade_t shade);

/*!
  In fb, set pixel (x,y) to shade
  Input:  fb       framebuffer to use as canvas
  x        x coordinate
  y        y coordinate
  shade    shade to set pixel
  Output: void
  Error behavior: when pixel (x,y) is not writeable (off screen)
  this function does nothing.
*/
void fb_set_pixel(framebuffer fb, uchar x, uchar y, shade_t shade);

/*!
  In fb, clear pixel (x,y). This is an alias for fb_set_pixel with a
  shade of zero.
  Input:  fb       framebuffer to use as canvas
  x        x coordinate
  y        y coordinate
  Output: void
  Error behavior: when pixel (x,y) is not writeable (off screen)
  this function does nothing.
*/
void fb_clear_pixel(framebuffer fb, uchar x, uchar y);

/*!
  Draw a line segment on fb from a to b of color shade.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  shade     color of line segment
  Output: void
*/
void fb_draw_line(framebuffer fb, point* a, point* b, shade_t shade);

/*!
  Pass this function two newly created points and it will draw a line
  connecting the points before destroying the points for you.
  \param  fb      framebuffer to use as canvas
  a       one endpoint of the line to draw
  b       the other endpoint of the line to draw
  shade   desired shade of the line on fb
  \returns void
  \warning This function destroys \a and \b.
*/
private void fb_draw_anon_line(framebuffer fb, point* a, point* b, shade_t shade);

/*!
  Remove a line segment on fb from a to b.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  Output: void
*/
void fb_erase_line(framebuffer fb, point* pta, point* ptb);

/*!
  Pass this function two newly created points and it will erase a line
  connecting the points before destroying the points for you.
  \param  fb      framebuffer to use as canvas
  a       one endpoint of the line to draw
  b       the other endpoint of the line to draw
  shade   desired shade of the line on fb
  \returns void
  \warning This function destroys \a and \b.
*/
void fb_erase_anon_line(framebuffer fb, point* a, point* b);

private void _fb_draw_line(framebuffer fb, point* a, point* b, shade_t shade);

/*!
  Draw a circle in fb at center with radius of color shade.
  Input:  fb         framebuffer to use as canvas
  center     center of circle to draw
  radius     radius of circle to draw
  shade      shade to draw circle with on fb
  Output: void
*/
void fb_draw_circle(framebuffer fb, circle* c);

/*!
  Draw an ellipse in fb at center with specified x- and y-radii, of
  color shade.
  Input:  fb         framebuffer to use as canvas
  center     center of ellipse to draw
  x_radius   x radius of specified ellipse
  y_radius   y radius of specified ellipse
  shade      shade to draw ellipse with on fb
  Output: void
*/
void fb_draw_ellipse(framebuffer fb, point* center, ushort x_radius, ushort y_radius, shade_t
		   shade);

/*!
  Fill on fb from the four symmetric points on an ellipse described by
  a center and x- and y-offsets, to the axis of said ellipse.
  Input:  fb            framebuffer to use as canvas
  center        geometric center of the ellipse
  x             x offset of point to plot
  y             y offset of point to plot
  Output: void
  Note: points will be shaded with the value in center->shade.
*/
private void _fb_fill_four_ellipse_points(framebuffer fb, point* center, ushort x, ushort y);
/*!
  Draw on fb a shaded ellipse described by center, x- and y-radii.
  Input:  fb            framebuffer to use as canvas
  center        gemoetric center of the ellipse
  x_radius      x radius of ellipse to plot
  y_radius      y radius of ellipse to plot
  shade         color to fill ellipse with
  OPTIONAL TODO: allow for different specified border color
*/
void fb_draw_ellipse_fill(framebuffer fb, point* center, ushort x_radius, ushort y_radius, shade_t shade);

/*!
  Plot on fb the four symmetric points on an ellipse described by a
  center and x- and y-offsets.
  Input:  fb            framebuffer to use as canvas
  center        geometric center of the ellipse
  x             x offset of point to plot
  y             y offset of point to plot
  Output: void
  Note: points will be shaded with the value in center->shade.
*/
private void _fb_plot_four_ellipse_points(framebuffer fb, point* center, ushort x, ushort y);

private pixel_t _fb_pixel_width_of_long(long);

private pixel_t _fb_pixel_height_of_long(long);

private pixel_t _fb_pixel_width_of_string(char* str);

/*! TODO: document below */
/*! TODO: remove header's var names */
/*! Methods to print points to the console. Will not work on an
 * embedded system (functions will return immediately). */

/*!
  Print a point (ordered pair) to the console. Only applicable on a
  computer (not for use on microcontrollers).

  ** NOTICE **
  This function appends a newline to the console after printing the
  ordered pair. To just print the ordered pair (sans newline) call
  function \fb_print_point_console instead.

  \param  p       point to printf to the console
  \returns void
*/
void fb_println_point_console(point* p);

/*!
  Print a point (ordered pair) to the console. Only applicable on a
  computer (not for use on microcontrollers).

  ** NOTICE **

  This function does not append a newline to the console after
  printing the ordered pair. To just print the ordered pair WITH
  a newline call function fb_println_point_console instead.

  \param  p       point to printf to the console
  \returns void
*/
void fb_print_point_console(point* p);

#endif	/*! __FRAMEBUFFER__ */
