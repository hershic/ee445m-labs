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

/*! Draw string on \fb starting at \top_left_corner.
 *  \brief Draw string on fb starting at top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string Character array to draw
 *  \returns void
 */
void fb_draw_string(framebuffer fb, point* top_left_corner, char* string) {

    _fb_draw_string(fb, top_left_corner, string);
}

/*! Call \fb_draw_string and destroy \top_left_corner.
 *  \brief Call fb_draw_string and destroy top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on
 *  fb. Will be destroyed.
 *  \param string Character array to draw
 *  \returns void
 */
inline
void fb_draw_string_anon_pt(framebuffer fb, point* top_left_corner, char* string) {

    _fb_draw_string(fb, top_left_corner, string);
    free(top_left_corner);
}

/*! Erase \string on \fb starting at \top_left_corner.
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

/*! Call \fb_erase_string_anon and free \top_left_corner.
 *  \brief Erase string on fb starting at top_left_corner and free top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string String to erase from fb
 *  \returns void
 *  \Note In order to erase a string successfully the fonts, char
 *  arrays and coordinates must be identical.
 *  \Warning This will clear the values of the affected pixels; it is
 *  a destructive action and will not 'undo' changes to the canvas.
 */
inline
void fb_erase_string_anon(framebuffer fb, point* top_left_corner, char* string) {

    fb_erase_string(fb, top_left_corner, string);
    free(top_left_corner);
}

/*! Erase char \c on \fb starting at \top_left_corner.
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

/*! Call \fb_erase_char and free \top_left_corner.
 *  \brief Call \fb_erase_char and free \top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of \top_left_corner on char \c
 *  on \fb. Will be destroyed.
 *  \param c Char to erase from \fb
 *  \returns void
 *  \Note In order to erase a char successfully the fonts, char and
 *  coordinates must be identical.
 *  \Warning This will clear the values of the affected pixels; it is
 *  a destructive action and will not 'undo' changes to the canvas.
 */
inline void fb_erase_char_anon(framebuffer fb, point* top_left_corner, char c) {

    fb_erase_char_anon(fb, top_left_corner);
    free(top_left_corner);
}

/*! Draw \string on the \framebuffer starting square against \top_left_corner.
 * \brief For internal use only.
 * \details Draw a string on the framebuffer starting square against \top_left_corner.
 * \param fb Framebuffer to use as canvas
 * \param top_left_corner Coordinate of top left corner of string on fb
 * \param string String to draw
 * \returns void
 */
private void _fb_draw_string(framebuffer fb, point* top_left_corner, char* string);

/*! Peforms the base work of all framebuffer string drawing requests.
 * \brief For internal use only.
 * \details Peforms the base work of all framebuffer string drawing requests.
 * \param fb Framebuffer to use as canvas
 * \param pen Coordinate of top left corner of char on fb
 * \param c Character to draw
 * \returns void
 */
private void _fb_draw_char(framebuffer, point*, char);

/*! Using \fb as a canvas, draw \sh.
 *  \brief Using \fb as a canvas, draw \sh.
 *  \param fb Framebuffer to use as canvas
 *  \param sh Shape to draw
 *  \returns void
 *  \Note This function can also draw points and lines, i.e. a shape
 *   with only one or two points respectively.
 */
void fb_draw_shape(framebuffer fb, shape* sh);

/*! Draw a line segment on \fb from \point1 to \point2.
 *  \brief Draw a line segment on \fb from \point1 to \point2.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 Beginning of line segment
 *  \param point2 End of line segment
 *  \returns void
 *  \bug This function is not implemented yet (pass-through).
 */
inline void fb_draw_line_gradient(framebuffer fb,
				  point*      point1,
				  point*      point2,
				  shade_t     shade) {

    fb_draw_line(fb, point1, point2, shade);
}

/*! Using \fb as a canvas, draw \sh.
 *  \brief Using \fb as a canvas, draw \sh.
 *  \param fb Framebuffer to use as canvas
 *  \param sh Shape to draw
 *  \returns void
 *  \note This function can also draw points and lines, i.e. a shape
 *  with only one or two points respectively.
 */
inline void fb_erase_shape(framebuffer fb, shape* sh) {

    _fb_draw_shape(fb, sh, FB_COLOR_ERASE);
}

/*! Method to draw multiple shapes on \fb.
 *  Method to draw multiple shapes on \fb. Provide the number of shapes
 *  to draw for va_args.
 *  \param fb Framebuffer to use as canvas
 *  \param numShapes Number of shapes to draw
 *  \returns void
 */
void fb_draw_multiple_shapes(framebuffer, ushort, ...);

/*! Method to draw an array of shapes on \fb.
 *  Method to draw an array of shapes on \fb.
 *  \param fb Framebuffer to use as canvas
 *  \param numShapes Number of shapes to draw
 *  \param shape_arr Array of shapes to draw
 *  \returns void
 */
void fb_draw_shape_arr(framebuffer fb, ushort numShapes, shape** shape_arr);

/*! Method to erase an array of shapes from \fb.
 *  Method to erase an array of shapes from \fb.
 *  \param fb Framebuffer to use as canvas
 *  \param numShapes Number of shapes to erase
 *  \param shape_arr Array containing shapes to erase
 *  \returns void
 */
void fb_erase_shape_arr(framebuffer fb, ushort numShapes, shape** shape_arr);

/*! Helper function that performs the mechanics of drawing a shape.
 *  \brief For internal use only.
 *  \details Helper function that performs the mechanics of drawing a
 *  shape. The purpose of this function is to reduce duplicated code
 *  in memory.
 *  \param fb Framebuffer to use as canvas
 *  \param sh Shape to draw
 *  \param shade Color of shape
 *  \returns void
 */
private void _fb_draw_shape(framebuffer fb, shape* sh, shade_t shade);

/*! In \fb, set pixel (x,y) to \shade.
 *  \brief In fb, set pixel (x,y) to \shade.
 *  \param fb Framebuffer to use as canvas
 *  \param x X coordinate
 *  \param y Y coordinate
 *  \param shade Shade to set pixel
 *  \returns void
 *  \Note Error behavior: when pixel (x,y) is not writeable (off screen)
 *  this function does nothing.
 */
void fb_set_pixel(framebuffer fb, uchar x, uchar y, shade_t shade);

/*! Clear pixel (x,y) in \fb.
 *  \brief Clear pixel (x,y) in \fb.
 *  \details This is an alias for \fb_set_pixel with a shade of zero.
 *  \param fb Framebuffer to use as canvas
 *  \param x X coordinate
 *  \param y Y coordinate
 *  \returns void
 *  \Note Error behavior: when pixel (x,y) is not writeable (off screen)
 *  this function does nothing.
 */
inline void fb_clear_pixel(framebuffer fb, uchar x, uchar y) {

    fb_set_pixel(fb, x, y, FB_COLOR_ERASE);
}

/*! Draw a line segment on \fb from \point1 to \point2 of color \shade.
 *  \brief Draw a line segment on \fb from \point1 to \point2 of color
 *  \shade.to \b of color \shade.shade.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 Beginning of line segment
 *  \param point2 End of line segment
 *  \param shade Color of line segment
 *  \returns void
 */
inline
void fb_draw_line(framebuffer fb, point* point1, point* point2, shade_t shade) {

    _fb_draw_line(fb, point1, point2, shade);
}

/*! Draws a line connecting two points before destroying the two points.
 *  \brief Draws a line connecting two points before destroying the two points.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 One endpoint of the line to draw
 *  \param point2 The other endpoint of the line to draw
 *  \param shade Desired shade of the line on fb
 *  \returns void
 *  \warning This function destroys \a and \b.
 */
inline void fb_draw_anon_line(framebuffer fb, point* point1, point* point2, shade_t shade) {

    _fb_draw_line(fb, point1, point2, shade);
    SHDestroyPoint(point1);
    SHDestroyPoint(point2);
}

/*! Remove a line segment on \fb from \point1 to \point2.
 *  \brief Remove a line segment on \fb from \point1 to \point2.
 *  \param fb Framebuffer to use as canvas
 *  \param a Beginning of line segment
 *  \param b End of line segment
 *  \returns void
 */
inline void fb_erase_line(framebuffer fb, point* point1, point* point2) {

    _fb_draw_line(fb, point1, point2, FB_COLOR_ERASE);
}

/*! Erases a line connecting two points before destroys the two points.
 *  \brief Erases a line connecting two points before destroys the two points.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 One endpoint of the line to draw
 *  \param point2 The other endpoint of the line to draw
 *  \param shade Desired shade of the line on fb
 *  \returns void
 *  \warning This function destroys \a and \b.
 */
inline void fb_erase_anon_line(framebuffer fb, point* point1, point* point2) {

    _fb_draw_line(fb, point1, point2, FB_COLOR_ERASE);
    SHDestroyPoint(point1);
    SHDestroyPoint(point2);
}

/*!
 * \brief The heavy-lifter (pixel-setter) in line-segment-drawing.
 * \param fb The framebuffer to draw a line on
 * \param point1 The first endpoint of the line-segment to draw
 * \param point2 The final endpoint of the line-segment to draw
 * \param shade The shade of the line-segment to draw
 * \returns void
 */
private void _fb_draw_line(framebuffer, point*, point*, shade_t);

/*! Draw a circle in \fb at \center with radius of color \shade.
 *  \brief Draw a circle in \fb at \center with radius of color \shade.
 *  \param fb Framebuffer to use as canvas
 *  \param center Center of circle to draw
 *  \param radius Radius of circle to draw
 *  \param shade Shade to draw circle with on fb
 *  \returns void
 */
inline void fb_draw_circle(framebuffer fb, circle* c) {

    fb_draw_ellipse(fb, c->center, c->radius, c->radius, c->center->shade);
}

/*! Draw an ellipse in fb at center with x- and y- radii of color shade.
 *  \brief Draw an ellipse in fb at center with specified x- and y-radii of
 *  color shade.
 *  \param fb Framebuffer to use as canvas
 *  \param center Center of ellipse to draw
 *  \param x_radius X radius of specified ellipse
 *  \param y_radius Y radius of specified ellipse
 *  \param shade Shade to draw ellipse with on fb
 *  \returns void
 */
void fb_draw_ellipse(framebuffer fb,
                     point*      center,
                     ushort      x_radius,
                     ushort      y_radius,
                     shade_t     shade);

/*! Fill an ellipse on fb.
 *  \brief Fill an ellipse on fb.
 *  \details Fill on fb from the four symmetric points on an ellipse
 *  described by a center and x- and y-offsets, to the axis of said
 *  ellipse.
 *  \param fb Framebuffer to use as canvas
 *  \param center Geometric center of the ellipse
 *  \param x X offset of point to plot
 *  \param y Y offset of point to plot
 *  \returns void
 *  \note Points will be shaded with the value in center->shade.
 */
private void _fb_fill_four_ellipse_points(framebuffer fb,
					  point*      center,
					  ushort      x,
					  ushort      y);

/*! Draw on fb a shaded ellipse described by center, x- and y-radii.
 *  \brief Draw on fb a shaded ellipse described by center, x- and y-radii.
 *  \param fb Framebuffer to use as canvas
 *  \param center Gemoetric center of the ellipse
 *  \param x_radius X radius of ellipse to plot
 *  \param y_radius Y radius of ellipse to plot
 *  \param shade Color to fill ellipse with
 *  OPTIONAL: allow for different specified border color
 */
void fb_draw_ellipse_fill(framebuffer fb,
			  point*      center,
			  ushort      x_radius,
			  ushort      y_radius,
			  shade_t     shade);

/*! Plot on fb the four symmetric points on an ellipse.
 *  \brief Plot on fb the four symmetric points on an ellipse
 *  described by a center and x- and y-offsets.
 *  \param fb Framebuffer to use as canvas
 *  \param center Geometric center of the ellipse
 *  \param x X offset of point to plot
 *  \param y Y offset of point to plot
 *  \returns void
 *  \note Points will be shaded with the value in \center->shade.
 */
private
void _fb_plot_four_ellipse_points(framebuffer fb,
				  point*      center,
				  ushort      x,
				  ushort      y);


/* TODO: document */
inline
unsigned char max_pixel_width_of_long(uchar x, uchar y) {

    return max_uc(_fb_pixel_width_of_long(x), _fb_pixel_width_of_long(y));
    }

inline
unsigned char max_pixel_height_of_long(uchar x, uchar y) {

    return max_uc(_fb_pixel_height_of_long(x),_fb_pixel_height_of_long(y));
}


/* private pixel_t _fb_pixel_width_of_long(long); */
/* private pixel_t _fb_pixel_height_of_long(long); */
/* private pixel_t _fb_pixel_width_of_string(char*); */

/*! Methods to print points to the console. Will not work on an
 * embedded system (functions will return immediately). */

/*! Print a point (ordered pair) to the console followed by a newline.
 *  \brief Print a point (ordered pair) to the console followed by a newline.
 *  \note This function is only applicable on a computer
 *  (not for use on microcontrollers).
 *  \details This function appends a newline to the console after
 *  printing the ordered pair. To just print the ordered pair (sans
 *  newline) call function \fb_console_print_point instead.
 *  \param p Point to printf to the console
 *  \returns void
 */
void fb_console_println_point(point*);

/*! Print a point (ordered pair) to the console.
 *  \brief Print a point (ordered pair) to the console.
 *  \note This function is only applicable on a computer
 *  (not for use on microcontrollers).
 *  \details This function does not append a newline to the console
 *  after printing the ordered pair. To just print the ordered pair
 *  WITH a newline call function fb_console_println_point instead.
 *  \param p Point to printf to the console
 *  \returns void
 */
void fb_console_print_point(point*);

/*! Print a coordinate (ordered pair) to the console followed by a newline.
 *  \brief Print a coordinate (ordered pair) to the console followed by a newline.
 *  \note This function is only applicable on a computer
 *  (not for use on microcontrollers).
 *  \details This function appends a newline to the console after
 *  printing the ordered pair. To just print the ordered pair (sans
 *  newline) call function \fb_console_print_coordinate instead.
 *  \param x X coordinate of the coordinate to print
 *  \param y Y coordinate of the coordinate to print
 *  \param shade Shade of the coodrinate to print
 *  \returns void
 */
void fb_console_println_coordinate(uchar, uchar, shade_t);

/*! Print a coordinate (ordered pair) to the console.
 *  \brief Print a coordinate (ordered pair) to the console.
 *  \note This function is only applicable on a computer
 *  (not for use on microcontrollers).
 *  \details This function does not append a newline to the console
 *  after printing the ordered pair. To just print the ordered pair
 *  WITH a newline call function fb_console_println_coordinate instead.
 *  \param x X coordinate of the coordinate to print
 *  \param y Y coordinate of the coordinate to print
 *  \param shade Shade of the coodrinate to print
 *  \returns void
 */
void fb_console_print_coordinate(uchar, uchar, shade_t);

#endif	/*! __FRAMEBUFFER__ */
