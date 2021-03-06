/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __FRAMEBUFFER__
#define __FRAMEBUFFER__

#include <stdarg.h>
#include "libstd/defines.h"
#include "libstd/nexus.h"
#include "libmath/geometry/shape.h"
#include "libfont/font.h"

/* OPTIMIZE: Pack this so we don't waste memory */
typedef char** framebuffer;

/*! Create a framebuffer object and return the handle.
 *  \brief Create a framebuffer object and return the handle.
 *  \returns framebuffer Newly malloc'd framebuffer
 *  \ingroup Framebuffer
 */
framebuffer fb_init(void);

/*! Destroy a framebuffer object.
 *  \brief Destroy a framebuffer object.
 *  \param fb Framebuffer to destroy
 *  \ingroup Framebuffer
 */
void fb_destroy(framebuffer);

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
 *  \ingroup Framebuffer
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
 *  \ingroup Framebuffer
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
 *  \ingroup Framebuffer
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
 *  \ingroup Framebuffer
 */
inline
void fb_erase_char_anon(framebuffer fb, point* top_left_corner, char c) {

    fb_erase_char_anon(fb, top_left_corner, c);
    free(top_left_corner);
}

/*! Draw \string on the \framebuffer starting square against \top_left_corner.
 *  \brief For internal use only.
 *  \details Draw a string on the framebuffer starting square against \top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string String to draw
 *  \returns void
 *  \ingroup Framebuffer
 */
private void _fb_draw_string(framebuffer, point*, char*);

/*! Draw string on \fb starting at \top_left_corner.
 *  \brief Draw string on fb starting at top_left_corner.
 *  \param fb Framebuffer to use as canvas
 *  \param top_left_corner Coordinate of top left corner of string on fb
 *  \param string Character array to draw
 *  \returns void
 *  \ingroup Framebuffer
 */
inline
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
 *  \ingroup Framebuffer
 */
inline
void fb_draw_string_anon_pt(framebuffer fb, point* top_left_corner, char* string) {

    _fb_draw_string(fb, top_left_corner, string);
    free(top_left_corner);
}


/*! Peforms the base work of all framebuffer string drawing requests.
 *  \brief For internal use only.
 *  \details Peforms the base work of all framebuffer string drawing requests.
 *  \param fb Framebuffer to use as canvas
 *  \param pen Coordinate of top left corner of char on fb
 *  \param c Character to draw
 *  \returns void
 *  \ingroup Framebuffer
 */
private void _fb_draw_char(framebuffer, point*, char);

/*! Using \fb as a canvas, draw \sh.
 *  \brief Using \fb as a canvas, draw \sh.
 *  \param fb Framebuffer to use as canvas
 *  \param sh Shape to draw
 *  \returns void
 *  \Note This function can also draw points and lines, i.e. a shape
 *  with only one or two points respectively.
 *  \ingroup Framebuffer
 */
void fb_draw_shape(framebuffer, shape*);

/*! Method to draw multiple shapes on \fb.
 *  Method to draw multiple shapes on \fb. Provide the number of shapes
 *  to draw for va_args.
 *  \param fb Framebuffer to use as canvas
 *  \param numShapes Number of shapes to draw
 *  \returns void
 *  \ingroup Framebuffer
 */
void fb_draw_shapes(framebuffer, ushort, ...);

/*! Method to draw an array of shapes on \fb.
 *  Method to draw an array of shapes on \fb.
 *  \param fb Framebuffer to use as canvas
 *  \param numShapes Number of shapes to draw
 *  \param shape_arr Array of shapes to draw
 *  \returns void
 *  \ingroup Framebuffer
 */
void fb_draw_shape_arr(framebuffer, ushort, shape**);

/*! Method to erase an array of shapes from \fb.
 *  Method to erase an array of shapes from \fb.
 *  \param fb Framebuffer to use as canvas
 *  \param numShapes Number of shapes to erase
 *  \param shape_arr Array containing shapes to erase
 *  \returns void
 *  \ingroup Framebuffer
 */
void fb_erase_shape_arr(framebuffer, ushort, shape**);

/*! Helper function that performs the mechanics of drawing a shape.
 *  \brief For internal use only.
 *  \details Helper function that performs the mechanics of drawing a
 *  shape. The purpose of this function is to reduce duplicated code
 *  in memory.
 *  \param fb Framebuffer to use as canvas
 *  \param sh Shape to draw
 *  \param shade Color of shape
 *  \returns void
 *  \ingroup Framebuffer
 */
private void _fb_draw_shape(framebuffer, shape*, shade_t);

/*! Using \fb as a canvas, draw \sh.
 *  \brief Using \fb as a canvas, draw \sh.
 *  \param fb Framebuffer to use as canvas
 *  \param sh Shape to draw
 *  \returns void
 *  \note This function can also draw points and lines, i.e. a shape
 *  with only one or two points respectively.
 *  \ingroup Framebuffer
 */
inline void fb_erase_shape(framebuffer fb, shape* sh) {

    _fb_draw_shape(fb, sh, FB_COLOR_ERASE);
}

/*! In \fb, set pixel (x,y) to \shade.
 *  \brief In fb, set pixel (x,y) to \shade.
 *  \param fb Framebuffer to use as canvas
 *  \param x X coordinate
 *  \param y Y coordinate
 *  \param shade Shade to set pixel
 *  \returns void
 *  \Note Error behavior: when pixel (x,y) is not writeable (off screen)
 *  this function does nothing.
 *  \ingroup Framebuffer
 */
void fb_set_pixel(framebuffer, uchar, uchar, shade_t);

/*! Clear pixel (x,y) in \fb.
 *  \brief Clear pixel (x,y) in \fb.
 *  \details This is an alias for \fb_set_pixel with a shade of zero.
 *  \param fb Framebuffer to use as canvas
 *  \param x X coordinate
 *  \param y Y coordinate
 *  \returns void
 *  \Note Error behavior: when pixel (x,y) is not writeable (off screen)
 *  this function does nothing.
 *  \ingroup Framebuffer
 */
inline void fb_clear_pixel(framebuffer fb, uchar x, uchar y) {

    fb_set_pixel(fb, x, y, FB_COLOR_ERASE);
}

/*! Erase pixel (x,y) in \fb.
 *  \brief Erase pixel (x,y) in \fb.
 *  \details This is an alias for \fb_set_pixel with a shade of zero.
 *  \param fb Framebuffer to use as canvas
 *  \param x X coordinate
 *  \param y Y coordinate
 *  \returns void
 *  \Note Error behavior: when pixel (x,y) is not writeable (off screen)
 *  this function does nothing.
 *  \ingroup Framebuffer
 */
inline void fb_erase_pixel(framebuffer fb, uchar x, uchar y) {

    fb_clear_pixel(fb, x, y);
}

/*!
 * \brief The heavy-lifter (pixel-setter) in line-segment-drawing.
 * \param fb The framebuffer to draw a line on
 * \param point1 The first endpoint of the line-segment to draw
 * \param point2 The final endpoint of the line-segment to draw
 * \param shade The shade of the line-segment to draw
 * \returns void
 *  \ingroup Framebuffer
 */
private void _fb_draw_line(framebuffer, point*, point*, shade_t);

/*! Erases a line connecting two points before destroys the two points.
 *  \brief Erases a line connecting two points before destroys the two points.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 One endpoint of the line to draw
 *  \param point2 The other endpoint of the line to draw
 *  \param shade Desired shade of the line on fb
 *  \returns void
 *  \warning This function destroys \a and \b.
 *  \ingroup Framebuffer
 */
inline
void fb_erase_anon_line(framebuffer fb, point* point1, point* point2) {

    _fb_draw_line(fb, point1, point2, FB_COLOR_ERASE);
    SHDestroyPoint(point1);
    SHDestroyPoint(point2);
}

/*! Remove a line segment on \fb from \point1 to \point2.
 *  \brief Remove a line segment on \fb from \point1 to \point2.
 *  \param fb Framebuffer to use as canvas
 *  \param a Beginning of line segment
 *  \param b End of line segment
 *  \returns void
 *  \ingroup Framebuffer
 */
inline
void fb_erase_line(framebuffer fb, point* point1, point* point2) {

    _fb_draw_line(fb, point1, point2, FB_COLOR_ERASE);
}

/*! Draws a line connecting two points before destroying the two points.
 *  \brief Draws a line connecting two points before destroying the two points.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 One endpoint of the line to draw
 *  \param point2 The other endpoint of the line to draw
 *  \param shade Desired shade of the line on fb
 *  \returns void
 *  \warning This function destroys \a and \b.
 *  \ingroup Framebuffer
 */
inline
void fb_draw_anon_line(framebuffer fb, point* point1, point* point2, shade_t shade) {

    _fb_draw_line(fb, point1, point2, shade);
    SHDestroyPoint(point1);
    SHDestroyPoint(point2);
}

/*! Draw a line segment on \fb from \point1 to \point2 of color \shade.
 *  \brief Draw a line segment on \fb from \point1 to \point2 of color
 *  \shade.to \b of color \shade.shade.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 Beginning of line segment
 *  \param point2 End of line segment
 *  \param shade Color of line segment
 *  \returns void
 *  \ingroup Framebuffer
 */
inline
void fb_draw_line(framebuffer fb, point* point1, point* point2, shade_t shade) {

    _fb_draw_line(fb, point1, point2, shade);
}

/*! Draw a line segment on \fb from \point1 to \point2.
 *  \brief Draw a line segment on \fb from \point1 to \point2.
 *  \param fb Framebuffer to use as canvas
 *  \param point1 Beginning of line segment
 *  \param point2 End of line segment
 *  \returns void
 *  \bug This function is not implemented yet (pass-through).
 *  \ingroup Framebuffer
 */
inline void fb_draw_line_gradient(framebuffer fb,
				  point*      point1,
				  point*      point2,
				  shade_t     shade) {

    fb_draw_line(fb, point1, point2, shade);
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
 *  \ingroup Framebuffer
 */
void fb_draw_ellipse(framebuffer, point*, ushort, ushort, shade_t);

/*! Draw a circle in \fb at \center with radius of color \shade.
 *  \brief Draw a circle in \fb at \center with radius of color \shade.
 *  \param fb Framebuffer to use as canvas
 *  \param center Center of circle to draw
 *  \param radius Radius of circle to draw
 *  \param shade Shade to draw circle with on fb
 *  \returns void
 *  \ingroup Framebuffer
 */
inline void fb_draw_circle(framebuffer fb, circle* c) {

    fb_draw_ellipse(fb, c->center, c->radius, c->radius, c->center->shade);
}

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
 *  \ingroup Framebuffer
 */
private void _fb_fill_four_ellipse_points(framebuffer, point*, ushort, ushort);

/*! Draw on fb a shaded ellipse described by center, x- and y-radii.
 *  \brief Draw on fb a shaded ellipse described by center, x- and y-radii.
 *  \param fb Framebuffer to use as canvas
 *  \param center Gemoetric center of the ellipse
 *  \param x_radius X radius of ellipse to plot
 *  \param y_radius Y radius of ellipse to plot
 *  \param shade Color to fill ellipse with
 *  \ingroup Framebuffer
 *  OPTIONAL: allow for different specified border color
 */
void fb_draw_ellipse_fill(framebuffer, point*, ushort, ushort, shade_t);

/*! Plot on fb the four symmetric points on an ellipse.
 *  \brief Plot on fb the four symmetric points on an ellipse
 *  described by a center and x- and y-offsets.
 *  \param fb Framebuffer to use as canvas
 *  \param center Geometric center of the ellipse
 *  \param x X offset of point to plot
 *  \param y Y offset of point to plot
 *  \returns void
 *  \note Points will be shaded with the value in \center->shade.
 *  \ingroup Framebuffer
 */
private
void _fb_plot_four_ellipse_points(framebuffer, point*, ushort, ushort);

/*! Print a point (ordered pair) to the console followed by a newline.
 *  \brief Print a point (ordered pair) to the console followed by a newline.
 *  \note This function is only applicable on a computer
 *  (not for use on microcontrollers).
 *  \details This function appends a newline to the console after
 *  printing the ordered pair. To just print the ordered pair (sans
 *  newline) call function \fb_console_print_point instead.
 *  \param p Point to printf to the console
 *  \returns void
 *  \ingroup Framebuffer
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
 *  \ingroup Framebuffer
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
 *  \ingroup Framebuffer
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
 *  \ingroup Framebuffer
 */
void fb_console_print_coordinate(uchar, uchar, shade_t);

/** These functions were not implemented at the conclusion of EE445L. **/
/* private pixel_t _fb_pixel_width_of_long(long); */
/* private pixel_t _fb_pixel_height_of_long(long); */
/* private pixel_t _fb_pixel_width_of_string(char*); */

#endif	/*! __FRAMEBUFFER__ */
