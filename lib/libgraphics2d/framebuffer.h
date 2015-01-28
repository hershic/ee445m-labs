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
#define max_pixel_width_of_long(x, y)  (max_uc(_FBPixelWidthOfLong(x), _FBPixelWidthOfLong(y)))
#define max_pixel_height_of_long(x, y) (max_uc(_FBPixelHeightOfLong(x),_FBPixelHeightOfLong(y)))

unsigned char max_uc(unsigned char one, unsigned char two);

/*
  Convert an intever to a string.
  Inputs;  i        int to convert into a string
  buffer   buffer for string contents
  length   length of allocated buffer
  Outputs: char*    buffer containing i as a string
*/
char* itoa(int i, char* buffer, uchar length);

/*
  Create a framebuffer object and return the handle.
  Inputs:  none
  Outputs: framebuffer
*/
framebuffer FBInit(void);

/*
  Destroy a framebuffer object.
  Inputs:  fb    framebuffer to send to valhalla
  Outputs: none
*/
void FBDestroy(framebuffer fb);

/*
  Draw string on fb starting at top_left_corner.
  Inputs:  fb               framebuffer to use as canvas
  top_left_corner  coordinate of top left corner on string on fb
  string           string to draw
  Outputs: void
*/
void FBDrawString(framebuffer fb, point* top_left_corner, char* string);

/* TODO: document */
void FBDrawStringAnonPt(framebuffer fb, point* top_left_corner, char* string);

/*
  Erase string on fb starting at top_left_corner.
  Inputs:  fb               framebuffer to use as canvas
  top_left_corner  coordinate of top left corner on string on fb
  string           string to erase
  Outputs: void
*/
void FBEraseString(framebuffer fb, point* top_left_corner, char* string);

void FBEraseStringAnonPt(framebuffer fb, point* top_left_corner, char* string);

void FBEraseChar(framebuffer, point* top_left_corner);

/*
  Helper function that peforms the base work of all framebuffer string drawing requests.
  Inputs:  fb              framebuffer to use as canvas
  top_left_corner coordinate of top left corner on string on fb
  string          string to draw or undraw
  Outputs: void
*/
private void _FBDrawString(framebuffer fb, point* top_left_corner, char* string);

/*
  Using fb as a canvas, draw sh.
  Input:  fb       framebuffer to use as canvas
  sh       shape to draw
  Output: none

  ** NOTICE **
  Note that this function can also draw points and lines, i.e. a shape
  with only one or two points respectively.
*/
void FBDrawShape(framebuffer fb, shape* sh);

/*
  Using fb as a canvas, draw sh.
  Input:  fb       framebuffer to use as canvas
  sh       shape to draw
  Output: none

  ** NOTICE **
  Note that this function can also draw points and lines, i.e. a shape
  with only one or two points respectively.
*/
void FBEraseShape(framebuffer fb, shape* sh);

/*
  Method to draw multiple shapes on fb. Provide the number of shapes
  to draw for va_args.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to draw before returning
  Output: void
*/
void FBDrawMultipleShapes(framebuffer fb, ushort numShapes, ...);

/*
  Method to draw an array of shapes on fb.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to draw before returning
  shapeArr   array containing shapes to draw
  Output: void
*/
void FBDrawShapeArr(framebuffer fb, ushort numShapes, shape** shapeArr);

/*
  Method to erase an array of shapes from fb.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to erase before returning
  shapeArr   array containing shapes to erase
  Output: void
*/
void FBEraseShapeArr(framebuffer fb, ushort numShapes, shape** shapeArr);

/*
  Internal helper function that performs the mechanics of drawing a
  shape. The purpose of this function is to reduce duplicated code in
  memory.
  Input:  fb        framebuffer to use as canvas
  sh        shape to draw
  shade     color of shape
  Output: void
*/
private void _FBDrawShape(framebuffer fb, shape* sh, shade_t shade);

/*
  In fb, set pixel (x,y) to shade
  Input:  fb       framebuffer to use as canvas
  x        x coordinate
  y        y coordinate
  shade    shade to set pixel
  Output: void
  Error behavior: when pixel (x,y) is not writeable (off screen)
  this function does nothing.
*/
void FBSetPixel(framebuffer fb, uchar x, uchar y, shade_t shade);

/*
  In fb, clear pixel (x,y). This is an alias for FBSetPixel with a
  shade of zero.
  Input:  fb       framebuffer to use as canvas
  x        x coordinate
  y        y coordinate
  Output: void
  Error behavior: when pixel (x,y) is not writeable (off screen)
  this function does nothing.
*/
void FBClearPixel(framebuffer fb, uchar x, uchar y);

/*
  Draw a line segment on fb from a to b of color shade.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  shade     color of line segment
  Output: void
*/
void FBDrawLine(framebuffer fb, point* a, point* b, shade_t shade);

/*
  Pass this function two newly created points and it will draw a line
  connecting the points before destroying the points for you.
  Inputs:  fb      framebuffer to use as canvas
  a       one endpoint of the line to draw
  b       the other endpoint of the line to draw
  shade   desired shade of the line on fb
  Outputs: void
  \warning This function destroys \a and \b.
*/
private void FBDrawAnonLine(framebuffer fb, point* a, point* b, shade_t shade);

/*
  Remove a line segment on fb from a to b.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  Output: void
*/
void FBEraseLine(framebuffer fb, point* pta, point* ptb);

/*
  Pass this function two newly created points and it will erase a line
  connecting the points before destroying the points for you.
  Inputs:  fb      framebuffer to use as canvas
  a       one endpoint of the line to draw
  b       the other endpoint of the line to draw
  shade   desired shade of the line on fb
  Outputs: void
  \warning This function destroys \a and \b.
*/
void FBEraseAnonLine(framebuffer fb, point* a, point* b);

private void _FBDrawLine(framebuffer fb, point* a, point* b, shade_t shade);

/*
  Draw a circle in fb at center with radius of color shade.
  Input:  fb         framebuffer to use as canvas
  center     center of circle to draw
  radius     radius of circle to draw
  shade      shade to draw circle with on fb
  Output: void
*/
void FBDrawCircle(framebuffer fb, circle* c);

/*
  Draw an ellipse in fb at center with specified x- and y-radii, of
  color shade.
  Input:  fb         framebuffer to use as canvas
  center     center of ellipse to draw
  x_radius   x radius of specified ellipse
  y_radius   y radius of specified ellipse
  shade      shade to draw ellipse with on fb
  Output: void
*/
void FBDrawEllipse(framebuffer fb, point* center, ushort x_radius, ushort y_radius, shade_t
		   shade);

/*
  Fill on fb from the four symmetric points on an ellipse described by
  a center and x- and y-offsets, to the axis of said ellipse.
  Input:  fb            framebuffer to use as canvas
  center        geometric center of the ellipse
  x             x offset of point to plot
  y             y offset of point to plot
  Output: void
  Note: points will be shaded with the value in center->shade.
*/
private void _FBFillFourEllipsePoints(framebuffer fb, point* center, ushort x, ushort y);
/*
  Draw on fb a shaded ellipse described by center, x- and y-radii.
  Input:  fb            framebuffer to use as canvas
  center        gemoetric center of the ellipse
  x_radius      x radius of ellipse to plot
  y_radius      y radius of ellipse to plot
  shade         color to fill ellipse with
  OPTIONAL TODO: allow for different specified border color
*/
void FBDrawEllipseFill(framebuffer fb, point* center, ushort x_radius, ushort y_radius, shade_t shade);

/*
  Plot on fb the four symmetric points on an ellipse described by a
  center and x- and y-offsets.
  Input:  fb            framebuffer to use as canvas
  center        geometric center of the ellipse
  x             x offset of point to plot
  y             y offset of point to plot
  Output: void
  Note: points will be shaded with the value in center->shade.
*/
private void _FBPlotFourEllipsePoints(framebuffer fb, point* center, ushort x, ushort y);

private pixel_t _FBPixelWidthOfLong(long l);

private pixel_t _FBPixelHeightOfLong(long l);

private pixel_t _FBPixelWidthOfString(char* str);

/* TODO: document below */
/* Methods to print points to the console. Will not work on an
 * embedded system (functions will return immediately). */

/*
  Print a point (ordered pair) to the console. Only applicable on a
  computer (not for use on microcontrollers).

  ** NOTICE **
  This function appends a newline to the console after printing the
  ordered pair. To just print the ordered pair (sans newline) call
  function FBPrintPointToConsoleWithoutNewline instead.

  Inputs:  p       point to printf to the console
  Outputs: void
*/
void FBPrintPointToConsole(point* p);

/*
  Print a point (ordered pair) to the console. Only applicable on a
  computer (not for use on microcontrollers).

  ** NOTICE **

  This function does not append a newline to the console after
  printing the ordered pair. To just print the ordered pair WITH
  a newline call function FBPrintPointToConsole instead.

  Inputs:  p       point to printf to the console
  Outputs: void
*/
void FBPrintPointToConsoleWithoutNewline(point* p);

#endif	/* __FRAMEBUFFER__ */
