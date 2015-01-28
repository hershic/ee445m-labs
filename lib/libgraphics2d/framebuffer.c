/* framebuffer.c
 * Hershal Bhave and Eric Crosson
 * 2014-02-08
 * Function: represent a framebuffer of an image in memory.
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "string.h"
#include "g2d_defines.h"
#include "framebuffer.h"

/*
  Create a framebuffer object and return the handle.
  Inputs:  none
  Outputs: framebuffer
*/
framebuffer FBInit() {
  /* OPTIONAL TODO: bit-strap this struct */
  unsigned char i;
  unsigned char** fb = (unsigned char**) calloc(OLED_WIDTH, sizeof(unsigned char*));
  for(i = 0; i < OLED_WIDTH; ++i) {
    fb[i] = (unsigned char*) calloc(OLED_HEIGHT, sizeof(unsigned char));
  }
  return (framebuffer) fb;
}

/*
  Destroy a framebuffer object.
  Inputs:  fb    framebuffer to send to valhalla
  Outputs: none
*/
void FBDestroy(framebuffer fb) {

  unsigned char i;
  for(i = 0; i < OLED_WIDTH; ++i) {
    free(fb[i]);
  }
  free(fb);
}

/*
  Using fb as a canvas, draw sh.
  Input:  fb       framebuffer to use as canvas
  sh       shape to draw
  Output: none

  ** NOTICE **
  Note that this function can also draw points and lines, i.e. a shape
  with only one or two points respectively.
*/
void FBDrawShape(framebuffer fb, shape* sh) {
  when (sh->points[0] != null) {
    _FBDrawShape(fb, sh, sh->points[0]->shade);
  }
}

/*
  Using fb as a canvas, draw sh.
  Input:  fb       framebuffer to use as canvas
  sh       shape to draw
  Output: none

  ** NOTICE **
  Note that this function can also draw points and lines, i.e. a shape
  with only one or two points respectively.
*/
void FBEraseShape(framebuffer fb, shape* sh) {
  _FBDrawShape(fb, sh, FB_COLOR_ERASE);
}

/*
  Internal helper function that performs the mechanics of drawing a
  shape. The purpose of this function is to reduce duplicated code in
  memory.
  Input:  fb        framebuffer to use as canvas
  sh        shape to draw
  shade     color of shape
  Output: void
*/
private void _FBDrawShape(framebuffer fb, shape* sh, shade_t shade) {

  unsigned short p;
  for(p=0; p < sh->num_points; ++p) {
    FBSetPixel(fb, sh->points[p]->x, sh->points[p]->y, shade);
    /* TODO: add gradient between points */
    if (p > 0) {
      FBDrawLine(fb, sh->points[p-1], sh->points[p], shade);
    }
  }

  /* Enclose the figure */
  if(p>1) {
    FBDrawLine(fb, sh->points[0], sh->points[p-1], shade);
  }
}

/*
  Method to draw multiple shapes on fb. Provide the number of shapes
  to draw for va_args.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to draw before returning
  Output: void
*/
void FBDrawMultipleShapes(framebuffer fb, ushort numShapes, ...) {

  unsigned char i;
  va_list args;

  va_start(args, numShapes);
  for(i=0; i < numShapes; ++i) {
    FBDrawShape(fb, va_arg(args, shape*));
  }
  va_end(args);                 /* clean up the list */
}

/*
  Method to draw an array of shapes on fb.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to draw before returning
  shapeArr   array containing shapes to draw
  Output: void
*/
void FBDrawShapeArr(framebuffer fb, ushort numShapes, shape** shapeArr) {

  unsigned char i;
  for(i=0; i < numShapes; ++i) {
    FBDrawShape(fb, shapeArr[i]);
  }
}

/*
  Method to erase an array of shapes from fb.
  Input:  fb         framebuffer to use as canvas
  numShapes  number of shapes to erase before returning
  shapeArr   array containing shapes to erase
  Output: void
*/
void FBEraseShapeArr(framebuffer fb, ushort numShapes, shape** shapeArr) {

  unsigned char i;
  for(i=0; i < numShapes; ++i) {
    FBEraseShape(fb, shapeArr[i]);
  }
}

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
void FBSetPixel(framebuffer fb, uchar x, uchar y, shade_t shade) {
  if (x < FB_WIDTH && y < FB_HEIGHT) {
    fb[x][y] = shade;
  }
}

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
void FBClearPixel(framebuffer fb, uchar x, uchar y) {
  FBSetPixel(fb, x, y, FB_COLOR_ERASE);
}

/*
  Draw a line segment on fb from a to b of color shade.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  shade     color of line segment
  Output: void
*/
void FBDrawLine(framebuffer fb, point* pta, point* ptb, shade_t shade) {
  _FBDrawLine(fb, pta, ptb, shade);
}

/*
  Remove a line segment on fb from a to b.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  Output: void
*/
void FBEraseLine(framebuffer fb, point* pta, point* ptb) {
  _FBDrawLine(fb, pta, ptb, FB_COLOR_ERASE);
}

/*
  Internal helper function that performs the mechanics of drawing a
  line. The purpose of this function is to reduce duplicated code in
  memory.
  Input:  fb        framebuffer to use as canvas
  a         beginning of line segment
  b         end of line segment
  shade     color of line segment
  Output: void
*/
private void _FBDrawLine(framebuffer fb, point* pta, point* ptb, shade_t shade) {
  point* a;
  point* b;
  int dx, dy, sx, sy, e2, err;

  a = shape_duplicate_point(pta);
  b = shape_duplicate_point(ptb);

  dx = abs(b->x-a->x); sx = (a->x < b->x) ? 1 : -1;
  dy = abs(b->y-a->y); sy = (a->y < b->y) ? 1 : -1;
  err = (dx>dy ? dx : -dy)/2;
  for(;;) {
    FBSetPixel(fb, a->x, a->y, shade);
    if (a->x == b->x && a->y == b->y) {break;}
    e2 = err;
    if (e2 >-dx) {err -= dy; a->x += sx;}
    if (e2 < dy) {err += dx; a->y += sy;}
  }
  SHDestroyPoint(a);
  SHDestroyPoint(b);
}

/*
  Draw a circle in fb at center with radius of color shade.
  Input:  fb         framebuffer to use as canvas
  center     center of circle to draw
  radius     radius of circle to draw
  shade      shade to draw circle with on fb
  Output: void
*/
void FBDrawCircle(framebuffer fb, circle* c) {
  FBDrawEllipse(fb, c->center, c->radius, c->radius, c->center->shade);
}

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
void FBDrawEllipse(framebuffer fb, point* center,
                   ushort x_radius, ushort y_radius,
                   shade_t shade) {
  ushort x, y;
  int xx, yy, xx2, yy2, dx, dy, stop_x, stop_y, error;

  x = x_radius;
  y = 0;

  xx = x_radius * x_radius; // width^2
  yy = y_radius * y_radius; // height^2

  xx2 = xx*2;
  yy2 = yy*2;

  dx = yy * (1 - 2*x_radius);
  dy = xx;

  stop_x = yy*2*x_radius;
  stop_y = 0;

  error = 0;

  while (stop_x > stop_y) {
    _FBPlotFourEllipsePoints(fb, center, x, y);
    ++y;
    stop_y += xx2;
    error += dy;
    dy += xx2;
    if (2*error + dx > 0) {
      --x;
      stop_x -= yy2;
      error += dx;
      dx += yy2;
    }
  }

  /* First set of points is done, now plot the second set */
  x = 0;
  y = y_radius;
  dx = yy;
  dy = xx * (1 - 2*y_radius);
  error = 0;
  stop_x = 0;
  stop_y = xx2*y_radius;

  while (stop_x <= stop_y) {
    _FBPlotFourEllipsePoints(fb, center, x, y);
    x++;
    stop_x += yy2;
    error += dx;
    dx += yy2;
    if (2*error + dy > 0) {
      --y;
      stop_y -= xx2;
      error += dy;
      dy += xx2;
    }
  }
}

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
private void _FBPlotFourEllipsePoints(framebuffer fb, point* center, ushort x, ushort y) {
  FBSetPixel(fb, center->x + x, center->y + y, center->shade);
  FBSetPixel(fb, center->x - x, center->y + y, center->shade);

  FBSetPixel(fb, center->x - x, center->y - y, center->shade);
  FBSetPixel(fb, center->x + x, center->y - y, center->shade);
}

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
private void _FBFillFourEllipsePoints(framebuffer fb, point* center, ushort x, ushort y) {
  int i;
  for(i = -x; i <= x; ++i) {
    FBSetPixel(fb, center->x + i, center->y + y, center->shade);
    FBSetPixel(fb, center->x + i, center->y - y, center->shade);
  }
}

/*
  Draw on fb a shaded ellipse described by center, x- and y-radii.
  Input:  fb            framebuffer to use as canvas
  center        gemoetric center of the ellipse
  x_radius      x radius of ellipse to plot
  y_radius      y radius of ellipse to plot
  shade         color to fill ellipse with
  OPTIONAL TODO: allow for different specified border color
*/
void FBDrawEllipseFill(framebuffer fb, point* center,
                       ushort x_radius, ushort y_radius,
                       shade_t shade) {
  ushort x, y;
  int xx, yy, xx2, yy2, dx, dy, last_y_filled, stop_x, stop_y, error, i;
  xx = x_radius * x_radius; // width^2
  yy = y_radius * y_radius; // height^2

  xx2 = xx*2;
  yy2 = yy*2;

  error = 0;

  // fill the horizontal diameter
  for (i = -x_radius; i <= x_radius; ++i) {
    FBSetPixel(fb, center->x + i, center->y, center->shade);
  }

  /* First set of points is done, now plot the second set */
  x = 0;
  y = y_radius;
  dx = yy;
  dy = xx * (1 - 2*y_radius);
  stop_x = 0;
  stop_y = xx2*y_radius;

  // OPTIONAL OPTIMIZE: this loop
  last_y_filled = y;
  while (stop_x <= stop_y) {
    if (y != last_y_filled) {
      _FBFillFourEllipsePoints(fb, center, x, y);
      last_y_filled = y;
    }
    x++;
    stop_x += yy2;
    error += dx;
    dx += yy2;
    if (2*error + dy > 0) {
      --y;
      stop_y -= xx2;
      error += dy;
      dy += xx2;
    }
  }
}

void FBDrawStringAnonPt(framebuffer fb, point* top_left_corner, char* string) {
  _FBDrawString(fb, top_left_corner, string);
  free(top_left_corner);
}

/*
  Draw string on fb starting at top_left_corner.
  Inputs:  fb               framebuffer to use as canvas
  top_left_corner  coordinate of top left corner on string on fb
  string           string to draw
  Outputs: void
*/
void FBDrawString(framebuffer fb, point* top_left_corner, char* string) {
  _FBDrawString(fb, top_left_corner, string);
}

void FBEraseStringAnonPt(framebuffer fb, point* top_left_corner, char* string) {
  FBEraseString(fb, top_left_corner, string);
  free(top_left_corner);
}

/*
  Erase string on fb starting at top_left_corner.
  Inputs:  fb               framebuffer to use as canvas
  top_left_corner  coordinate of top left corner on string on fb
  string           string to erase
  Outputs: void
*/
void FBEraseString(framebuffer fb, point* top_left_corner, char* string) {
  point* pt = shape_duplicate_point(top_left_corner);
  pt->shade = FB_COLOR_ERASE;
  _FBDrawString(fb, pt, string);
  SHDestroyPoint(pt);
}

/*
  Helper function that peforms the base work of all framebuffer string drawing requests.
  Inputs:  fb              framebuffer to use as canvas
  top_left_corner coordinate of top left corner on string on fb
  string          string to draw or undraw
  Outputs: void
*/
private void _FBDrawString(framebuffer fb, point* top_left_corner, char* string) {
  uchar active, col, row, mask;
  point* pen = shape_duplicate_point(top_left_corner);

  while(*string != null) {
    for(col=0; col < VALVANO_FONT_WIDTH; ++col) {
      mask = 0x01;
      if (pen->x+col >= FB_WIDTH) {break;}
      for(row=0; row < VALVANO_FONT_HEIGHT; ++row) {
        if (pen->y+row >= FB_HEIGHT) {break;}
        active = mask & valvanoFont[*string][col];
        FBSetPixel(fb, pen->x + col, pen->y + row, active ? pen->shade : 0);
        mask = mask << 1;
      }
    }
    pen->x += VALVANO_FONT_WIDTH + VALVANO_FONT_KERNING;
    ++string;
  }
  SHDestroyPoint(pen);
}

/*
  Convert an intever to a string.
  Inputs;  i        int to convert into a string
  buffer   buffer for string contents
  length   length of allocated buffer
  Outputs: char*    buffer containing i as a string
*/
char* itoa(int i, char* buffer, uchar length) {
  if (snprintf(buffer, length, "%02d", i) == -1)
    return ""; /* base case, or error */

  return buffer;
}

/*
  Pass this function two newly created points and it will draw a line
  connecting the points before destroying the points for you.
  Inputs:  fb      framebuffer to use as canvas
  a       one endpoint of the line to draw
  b       the other endpoint of the line to draw
  shade   desired shade of the line on fb
  Outputs: void
*/
void FBDrawAnonLine(framebuffer fb, point* a, point* b, shade_t shade) {
  _FBDrawLine(fb, a, b, shade);
  SHDestroyPoint(a);
  SHDestroyPoint(b);
}

/*
  Pass this function two newly created points and it will erase a line
  connecting the points before destroying the points for you.
  Inputs:  fb      framebuffer to use as canvas
  a       one endpoint of the line to draw
  b       the other endpoint of the line to draw
  shade   desired shade of the line on fb
  Outputs: void
*/
void FBEraseAnonLine(framebuffer fb, point* a, point* b) {
  _FBDrawLine(fb, a, b, FB_COLOR_ERASE);
  SHDestroyPoint(a);
  SHDestroyPoint(b);
}

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
void FBPrintPointToConsole(point* p) {
#ifdef __GNUC__
  printf("(%d,%d):%d\n", p->x, p->y, p->shade);
#endif
}

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
void FBPrintPointToConsoleWithoutNewline(point* p) {
#ifdef __GNUC__
  printf("(%d,%d):%d", p->x, p->y, p->shade);
#endif
}
