#include <math.h>
#include <stdlib.h>
#include "string.h"
#include "g2d_defines.h"
#include "framebuffer.h"

#define NDEBUG

#ifndef NDEBUG
#include <stdio.h>
#endif

framebuffer FBInit() {
  /* OPTIONAL OPTIMIZE: bit-strap this struct to reduce memory consumption */
  unsigned char i;
  unsigned char** fb = (unsigned char**) calloc(OLED_WIDTH, sizeof(unsigned char*));
  for(i = 0; i < OLED_WIDTH; ++i) {
    fb[i] = (unsigned char*) calloc(OLED_HEIGHT, sizeof(unsigned char));
  }
  return (framebuffer) fb;
}

void FBDestroy(framebuffer fb) {

  unsigned char i;
  for(i = 0; i < OLED_WIDTH; ++i) {
    free(fb[i]);
  }
  free(fb);
}

void FBDrawShape(framebuffer fb, shape* sh) {

  when (sh->points[0] != null) {
    _FBDrawShape(fb, sh, sh->points[0]->shade);
  }
}

void FBEraseShape(framebuffer fb, shape* sh) {

  _FBDrawShape(fb, sh, FB_COLOR_ERASE);
}

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

void FBDrawMultipleShapes(framebuffer fb, ushort numShapes, ...) {

  unsigned char i;
  va_list args;

  va_start(args, numShapes);
  for(i=0; i < numShapes; ++i) {
    FBDrawShape(fb, va_arg(args, shape*));
  }
  va_end(args);                 /* clean up the list */
}

void FBDrawShapeArr(framebuffer fb, ushort numShapes, shape** shapeArr) {

  unsigned char i;
  for(i=0; i < numShapes; ++i) {
    FBDrawShape(fb, shapeArr[i]);
  }
}

void FBEraseShapeArr(framebuffer fb, ushort numShapes, shape** shapeArr) {

  unsigned char i;
  for(i=0; i < numShapes; ++i) {
    FBEraseShape(fb, shapeArr[i]);
  }
}

void FBSetPixel(framebuffer fb, uchar x, uchar y, shade_t shade) {

  if (x < FB_WIDTH && y < FB_HEIGHT) {
    fb[x][y] = shade;
  }
}

void FBClearPixel(framebuffer fb, uchar x, uchar y) {

  FBSetPixel(fb, x, y, FB_COLOR_ERASE);
}

void FBDrawLine(framebuffer fb, point* pta, point* ptb, shade_t shade) {

  _FBDrawLine(fb, pta, ptb, shade);
}

void FBEraseLine(framebuffer fb, point* pta, point* ptb) {

  _FBDrawLine(fb, pta, ptb, FB_COLOR_ERASE);
}

private void _FBDrawLine(framebuffer fb, point* pta, point* ptb, shade_t shade) {

  point* a;
  point* b;
  int dx, dy, sx, sy, e2, err;

  a = SHDuplicatePoint(pta);
  b = SHDuplicatePoint(ptb);

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

void FBDrawCircle(framebuffer fb, circle* c) {

  FBDrawEllipse(fb, c->center, c->radius, c->radius, c->center->shade);
}

void FBDrawEllipse(framebuffer fb,
		   point*      center,
                   ushort      x_radius,
		   ushort      y_radius,
                   shade_t     shade) {

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

private void _FBPlotFourEllipsePoints(framebuffer fb, point* center, ushort x, ushort y) {

  FBSetPixel(fb, center->x + x, center->y + y, center->shade);
  FBSetPixel(fb, center->x - x, center->y + y, center->shade);
  FBSetPixel(fb, center->x - x, center->y - y, center->shade);
  FBSetPixel(fb, center->x + x, center->y - y, center->shade);
}

private void _FBFillFourEllipsePoints(framebuffer fb, point* center, ushort x, ushort y) {

  int i;
  for(i = -x; i <= x; ++i) {
    FBSetPixel(fb, center->x + i, center->y + y, center->shade);
    FBSetPixel(fb, center->x + i, center->y - y, center->shade);
  }
}

void FBDrawEllipseFill(framebuffer fb,
		       point*      center,
                       ushort      x_radius,
		       ushort      y_radius,
                       shade_t     shade) {

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

void FBDrawString(framebuffer fb, point* top_left_corner, char* string) {

  _FBDrawString(fb, top_left_corner, string);
}

void FBEraseStringAnonPt(framebuffer fb, point* top_left_corner, char* string) {

  FBEraseString(fb, top_left_corner, string);
  free(top_left_corner);
}

void FBEraseString(framebuffer fb, point* top_left_corner, char* string) {

  point* pt = SHDuplicatePoint(top_left_corner);
  pt->shade = FB_COLOR_ERASE;
  _FBDrawString(fb, pt, string);
  SHDestroyPoint(pt);
}

private void _FBDrawString(framebuffer fb, point* top_left_corner, char* string) {

  uchar active, col, row, mask;
  point* pen = SHDuplicatePoint(top_left_corner);

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

char* itoa(int i, char* buffer, uchar length) {

  if (snprintf(buffer, length, "%02d", i) == -1) {
    return ""; /* base case, or error */
  }
  return buffer;
}

void FBDrawAnonLine(framebuffer fb, point* a, point* b, shade_t shade) {

  _FBDrawLine(fb, a, b, shade);
  SHDestroyPoint(a);
  SHDestroyPoint(b);
}

void FBEraseAnonLine(framebuffer fb, point* a, point* b) {

  _FBDrawLine(fb, a, b, FB_COLOR_ERASE);
  SHDestroyPoint(a);
  SHDestroyPoint(b);
}

void FBPrintPointToConsole(point* p) {

#ifdef __GNUC__
  printf("(%d,%d):%d\n", p->x, p->y, p->shade);
#endif
  /* OPTIONAL: log error */
}

void FBPrintPointToConsoleWithoutNewline(point* p) {

#ifdef __GNUC__
  printf("(%d,%d):%d", p->x, p->y, p->shade);
#endif
  /* OPTIONAL: log error */
}
