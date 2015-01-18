#ifndef __SHAPE__
#define __SHAPE__

/* shape.h
 * Hershal Bhave and Eric Crosson
 * 2014-02-08
 * Representation of a shape (polygon) comprised of vertices (points)
 * and connected by line segments.
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include <stdlib.h>
#include "g2d_defines.h"

typedef struct point {
  /* A point consists of x,y coordinates */
  unsigned short x;
  unsigned short y;

  /* 4-bit colors */
  shade_t shade : SHADE_SIZE;
} point;

typedef struct shape {
  /* A shape consists of multiple points */
  unsigned short numPoints;
  point** points;
} shape;

typedef struct circle {
  /* A circle consists of a center and a radius */
  point* center;
  ushort radius;
} circle;

/* Macro to create a duplicate of a point object. Don't forget to
 * destroy the duplicate. */
#define SHDuplicatePoint(p) SHCreatePoint(p->x, p->y, p->shade);

/* Macro to keep the consistency in Creating/Destroying objects. */
#define SHDestroyPoint(p)   free(p);

/* Functions for managing point objects */
point* SHCreatePoint(ushort x, ushort y, shade_t shade);
void   FBPrintPointToConsole(point* p);

/* Functions for managing shape objects */
shape* SHCreateShape(ushort numPoints, ...);
shape* SHDuplicateShape(shape* s);
void   SHDestroyShape(shape* sh);

/* Functions for easy creation of common shapes */
circle* SHCreateCircle(ushort radius, point* center);
shape*  SHCreateTriangle(point* p0, point* p1, point* p2);
shape*  SHCreateQuad(point* top_left_corner, uchar width, uchar height);

#endif	/*  __SHAPE__ */
