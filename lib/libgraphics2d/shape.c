/* shape.c
 * Hershal Bhave and Eric Crosson
 * 2014-02-08
 * Representation of a shape (polygon) comprised of vertices (points)
 * and connected by line segments.
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include <stdlib.h>
#include <stdarg.h>
#include "shape.h"

/*
  Return a point that describes the given arguments.
  Input:  x        x coordinate
          y        y coordinate
          color    shading of this point
  Output: A newly created point
*/
point* SHCreatePoint(ushort x, ushort y, shade_t shade) {
    point* p = malloc(sizeof(point));
    p->x = x;
    p->y = y;
    p->shade = shade;
    return p;
}

/*
  A handy function to create a triangle.
  Input:  p0          vertex 1 of the triangle
          p1          vertex 2 of the triangle
          p2          vertex 3 of the triangle
  Output: pointer to the described triangle
 */
shape* SHCreateTriangle(point* p0, point* p1, point* p2) {
  return SHCreateShape(3, p0, p1, p2);
}

/*
  A handy function to create a general quadrilateral.
  Input:  p0          vertex 1 of the triangle
          p1          vertex 2 of the triangle
          p2          vertex 3 of the triangle
  Output: pointer to the described quadrilateral
 */
shape* SHCreateQuad(point* top_left_corner, uchar width, uchar height) {
    ushort x = top_left_corner->x;
    ushort y = top_left_corner->y;
    shade_t shade = top_left_corner->shade;
    return SHCreateShape(4,
			 top_left_corner,
			 SHCreatePoint(x+width, y,        shade),
			 SHCreatePoint(x+width, y+height, shade),
			 SHCreatePoint(x,       y+height, shade));
}

/*
  Return a pointer to a shape with numPoints points.
  Input:  numpoints        number of points to follow
          points...        comma separated points
  Output: pointer to the described shape
  Note: va_args requires a preceeding argument.
 */
shape* SHCreateShape(ushort numPoints, ...) {

  unsigned char i;
  shape* sh = (shape*) calloc(1, sizeof(shape));
  point** pts = (point**) calloc(numPoints, sizeof(point*));
  va_list args;

  va_start(args, numPoints);
  for(i=0; i < numPoints; ++i) {
    pts[i] = va_arg(args, point*);
  }
  va_end(args);			/* clean up the list */

  sh->numPoints = numPoints;
  sh->points = pts;
  return sh;
}

/*
  Destroy a shape object.
  Inputs:  sh    shape object to destroy
  Outputs: void
 */
void SHDestroyShape(shape* sh) {

  ushort i;
  for(i=0; i<sh->numPoints; ++i) {
    free(sh->points[i]);
  }
  free(sh->points);
}

/*
  Return a pointer to a circle in memory described by center and
  radius.
  Input:  center      point describing the center of the circle
          radius      length of the radius in "units"
  Output: pointer to the described circle
 */
circle* SHCreateCircle(ushort radius, point* center) {

    circle* c = (circle*) calloc(1, sizeof(circle));
    c->center = center;
    c->radius = radius;
    return c;
}

/*
  Destroy a circle object.
  radius.
  Input:  circle      circle object to destroy
  Output: void
 */
void SHDestroyCircle(circle* cir) {
  /* free(cir->pt_center); */
  free(cir);
}

/*
  Duplicate a shape object.
  radius.
  Input:  shape      the shape to duplicate
  Output: shape      a duplicate of the given shape
 */
shape* SHDuplicateShape(shape* s) {

    ushort i;
    shape* dup = (shape*) calloc(1, sizeof(shape));

    dup->numPoints = s->numPoints;
    dup->points = (point**) calloc(dup->numPoints, sizeof(point*));

    for(i=0; i < dup->numPoints; ++i) {
	dup->points[i] = s->points[i];
    }

    return dup;
}
