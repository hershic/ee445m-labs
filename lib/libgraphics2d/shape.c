#include <stdlib.h>
#include <stdarg.h>

#include "shape.h"

point* SHCreatePoint(ushort x, ushort y, shade_t shade) {
    point* p = malloc(sizeof(point));
    p->x = x;
    p->y = y;
    p->shade = shade;
    return p;
}

shape* SHCreateTriangle(point* p0, point* p1, point* p2) {
  return SHCreateShape(3, p0, p1, p2);
}

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

void SHDestroyShape(shape* sh) {

  ushort i;
  for(i=0; i<sh->numPoints; ++i) {
    free(sh->points[i]);
  }
  free(sh->points);
}

circle* SHCreateCircle(ushort radius, point* center) {

    circle* c = (circle*) calloc(1, sizeof(circle));
    c->center = center;
    c->radius = radius;
    return c;
}

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
