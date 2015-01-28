#include <stdlib.h>
#include <stdarg.h>

#include "shape.h"

point* shape_create_point(ushort x, ushort y, shade_t shade) {
    point* p = malloc(sizeof(point));
    p->x = x;
    p->y = y;
    p->shade = shade;
    return p;
}

shape* shape_create_triangle(point* p0, point* p1, point* p2) {

  return shape_create(3, p0, p1, p2);
}

shape* shape_create_quad(point* top_left_corner, uchar width, uchar height) {

    ushort x = top_left_corner->x;
    ushort y = top_left_corner->y;
    shade_t shade = top_left_corner->shade;
    return shape_create(4,
			top_left_corner,
			shape_create_point(x+width, y,        shade),
			shape_create_point(x+width, y+height, shade),
			shape_create_point(x,       y+height, shade));
}

shape* shape_create(ushort num_points, ...) {

  unsigned char i;
  shape* sh = (shape*) calloc(1, sizeof(shape));
  point** pts = (point**) calloc(num_points, sizeof(point*));
  va_list args;

  va_start(args, num_points);
  for(i=0; i < num_points; ++i) {
    pts[i] = va_arg(args, point*);
  }
  va_end(args);			/* clean up the list */

  sh->num_points = num_points;
  sh->points = pts;
  return sh;
}

void shape_destroy_shape(shape* sh) {

  ushort i;
  for(i=0; i<sh->num_points; ++i) {
    free(sh->points[i]);
  }
  free(sh->points);
}

circle* shape_create_circle(ushort radius, point* center) {

    circle* cir = (circle*) calloc(1, sizeof(circle));
    cir->center = center;
    cir->radius = radius;
    return cir;
}

shape* shape_duplicate_shape(shape* s) {

    ushort i;
    shape* dup = (shape*) calloc(1, sizeof(shape));

    dup->num_points = s->num_points;
    dup->points = (point**) calloc(dup->num_points, sizeof(point*));

    for(i=0; i < dup->num_points; ++i) {
	dup->points[i] = s->points[i];
    }
    return dup;
}
