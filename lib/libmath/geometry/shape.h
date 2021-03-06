/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SHAPE__
#define __SHAPE__

#include <stdlib.h>
#include "libstd/defines.h"

/*! \struct point
 *  \brief Representation of an ordered pair with a shade
 *  \details An ordered pair to a Cartesian coordinate system.
 *  \ingroup Geometry
 */
typedef struct point {

    unsigned short x;
    unsigned short y;
    /* 4-bit colors */
    shade_t shade : SHADE_SIZE;
} point;

/*! \struct shape
 *  \brief Representation of a shape
 *  \details A shape defined by multiple points.
 *  \ingroup Geometry
 */
typedef struct shape {

    unsigned short num_points;
    point** points;
} shape;

/*! \struct circle
 *  \brief Representation of a circle
 *  \details A circle defined by a center and a radius.
 *  \ingroup Geometry
 */
typedef struct circle {

    /* A circle consists of a center and a radius */
    point* center;
    ushort radius;
} circle;

/*! Destroys a point described by x and y coordinates.
 *  \param p Point to destroy
 *  \ingroup Geometry
 */
inline void shape_destroy_point(point* p) {

    free(p);
}

/*! Destroys a circle described by center and radius.
 *  \param cir Circle to destroy
 *  \ingroup Geometry
 */
inline void shape_destroy_circle(circle* cir) {

    free(cir);
}

/*! Return a point that describes the given arguments.
 *  \param x x coordinate
 *  \param y y coordinate
 *  \param shade shading of this point
 *  \returns point* The newly created point
 *  \ingroup Geometry
 */
point* shape_create_point(ushort, ushort, shade_t);

/*! Duplicate a point
 *  \param p Point to duplicate
 *  \returns point* Duplicated point
 *  \warning Don't forget to destroy each duplicated (created) point.
 *  \ingroup Geometry
 */
inline point* shape_duplicate_point(point* p) {

    return shape_create_point(p->x, p->y, p->shade);
}

/*! Return a pointer to a shape with numPoints points.
 *  \param  numpoints Number of vertices in this polygon
 *  \param points... Comma separated point-defined polygon
 *  \returns point* Pointer to the described polygon
 *  \ingroup Geometry
 *  \note va_args requires a preceeding argument (size va_args).
 */
shape* shape_create(ushort, ...);

/*! Duplicate a shape object.
 *  \param shape The shape to duplicate
 *  \returns shape A duplicate of the given shape
 *  \ingroup Geometry
 */
shape* shape_duplicate_shape(shape*);

/*! Destroy a shape object.
 *  \param sh Shape object to destroy
 *  \returns void
 *  \ingroup Geometry
 */
void   shape_destroy_shape(shape*);

/*! Return a pointer to a circle in memory described by center and
 *  radius.
 *  \param center Point describing the center of the circle
 *  \param radius Length of the radius in units
 *  \returns shape* to newly described circle
 *  \ingroup Geometry
 */
circle* shape_create_circle(ushort, point*);

/*! A handy function to create a triangle.
 *  \param p0 Vertex a
 *  \param p1 Vertex b
 *  \param p2 Vertex c
 *  \returns shape* to newly described triangle
 *  \ingroup Geometry
 */
shape*  shape_create_triangle(point*, point*, point*);

/*! A handy function to create a general quadrilateral.
 *  \param top_left_corner Coordinates of the top left corner of the quadrilateral
 *  \param width Width of the quadrilateral
 *  \param p2 height Height of the quadrilateral
 *  \returns shape* to newly described quadrilateral
 *  \ingroup Geometry
 */
shape*  shape_create_quad(point*, uchar, uchar);

#endif	/*  __SHAPE__ */
