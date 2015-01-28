/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SHAPE__
#define __SHAPE__

/*!
 *  \brief     Two dimensional shape library.
 *  \details   Graphics framework for describing 2D systems.
 *  \author    Hershal Bhave
 *  \author    Eric Crosson
 *  \version   0.1
 *  \date      2015
 *  \pre       None
 *  \bug       No scoreboard to detect UART collisions yet
 *  \warning   Destroy whatever you create.
 *  \note      This library is intended for use with the LM3S1968.
 *  \copyright GNU Public License.
 */

#include <stdlib.h>
#include "g2d_defines.h"

/*! \struct point
 *  \brief Representation of an ordered pair with a shade
 *  \details An ordered pair to a Cartesian coordinate system.
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
 */
typedef struct shape {

    unsigned short num_points;
    point** points;
} shape;

/*! \struct circle
 *  \brief Representation of a circle
 *  \details A circle defined by a center and a radius.
 */
typedef struct circle {

    /* A circle consists of a center and a radius */
    point* center;
    ushort radius;
} circle;

/* TODO: either document or convert these to inline functions */
/* Macro to create a duplicate of a point object. Don't forget to
 * destroy the duplicate. */
inline point* shape_duplicate_point(p) {

    return shape_create_point(p->x, p->y, p->shade);
}

/*! Destroys a point described by x and y coordinates.
 * \param p Point to destroy
 */
inline void shape_destroy_point(point* p) {

    free(p);
}

/*! Destroys a circle described by center and radius.
 *  \param cir Circle to destroy
 */
inline void shape_destroy_circle(circle* cir) {

    free(cir);
}

/*! Return a point that describes the given arguments.
 *  \param x x coordinate
 *  \param y y coordinate
 *  \param shade shading of this point
 *  Output: A newly created point
 */
point* shape_create_point(ushort, ushort, shade_t);

/*! Return a pointer to a shape with numPoints points.
 *  \param  numpoints Number of vertices in this polygon
 *  \param points... Comma separated point-defined polygon
 *  \returns point* Pointer to the described polygon
 *  Note: va_args requires a preceeding argument.
 */
shape* shape_create(ushort numPoints, ...);

/*! Duplicate a shape object.
 *  \param shape The shape to duplicate
 *  \returns shape A duplicate of the given shape
 */
shape* shape_duplicate_shape(shape* s);

/*! Destroy a shape object.
 *  \param sh Shape object to destroy
 *  \returns void
 */
void   shape_destroy_shape(shape* sh);

/*! Return a pointer to a circle in memory described by center and
 *  radius.
 *  \param center Point describing the center of the circle
 *  \param radius Length of the radius in units
 *  \returns shape* to newly described circle
 */
circle* shape_create_circle(ushort radius, point* center);

/*! A handy function to create a triangle.
 *  \param p0 Vertex a
 *  \param p1 Vertex b
 *  \param p2 Vertex c
 *  \returns shape* to newly described triangle
 */
shape*  shape_create_triangle(point* p0, point* p1, point* p2);

/*! A handy function to create a general quadrilateral.
 *  \param top_left_corner Coordinates of the top left corner of the quadrilateral
 *  \param width Width of the quadrilateral
 *  \param p2 height Height of the quadrilateral
 *  \returns shape* to newly described quadrilateral
 */
shape*  shape_create_quad(point* top_left_corner, uchar width, uchar height);

#endif	/*  __SHAPE__ */
