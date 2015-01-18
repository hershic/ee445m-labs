#ifndef __CLOCK__
#define __CLOCK__

/* clock.h
 * Hershal Bhave and Eric Crosson
 * 2014-02-09
 * Clock Hands Library
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include "shape.h"
#include "timekit.h"

#define CH_HOURS	1
#define CH_MINUTES	2
#define CH_SECONDS	3
#define CH_MS		4

#define CH_MAX_HOURS	12
#define CH_MAX_MINUTES	60
#define CH_MAX_SECONDS	60
#define CH_MAX_MS	1000

#define CH_MAX_DEGREES  360
#define CH_ANGLE_OFFSET 90.0

#define PI_PI		3.142159

typedef struct clockhand {

  /* Client Properties: */
  /* Radius in units of pixels */
  ushort radius;
  /* The point is managed by client code */
  point* center;
  /* Hand type; use the #defines */
  uchar type;

  /* ClockFace-managed Properties */
  /* shapeBuffer is an internally-managed shape */
  shape* shapeBuffer;
  /* The origin of this angle is at 12-o-clock */
  ushort angle;
  point* pointOnCircumference;
} clockhand;


typedef struct clockface {

  /* The individual hands' client properties managed by the client
   * code, but the clockface manages the angle and shapeBuffer */
  clockhand** hands;
  ushort numHands;

  /* Time is also managed by the client code (i.e. the client will
   * call TKIncrement*), but the clock face will notice this when
   * CFUpdateTime is called and the associated hands' shapes will be
   * updated to reflect the time given by the timehandle pointer */
  clocktime* timehandle;

  /* shapeBuffer is an internally-managed array of clock hand
   * shapes */
  shape** shapeBuffer;
} clockface;


clockhand* CHCreateClockHand(point* center, ushort radius, uchar type);
void CHCalculateAngle(clockhand* ch, clocktime* tm);
shape* CHToShape(clockhand* ch);
void CHDestroyClockHand(clockhand* ch);
void CHDestroyClockHandShape(clockhand* ch);

clockface* CFCreateClockFace(ushort numHands, clockhand** chs, clocktime* th);
void CFDestroyClockFace(clockface* cf);
void CFUpdateTime(clockface* cf);
shape** CFToShapes(clockface* cf);
void CFDestroyShapes(clockface* cf);

#endif	/* __CLOCK__ */
