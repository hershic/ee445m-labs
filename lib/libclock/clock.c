/* clock.c
 * Hershal Bhave and Eric Crosson
 * 2014-02-09
 * Implementation for the Clock Hands Library
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "clock.h"

shape* CHToShape(clockhand* ch) {
  float x, y;

  x = cosf((ch->angle - CH_ANGLE_OFFSET) * PI_PI / 180) * ch->radius;
  y = sinf((ch->angle - CH_ANGLE_OFFSET) * PI_PI / 180) * ch->radius;

  /* avoid underflow (subtracting too much from an unsigned number) */
  if(-x > ch->center->x) {x = -ch->center->x;}
  if(-y > ch->center->y) {y = -ch->center->y;}

  ch->pointOnCircumference->x = ch->center->x+x;
  ch->pointOnCircumference->y = ch->center->y+y;

  ch->shapeBuffer = shape_create(2, ch->center, ch->pointOnCircumference);
  return ch->shapeBuffer;
}

clockhand* CHCreateClockHand(point* center, ushort radius, uchar type) {
  clockhand* ch = (clockhand*) calloc(1, sizeof(clockhand));

  ch->radius = radius;
  ch->center = shape_duplicate_point(center);
  ch->type = type;

  ch->angle = 0;

  ch->pointOnCircumference = shape_duplicate_point(center);

  return ch;
}

void CHCalculateAngle(clockhand* ch, clocktime* tm) {
  ushort tmpHours;

  switch(ch->type) {
  case CH_HOURS:
    tmpHours = tm->hours;
    while(tmpHours > 12) {
      tmpHours -= 12;
    }
    ch->angle = (short)(30*tmpHours + (tm->minutes)/2.0);
    break;
  case CH_MINUTES:
    ch->angle = (short)(6.0*tm->minutes + tm->seconds/10.0);
    break;
  case CH_SECONDS:
    ch->angle = 6*tm->seconds + 3.0/500*tm->ms;
    break;
  case CH_MS:
    ch->angle = (short)((9/25.0)*(tm->ms));
    break;
  }

  /* printf("--- ANGLE FOR %i: %i --- %i\n", ch->type, ch->angle, CH_SECONDS); */
}

void CHDestroyClockHand(clockhand* ch) {

  /* SHDestroyShape(ch->shapeBuffer); */
  /* free(ch->shapeBuffer); */
  free(ch->pointOnCircumference);
  free(ch->center);
  free(ch);
}

void CHDestroyClockHandShape(clockhand* ch) {

  /* point* p; */
  /* p = SHCreatePoint(p->x, p->y, p->shade); */
  /* SHDestroyShape(ch->shapeBuffer); */
  /* ch->center = p; */

  free(ch->shapeBuffer->points);
  free(ch->shapeBuffer);
}

clockface* CFCreateClockFace(ushort numHands, clockhand** chs, clocktime* th) {

  ushort i;
  clockface* cf = (clockface*) calloc(1, sizeof(clockface));

  /* Moved to CFToShapes */
  /* cf->shapeBuffer = (shape**)malloc(numHands * sizeof(shape*)); */
  cf->numHands = numHands;

  /* Shallow copy of clockhands */
  cf->hands = (clockhand**) calloc(numHands, sizeof(clockhand*));
  for(i=0; i < numHands; ++i) {
      cf->hands[i] = chs[i];
  }

  /* Don't deep copy timehandle, we need it for the interrupt */
  cf->timehandle = th;

  return cf;
}

void CFDestroyClockFace(clockface* cf) {
  free(cf->hands);
  free(cf);
}

clockhand* CHDuplicateClockHand(clockhand* ch) {
    clockhand* dup = (clockhand*) calloc(1, sizeof(clockhand));

    dup->radius = ch->radius;
    dup->center = shape_duplicate_point(ch->center);
    dup->type = ch->type;
    dup->shapeBuffer = shape_duplicate_shape(ch->shapeBuffer);
    dup->pointOnCircumference = shape_duplicate_point(ch->pointOnCircumference);
    return dup;
}

void CFUpdateTime(clockface* cf) {
  ushort i;
  for(i=0; i<cf->numHands; ++i) {
    CHCalculateAngle(cf->hands[i], cf->timehandle);
  }
}

shape** CFToShapes(clockface* cf) {
  ushort i;

  cf->shapeBuffer = (shape**)malloc(cf->numHands * sizeof(shape*));

  for(i=0; i<cf->numHands; ++i) {
    cf->shapeBuffer[i] = CHToShape(cf->hands[i]);
  }
  return cf->shapeBuffer;
}

void CFDestroyShapes(clockface* cf) {
  ushort i;
  for(i=0; i<cf->numHands; ++i) {
    /* free(cf->shapeBuffer[i]); */
    CHDestroyClockHandShape(cf->hands[i]);
  }
  free(cf->shapeBuffer);
}
