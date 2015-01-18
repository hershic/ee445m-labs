/* graphics2d.c
 * Hershal Bhave and Eric Crosson
 * 2014-02-08
 * Graphics engine for interfacing with vendor's libs
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#ifdef __COMPILE_TO_MICRO__
#include "rit128x96x4.h"
#endif	
#include "graphics2d.h"

/*
   The One Buffer To Rule Them All (TM)

   This buffer is what we will use to write to the display. It is
   created here so that it can be easily passed around to those who
   need it and it doesn't need to keep being malloc'd all the time.
*/
unsigned char packedDeviceBuffer[RIT_FB_WIDTH * RIT_FB_HEIGHT] = {0};

/*
  Return fb in the format understood by RIT functions.
  Inputs:  fb     framebuffer to convert into a char*
  Outputs: fb converted into RITFormat
*/
unsigned char* G2ConvertFBToRITFormat(framebuffer fb) {

  /* i is x, j is y */
  unsigned char i, j;
  unsigned char a, b;

  for(j=0; j<OLED_HEIGHT; ++j) {
    for(i=0; i<OLED_WIDTH; i+=2) {
      a = fb[i][j];
      b = fb[i+1][j];

      packedDeviceBuffer[(i/2)+j*RIT_FB_WIDTH] = (a<<4) | (b&0xF);
    }
  }
  return packedDeviceBuffer;
}

/*
  Draw fb on the OLED.
  Inputs:  fb    framebuffer type (char**)
  Outputs: void
*/
void G2Draw(framebuffer fb) {
  G2ConvertFBToRITFormat(fb);
#ifndef __GNUC__
  RIT128x96x4ImageDraw(packedDeviceBuffer, 0, 0, OLED_WIDTH, OLED_HEIGHT);
#endif	
}
