#ifdef __COMPILE_TO_MICRO__
#include "rit128x96x4.h"
#endif
#include "graphics2d.h"

#define NDEBUG

#ifndef NDEBUG
#include <stdio.h>
#endif

/*!
 * \var g_PackedDeviceBuffer
 *
 * \brief The One Buffer To Rule Them All (TM)
 * \details This buffer is what we will use to write to the
 * display. It is created here so that it can be easily passed around
 * to those who need it and it doesn't need to keep being malloc'd all
 * the time.
*/
unsigned char g_PackedDeviceBuffer[RIT_FB_WIDTH * RIT_FB_HEIGHT] = {0};

unsigned char* G2ConvertFBToRITFormat(framebuffer fb) {

  /* i is x, j is y */
  unsigned char i, j;
  unsigned char a, b;

#ifndef NDEBUG
  printf("%s Unpacking framebuffer...", __FUNCTION__);
#endif

  for(j=0; j<OLED_HEIGHT; ++j) {
    for(i=0; i<OLED_WIDTH; i+=2) {
      a = fb[i][j];
      b = fb[i+1][j];

      g_PackedDeviceBuffer[(i/2)+j*RIT_FB_WIDTH] = (a<<4) | (b&0xF);
    }
  }

#ifndef NDEBUG
  printf("%s Done unpacking framebuffer...", __FUNCTION__);
#endif

  return g_PackedDeviceBuffer;
}

void G2Draw(framebuffer fb) {

  G2ConvertFBToRITFormat(fb);
#ifndef __GNUC__
  RIT128x96x4ImageDraw(g_PackedDeviceBuffer, 0, 0, OLED_WIDTH, OLED_HEIGHT);
#endif
}
