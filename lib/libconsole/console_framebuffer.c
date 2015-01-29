#include <stdio.h>
#include "libstd/defines.h"
#include "console_framebuffer.h"

void printFramebuffer(unsigned char* fb) {
  unsigned char i, j, a, b;

  for(j=0; j<OLED_HEIGHT; ++j) {
    for(i=0; i<OLED_WIDTH; i+=2) {
      a = (fb[(i/2)+j*RIT_FB_WIDTH] >> 4);
      b = (fb[(i/2)+j*RIT_FB_WIDTH] & 0x0F);

      switch(a/4) {
      case 0: a=SHADE_0; break;
      case 1: a=SHADE_1; break;
      case 2: a=SHADE_2; break;
      case 3: a=SHADE_3; break;
      }

      switch(b/4) {
      case 0: b=SHADE_0; break;
      case 1: b=SHADE_1; break;
      case 2: b=SHADE_2; break;
      case 3: b=SHADE_3; break;
      }

      printf("%c%c", a, b);
    }
    printf("\n");
  }
}
