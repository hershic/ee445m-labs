/* This file is not intended to be flashed on the board */

/*
 * Hershal Bhave and Eric Crosson
 * 2014-02-08
 * Function: display a frame buffer in console exactly as it would
 *           look on the OLED
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * *nix
 */

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include "shape.h"
#include "clock.h"
#include "framebuffer.h"
#include "graphics2d.h"
#include "timekit.h"
/* #include "../src/lmsmenu.h" */

int usleep(int usec);

void mockCheckout(void);
void demonstrateVoidPointerMenus(void);
void demonstrateClockMenu(void);
/* void demonstrateMenu(void); */
void demonstrateClock(void);
void demonstrateDrawString(void);
void demonstrateDrawShapes(void);

void printFramebuffer(unsigned char* fb);

void pseudointerrupt(clocktime* tm);

int main(void) {
    /* mockCheckout(); */
    /* demonstrateVoidPointerMenus(); */
    /* demonstrateClockMenu(); */
    /* demonstrateMenu(); */
    demonstrateClock();
    /* demonstrateDrawString(); */
    /* demonstrateDrawShapes(); */
}

void mockCheckout() {

}

/* void demonstrateVoidPointerMenus() { */
/*     framebuffer fb = FBInit(); */
/*  */
/*     clocktime* time = TKCreateTimeHandle(TK_PRINT_MODE_HOURS_MINUTES_SECONDS_MS); */
/*     mu_menu* main = MUCreateMenu("I'm a MU_MENU", 2); */
/*     mu_clock_menu* clock = */
/*       MUCreateClockSettingMenu("I'm a MU_CLOCK_MENU", 1, time); */
/*  */
/*     printf("Main is: %x\n", MUMenuDecoder(main)); */
/*     printf("Clock is: %x\n", MUMenuDecoder(clock)); */
/*  */
/*     MUCDestroyClockMenu(clock); */
/*     MUDestroyMenu(main); */
/*     TKDestroy(time); */
/*     FBDestroy(fb); */
/* } */

/* void demonstrateClockMenu() { */
/*     framebuffer fb = FBInit(); */
/*  */
/*     mu_menu* main = MUCreateMenu("Set:", 2); */
/*  */
/*     clocktime* time = TKCreateTimeHandle(TK_PRINT_MODE_HOURS_MINUTES_SECONDS_MS); */
/*     mu_clock_menu* clock = MUCreateClockSettingMenu("Set clock time", 1, time); */
/*     mu_clock_menu* alarm = MUCreateClockSettingMenu("Set alarm time", 1, time); */
/*  */
/*     MUSetOptionStrings(main, "clock time", "alarm time"); */
/*     MUSetClockMenuItems(main, clock, alarm); */
/*  */
/*     MUCPrevScreen(clock, main); */
/*     MUCPrevScreen(alarm, main); */
/*  */
/*     FBDrawMUMenu(fb, main); */
/*     printFramebuffer( G2ConvertFBToRITFormat(fb) ); */
/*     usleep(1000000); */
/*  */
/*     mu_clock_menu* next = MUCControllerHandleInputEvent(main, MU_IO_SELECT); */
/*     FBDestroy(fb);		/\* clear the framebuffer *\/ */
/*     fb = FBInit();		/\* reallocate *\/ */
/*     FBDrawMUClockMenu(fb, next); */
/*     printFramebuffer( G2ConvertFBToRITFormat(fb) ); */
/*     usleep(1000000); */
/*  */
/*     MUCHandleInputEvent(next, MU_IO_UP); /\* inc hours *\/ */
/*     MUCHandleInputEvent(next, MU_IO_UP); /\* inc hours *\/ */
/*     MUCHandleInputEvent(next, MU_IO_RIGHT); /\* sel min *\/ */
/*     MUCHandleInputEvent(next, MU_IO_DOWN);  /\* dec min *\/ */
/*     FBDestroy(fb);		/\* clear the framebuffer *\/ */
/*     fb = FBInit();		/\* reallocate *\/ */
/*     FBDrawMUClockMenu(fb, next); */
/*     printFramebuffer( G2ConvertFBToRITFormat(fb) ); */
/*     FBDestroy(fb);		/\* clear the framebuffer *\/ */
/*  */
/*     MUDestroyMenu(main); */
/*     MUCDestroyClockMenu(alarm); */
/*     MUCDestroyClockMenu(clock); */
/* } */

/* Note that the relationship between menua and clock_menu is
 * suboptimal at present because c doesn't have any sort of reasonable
 * inheritance. */

/* void demonstrateMenu() { */
/*     framebuffer fb = FBInit(); */
/*  */
/*     mu_menu* m = MUCreateMenu("Suicide booth", 2); */
/*     mu_menu* q = MUCreateMenu("You have selected quick and painless", 1); */
/*     mu_menu* p = MUCreateMenu("You have selected painful and horrible", 1); */
/*     mu_menu* d = MUCreateMenu("You are dead.", 1); */
/*  */
/*     MUSetOptionStrings(m, "Quick and painless", "Painful and horrible"); */
/*     MUSetMenuItems(m, q, p); */
/*  */
/*     MUSetMenuItems(p, "Return to start", m); */
/*     MUSetMenuItems(q, "Return to start", m); */
/*     MUSetMenuItems(d, "Return to start", m); */
/*  */
/*     MUPrevScreen(d, m); */
/*     MUPrevScreen(p, m); */
/*     MUPrevScreen(q, m); */
/*  */
/*     mu_menu* current = m; */
/*     mu_menu* next = (mu_menu*) 0x1; */
/*  */
/*     /\* Display main menu *\/ */
/*     FBDrawMUMenu(fb, current); */
/*     printFramebuffer( G2ConvertFBToRITFormat(fb) ); */
/*     printf("-----------\n"); */
/*     usleep(500000); */
/*  */
/*     /\* Select the first option and redisplay *\/ */
/*     next = MUHandleInputEvent(m, MU_IO_SELECT); */
/*     FBDrawMUMenu(fb, next); */
/*     printFramebuffer( G2ConvertFBToRITFormat(fb) ); */
/*     printf("-----------\n"); */
/*  */
/*     MUDestroyMenu(m); */
/*     MUDestroyMenu(p); */
/*     MUDestroyMenu(q); */
/*     MUDestroyMenu(d); */
/*     FBDestroy(fb); */
/* } */

void demonstrateClock(void) {


  point* pt_center = SHCreatePoint(OLED_WIDTH/2, OLED_HEIGHT/2, 15);
  point* pt_offset_ms = SHCreatePoint(OLED_WIDTH/2, 3*OLED_HEIGHT/4, 15);
  circle* circle_cir = SHCreateCircle(46, pt_center);
  circle* circle_ms = SHCreateCircle(12, pt_offset_ms);

  clocktime* tm = TKCreateTimeHandle(TK_PRINT_MODE_HOURS_MINUTES_SECONDS_MS);

  clockhand* ch_hour = CHCreateClockHand(pt_center, 25, CH_HOURS); // px
  clockhand* ch_min = CHCreateClockHand(pt_center, 32, CH_MINUTES); // px
  clockhand* ch_sec = CHCreateClockHand(pt_center, 36, CH_SECONDS); // px
  clockhand* ch_ms = CHCreateClockHand(pt_offset_ms, 10, CH_MS); // px

  ushort numhands = 4;
  clockhand** hands_arr = (clockhand**)malloc(numhands*sizeof(clockhand*));
  hands_arr[0] = ch_hour;
  hands_arr[1] = ch_min;
  hands_arr[2] = ch_sec;
  hands_arr[3] = ch_ms;
  clockface* cf = CFCreateClockFace(numhands, hands_arr, tm);

  shape** shapeptr;

  TKIncrementHours(tm, 10);
  TKIncrementMinutes(tm, 40);

  framebuffer fb = FBInit();

  /* Begin clock face layout */
  point* twelve_corner = SHCreatePoint(58, 4, FB_COLOR_MUCH);
  point* six_corner = SHCreatePoint(62, 86, FB_COLOR_MUCH);
  point* three_corner = SHCreatePoint(21, 45, FB_COLOR_MUCH);
  point* nine_corner = SHCreatePoint(103, 45, FB_COLOR_MUCH);
  FBDrawCircle(fb, circle_cir);
  FBDrawString(fb, twelve_corner, "12");
  FBDrawString(fb, six_corner, "6");
  FBDrawString(fb, three_corner, "3");
  FBDrawString(fb, nine_corner, "9");
  free(twelve_corner);
  free(six_corner);
  free(three_corner);
  free(nine_corner);
  /* End clock face layout */

  while(1) {
    /* printf("-----  %s  -----\n", TKToString(tm)); */
    pseudointerrupt(tm);
    CFUpdateTime(cf);
    shapeptr = CFToShapes(cf);
    FBDrawCircle(fb, circle_ms);
    FBDrawShapeArr(fb, numhands, shapeptr);
    printFramebuffer( G2ConvertFBToRITFormat(fb) );
    FBEraseShapeArr(fb, numhands, shapeptr);
    CFDestroyShapes(cf);
    usleep(500000); // .5 second
  }

  /* cleanup */
  /* CFDestroyClockFace(cf); */
  free(circle_cir);
  free(circle_ms);
  free(pt_offset_ms);
  free(pt_center);
  TKDestroy(tm);

  CHDestroyClockHand(ch_hour);
  CHDestroyClockHand(ch_min);
  CHDestroyClockHand(ch_sec);
  CHDestroyClockHand(ch_ms);
  CFDestroyClockFace(cf);
  free(hands_arr);

  /* free(fb); */
  FBDestroy(fb);
}

void pseudointerrupt(clocktime* tm) {
  TKIncrementMS(tm,100);
  /* TKIncrementHours(tm,1); */
}

void demonstrateDrawShapes(void) {
  point* p0 = SHCreatePoint(10, 10, 15);
  point* p1 = SHCreatePoint(10, 20, 15);
  point* p2 = SHCreatePoint(30, 30, 15);
  shape* tri = SHCreateTriangle(p0, p1, p2);

  point* p2_0 = SHCreatePoint(50, 50, 11);
  shape* square = SHCreateQuad(p2_0, 10, 10);

  point* round_center =SHCreatePoint(70, 35, 7);
  /* circle* circle = SHCreateCircle(15, round_center); */

  shape** shapeArr = (shape**)malloc(2*sizeof(shape*));
  shapeArr[0] = tri; shapeArr[1] = square;

  framebuffer fb = FBInit();

  FBDrawLine(fb, SHCreatePoint(0, 0, 15), SHCreatePoint(0, 10, 15), 15);

  /* FBDrawShape(fb, tri); */
  /* FBDrawShape(fb, square); */
  /* FBDrawMultipleShapes(fb, 2, square, tri); */
  FBDrawShapeArr(fb, 2, shapeArr);
  free(shapeArr);

  FBDrawEllipseFill(fb, round_center, 30, 5, round_center->shade);
  printFramebuffer( G2ConvertFBToRITFormat(fb) );
  FBDestroy(fb);
}

void demonstrateDrawString() {
    framebuffer fb = FBInit();
    point* pt = SHCreatePoint(20, 35, 15);

    FBDrawString(fb, pt, "A B C D E F G");
    printFramebuffer( G2ConvertFBToRITFormat(fb) );

    FBDestroy(fb);
    free(pt);
}

// Print framebuffer contents to console.
// Inputs:  framebuffer    framebuffer to display
// Outputs: void
/* TODO: Fix the bit-strapping!! */
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
