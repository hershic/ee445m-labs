#include "g2d_defines.h"
#include "graphlib.h"
#include "framebuffer.h"
#include "graph_framebuffer.h"
#include "graphics2d.h"
#include "console_framebuffer.h"

#include <stdio.h>
#include <time.h>
#include <math.h>

void usleep(long);

int main() {
    int i;
    int delta = 0;
    uchar y_range_max = 100;
    framebuffer fb = FBInit();
    graph* g = GLCreateGraph(0, 10, 0, y_range_max, 10);
    GLSetTitle(g, "Graph test");
    GLLabelAxes(g, "X Axis", "Y Allies");

    /* GLPushDataPoint(g, 0); */
    /* GLPushDataPoint(g, 2); */
    /* GLPushDataPoint(g, 3); */
    /* GLPushDataPoint(g, 4); */
    /* GLPushDataPoint(g, 20); */
    /* GLPushDataPoint(g, 25); */
    /* GLPushDataPoint(g, 28); */
    /* GLPushDataPoint(g, 30); */
    /* GLPushDataPoint(g, 40); */
    /* GLPushDataPoint(g, 50); */
    /* GLPushDataPoint(g, 60); */
    /* GLPushDataPoint(g, 70); */
    /* GLPushDataPoint(g, 50); */
    /* GLPushDataPoint(g, 60); */
    /* GLPushDataPoint(g, 70); */
    /* GLPushDataPoint(g, 50); */
    /* GLPushDataPoint(g, 60); */
    /* GLPushDataPoint(g, 70); */
    /*  */
    /* FBDrawFullscreenGraph(fb, g); */
    /* printFramebuffer( G2ConvertFBToRITFormat(fb) ); */
    while(1) {

      FBEraseFullscreenGraph(fb, g);
      GLPushDataPoint(g, y_range_max/2*sin(i++) + y_range_max/2);
      FBDrawFullscreenGraph(fb, g);
      printFramebuffer( G2ConvertFBToRITFormat(fb) );
      usleep(250000); // .5 second
    }

}
