/* framebuffer.c
 * Hershal Bhave and Eric Crosson
 * 2014-02-08, extracted on 2015-01-18
 * Function: represent a framebuffer of an oscope in memory.
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include "graph_framebuffer.h"
#include "shape.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

/* Below are the functions that handle graphing graph (graphlib.h) objects */

/*
  A wrapper for FBDrawGraph where the user does not have to fill in
  implicit information about the graph size.
  Inputs:  fb   framebuffer to use as canvas
  g    graph to draw on fb
  Outputs: void
*/
void FBDrawFullscreenGraph(framebuffer fb, graph* g) {
  point* origin = shape_create_point((pixel_t) 0, (pixel_t) 0, (shade_t) 15);
  FBDrawGraph(fb, g, origin, (pixel_t) OLED_WIDTH, (pixel_t) OLED_HEIGHT);
  SHDestroyPoint(origin);
}

/*
  A wrapper for FBEraseGraph where the user does not have to fill in
  implicit information about the graph size.
  Inputs:  fb   framebuffer to use as canvas
  g    graph to draw on fb
  Outputs: void
*/
void FBEraseFullscreenGraph(framebuffer fb, graph* g) {
  point* origin = shape_create_point((x_pixel_t) 0, (y_pixel_t) 0, (shade_t) FB_COLOR_ERASE);
  FBEraseGraph(fb, g, origin, (x_pixel_t) OLED_WIDTH, (y_pixel_t) OLED_HEIGHT);
  SHDestroyPoint(origin);
}

/*
  A wrapper for FBEraseGraphData where the user does not have to fill
  in implicit information about the graph size.
  Inputs:  fb   framebuffer to use as canvas
  g    graph to draw on fb
  Outputs: void
*/
void FBEraseFullscreenGraphData(framebuffer fb, graph* g) {
  point* origin = shape_create_point((x_pixel_t) 0, (y_pixel_t) 0, (shade_t) FB_COLOR_ERASE);
  FBEraseGraphData(fb, g, origin, (x_pixel_t) OLED_WIDTH, (y_pixel_t) OLED_HEIGHT);
  SHDestroyPoint(origin);
}

/*
  This method handles the drawing of each part of a graph object. The
  task of drawing each part has been delegated to a subroutine for
  ease of debugging and testing. Before calling each subroutine, this
  method sets the internal framebuffer variables inside of the
  specified graph object.
  Inputs:  fb                framebuffer to use as canvas
  g                 graph to draw on fb
  top_left_corner   top left corner of graph
  width             width of graph
  height            height of graph
  Outputs: void

  ** NOTICE **
  This is where the rubber meets the road.
*/
void FBDrawGraph(framebuffer fb, graph* g, point* top_left_corner,
                 pixel_t width, pixel_t height) {
  /* Determine how many pixels to reserve for the scale numbers next
   * to the axes */
  pixel_t x_range_padding = max_pixel_width_of_long(g->y_min, g->y_max);
  pixel_t y_range_padding = max_pixel_height_of_long(g->x_min, g->x_max);

  /* {x,y}_pixel_{start,end} information in /resources/2014-04-07 10.07.01.jpg */
  g->x_pixel_start = top_left_corner->x + x_range_padding;
  g->x_pixel_end   = top_left_corner->x + width;
  g->y_pixel_start = top_left_corner->y;
  g->y_pixel_end   = top_left_corner->y + height - 2 - y_range_padding;

  _FBDrawGraphBounds(fb, g, x_range_padding, y_range_padding, top_left_corner, width, height, top_left_corner->shade);
  _FBDrawGraphData(fb, g, top_left_corner, top_left_corner->shade);
  _FBDrawGraphTitle(fb, g);
  _FBDrawGraphAxesTitles(fb, g);
  _FBDrawGraphAxesScale(fb, g);
}

/*
  Using fb as a canvas, erase g which starts at top_left_corner, being
  of dimensions width and height.
  Inputs:  fb                framebuffer to use as canvas
  g                 graph to draw on fb
  top_left_corner   top left corner of graph
  width             width of graph
  height            height of graph
  Outputs: void
*/
void FBEraseGraph(framebuffer fb, graph* g, point* top_left_corner,
                  pixel_t width, pixel_t height) {
  /* Determine how many pixels to reserve for the scale numbers next
   * to the axes */
  pixel_t x_range_padding = max_pixel_width_of_long(g->y_min, g->y_max);
  pixel_t y_range_padding = max_pixel_height_of_long(g->x_min, g->x_max);

  /* {x,y}_pixel_{start,end} information in /resources/2014-04-07 10.07.01.jpg */
  g->x_pixel_start = top_left_corner->x + x_range_padding;
  g->x_pixel_end   = top_left_corner->x + width;
  g->y_pixel_start = top_left_corner->y;
  g->y_pixel_end   = top_left_corner->y + height - 2 - y_range_padding;

  _FBDrawGraphBounds(fb, g, x_range_padding, y_range_padding, top_left_corner, width, height, FB_SHADE_ERASE);
  _FBDrawGraphData(fb, g, top_left_corner, FB_SHADE_ERASE);
  _FBDrawGraphTitle(fb, g);
  _FBDrawGraphAxesTitles(fb, g);
  _FBDrawGraphAxesScale(fb, g);
}

/*
  Using fb as a canvas, erase data points from g which starts at
  top_left_corner, being of dimensions width and height.
  Inputs:  fb                framebuffer to use as canvas
  g                 graph to draw on fb
  top_left_corner   top left corner of graph
  width             width of graph
  height            height of graph
  Outputs: void
*/
void FBEraseGraphData(framebuffer fb, graph* g, point* top_left_corner,
                      pixel_t width, pixel_t height) {
  /* Determine how many pixels to reserve for the scale numbers next
   * to the axes */
  pixel_t x_range_padding = max_pixel_width_of_long(g->y_min, g->y_max);
  pixel_t y_range_padding = max_pixel_height_of_long(g->x_min, g->x_max);

  /* {x,y}_pixel_{start,end} information in /resources/2014-04-07 10.07.01.jpg */
  g->x_pixel_start = top_left_corner->x + x_range_padding;
  g->x_pixel_end   = top_left_corner->x + width;
  g->y_pixel_start = top_left_corner->y;
  g->y_pixel_end   = top_left_corner->y + height - 2 - y_range_padding;

  _FBDrawGraphData(fb, g, top_left_corner, FB_SHADE_ERASE);
}

/*
  Using fb as a canvas, draw g which starts at top_left_corner, being
  of dimensions width and height. Use shade to draw the bounds.
  Inputs:  fb                framebuffer to use as canvas
  g                 graph to draw on fb
  top_left_corner   top left corner of graph
  width             width of graph
  height            height of graph
  Outputs: void
*/
private void _FBDrawGraphBounds(framebuffer fb,
                                graph* g,
                                pixel_t x_range_padding,
                                pixel_t y_range_padding,
                                point* top_left_corner,
                                pixel_t width,
                                pixel_t height,
                                shade_t shade) {
  /* Draw X axis boundaries (aka vertical lines) */
  FBDrawAnonLine(fb,
                 shape_create_point(top_left_corner->x + x_range_padding,
                               top_left_corner->y,
                               shade),
                 shape_create_point(top_left_corner->x + x_range_padding,
                               top_left_corner->y + (height - y_range_padding),
                               shade),
                 shade);
  /* Draw Y axis boundaries (aka horizontal lines) */
  FBDrawAnonLine(fb,
                 shape_create_point(top_left_corner->x + x_range_padding,
                               top_left_corner->y + height - y_range_padding,
                               shade),
                 shape_create_point(top_left_corner->x + x_range_padding + width,
                               top_left_corner->y + height - y_range_padding,
                               shade),
                 shade);
}

/*
  Using fb as a canvas, draw data points of g which starts at
  top_left_corner, being of dimensions width and height. Use shade to
  draw the bounds.
  Inputs:  fb                framebuffer to use as canvas
  g                 graph to draw on fb
  top_left_corner   top left corner of graph
  width             width of graph
  height            height of graph
  Outputs: void
*/
private void _FBDrawGraphData(framebuffer fb, graph* g, point* top_left_corner, shade_t shade) {

  long idx = 0;
  /* Temporary variables reused in printfs */
  pixel_t x1, x2, y1, y2;
  /* Set this to true from lldb/gdb to see useful debugging information */
  bool debug = true;

  for(idx=0;
      (((g->all_data_points_valid == true) && (idx < g->x_index_max-1))) ||
        (g->all_data_points_valid == false && (idx < (g->most_recent_data_point - 1)));
      ++idx) {

    fprintf(stderr, "Printing index %ld\n", idx);

    if ((idx <= g->most_recent_data_point) &&
        (idx > g->most_recent_data_point + GL_BLANK_STEPS_PRECEEDING_STALE_DATA)) {
      /* Draw nothing */
      when (debug) {fprintf(stderr, "Not drawing this cycle [idx:%ld]\n", idx);}
    } else {

      x1 = g->x_pixel_start + ((idx)*(g->x_pixel_end-g->x_pixel_start)/(g->x_max-g->x_min));
      y1 = g->y_pixel_end   - ((g->data[idx]+g->y_min)*(g->y_pixel_end-g->y_pixel_start)/(g->y_max-g->y_min));
      x2 = g->x_pixel_start + ((idx+1)*(g->x_pixel_end-g->x_pixel_start)/(g->x_max-g->x_min));
      y2 = g->y_pixel_end   - ((g->data[idx+1]+g->y_min)*(g->y_pixel_end-g->y_pixel_start)/(g->y_max-g->y_min));

      FBDrawAnonLine(fb,
                     shape_create_point(x1, y1, shade),
                     shape_create_point(x2, y2, shade),
                     shade);

      when (debug) {fprintf(stderr, "Line from:\t(%u,\t%u) to (%u,\t%u)\n",x1,y1,x2,y2);}
    }
  }
}

/* TODO: document this function */
private void _FBDrawGraphTitle(framebuffer fb, graph* g) {

}

/* TODO: document this function */
private void _FBDrawGraphAxesTitles(framebuffer fb, graph* g) {

}

/* TODO: document this function */
private void _FBDrawGraphAxesScale(framebuffer fb, graph* g) {

}

/*
  Return the greater of two unsigned chars.
  Inputs:  one                uchar a to compare
  two                uchar b to compare
  Outputs: unsigned char      the max of one and two
*/
unsigned char max_uc(unsigned char one, unsigned char two) {
  if (one > two) {return one;} else return two;
}

/*
  Return the height of a string in pixels.
  Inputs:  str       string to calculate the height of
  Outputs: pixel_t   pixel height of str
*/
pixel_t _FBPixelHeightOfString(char* str) {
  /* TODO: consider line wrapping */
  return VALVANO_FONT_LINE_SPACING + VALVANO_FONT_HEIGHT;
}

/*
  Return the width of a string in pixels.
  Inputs:  str       string to calculate the width of
  Outputs: pixel_t   pixel width of str
*/
private pixel_t _FBPixelWidthOfString(char* str) {
  return VALVANO_FONT_KERNING + ((VALVANO_FONT_KERNING + VALVANO_FONT_WIDTH) * strlen(str));
}

/*
  Return the width of a long in pixels.
  Inputs:  l         long to calculate the width of
  Outputs: pixel_t   pixel width of l
*/
private pixel_t _FBPixelWidthOfLong(long l) {
  // 2 pixels because we want one to pad the front and one on the back
  return VALVANO_FONT_KERNING + (log(l) * VALVANO_FONT_WIDTH);
}

/*
  Return the height of a long in pixels.
  Inputs:  l         l to calculate the height of
  Outputs: pixel_t   pixel height of l
*/
private pixel_t _FBPixelHeightOfLong(long l) {
  // 2 pixels because we want one to pad the top and one on the bottom
  return VALVANO_FONT_LINE_SPACING + VALVANO_FONT_HEIGHT;
}
