#ifndef __GRAPHLIB__
#define __GRAPHLIB__

#include "libstd/defines.h"

/*
  TODO: finish implementing this behavior

  As it stands, this is supposed to be the number of pixels to leave
  blank between new data and old. However, it is the number of steps to
  leave blank, which does not correlate to pixels at all. Do the math
  and implement the desired behavior.
 */
#define GL_BLANK_STEPS_PRECEEDING_STALE_DATA 2

/* A graph struct

   This graph manages nearly all aspects of graph-life internally.
   All it expects from a user is proper configuration (see
   GLCreateGraph) and for values to be pushed (see GLPushDataPoint)
   onto the graph. When there is no more room on the right to display
   information, the graph object will wrap the current index back to
   the left side of the graph and overwrite the oldest existing data.

   Limitations:
   - Y axis can currently only be left-justified
   - Window cannot be adjusted yet after creation
 */
typedef struct graph {
    /* Strings labeling important graph areas */
    char* x_title;
    char* y_title;
    char* title;

    /* Bounds of visible data -- this version of graphlib does not
     * support dynamic ranges */
    long x_min, x_max;
    long y_min, y_max;

    /* Honeypot -- all the gold (information) goes in here */
    long* data;

    /* Current position within- and size of- data */
    unsigned long x_index;
    unsigned long x_index_max;

    /* Below variables concern the state of fresh information in the
     * graph. Since all data points start at 0, which could be valid,
     * we need another way to differentiate valid data from invalid
     * data. */

    /* This signifies that the data has filled up once and x_index has
     * wrapped back to zero. Implications: only display all points
     * when this is true. */
    bool all_data_points_valid;

    /* This field indicates the most recent data point (index) in our
     * array of data. This is useful in conjunction with the above
     * member variable. If all_data_points_valid is not set, graph up
     * to most_recent_data_point. If all_data_points_valid is set,
     * then the data points have 'wrapped around' the screen much like
     * an old-school o-scope. In this case, draw up to
     * most_recent_data_point, draw a break for
     * GL_BLANK_STEPS_PRECEEDING_STALE_DATA steps, and then graph the
     * older data. */
    long most_recent_data_point;

    /* Below variables used for developer sanity during graphing
     * routines. These fields WILL change with every new graph
     * size. Don't use them, they are for internal use only. */
    pixel_t x_pixel_start, x_pixel_end;
    pixel_t y_pixel_start, y_pixel_end;
} graph;

/* The joys of programming for embedded systems. */
char* strdup(const char *str);

/*** Memory management of graph objects ***/
/* Method to create an instance of a graph. */
graph* GLCreateGraph(long x_min, long x_max, long y_min, long y_max, long x_steps);
/* Corresponding method to destroy a graph instance. */
void GLDestroyGraph(graph* graph);

/* Set the title of graph and return the head of its new char*. */
char* GLSetTitle(graph* graph, char* title);

/* Set the titles of the x and y axes. */
void GLLabelAxes(graph* graph, char* x_axis, char* y_axis);

/* Push a data point onto the end of the graph. The graph will
 * automatically wrap around when there is no more room in the
 * internal buffer. */
void GLPushDataPoint(graph* graph, long y_val);

/* Find a value that, when graphed, will not be visible on the graph
 * due to the current range of the graph's window. Note that resizing
 * the window may result in 'visible' OffScreenValues. */
long GLOffScreenValue(graph* graph);

#endif // __GRAPHLIB__
