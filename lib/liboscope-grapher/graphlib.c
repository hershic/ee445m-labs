#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphlib.h"

/* Features to add if we ever get around to it

   - [ ] draw graph title
   - [ ] draw axis titles
   - [ ] draw x = 0 when visible
 */

char* strdup(const char *str) {
  if (str != null) {
    char *copy = malloc(strlen(str) + 1);
    if (copy != null)
      return strcpy(copy, str);
  }
  return null;
}

/* Method to create an instance of a graph. */
graph* GLCreateGraph(long x_min, long x_max, long y_min, long y_max, long x_steps) {
    graph* g = (graph*) calloc(1, sizeof(graph));
    g->x_min = x_min;
    g->x_max = x_max;
    g->y_min = y_min;
    g->y_max = y_max;

    g->data = (long*) calloc(x_steps, sizeof(long));
    g->x_index = 0;
    g->x_index_max = x_steps;

    g->all_data_points_valid = false;

    return (graph*) g;
}

/* Set the title of graph and return the head of its new char*. */
char* GLSetTitle(graph* g, char* title) {
    g->title = strdup(title);
    return g->title;
}

/* Set the titles of the x and y axes. */
void GLLabelAxes(graph* g, char* x_axis, char* y_axis) {
    g->x_title = strdup(x_axis);
    g->y_title = strdup(y_axis);
}

/* Push a data point onto the end of the graph. The graph will
 * automatically wrap around when there is no more room in the
 * internal buffer. */
void GLPushDataPoint(graph* g, long y_val) {
    /* Procedure: push the y data point at the current x_position and
     * then increment the current x_position. */
    g->data[g->x_index] = y_val;
    g->most_recent_data_point = g->x_index++;

    if (g->x_index >= g->x_index_max) {
	g->x_index = 0;
	g->all_data_points_valid = true; // one-way toggle to true
    }
}

/* Find a value that, when graphed, will not be visible on the graph
 * due to the current range of the graph's window. Note that resizing
 * the window may result in 'visible' OffScreenValues. */
long GLOffScreenValue(graph* g) {
    long off_screen_value = g->y_max + 1;
    if (off_screen_value < g->y_max) {
	off_screen_value = g->y_min - 1;
    } if (off_screen_value > g->y_min) {
	off_screen_value = LONG_MIN;
    }
    return off_screen_value;
}

/* Method to destroy a graph instance. Corresponds to
 * GLCreateGraph() */
void GLDestroyGraph(graph* g) {
    free(g->data);
    free(g->title);
    free(g->x_title);
    free(g->y_title);
    free(g);
}
