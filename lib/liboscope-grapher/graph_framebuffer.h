#ifndef __GRAPH_FRAMEBUFFER__
#define __GRAPH_FRAMEBUFFER__

#include "graphlib.h"
#include "libstd/defines.h"
#include "framebuffer.h"

/* Graph functions */
void FBDrawGraph(framebuffer fb, graph* g, point* top_left_corner, pixel_t width, pixel_t height);
void FBEraseGraph(framebuffer fb, graph* g, point* top_left_corner, pixel_t width, pixel_t height);
void FBEraseGraphData(framebuffer fb, graph* g, point* top_left_corner, pixel_t width, pixel_t height);
void FBDrawFullscreenGraph(framebuffer fb, graph* g);
void FBEraseFullscreenGraph(framebuffer fb, graph* g);
void FBEraseFullscreenGraphData(framebuffer fb, graph* g);
void _FBDrawGraphBounds(framebuffer fb,
				graph* g,
				pixel_t x_range_padding,
				pixel_t y_range_padding,
				point* top_left_corner,
				pixel_t width,
				pixel_t height,
				shade_t shade);
void _FBDrawGraphData(framebuffer fb, graph* g, point* top_left_corner, shade_t shade);
void _FBDrawGraphTitle(framebuffer fb, graph* g);
void _FBDrawGraphAxesTitles(framebuffer fb, graph* g);
void _FBDrawGraphAxesScale(framebuffer fb, graph* g);

#endif	/* __GRAPH_FRAMEBUFFER__ */
