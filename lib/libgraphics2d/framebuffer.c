/* -*- mode: c; c-basic-offset: 4; -*- */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "string.h"
#include "g2d_defines.h"
#include "framebuffer.h"

#define NDEBUG

framebuffer fb_init() {

    /* OPTIONAL OPTIMIZE: bit-strap this struct to reduce memory consumption */
    unsigned char i;
    unsigned char** fb = (unsigned char**) calloc(OLED_WIDTH, sizeof(unsigned char*));
    for(i = 0; i < OLED_WIDTH; ++i) {
	fb[i] = (unsigned char*) calloc(OLED_HEIGHT, sizeof(unsigned char));
    }
    return (framebuffer) fb;
}

void fb_destroy(framebuffer fb) {

    unsigned char i;
    for(i = 0; i < OLED_WIDTH; ++i) {
	free(fb[i]);
    }
    free(fb);
}

void fb_draw_shape(framebuffer fb, shape* sh) {

    if (sh->points[0] != null) {
	_fb_draw_shape(fb, sh, sh->points[0]->shade);
    }
}

private
void _fb_draw_shape(framebuffer fb, shape* sh, shade_t shade) {

    unsigned short p;
    for(p=0; p < sh->num_points; ++p) {
	/* TODO: determine if the below line is necessary */
	/* fb_set_pixel(fb, sh->points[p]->x, sh->points[p]->y, shade); */
	if (p > 0) {
	    fb_draw_line_gradient(fb, sh->points[p-1], sh->points[p], shade);
	}
    }

    /* Enclose the figure */
    if(p>1) {
	fb_draw_line_gradient(fb, sh->points[0], sh->points[p-1], shade);
    }
}

void fb_draw_multiple_shapes(framebuffer fb, ushort numShapes, ...) {

    unsigned char i;
    va_list args;

    va_start(args, numShapes);
    for(i=0; i < numShapes; ++i) {
	fb_draw_shape(fb, va_arg(args, shape*));
    }
    va_end(args);                 /* clean up the list */
}

void fb_draw_shape_arr(framebuffer fb, ushort numShapes, shape** shape_arr) {

    unsigned char i;
    for(i=0; i < numShapes; ++i) {
	fb_draw_shape(fb, shape_arr[i]);
    }
}

void fb_erase_shape_arr(framebuffer fb, ushort numShapes, shape** shape_arr) {

    unsigned char i;
    for(i=0; i < numShapes; ++i) {
	fb_erase_shape(fb, shape_arr[i]);
    }
}

void fb_set_pixel(framebuffer fb, uchar x, uchar y, shade_t shade) {

    if (x < FB_WIDTH && y < FB_HEIGHT) {
	fb[x][y] = shade;
    } else {
#ifndef NDEBUG
	printf("%sPixel off screen: ", __FUNCTION__);
	fb_console_println_coordinate(x, y, shade);
#endif
    }
}

void fb_erase_char(framebuffer fb, point* top_left_corner, char c) {

    point* pen = shape_duplicate_point(top_left_corner);
    pen->shade = FB_COLOR_ERASE;
    _fb_draw_char(fb, pen, c);
    SHDestroyPoint(pen);
}

private
void _fb_draw_line(framebuffer fb, point* point1, point* point2, shade_t shade) {

    point* a;
    point* b;
    int dx, dy, sx, sy, e2, err;

    a = shape_duplicate_point(point1);
    b = shape_duplicate_point(point2);

    dx = abs(b->x-a->x); sx = (a->x < b->x) ? 1 : -1;
    dy = abs(b->y-a->y); sy = (a->y < b->y) ? 1 : -1;
    err = (dx>dy ? dx : -dy)/2;
    for(;;) {
	fb_set_pixel(fb, a->x, a->y, shade);
	if ((a->x == b->x) && (a->y == b->y)) {break;}
	e2 = err;
	if (e2 >-dx) {err -= dy; a->x += sx;}
	if (e2 < dy) {err += dx; a->y += sy;}
    }
    SHDestroyPoint(a);
    SHDestroyPoint(b);
}

private
void _fb_plot_four_ellipse_points(framebuffer fb, point* center, ushort x, ushort y) {

    fb_set_pixel(fb, center->x + x, center->y + y, center->shade);
    fb_set_pixel(fb, center->x - x, center->y + y, center->shade);
    fb_set_pixel(fb, center->x - x, center->y - y, center->shade);
    fb_set_pixel(fb, center->x + x, center->y - y, center->shade);
}

private
void _fb_fill_four_ellipse_points(framebuffer fb, point* center, ushort x, ushort y) {

    int i;
    for(i = -x; i <= x; ++i) {
	fb_set_pixel(fb, center->x + i, center->y + y, center->shade);
	fb_set_pixel(fb, center->x + i, center->y - y, center->shade);
    }
}

void fb_draw_ellipse(framebuffer fb,
		     point*      center,
		     ushort      x_radius,
		     ushort      y_radius,
		     shade_t     shade) {

    ushort x, y;
    int xx, yy, xx2, yy2, dx, dy, stop_x, stop_y, error;

    x = x_radius;
    y = 0;

    xx = x_radius * x_radius; // width^2
    yy = y_radius * y_radius; // height^2

    xx2 = xx*2;
    yy2 = yy*2;

    dx = yy * (1 - 2*x_radius);
    dy = xx;

    stop_x = yy*2*x_radius;
    stop_y = 0;

    error = 0;

    while (stop_x > stop_y) {
	_fb_plot_four_ellipse_points(fb, center, x, y);
	++y;
	stop_y += xx2;
	error += dy;
	dy += xx2;
	if (2*error + dx > 0) {
	    --x;
	    stop_x -= yy2;
	    error += dx;
	    dx += yy2;
	}
    }

    /* First set of points is done, now plot the second set */
    x = 0;
    y = y_radius;
    dx = yy;
    dy = xx * (1 - 2*y_radius);
    error = 0;
    stop_x = 0;
    stop_y = xx2*y_radius;

    while (stop_x <= stop_y) {
	_fb_plot_four_ellipse_points(fb, center, x, y);
	x++;
	stop_x += yy2;
	error += dx;
	dx += yy2;
	if (2*error + dy > 0) {
	    --y;
	    stop_y -= xx2;
	    error += dy;
	    dy += xx2;
	}
    }
}

void fb_draw_ellipse_fill(framebuffer fb,
			  point*      center,
			  ushort      x_radius,
			  ushort      y_radius,
			  shade_t     shade) {

    ushort x, y;
    int xx, yy, xx2, yy2, dx, dy, last_y_filled, stop_x, stop_y, error, i;
    xx = x_radius * x_radius; // width^2
    yy = y_radius * y_radius; // height^2

    xx2 = xx*2;
    yy2 = yy*2;

    error = 0;

    // fill the horizontal diameter
    for (i = -x_radius; i <= x_radius; ++i) {
	fb_set_pixel(fb, center->x + i, center->y, center->shade);
    }

    /* First set of points is done, now plot the second set */
    x = 0;
    y = y_radius;
    dx = yy;
    dy = xx * (1 - 2*y_radius);
    stop_x = 0;
    stop_y = xx2*y_radius;

    // OPTIONAL OPTIMIZE: this loop
    last_y_filled = y;
    while (stop_x <= stop_y) {
	if (y != last_y_filled) {
	    _fb_fill_four_ellipse_points(fb, center, x, y);
	    last_y_filled = y;
	}
	x++;
	stop_x += yy2;
	error += dx;
	dx += yy2;
	if (2*error + dy > 0) {
	    --y;
	    stop_y -= xx2;
	    error += dy;
	    dy += xx2;
	}
    }
}

void fb_erase_string(framebuffer fb, point* top_left_corner, char* string) {

    point* pt = shape_duplicate_point(top_left_corner);
    pt->shade = FB_COLOR_ERASE;
    _fb_draw_string(fb, pt, string);
    SHDestroyPoint(pt);
}

private
void _fb_draw_string(framebuffer fb, point* top_left_corner, char* string) {

    point* pen = shape_duplicate_point(top_left_corner);
    while(*string != null) {
	_fb_draw_char(fb, pen, *(string++));
	pen->x += FONT_VALVANO_WIDTH + FONT_VALVANO_KERNING;
    }
    SHDestroyPoint(pen);
}

/* TODO: Allow for newline/carriage returns, printf style perhaps */
/* TODO: abstract font usage (modal), allow for multiple fonts to be
 * loaded/unloaded from memory (map). */
private
void _fb_draw_char(framebuffer fb, point* pen, char c) {

    uchar active, col, row, mask;
    for(col=0; col < FONT_VALVANO_WIDTH; ++col) {
	mask = 0x01;
	if (pen->x+col >= FB_WIDTH) {
	    /* We have exceeded the number of pixels on the screen */
#ifndef NDEBUG
	    printf("%s pen has gone offscreen. Ignoring rest of character...\n", __FUNCTION__);
#endif
	    break;
	}
	for(row=0; row < FONT_VALVANO_HEIGHT; ++row) {
	    if (pen->y+row >= FB_HEIGHT) {break;}
	    active = mask & font_valvano[c][col];
	    fb_set_pixel(fb, pen->x + col, pen->y + row, active ? pen->shade : 0);
	    mask = mask << 1;
	}
    }
}


char* itoa(int i, char* buffer, uchar length) {

    if (snprintf(buffer, length, "%02d", i) == -1) {
	return ""; /* base case, or error */
    }
    return buffer;
}

void fb_console_println_point(point* p) {

#ifdef __GNUC__
    printf("(%d,%d):%d\n", p->x, p->y, p->shade);
#else
#ifndef NDEBUG
    printf("%s This function was called in an inappropriate context.\n", __FUNCTION__);
    printf("%s Please consult the documentation.\n", __FUNCTION__);
#endif
#endif
}

void fb_console_print_point(point* p) {

#ifdef __GNUC__
    printf("(%d,%d):%d", p->x, p->y, p->shade);
#ifndef NDEBUG
    printf("%s This function was called in an inappropriate context.\n", __FUNCTION__);
    printf("%s Please consult the documentation.\n", __FUNCTION__);
#endif
#endif
}

void fb_console_println_coordinate(uchar x, uchar y, shade_t shade) {

#ifdef __GNUC__
    printf("(%d,%d):%d\n", x, y, shade);
#ifndef NDEBUG
    printf("%s This function was called in an inappropriate context.", __FUNCTION__);
    printf("Please consult the documentation.\n");
#endif
#endif
}

/* TODO: create a header prototype */
void fb_console_print_coordinate(uchar x, uchar y, shade_t shade) {

#ifdef __GNUC__
    printf("(%d,%d):%d", x, y, shade);
#ifndef NDEBUG
    printf("%s This function was called in an inappropriate context.", __FUNCTION__);
    printf("Please consult the documentation.\n");
#endif
#endif
}
