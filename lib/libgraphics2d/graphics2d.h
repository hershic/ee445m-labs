#ifndef __GRAPHICS_2D__
#define __GRAPHICS_2D__

/* graphics2d.h
 * Hershal Bhave and Eric Crosson
 * 2014-02-08
 * Graphics engine for interfacing with vendor's libs
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968
 */

#include "g2d_defines.h"
#include "framebuffer.h"

void G2Draw(framebuffer fb);
unsigned char* G2ConvertFBToRITFormat(framebuffer fb);

#endif	/*  __GRAPHICS_2D__ */
