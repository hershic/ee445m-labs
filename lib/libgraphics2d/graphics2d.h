#ifndef __GRAPHICS_2D__
#define __GRAPHICS_2D__

#include "libstd/defines.h"
#include "libframebuffer/framebuffer.h"

/*! Draw (unpacked) fb on the OLED.
 *  \param fb Framebuffer type (char**)
 *  \returns void
*/
void G2Draw(framebuffer);

/*! Unpack fb into \packedDeviceBuffer in the format understood by RIT
 *  functions.
 *  \param fb Framebuffer to convert into a char*
 *  \returns fb \fb in RITFormat (unpacked char* array)
*/
unsigned char* G2ConvertFBToRITFormat(framebuffer);

#endif	/*  __GRAPHICS_2D__ */
