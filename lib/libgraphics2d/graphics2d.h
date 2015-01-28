#ifndef __GRAPHICS_2D__
#define __GRAPHICS_2D__

/*!
 * \brief Framebuffer/OLED interaction.
 * \details Graphics engine for interfacing with display vendor's libs.
 *  \author    Hershal Bhave
 *  \author    Eric Crosson
 *  \version   0.1
 *  \date      2014
 *  \copyright GNU Public License.
 */

#include "g2d_defines.h"
#include "framebuffer.h"

/*! Draw (unpacked) fb on the OLED.
 *  \param fb Framebuffer type (char**)
 *  \returns void
*/
void G2Draw(framebuffer fb);

/*! Unpack fb into \packedDeviceBuffer in the format understood by RIT
 *  functions.
 *  \param fb Framebuffer to convert into a char*
 *  \returns fb \fb in RITFormat (unpacked char* array)
*/
unsigned char* G2ConvertFBToRITFormat(framebuffer fb);

#endif	/*  __GRAPHICS_2D__ */
