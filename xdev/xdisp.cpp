#include "xdisp.h"
#include "MiniLib/MTL/mtlMemory.h"

#define xdisp_w 256
#define xdisp_h 256

bool xerx::MonochromeDisplay::Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM)
{
	switch (func)
	{
	case 16:
		SetSource(&RAM[param].u);
		break;
	case 17:
		Refresh();
		break;
	default: return false;
	}
	return true;
}

xerx::MonochromeDisplay::MonochromeDisplay( void ) : xerx::Device(0x0ABE, "Xerxes Monochrome Display", xerx::Device::Display), m_src(NULL), m_out(NULL), m_scale(1), m_frame(0)
{}

void xerx::MonochromeDisplay::SetSource(xerx::uword *pixels)
{
	m_src = pixels;
}

void xerx::MonochromeDisplay::Refresh( void )
{
	// 4096 words
	// 256x256 bits

	++m_frame;

	if (m_out == NULL) { return; }

	unsigned char *py = (unsigned char*)m_out->pixels;
	for (unsigned int y = 0; y < xdisp_h; ++y) {
		unsigned char *px = py;
		for (unsigned int x = 0; x < xdisp_w; ++x) {
			unsigned int i = y * xdisp_w + x;
			unsigned char bit = (m_src[i / xerx::WRD_BITS] & (1 << (i % xerx::WRD_BITS))) ? 0xFF : 0x00;
			Uint32 color = SDL_MapRGB(m_out->format, bit, bit, bit);
			unsigned int dither = (y ^ x ^ m_frame) & 1;
			for (unsigned int s = 0; s < m_scale; ++s) {
				//if (dither == 1) {
					SDL_GetRGB(color, m_out->format, px, px + 1, px + 2);
				//}
				px += m_out->format->BytesPerPixel;
			}
		}
		py += SDL_GetVideoSurface()->pitch;
		for (unsigned int s = 1; s < m_scale; ++s) {
			mtlCopy(py, py - m_out->pitch, m_out->pitch);
			py += m_out->pitch;
		}
	}
	if (m_scale >= 2) {
		unsigned char *p = (unsigned char*)m_out->pixels;
		for (unsigned int y = 0; y < m_out->h; y += m_scale) {
			for (unsigned int x = 0; x < m_out->pitch; ++x) {
				p[x] = p[x] >> 1;
			}
			p += m_out->pitch * m_scale;
		}
	}
	SDL_Flip(m_out);
}

bool xerx::MonochromeDisplay::SetVideoMode(xerx::uword scale)
{
	if (m_out != NULL) {
		SDL_FreeSurface(m_out);
		m_out = NULL;
	}
	m_scale = scale;
	m_out = SDL_SetVideoMode(256 * scale, 256 * scale, 24, SDL_SWSURFACE);
	return m_out != NULL;
}
