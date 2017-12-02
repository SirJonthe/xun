#ifndef XDISP_H__
#define XDISP_H__

#include <SDL/SDL.h>
#include "xdev.h"

namespace xerx
{
	class MonochromeDisplay : public xerx::Device
	{
	private:
		xerx::uword *m_src;
		SDL_Surface *m_out;
		xerx::uword  m_scale;
		unsigned int m_frame;

	protected:
		bool Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM);
		void SetSource(xerx::uword *pixels);
		void Refresh( void );

	public:
		MonochromeDisplay( void );
		bool SetVideoMode(xerx::uword scale);
	};
}

#endif // XDISP_H__
