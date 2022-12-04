#ifndef XKEYB_H__
#define XKEYB_H__

#include "xdev.h"

namespace xerx
{

	class Keyboard : public xerx::Device
	{
	private:
		char          m_keybuffer[256];
		unsigned char m_index;

	protected:
		bool        Function(uword func, uword &param, word_t *RAM);
		xerx::uword GetKeyDown( void );

	public:
		Keyboard( void );
	};

}

#endif // XKEYB_H__
