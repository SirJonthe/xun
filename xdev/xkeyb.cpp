#include <SDL/SDL.h>
#include "xkeyb.h"

bool xerx::Keyboard::Function(uword func, uword &param, word_t *RAM)
{
	switch (func)
	{
	case 16:
		param = GetKeyDown();
		break;
	default: return false;
	}
	return true;
}

xerx::uword xerx::Keyboard::GetKeyDown( void )
{
	// returns the next key down
	// returns 0 if there are no more keys down
	return 0;
}


xerx::Keyboard::Keyboard( void ) : xerx::Device(0, "", xerx::Device::Keyboard), m_index(0)
{
	for (int i = 0; i < sizeof(m_keybuffer); ++i) {
		m_keybuffer[i] = 0;
	}
}
