#include <ctime>
#include <SDL/SDL.h>
#include "xclock.h"

xerx::uword xerx::Clock::GetMS( void ) const
{
	return (xerx::uword)(SDL_GetTicks() % 1000);
}

xerx::uword xerx::Clock::GetS( void ) const
{
	time_t t = time(NULL);
	return (xerx::uword)localtime(&t)->tm_sec;
}

xerx::uword xerx::Clock::GetH( void ) const
{
	time_t t = time(NULL);
	return (xerx::uword)localtime(&t)->tm_hour;
}

xerx::uword xerx::Clock::GetY( void ) const
{
	time_t t = time(NULL);
	return (xerx::uword)localtime(&t)->tm_year;
}

void xerx::Clock::Halt(xerx::uword time_ms) const
{
	SDL_Delay(time_ms);
}

bool xerx::Clock::Function(uword func, uword &param, word_t *RAM)
{
	switch (func) {
	case 16:
		param = GetMS();
		break;
	case 17:
		param = GetS();
		break;
	case 18:
		param = GetH();
		break;
	case 19:
		param = GetY();
		break;
	case 20:
		Halt(param);
		break;
	default: return false;
	}
	return true;
}

xerx::Clock::Clock( void ) : xerx::Device(0xeffe, "Xerxes System Clock", xerx::Device::Compute)
{}
