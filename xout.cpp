#include <iostream>
#include "xout.h"

// For cursor movement:
// https://tldp.org/HOWTO/Bash-Prompt-HOWTO/x361.html

// For terminal colors
// https://en.wikipedia.org/wiki/ANSI_escape_code#Colors

static constexpr U8 palette[] = { 40, 41, 42, 43, 44, 45, 46, 47, 100, 101, 102, 103, 104, 105, 106, 107 };

void Monitor::Clear( void ) const
{
	std::cout << "\033[0;0H";
}

void Monitor::Refresh( void ) const
{
	for (uint32_t y = 0; y < HEIGHT - 1; ++y) {
		for (uint32_t x = 0; x < WIDTH; ++x) {
			std::cout << "\033[31;" << uint32_t(palette[pixels[y * WIDTH + x]]) << "m ";
		}
		std::cout << "\n";
	}
	for (uint32_t x = 0; x < WIDTH; ++x) {
		std::cout << "\033[31;" << uint32_t(palette[pixels[(HEIGHT - 1) * WIDTH + x]]) << "m ";
	}
	std::cout << std::flush;
}

void Monitor::Plot(U8 x, U8 y, U8 color)
{
	pixels[y * WIDTH + x] = color;
}

XWORD Monitor::Boot( void )
{
	for (uint32_t i = 0; i < WIDTH * HEIGHT; ++i) {
		pixels[i] = 0;
	}
	Refresh();
	return XWORD{ 0 };
}

XWORD Monitor::Cycle( void )
{
	Clear();
	Refresh();
	return XWORD{ 0 };
}

XWORD Monitor::Shutdown( void )
{
	std::cout << "\033[0m";
	return XWORD{ 0 };
}

void Monitor::Randomize( void )
{
	for (uint32_t y = 0; y < HEIGHT; ++y) {
		for (uint32_t x = 0; x < WIDTH; ++x) {
			pixels[y * WIDTH + x] = U8(rand() & 15);
		}
	}
}
