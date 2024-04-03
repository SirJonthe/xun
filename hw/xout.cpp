#include <iostream>
#include "xout.h"

void Monitor::Clear( void )
{
	for (uint32_t i = 0; i < WIDTH * HEIGHT; ++i) {
		m_pixels[i] = 0;
	}
}

void Monitor::Refresh( void ) const
{}

void Monitor::Plot(U8 x, U8 y, U8 color)
{
	m_pixels[y * WIDTH + x] = color;
}

void Monitor::PowerOn( void )
{
	if (IsPoweredOff()) {
		Device::PowerOn();
		Clear();
		Refresh();
	}
}

void Monitor::Cycle( void )
{
	Refresh();
}

void Monitor::PowerOff( void )
{
	if (IsPoweredOn()) {
		Clear();
		Refresh();
		Device::PowerOff();
	}
}
