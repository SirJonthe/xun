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

Monitor::Monitor( void ) : Device("XERXES(tm) High Resolution Display V452", 0x0003), m_mode(MSG_PIXMODE), m_fontatlas(NULL), m_fontatlas_width(0), m_fontatlas_height(0)
{}

Monitor::~Monitor( void )
{
	delete [] m_fontatlas;
}

void Monitor::Plot(U16 x, U16 y, U8 color)
{
	m_pixels[y * WIDTH + x] = color;
}

U8 Monitor::GetPixel(U16 x, U16 y) const
{
	return m_pixels[y * WIDTH + x];
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
