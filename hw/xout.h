#ifndef XOUT_H
#define XOUT_H

#include "../xdev.h"

class Monitor : public Device
{
public:
	static constexpr uint32_t WIDTH  = 320;
	static constexpr uint32_t HEIGHT = 240;

private:
	U8 m_pixels[WIDTH*HEIGHT];

private:
	void Clear( void );
	void Refresh( void ) const;

public:
	Monitor( void );

	void Plot(U16 x, U16 y, U8 color);
	U8 GetPixel(U16 x, U16 y) const;
	void PowerOn( void );
	void Cycle( void );
	void PowerOff( void );
};

#endif // XOUT_H
