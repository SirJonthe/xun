#ifndef XOUT_H
#define XOUT_H

#include "../xdev.h"

class Monitor : public Device
{
public:
	static constexpr uint32_t WIDTH = 80;
	static constexpr uint32_t HEIGHT = 24;

private:
	U8 pixels[WIDTH*HEIGHT];

private:
	void Clear( void ) const;
	void Refresh( void ) const;

public:
	void Plot(U8 x, U8 y, U8 color);
	XWORD Boot( void );
	XWORD Cycle( void );
	XWORD Shutdown( void );

	void Randomize( void );
};

#endif // XOUT_H
