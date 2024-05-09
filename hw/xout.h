#ifndef XOUT_H
#define XOUT_H

#include "../xdev.h"

class Monitor : public Device
{
public:
	static constexpr uint32_t WIDTH               = 320;
	static constexpr uint32_t HEIGHT              = 240;
	static constexpr U16 MSG_PIXMODE              = 0xface;
	static constexpr U16 MSG_TXTMODE              = 0xbeef;
	static constexpr U16 MSG_TXTMODE_LOADSCANLINE = 0xdef0; // Transfers a single line of text from the text map to the monitor.
	static constexpr U16 MSG_TXTMODE_LOADFONT     = 0xabcd; // Writes the local bitfont to the monitor internal memory for use in text mode.
	static constexpr U16 MSG_TXTMODE_LOADFONTMETA = 0xaffe; // Writes metadata about the bitfont to the monitor (such as width and height of the atlas, width and height of individual characters, etc.)

private:
	U8   m_pixels[WIDTH*HEIGHT];
	//U16  m_text[];
	U8  *m_fontatlas;
	U16  m_fontatlas_width;
	U16  m_fontatlas_height;
	U16  m_mode;

private:
	void Clear( void );
	void Refresh( void ) const;

protected:
	void DoPowerOn( void );
	void DoCycle( void );
	void DoPowerOff( void );

public:
	Monitor( void );
	~Monitor( void );

	void Plot(U16 x, U16 y, U8 color);
	U8 GetPixel(U16 x, U16 y) const;
};

#endif // XOUT_H
