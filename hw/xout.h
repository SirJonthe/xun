#ifndef XOUT_H
#define XOUT_H

#include "../xdev.h"

class Monitor : public Device
{
public:
	static constexpr uint32_t WIDTH               = 320;
	static constexpr uint32_t HEIGHT              = 240;
	static constexpr uint32_t MEMORY_SIZE         = 4096;
	static constexpr U16 MSG_PIXMODE              = 0xface;
	static constexpr U16 MSG_TXTMODE              = 0xbeef;
	static constexpr U16 MSG_TXTMODE_LOADFONT     = 0xabcd; // Writes the local bitfont to the monitor internal memory for use in text mode.
	static constexpr U16 MSG_TXTMODE_LOADFONTMETA = 0xaffe; // Writes metadata about the bitfont to the monitor (such as width and height of the atlas, width and height of individual characters, etc.)
	static constexpr U16 MSG_TXTMODE_SCROLL_DOWN  = 0x123a;
	static constexpr U16 MSG_TXTMODE_SCROLL_UP    = 0x321b;

private:
	struct Color {
		U8 r, g, b, a;
	};
	struct Palette {
		Color pal[16];
	};

private:
	U8       m_pixels[WIDTH*HEIGHT];
	U8       m_memory[MEMORY_SIZE];
	Palette  m_fg;
	Palette  m_bg;
	uint32_t m_char_px_width;
	uint32_t m_char_px_height;
	uint32_t m_atlas_char_width_count;
	uint32_t m_atlas_char_height_count;
	uint32_t m_cell_px_width;
	uint32_t m_cell_px_height;
	uint32_t m_scroll;
	uint32_t m_cx, m_cy;
	U16      m_mode;
	U8       m_first_char;
	U8       m_last_char;

private:
	void Clear( void );
	void Refresh( void ) const;
	U8 *GetCharMap( void );
	U8 *GetCurrentCharMapLine( void );

protected:
	void DoPowerOn( void );
	void DoCycle( void );
	void DoPowerOff( void );
	void Newline( void );
	bool HandlePacket(const Packet &msg);

public:
	Monitor( void );

	void Plot(U16 x, U16 y, U8 color);
	U8 GetPixel(U16 x, U16 y) const;

	U8 *GetVideo( void );
	const U8 *GetVideo( void ) const;
	const U8 *GetMemory( void ) const;

	uint32_t  GetCharPxWidth( void ) const;
	uint32_t  GetCharPxHeight( void ) const;
	uint32_t  GetAtlasCharWidthCount( void ) const;
	uint32_t  GetAtlasCharHeightCount( void ) const;
	uint32_t  GetCellPxWidth( void ) const;
	uint32_t  GetCellPxHeight( void ) const;
	U16       GetMode( void ) const;
	U8        GetFirstFontChar( void ) const;
	U8        GetLastFontChar( void ) const;
	uint32_t  GetCharMapWidth( void ) const;
	uint32_t  GetCharMapHeight( void ) const;
	U8       *GetScrollCharMapLine( void );
};

#endif // XOUT_H
