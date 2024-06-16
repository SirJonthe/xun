#include <iostream>
#include <climits>
#include "xhwids.h"
#include "xout.h"

void Monitor::Clear( void )
{
	for (uint32_t i = 0; i < WIDTH * HEIGHT * STRIDE; ++i) {
		m_pixels[i] = 0;
	}
}

U8 *Monitor::GetCharMap( void )
{
	return m_memory + m_atlas_char_height_count * m_cell_px_height * m_atlas_char_width_count * (m_cell_px_width / 8);
}

U8 *Monitor::GetColorMap( void )
{
	return GetCharMap() + GetCharMapWidth() * GetCharMapHeight();
}

U8 *Monitor::GetCurrentCharMapLine( void )
{
	return GetCharMap() + GetCharMapWidth() * ((m_cy - m_scroll) % GetCharMapHeight());
}

U8 *Monitor::GetCurrentColorMapLine( void )
{
	return GetCurrentCharMapLine() + GetCharMapWidth() * GetCharMapHeight();
}

void Monitor::DrawChar(char ch, char color_index, int x, int y)
{
	// TODO: This code needs to be in Monitor, not in demo_monitor.
	if (GetAtlasCharWidthCount() == 0 || GetAtlasCharHeightCount() == 0) {
		return;
	}
	Monitor::Colors colors = GetColors(color_index);
	if (x >= 0 && y >= 0 && x + GetCharPxWidth() < Monitor::WIDTH && y + GetCharPxHeight() < Monitor::HEIGHT) {
		
		U8 *pixels = GetVideoScanline(y) + x * Monitor::STRIDE;

		if (ch >= GetFirstFontChar() && ch <= GetLastFontChar()) {

			// [X] get the x and y atlas coordinate of the glyph
			uint32_t glyph_i = ch - GetFirstFontChar();            // The index of the glyph.
			uint32_t glyph_x = glyph_i % GetAtlasCharWidthCount(); // The X coordinate of the glyph on the atlas.
			uint32_t glyph_y = glyph_i / GetAtlasCharWidthCount(); // The Y coordinate of the glyph on the atlas.

			// [X] convert to pixel x and y
			uint32_t glyph_px_x = glyph_x * GetCellPxWidth();  // disregard that each byte contains 8 pixels in a row in this step.
			uint32_t glyph_px_y = glyph_y * GetCellPxHeight(); // disregard that each byte contains 8 pixels in a row in this step.

			// [X] convert to memory location
			const U8 *glyph = GetMemory() + ((glyph_px_y * GetAtlasCharWidthCount() * GetCellPxWidth()) + glyph_px_x) / CHAR_BIT;

			for (uint32_t y = 0; y < GetCharPxHeight(); ++y) {
				const U8 *glyph_scanline = glyph + (GetAtlasCharWidthCount() * GetCellPxWidth() / 8) * y;
				U8 *pixel_scanline = pixels + Monitor::PITCH * y;
				for (uint32_t x = 0; x < GetCharPxWidth(); ++x) {
					uint8_t p = *glyph_scanline & (1 << x);
					pixel_scanline[x * Monitor::STRIDE + 0] = p ? colors.bg.r : colors.fg.r;
					pixel_scanline[x * Monitor::STRIDE + 1] = p ? colors.bg.g : colors.fg.g;
					pixel_scanline[x * Monitor::STRIDE + 2] = p ? colors.bg.b : colors.fg.b;
				}
			}
		} else if (ch != ' ' && ch != '\n' && ch != 0 && ch != '\t' && ch != '\r') {
			// [X] tofu: just draw a box
			for (int n = 0; n < GetCharPxWidth(); ++n) {
				pixels[n * Monitor::STRIDE + 0] = colors.fg.r;
				pixels[n * Monitor::STRIDE + 1] = colors.fg.g;
				pixels[n * Monitor::STRIDE + 2] = colors.fg.b;
			}
			pixels += Monitor::PITCH;
			for (int n = 0; n < GetCharPxHeight() - 2; ++n) {
				pixels[0]                                                     = colors.fg.r;
				pixels[1]                                                     = colors.fg.g;
				pixels[2]                                                     = colors.fg.b;
				for (int m = 1; m < GetCharPxWidth() - 1; ++m) {
					pixels[m * Monitor::STRIDE + 0]                            = colors.fg.r;
					pixels[m * Monitor::STRIDE + 1]                            = colors.fg.g;
					pixels[m * Monitor::STRIDE + 2]                            = colors.fg.b;
				}
				pixels[(GetCharPxWidth() - 1) * Monitor::STRIDE + 0] = colors.fg.r;
				pixels[(GetCharPxWidth() - 1) * Monitor::STRIDE + 1] = colors.fg.g;
				pixels[(GetCharPxWidth() - 1) * Monitor::STRIDE + 2] = colors.fg.b;
				pixels += Monitor::PITCH;
			}
			for (int n = 0; n < GetCharPxWidth(); ++n) {
				pixels[n * Monitor::STRIDE + 0] = colors.fg.r;
				pixels[n * Monitor::STRIDE + 1] = colors.fg.g;
				pixels[n * Monitor::STRIDE + 2] = colors.fg.b;
			}
		} else if (ch == ' ') {
			for (uint32_t y = 0; y < GetCharPxHeight(); ++y) {
				U8 *pixel_scanline = pixels + Monitor::PITCH * y;
				for (uint32_t x = 0; x < GetCharPxWidth(); ++x) {
					pixel_scanline[x * Monitor::STRIDE + 0] = colors.bg.r;
					pixel_scanline[x * Monitor::STRIDE + 1] = colors.bg.g;
					pixel_scanline[x * Monitor::STRIDE + 2] = colors.bg.b;
				}
			}
		}
	}
}

void Monitor::DrawCharMap( void )
{
	if (GetCharPxWidth() > 0 && GetCharPxHeight() > 0) {
		U8 *line = GetScrollCharMapLine();
		U8 *colors = GetScrollColorMapLine();

		for (uint32_t h = 0; h < GetCharMapHeight(); ++h) {
			const int y = h * GetCharPxHeight();
			for (uint32_t w = 0; w < GetCharMapWidth(); ++w) {
				const int x = w * GetCharPxWidth();
				DrawChar(line[w], colors[w], x, y);
			}
			line += GetCharMapWidth();
			colors += GetCharMapWidth();
		}
	}
}

void Monitor::DoPowerOn( void )
{
	Clear();
}

void Monitor::DoCycle( void )
{
	while (Poll()) {}
	Clear();
	switch (m_mode) {
	case MSG_TXTMODE:
		DrawCharMap();
		break;
	}
}

void Monitor::DoPowerOff( void )
{
	Clear();
}

void Monitor::Newline( void )
{
	m_cx = 0;
	++m_cy;
	if ((m_cy - m_scroll) >= GetCharMapHeight()) {
		++m_scroll;
		U8 *scroll = GetScrollCharMapLine();
		U8 *colors = GetScrollColorMapLine();
		for (uint32_t n = 0; n < GetCharMapWidth(); ++n) {
			scroll[n] = ' ';
			colors[n] = 0;
		}
	}
}

bool Monitor::HandlePacket(const Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        return true;
		case Packet::TYPE_CONNECT:    return true;
		case Packet::TYPE_DISCONNECT: return true;
		case Packet::TYPE_PING:       return true;
		case Packet::TYPE_PONG:       return true;
		case Packet::TYPE_DATA:
			if (m_mode == MSG_TXTMODE) {
				U8 *line = GetCurrentCharMapLine();
				U8 *colors = GetCurrentColorMapLine();
				for (uint32_t i = 0; i < msg.header[Device::Packet::HEADER_SIZE]; ++i) {
					if (msg.payload[i] != '\n') {
						line[m_cx] = msg.payload[i] & 0x00ff;
						colors[m_cx] = (msg.payload[i] & 0xff00) >> 8;
						++m_cx;
						if (m_cx >= GetCharMapWidth()) {
							Newline();
							line = GetCurrentCharMapLine();
							colors = GetCurrentColorMapLine();
						}
					} else {
						Newline();
						line = GetCurrentCharMapLine();
						colors = GetCurrentColorMapLine();
					}
				}
			} else if (m_mode == MSG_PIXMODE) {
			} else {
				Error("Invalid video mode");
			}
			return true;
		case Packet::TYPE_KEYVALS:    return true;
		case MSG_TXTMODE:
		case MSG_PIXMODE:
			Info("Got display mode");
			m_mode = msg.header[Packet::HEADER_TYPE];
			return true;
		case MSG_TXTMODE_LOADFONT:
			Info("Got font data");
			for (uint32_t i = 0; i < msg.header[Device::Packet::HEADER_SIZE]; ++i) {
				const uint32_t n = (msg.header[Device::Packet::HEADER_SEQ] * Device::Packet::PAYLOAD_WORD_SIZE + i) * 2;
				if (n >= MEMORY_SIZE - 1) {
					Error("Out of memory");
					break;
				}
				m_memory[n    ] = (msg.payload[i] & 0xFF00) >> 8;
				m_memory[n + 1] = msg.payload[i] & 0x00FF;
			}
			return true;
		case MSG_TXTMODE_SCROLL_DOWN:
			if ((m_cy - m_scroll) < GetCharMapHeight()) {
				++m_scroll;
				U8 *line = GetCurrentCharMapLine();
				U8 *colors = GetCurrentColorMapLine();
				for (uint32_t i = 0; i < GetCharMapWidth(); ++i) {
					line[i] = ' ';
					colors[i] = 0;
				}
			}
			return true;
		case MSG_TXTMODE_LOADFONTMETA:
			Info("Got font meta data");
			if (msg.header[Device::Packet::HEADER_SIZE] == 8) {
				m_char_px_width           = msg.payload[0];
				m_char_px_height          = msg.payload[1];
				m_atlas_char_width_count  = msg.payload[2];
				m_atlas_char_height_count = msg.payload[3];
				m_first_char              = msg.payload[4];
				m_last_char               = msg.payload[5];
				m_cell_px_width           = msg.payload[6];
				m_cell_px_height          = msg.payload[7];
				for (uint32_t i = 0; i < GetCharMapWidth() * GetCharMapHeight(); ++i) {
					GetCharMap()[i] = ' ';
					GetColorMap()[i] = 0;
				}
				m_cx = 0;
				m_cy = 0;
				m_scroll = 0;
			} else {
				Error("Payload size not 8");
			}
			return true;
	}
	return false;
}

Monitor::Monitor( void ) :
	Device("XERXES(tm) Multi-Color Display V452", XHWID_MON),
	m_char_px_width(0), m_char_px_height(0),
	m_atlas_char_width_count(0), m_atlas_char_height_count(0),
	m_cell_px_width(0), m_cell_px_height(0),
	m_scroll(0), m_cx(0), m_cy(0),
	m_mode(MSG_PIXMODE),
	m_first_char(0), m_last_char(0)
{
	m_pal[0].pal[0]  = Color{   0,   0,   0, 255 };
	m_pal[0].pal[1]  = Color{ 204,   0,   0, 255 };
	m_pal[0].pal[2]  = Color{  78, 154,   6, 255 };
	m_pal[0].pal[3]  = Color{ 196, 160,   0, 255 };
	m_pal[0].pal[4]  = Color{  54, 101, 164, 255 };
	m_pal[0].pal[5]  = Color{ 117,  80, 123, 255 };
	m_pal[0].pal[6]  = Color{   6, 152, 154, 255 };
	m_pal[0].pal[7]  = Color{ 211, 215, 207, 255 };
	m_pal[0].pal[8]  = Color{  85,  87,  83, 255 };
	m_pal[0].pal[9]  = Color{ 239,  41,  41, 255 };
	m_pal[0].pal[10] = Color{ 138, 226,  52, 255 };
	m_pal[0].pal[11] = Color{ 252, 233,  79, 255 };
	m_pal[0].pal[12] = Color{ 114, 159, 207, 255 };
	m_pal[0].pal[13] = Color{ 173, 127, 168, 255 };
	m_pal[0].pal[14] = Color{  52, 226, 226, 255 };
	m_pal[0].pal[15] = Color{ 255, 255, 255, 255 };

	for (unsigned i = 0; i < 16; ++i) {
		Color c = m_pal[0].pal[i];
		m_pal[1].pal[i] = Color{ U8(~c.r), U8(~c.g), U8(~c.b), c.a };
	}

	SetCyclesPerSecond(60);
}

void Monitor::Plot(U16 x, U16 y, U8 color)
{
	m_pixels[y * WIDTH + x] = color;
}

U8 Monitor::GetPixel(U16 x, U16 y) const
{
	return m_pixels[y * WIDTH + x];
}

U8 *Monitor::GetVideo( void )
{
	return m_pixels;
}

const U8 *Monitor::GetVideo( void ) const
{
	return m_pixels;
}

U8 *Monitor::GetVideoScanline(U8 y)
{
	return GetVideo() + (WIDTH * y * STRIDE);
}

const U8 *Monitor::GetVideoScanline(U8 y) const
{
	return GetVideo() + (WIDTH * y * STRIDE);
}

const U8 *Monitor::GetMemory( void ) const
{
	return m_memory;
}

uint32_t Monitor::GetCharPxWidth( void ) const
{
	return m_char_px_width;
}

uint32_t Monitor::GetCharPxHeight( void ) const
{
	return m_char_px_height;
}

uint32_t Monitor::GetAtlasCharWidthCount( void ) const
{
	return m_atlas_char_width_count;
}

uint32_t Monitor::GetAtlasCharHeightCount( void ) const
{
	return m_atlas_char_height_count;
}

uint32_t Monitor::GetCellPxWidth( void ) const
{
	return m_cell_px_width;
}

uint32_t Monitor::GetCellPxHeight( void ) const
{
	return m_cell_px_height;
}

U16 Monitor::GetMode( void ) const
{
	return m_mode;
}

U8 Monitor::GetFirstFontChar( void ) const
{
	return m_first_char;
}

U8 Monitor::GetLastFontChar( void ) const
{
	return m_last_char;
}

uint32_t Monitor::GetCharMapWidth( void ) const
{
	return WIDTH / m_char_px_width;
}

uint32_t Monitor::GetCharMapHeight( void ) const
{
	return HEIGHT / m_char_px_height;
}

U8 *Monitor::GetScrollCharMapLine( void )
{
	return GetCharMap() + GetCharMapWidth() * (m_scroll % GetCharMapHeight());
}

U8 *Monitor::GetScrollColorMapLine( void )
{
	return GetScrollCharMapLine() + GetCharMapWidth() * GetCharMapHeight();
}

Monitor::Colors Monitor::GetColors(U8 color_index) const
{
	return Colors{
		m_pal[0].pal[(color_index & 0xf0) >> 4],
		m_pal[1].pal[(color_index & 0x0f)]
	};
}
