#include <iostream>
#include "xhwids.h"
#include "xout.h"

void Monitor::Clear( void )
{
	for (uint32_t i = 0; i < WIDTH * HEIGHT; ++i) {
		m_pixels[i] = 0;
	}
}

void Monitor::Refresh( void ) const
{}

U8 *Monitor::GetCharMap( void )
{
	return m_memory + m_atlas_char_height_count * m_cell_px_height * m_atlas_char_width_count * (m_cell_px_width / 8);
}

U8 *Monitor::GetCurrentCharMapLine( void )
{
	return GetCharMap() + GetCharMapWidth() * ((m_current_line - m_scroll) % GetCharMapHeight());
}

void Monitor::DoPowerOn( void )
{
	Clear();
	Refresh();
}

void Monitor::DoCycle( void )
{
	while (Poll()) {}
	Refresh();
}

void Monitor::DoPowerOff( void )
{
	Clear();
	Refresh();
}

bool Monitor::HandlePacket(const Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        return true;
		case Packet::TYPE_CONNECT:    return true;
		case Packet::TYPE_DISCONNECT: return true;
		case Packet::TYPE_PING:       return true;
		case Packet::TYPE_PONG:       return true;
		case Packet::TYPE_DATA:       return true;
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
		case MSG_TXTMODE_LOADLINE:
			Info("Got text line");
			{
				U8 *line = GetCurrentCharMapLine();
				uint32_t i = 0;
				uint32_t j = Device::Packet::PAYLOAD_WORD_SIZE * msg.header[Device::Packet::HEADER_SEQ];
				const uint32_t max = msg.header[Device::Packet::HEADER_SIZE] < GetCharMapWidth() - j ? msg.header[Device::Packet::HEADER_SIZE] : GetCharMapWidth() - j;
				std::cout << msg.header[Device::Packet::HEADER_SEQ] << ": ";
				for (; i < max; ++i, ++j) {
					line[j] = msg.payload[i];
				}
				for (; j < GetCharMapWidth(); ++j) {
					line[j] = ' ';
				}
			}
			return true;
		case MSG_TXTMODE_SCROLL_DOWN:
			if ((m_current_line - m_scroll) < GetCharMapHeight()) {
				++m_scroll;
				U8 *line = GetCurrentCharMapLine();
				for (uint32_t i = 0; i < GetCharMapWidth(); ++i) {
					line[i] = ' ';
				}
			}
			return true;
		case MSG_TXTMODE_NEWLINE:
			++m_current_line;
			if ((m_current_line - m_scroll) > GetCharMapHeight()) {
				++m_scroll;
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
				}
				m_current_line = 0;
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
	m_scroll(0), m_current_line(0),
	m_mode(MSG_PIXMODE),
	m_first_char(0), m_last_char(0)
{
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
