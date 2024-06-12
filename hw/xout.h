#ifndef XOUT_H
#define XOUT_H

#include "../xdev.h"

/// @brief A pixel-based monitor that can be used for output.
class Monitor : public Device
{
public:
	static constexpr uint32_t WIDTH                    = 320;    // The number of pixels in the X axis.
	static constexpr uint32_t HEIGHT                   = 240;    // The number of pixels in the Y axis.
	static constexpr uint32_t MEMORY_SIZE              = 4096;   // The number of bytes in the monitor internal memory.
	static constexpr U16      MSG_PIXMODE              = 0xface; // Packet type that sets the current monitor mode to pixel mode where each pixel is addressable.
	static constexpr U16      MSG_TXTMODE              = 0xbeef; // Packet type that sets the current monitor mode to text mode where only the computer transmits what characters to print on the monitor.
	static constexpr U16      MSG_TXTMODE_LOADFONT     = 0xabcd; // Writes the local bitfont to the monitor internal memory for use in text mode.
	static constexpr U16      MSG_TXTMODE_LOADFONTMETA = 0xaffe; // Writes metadata about the bitfont to the monitor (such as width and height of the atlas, width and height of individual characters, etc.)
	static constexpr U16      MSG_TXTMODE_SCROLL_DOWN  = 0x123a; // Packet type that scrolls the character map down (only when in text mode).
	static constexpr U16      MSG_TXTMODE_SCROLL_UP    = 0x321b; // Packet type that scrolls the character map up (only when in text mode).

private:
	/// @brief A single 32-bit color.
	struct Color {
		U8 r; // Red.
		U8 g; // Green.
		U8 b; // Blue.
		U8 a; // Alpha.
	};

	/// @brief A palette of 16 32-bit colors.
	struct Palette {
		Color pal[16]; // The palette.
	};

private:
	U8       m_pixels[WIDTH*HEIGHT];    // The state of each pixel on the monitor.
	U8       m_memory[MEMORY_SIZE];     // The monitor internal memory. Contains various data such as bit font and character map.
	Palette  m_pal;                     // The color palette.
	uint32_t m_char_px_width;           // The width of a single character, in pixels, of the stored bit font.
	uint32_t m_char_px_height;          // The height of a single character, in pixels, of the stored bit font.
	uint32_t m_atlas_char_width_count;  // The number of characters in the X axis of the full bit font atlas.
	uint32_t m_atlas_char_height_count; // The number of characters in the Y axis of the full bit font atlas.
	uint32_t m_cell_px_width;           // The width of a single character, in pixels, of the stored bit font including padding.
	uint32_t m_cell_px_height;          // The height of a single character, in pixels, of the stored bit font including padding.
	uint32_t m_scroll;                  // The current scroll line.
	uint32_t m_cx, m_cy;                // The current caret X and Y coordinate.
	U16      m_mode;                    // The display mode (pixel mode or text mode).
	U8       m_first_char;              // The first character in the bit font.
	U8       m_last_char;               // The last character in the bit font.

private:
	/// @brief Blanks the monitor to black.
	void Clear( void );

	/// @brief Returns a pointer to the first character in the character map.
	/// @return A pointer to the first character in the character map.
	U8 *GetCharMap( void );

	/// @brief Returns a pointer to the first character of the current scroll line in the character map.
	/// @return A pointer to the first character of the current scroll line in the character map.
	U8 *GetCurrentCharMapLine( void );

protected:
	/// @brief Powers on the monitor.
	void DoPowerOn( void );

	/// @brief Cycles the monitor.
	void DoCycle( void );

	/// @brief Powers off the monitor.
	void DoPowerOff( void );

	/// @brief Handles newlines by incrementing the Y caret, resetting the X caret, and incrementing the scroll if necessary.
	void Newline( void );

	/// @brief Overloads the built-in HandlePacket with rules for how to handle incoming packets of certain types.
	/// @param msg The message.
	/// @return True if the package was recognized and was handled properly.
	bool HandlePacket(const Packet &msg);

public:
	/// @brief Initializes the Monitor.
	Monitor( void );

	/// @brief Sets a pixel to a given color.
	/// @param x The X coordinate of the pixel to write.
	/// @param y The Y coordinate of the pixel to write.
	/// @param color The 8-bit RGB color.
	void Plot(U16 x, U16 y, U8 color);

	/// @brief Returns the color of a pixel.
	/// @param x The X coordinate of the pixel to read.
	/// @param y The Y coordinate of the pixel to read.
	/// @return 
	U8 GetPixel(U16 x, U16 y) const;

	/// @brief Returns a pointer to the pixels in the video memory.
	/// @return A pointer to the pixels in the video memory.
	U8 *GetVideo( void );

	/// @brief Returns a pointer to the pixels in the video memory.
	/// @return A pointer to the pixels in the video memory.
	const U8 *GetVideo( void ) const;

	/// @brief Returns a pointer to the internal memory.
	/// @return A pointer to the internal memory.
	const U8 *GetMemory( void ) const;

	/// @brief Returns the width of a single character, in pixels, of the stored bit font.
	/// @return The width of a single character, in pixels, of the stored bit font.
	uint32_t GetCharPxWidth( void ) const;

	/// @brief Returns the height of a single character, in pixels, of the stored bit font.
	/// @return The height of a single character, in pixels, of the stored bit font.
	uint32_t  GetCharPxHeight( void ) const;

	/// @brief Returns the number of characters in the X axis of the full bit font atlas.
	/// @return The number of characters in the X axis of the full bit font atlas.
	uint32_t GetAtlasCharWidthCount( void ) const;

	/// @brief Returns the number of characters in the Y axis of the full bit font atlas.
	/// @return The number of characters in the Y axis of the full bit font atlas.
	uint32_t GetAtlasCharHeightCount( void ) const;

	/// @brief Returns the width of a single character, in pixels, of the stored bit font including padding.
	/// @return The width of a single character, in pixels, of the stored bit font including padding.
	uint32_t GetCellPxWidth( void ) const;

	/// @brief Returns the height of a single character, in pixels, of the stored bit font including padding.
	/// @return The height of a single character, in pixels, of the stored bit font including padding.
	uint32_t GetCellPxHeight( void ) const;

	/// @brief Returns the display mode.
	/// @return The display mode.
	U16 GetMode( void ) const;

	/// @brief Returns the first ASCII character in the bit font.
	/// @return The first ASCII character in the bit font.
	U8 GetFirstFontChar( void ) const;

	/// @brief Returns the last ASCII character in the bit font.
	/// @return The last ASCII character in the bit font.
	U8 GetLastFontChar( void ) const;

	/// @brief Returns the number of characters in the X axis of the character map.
	/// @return The number of characters in the X axis of the character map.
	uint32_t GetCharMapWidth( void ) const;

	/// @brief Returns the number of characters in the Y axis of the character map.
	/// @return The number of characters in the Y axis of the character map.
	uint32_t GetCharMapHeight( void ) const;

	/// @brief Returns a pointer to the first character of the current scroll line.
	/// @return A pointer to the first character of the current scroll line.
	U8 *GetScrollCharMapLine( void );
};

#endif // XOUT_H
