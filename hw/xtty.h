#ifndef XTTY_H
#define XTTY_H

#include "../xdev.h"

/// @brief A teleprinter allows the XUN machine to print text to the host machine terminal.
/// @note A teleprinter comes connected to a XUN machine by default.
class Teleprinter : public Device
{
protected:
	/// @brief Overloads the built-in HandlePacket with rules for how to handle incoming packets of certain types.
	/// @param msg The message.
	/// @return True if the package was recognized and was handled properly.
	bool HandlePacket(const Device::Packet &msg);

public:
	/// @brief Initializes a Teleprinter.
	Teleprinter( void );

	/// @brief Prints a series of characters.
	/// @param msg The message to print. Each U16 value contains only a single ASCII character.
	/// @param size The number of characters in the message.
	void PrintChars(const U16 *msg, U16 size);

	/// @brief Prints a series of numbers.
	/// @param msg The series of numbers to print in decimal format.
	/// @param size The number of numbers in the message.
	void PrintNums(const U16 *msg, U16 size);
};

#endif