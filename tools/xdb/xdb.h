#ifndef XDB_H
#define XDB_H

#include "../xasm/xasm.h"
#include "../../xcomp.h"

/// @brief A debugger for binary programs.
class xdebugger
{
public:
	Computer m_computer;

public:
	/// @brief Creates a new debugging with a program attached.
	/// @param program The program to debug.
	xdebugger(const xbinary &program);

	/// @brief Performs one instruction.
	/// @return False if the program has terminated.
	bool step( void );

	/// @brief Draws a UI for easy debugging.
	void ui( void ) const;
};

/// @brief Decodes a given instruction into a readable string.
/// @param i The instruction to decode.
/// @return The readable string.
chars decode_instruction(XIS::Enum i);

#endif
