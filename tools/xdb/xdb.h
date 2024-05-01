#ifndef XDB_H
#define XDB_H

#include "../xcc/xasm/xasm.h"
#include "../../xcomp.h"

/// @brief A debugger for binary programs.
class xdebugger
{
public:
	Computer m_computer;

public:
	/// @brief Creates a new debugging with a program attached.
	/// @param program The program to debug.
	xdebugger(const xcc_binary &program);

	/// @brief Performs one instruction.
	/// @return False if the program has terminated.
	bool step( void );

	/// @brief Draws a UI for easy debugging.
	/// @param rows The number of rows in the UI to list.
	void ui(unsigned rows = 25) const;

	/// @brief Decodes a given instruction into a readable string.
	/// @param i The instruction to decode.
	/// @return The readable string.
	static chars decode(U16 instruction);
};

#endif
