#ifndef XC_H
#define XC_H

#include "xasm.h"

/// @brief Contains metadata about an output XASM binary.
struct xcout
{
	lexer   l;               // The state of the lexer when successfully exiting the assembler.
	xbinary out;             // The state of the output binary when successfully existing the assembler.
	U16     binary_size;     // The number of elements in the output binary. Zero if the assembly failed.
	U16     max_token_index; // The highest reached index in the input token stream. Generally only interesting if the assembly failed.
	U16     errors;          // The number of confirmed errors encountered.
};

/// @brief Compiles C source code.
/// @param l The lexer to use, loaded with the code to lex.
/// @param mem The memory to output to.
/// @return The binary output.
/// @sa init_lexer
xcout xcc(lexer l, xbinary mem);

#endif