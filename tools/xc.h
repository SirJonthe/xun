#ifndef XC_H
#define XC_H

#include "xasm.h"

/// @brief Contains metadata about an output XASM binary.
struct xc_out
{
	lexer   l;               // The state of the lexer when successfully exiting the assembler.
	xbinary binary;          // The state of the output binary when successfully existing the assembler.
	U16     binary_size;     // The number of elements in the output binary. Zero if the assembly failed.
	U16     max_token_index; // The highest reached index in the input token stream. Generally only interesting if the assembly failed.
	U16     errors;          // The number of confirmed errors encountered.
};

/// @brief Compiles C source code.
/// @param l The lexer to use, loaded with the code to lex.
/// @param mem The memory to output to.
/// @param sym_capacity The maximum amount of symbols that can be stored at any one given time.
/// @return The binary output.
/// @sa init_lexer
xc_out xcc(lexer l, xbinary mem, const U16 sym_capacity = 128);

#endif
