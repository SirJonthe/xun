#ifndef XCBE_H
#define XCBE_H

#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

/// @brief The writeable memory used for output in compiler/assembler output.
struct xbinary
{
	struct buffer
	{
		XWORD *buffer;
		U16    capacity;
		U16    index;
	};
	buffer head;
	buffer body;
	buffer tail;
};

/// @brief Contains metadata about an output XASM binary.
struct xc_out
{
	lexer   l;           // The state of the lexer when successfully exiting the assembler.
	xbinary binary;      // The state of the output binary when successfully existing the assembler.
	U16     binary_size; // The number of elements in the output binary. Zero if the assembly failed.
	token   max;         // The highest reached token in the input token stream. Generally only interesting if the assembly failed.
	U16     errors;      // The number of confirmed errors encountered.
};

#endif
