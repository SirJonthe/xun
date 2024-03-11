#ifndef XCBE_H
#define XCBE_H

#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

/// @brief The writeable memory used for output in compiler/assembler output.
struct xbinary
{
	XWORD *buffer;
	U16    capacity;
	U16    size;
};

struct xc_error
{
	enum {
		NONE,
		MEMORY,
		REDEF,
		VERIFY,
		INTERNAL
	};
	token tok;  // The token generating the error.
	U16   code; // The error code.
};

/// @brief Contains metadata about an output XASM binary.
struct xc_out
{
	lexer    l;      // The state of the lexer when successfully exiting the assembler.
	xbinary  binary; // The state of the output binary when successfully existing the assembler.
	token    max;    // The highest reached token in the input token stream. Generally only interesting if the assembly failed.
	U16      errors; // The number of confirmed errors encountered.
	xc_error error;  //
};

#endif
