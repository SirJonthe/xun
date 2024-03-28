#ifndef XB_H
#define XB_H

#include "../xcc.h"

/// @brief Compiles XUN B source code with the XUN instruction set.
/// @param l The lexer to use, loaded with the code to lex.
/// @param mem The memory to output to.
/// @param sym_capacity The maximum amount of symbols that can be stored at any one given time.
/// @return The binary output.
/// @sa init_lexer
xcc_out xb(lexer l, xcc_binary mem, const U16 sym_capacity = 128);

#endif