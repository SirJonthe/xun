#ifndef XASM_H
#define XASM_H

#include "../xcc.h"

/// @brief Assembles extended assembly language in the form of input tokens.
/// @param l The lexer to be used to read code.
/// @param memory The memory to write to.
/// @param sym_capacity The maximum amount of symbols that can be stored at any one given time.
/// @return Metadata relating to the output assembly.
/// @sa xasm_lex
xcc_out xasm(lexer l, xcc_binary memory, const U16 sym_capacity = 128);

/// @brief Assembles a single statement of extended assembly language in the form of input tokens.
/// @param ps The parser state.
/// @return True on success.
/// @sa xasm_lex
bool xasm_inline(xcc_parser_state ps);

#endif