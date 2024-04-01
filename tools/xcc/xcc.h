#ifndef XCC_H
#define XCC_H

#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

/// @brief Compares two strings.
/// @param a The first string.
/// @param a_count The length of the first string.
/// @param b The second string.
/// @param b_count The length of the second string.
/// @return True if exact match.
bool xcc_strcmp(const char *a, unsigned a_count, const char *b, unsigned b_count);

/// @brief Counts characters in a string until null terminator encountered.
/// @param s The input string.
/// @return The length of the input string.
/// @note Null terminator is not counted into returned length.
unsigned xcc_chcount(const char *s);

/// @brief The writeable memory used for output in compiler/assembler output.
struct xcc_binary
{
	XWORD *buffer;
	U16    capacity;
	U16    size;
};

/// @brief Contains fatal parser/compiler error.
struct xcc_error
{
	enum {
		NONE,
		MEMORY,
		UNDEF,
		REDEF,
		VERIFY,
		INTERNAL,
		UNEXPECTED
	};
	token    tok;  // The token generating the error.
	U16      code; // The error code.
	unsigned line; // The location where the error occurred in the compiler.
};

/// @brief Contains metadata about an output XASM binary.
struct xcc_out
{
	lexer      l;      // The state of the lexer when successfully exiting the assembler.
	xcc_binary binary; // The state of the output binary when successfully existing the assembler.
	token      max;    // The highest reached token in the input token stream. Generally only interesting if the assembly failed.
	U16        errors; // The number of confirmed errors encountered.
	xcc_error  error;  // The first fatal error encountered.
};

/// @brief The input stream of tokens. If the user provides a pre-lexed array of tokens then the match function will consume from that array, while if the token array is null then the match function will instead lex code as attached to the lexer.
/// @sa xcc_match
struct xcc_input_tokens
{
	lexer        l;        // The lexer to use for parsing input tokens.
	const token *tokens;   // The input tokens. Can be null. If so, then tokens will be lexed from code attached to the lexer instead.
	U16          capacity; // The number of tokens in the explicit token stream, 'tokens'.
	U16          index;    // The index of the currently unread token.
};

/// @brief Writes data to the binary buffer.
/// @param buf The buffer to write the data to.
/// @param data The data to write to the buffer.
/// @return False if the buffer is full. True otherwise.
bool xcc_write_word(xcc_binary &buf, XWORD data);

/// @brief The data structure containing meta data about a declared symbol in code, e.g. variables, constants, functions, structures, etc.
struct xcc_symbol
{
	enum {
		VAR,   // A modifiable value with local automatic storage.
		SVAR,  // A modifiable value with global static storage.
		PARAM, // A modifiable value, that does not modify the stack.
		LIT,   // An immediate constant.
		FN     // An immediate constant for use as a target for jump instructions. Local automatic storage.
	};
	chars       name;        // The name of the symbol.
	XWORD       data;        // The address of a variable/function, or the value of a literal.
	U16         category;    // VAR, PARAM, LIT, FN
	U16         scope_index; // The scope index this symbol is defined in.
	U16         param_count; // For functions, the number of parameters a function takes.
	U16         size;        // 0 for literals and parameters, 1 for functions, 1 or more for variables.
	xcc_symbol *param;       // For functions, this points to the first parameter. For parameters, this points to the next parameter.
};

/// @brief The data structure containing the current state of declared and defined symbols in the compiler.
struct xcc_symbol_stack
{
	xcc_symbol *symbols;
	U16         capacity;  // The maximum number of symbols that can be on the stack.
	U16         count;     // The number of symbols on the stack.
	U16         top_index; // The index of the top scope.
	U16         scope;     // The number of the topmost scope.
};

/// @brief Gets the stack size (in words) of the topmost scope.
/// @param s The symbol stack.
/// @return The stack size (in words) of the topmost scope.
U16 xcc_top_scope_stack_size(const xcc_symbol_stack &s);

/// @brief Adds a new topmost scope to the symbol stack.
/// @param ss The symbol stack.
/// @return False if the symbol stack is full. True otherwise.
bool xcc_push_scope(xcc_symbol_stack &ss);

/// @brief Removes the topmost scope, and all its symbols, from the symbol stack.
/// @param ss The symbol stack.
/// @return False if the symbol stack has no scopes to remove. True otherwise.
bool xcc_pop_scope(xcc_symbol_stack &ss);

/// @brief The main data structure used for parsing C code.
struct xcc_parser
{
	xcc_input_tokens  in;     // The parser input.
	xcc_binary        out;    // The parser output.
	token             max;    // The maximally reached token.
	xcc_symbol_stack  scopes; // The symbols ordered into scopes.
	xcc_symbol       *fn;     // The current function being parsed.
	xcc_error         error;  // The first fatal error.
};

/// @brief Initializes a new parser.
/// @param l The lexer.
/// @param bin_mem The memory used for generated code (binary).
/// @param sym_mem The memory used by the compiler to store symbol information.
/// @param sym_capacity The capacity of the symbol memory.
/// @return The constructed parser.
xcc_parser xcc_init_parser(lexer l, xcc_binary bin_mem, xcc_symbol *sym_mem, U16 sym_capacity);

/// @brief Sets an error in the parser.
/// @param p The parser.
/// @param code The error code.
/// @param line The line in which the error occurred in the compiler.
void xcc_set_error(xcc_parser *p, U16 code, unsigned line);

/// @brief Writes data to the binary buffer in the parser.
/// @param buf The parser containing the buffer to write the data to.
/// @param data The data to write to the buffer.
/// @return False if the buffer is full. True otherwise.
bool xcc_write_word(xcc_parser *p, XWORD data);

/// @brief Writes instructions to reference a symbol properly.
/// @param p The parser.
/// @param sym The symbol.
/// @param offset An offset on the value/address (default 0).
/// @return False if the buffer is full. True otherwise.
bool xcc_write_rel(xcc_parser *p, const xcc_symbol *sym, U16 offset = 0);

/// @brief Does a search for a given symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_symbol(const chars &name, xcc_parser *p);

/// @brief Does a search for a given variable symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_var(const chars &name, xcc_parser *p);

/// @brief Does a search for a given literal symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_lit(const chars &name, xcc_parser *p);

/// @brief Does a search for a given function symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_fn(const chars &name, xcc_parser *p);

/// @brief Adds a symbol to the topmost symbol scope.
/// @param name The name of the symbol.
/// @param category The category of the symbol.
/// @param p The parser.
/// @param value The initial value of the symbol.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
xcc_symbol *xcc_add_symbol(const chars &name, unsigned category, xcc_parser *p, U16 value);

/// @brief Adds a specified amount of memory on the stack. The result is the same as adding an anonymous symbol of a given size to the symbol stack.
/// @param p The parser.
/// @param size The number of memory locations to add.
/// @return True if the memory was successfully added to the stack.
/// @note The parser receives an error if there is an internal error when adding a symbol.
bool xcc_add_memory(xcc_parser *p, U16 size);

/// @brief Adds a variable (automatic local storage) symbol to the topmost symbol scope.
/// @param name The name of the symbol.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
xcc_symbol *xcc_add_var(const chars &name, xcc_parser *p);

/// @brief Adds a variable (static global storage) symbol to the topmost symbol scope.
/// @param name The name of the symbol.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
xcc_symbol *xcc_add_svar(const chars &name, xcc_parser *p);

/// @brief Adds a parameter symbol to the topmost symbol scope.
/// @param name The name of the symbol.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
xcc_symbol *xcc_add_param(const chars &name, xcc_parser *p);

/// @brief Adds a literal symbol to the topmost symbol scope.
/// @param name The name of the symbol.
/// @param value The initial value of the symbol.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
xcc_symbol *xcc_add_lit(const chars &name, U16 value, xcc_parser *p);

/// @brief Adds a function symbol to the topmost symbol scope.
/// @param name The name of the symbol.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
xcc_symbol *xcc_add_fn(const chars &name, xcc_parser *p);

/// @brief Returns the size (in words) of the topmost symbol scope.
/// @param p The parser containing the symbol stack.
/// @return The size (in words) of the topmost symbol scope.
U16 xcc_top_scope_stack_size(const xcc_parser *p);

/// @brief Returns the first unread token in the parser without moving the state forward.
/// @param p The parser.
/// @param lexfn The lexing function to use.
/// @return The first unread token in the parser.
token xcc_peek(xcc_parser *p, token (*lexfn)(lexer*));

/// @brief Tries to match the next token in the parser to an expected type.
/// @param p The parser.
/// @param type The expected user type of the read token.
/// @param out The read token. If null, the token will not be read out from the function.
/// @param lexfn The lexing function to use.
/// @return True if the parsed token user type matches the expected type.
bool xcc_match(xcc_parser *p, unsigned type, token *out, token (*lexfn)(lexer*));

/// @brief Manages the parser state so that it can roll back on failure.
/// @note When matching matterns, use manage_state to and new_state to create a new parser_state.
/// @sa xcc_manage_state
/// @sa xcc_new_state
struct xcc_parser_state
{
	xcc_parser *p;             // The main parser.
	xcc_parser  restore_point; // The restore point if the current parsing fails.
	unsigned    end;           // The end token to know if the parser has reached an end.
};

/// @brief Constructs a new parser state from a current parser and an end token.
/// @param p The parser.
/// @param end A token user type representing the end of the token stream.
/// @return A new parser state.
xcc_parser_state xcc_new_state(xcc_parser *p, unsigned end);

/// @brief Manages the parser state so it properly rewinds if the parsing fails.
/// @param ps The current parser state.
/// @param success The success of the parsing.
/// @return The success of the parsing.
bool xcc_manage_state(xcc_parser_state &ps, bool success);

#endif
