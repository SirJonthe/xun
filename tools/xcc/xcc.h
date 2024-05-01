#ifndef XCC_H
#define XCC_H

#include "../../xarch.h"
#include "../../lib/parsec/lex.h"
#include "../../lib/sum/sum.h"

static constexpr unsigned XCC_DEFAULT_SYM_CAPACITY  =  128; // The default symbol capacity to use when compiling.
static constexpr unsigned XCC_DEFAULT_FILE_CAPACITY = 1024; // The default maximum number of files that can be compiled in a single compiler call.

/// @brief Contains the text of a single file.
struct xcc_text
{
	char               *txt; // The text.
	uint32_t            len; // The length, in characters, of the text.
	cc0::sum::md5::sum  sum; // The checksum of the file.

	/// @brief Creates a new instance.
	xcc_text( void );

	/// @brief Copies data inside an existing instance.
	xcc_text(const xcc_text &txt);

	/// @brief Copies data inside an existing instance.
	/// @return Reference to destination instance.
	xcc_text &operator=(const xcc_text &txt);

	/// @brief Frees allocated memory.
	~xcc_text( void );
};

/// @brief Creates a new text buffer.
/// @param txt The buffer to allocate memory in.
/// @param len The length of the buffer.
void xcc_new_text(xcc_text &txt, uint32_t len);

/// @brief Deletes text from memory.
/// @param txt The text to delete from memory.
void xcc_clear_text(xcc_text &txt);

/// @brief Loads the contents of a file into a specified output.
/// @param filename The file to buffer.
/// @param out The buffered output of the file.
/// @return True if load was successful.
bool xcc_load_text(const chars::view &filename, xcc_text &out);

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
		UNEXPECTED,
		MISSING
	};
	token       tok;  // The token generating the error.
	U16         code; // The error code.
	chars       file; // The name of the file (may be abbreviated to 31) characters.
	const char *ifile;
	unsigned    iline; // The location where the error occurred in the compiler.
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

/// @brief Writes data to the binary buffer.
/// @param buf The buffer to write the data to.
/// @param data The data to write to the buffer.
/// @return False if the buffer is full. True otherwise.
bool xcc_write_word(xcc_binary &buf, XWORD data);

/// @brief The file hashing function.
typedef cc0::sum::md5 xcc_filehasher;

/// @brief The checksum type from the file hashing function.
typedef xcc_filehasher::sum xcc_filesum;

/// @brief The data structure containing meta data about a declared symbol in code, e.g. variables, constants, functions, structures, etc.
struct xcc_symbol
{
	enum {
		STORAGE_AUTO,   // A modifiable value with local automatic storage.
		STORAGE_STATIC, // A modifiable value with global static storage.
		STORAGE_PARAM,  // A modifiable value, that does not modify the stack.
		STORAGE_LIT,    // An immediate constant.
		STORAGE_FN,     // A modifiable value with local automatic storage for use as a target for jump instructions.
		STORAGE_LBL     // A modifiable value with local automatic storage for use as a target for jump instructions.
	};
	enum { // This also represents the order in which types are promoted (i.e. to the higher type number).
		TYPE_SIGNED,   // signed integer
		TYPE_UNSIGNED, // unsigned integer
		TYPE_POINTER,  // pointer (unsigned integer)
		TYPE_FLOAT,    // signed floating-point value
		TYPE_LBL,      // a label
		TYPE_GROUP     // structs and classes
	};
	token       tok;         // The token successfully converted into the symbol. The name of the symbol is in here along with other meta data.
	XWORD       data;        // The address of a variable/function, or the value of a literal.
	U16         storage;     // STORAGE_AUTO, STORAGE_STATIC, STORAGE_PARAM, STORAGE_LIT, STORAGE_FN, STORAGE_LBL
	U16         type;        // TYPE_SIGNED, TYPE_UNSIGNED, TYPE_POINTER, TYPE_FLOAT, TYPE_LBL, TYPE_GROUP
	U16         scope_index; // The scope index this symbol is defined in.
	U16         param_count; // For functions, the number of parameters a function takes.
	U16         size;        // 0 for literals and parameters, 1 for functions, 1 or more for variables.
	U16         link;        // Indicates that the symbol is declared, but not defined and requires linkage between definition and a later declaration.
	xcc_symbol *param;       // For functions, this points to the first parameter. For parameters, this points to the next parameter. For groups, this points to the first member. For members this points to the next member.
	chars       file;        // The short name of the file this symbol was declared in.
	bool        variadic;    // If the symbol is a function, then TRUE denotes that the function can take at least param_count arguments, but possibly any number more.
};

/// @brief The data structure containing the current state of declared and defined symbols in the compiler.
struct xcc_symbol_stack
{
	xcc_symbol *symbols;
	U16         capacity;  // The maximum number of symbols that can be on the stack.
	U16         count;     // The number of symbols on the stack.
	U16         scope;     // The number of the topmost scope.
};

/// @brief Gets the stack size (in words) of the topmost scope.
/// @param s The symbol stack.
/// @return The stack size (in words) of the topmost scope.
U16 xcc_top_scope_stack_size(const xcc_symbol_stack &s);

/// @brief Gets the stack size (in words) of the scopes from top to specified index.
/// @param s The symbol stack.
/// @param scope_index The scope index to measure size down to.
/// @return The stack size (in words) of the scopes from top to specified index.
U16 xcc_loop_stack_size(const xcc_symbol_stack &s, U16 scope_index);

/// @brief A structure to hold an array of checksums from files. Its main intended use is to keep track of all compiled files, and prevent compiling an already compiled file.
struct xcc_filesums
{
	xcc_filesum *sums;     // The array of file checksums.
	unsigned     capacity; // The capacity of the file checksum array.
	unsigned     count;    // The current count of file checksums in the file checksums array.
};

/// @brief Attempts to add a file checksum to a file checksum array.
/// @param fs The file checksum array.
/// @param s The checksum to add.
/// @return False if the file checksum array is full, or if the checksum already exists in the checksum array. True otherwise.
bool xcc_add_filesum(xcc_filesums &fs, const xcc_filesum &s);

/// @brief The main data structure used for parsing C code.
struct xcc_parser
{
	lexer             in;      // The parser input.
	xcc_binary        out;     // The parser output.
	token             max;     // The maximally reached token.
	xcc_symbol_stack  scopes;  // The symbols ordered into scopes.
	xcc_symbol       *fn;      // The current function being parsed.
	xcc_error         error;   // The first fatal error.
	chars             file;    // The short name of the current file.
	xcc_filesums      fsums;   // The checksum of the currently read file.
};

/// @brief Initializes a new parser.
/// @param l The lexer.
/// @param bin_mem The memory used for generated code (binary).
/// @param sym_mem The memory used by the compiler to store symbol information.
/// @param sym_capacity The capacity of the symbol memory.
/// @param file_mem The memory used by the compiler to keep track of compiled files.
/// @param file_capacity The capacity of the file memory.
/// @return The constructed parser.
xcc_parser xcc_init_parser(lexer l, xcc_binary bin_mem, xcc_symbol *sym_mem, U16 sym_capacity, xcc_filesum *file_mem, unsigned file_capacity);

/// @brief Sets an error in the parser.
/// @param p The parser.
/// @param t The token causing the error.
/// @param code The error code.
/// @param file The name (including path) of the file the error occurred in (max 31 characters).
/// @param ifile The filename of the file in the compiler in which the error was triggered.
/// @param iline The line in which the error occurred in the compiler.
void xcc_set_error(xcc_parser *p, const token &t, U16 code, const chars &file, const char *ifile, unsigned iline);

/// @brief Adds a new topmost scope to the symbol stack.
/// @param p The parser.
/// @param reset_lsp True (default) if local stack pointer should be reset to 0. This is used when transitioning from code space to global space, and global space to function space (since we have A, B, C relative offset registers), but not when adding scopes to the stack within functions.
/// @return False if the symbol stack is full. True otherwise.
/// @note 
bool xcc_push_scope(xcc_parser *p, bool reset_lsp = true);

/// @brief Removes the topmost scope, and all its symbols, from the symbol stack.
/// @param p The parser.
/// @return False if the symbol stack has no scopes to remove. True otherwise.
bool xcc_pop_scope(xcc_parser *p);

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
/// @param reverse_search If true, starts symbol search from stack 0 to n, rather than n to 0 (which is default).
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_symbol(const chars &name, xcc_parser *p, bool reverse_search = false);

/// @brief Does a search for a given variable symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @param reverse_search If true, starts symbol search from stack 0 to n, rather than n to 0 (which is default).
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_var(const chars &name, xcc_parser *p, bool reverse_search = false);

/// @brief Does a search for a given literal symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @param reverse_search If true, starts symbol search from stack 0 to n, rather than n to 0 (which is default).
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_lit(const chars &name, xcc_parser *p, bool reverse_search = false);

/// @brief Does a search for a given function symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @param reverse_search If true, starts symbol search from stack 0 to n, rather than n to 0 (which is default).
/// @return The found symbol. Null if no symbol was found.
xcc_symbol *xcc_find_fn(const chars &name, xcc_parser *p, bool reverse_seach = false);

/// @brief Does a search for a given label symbol.
/// @param name The name of the symbol.
/// @param p The parser containing the symbol stack to search.
/// @return The found symbol. Null if no symbol was found.
/// @param reverse_search If true, starts symbol search from stack 0 to n, rather than n to 0 (which is default).
/// @note Due to the dangers of jumping, only the top scope is searched.
xcc_symbol *xcc_find_lbl(const chars &name, xcc_parser *p, bool reverse_search = false);

/// @brief Adds a symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param storage The storage of the symbol.
/// @param p The parser.
/// @param value The initial value of the symbol.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_symbol(const token &tok, unsigned storage, xcc_parser *p, U16 value);

/// @brief Adds a specified amount of memory on the stack. The result is the same as adding an anonymous symbol of a given size to the symbol stack.
/// @param p The parser.
/// @param size The number of memory locations to add.
/// @return True if the memory was successfully added to the stack.
/// @note The parser receives an error if there is an internal error when adding a symbol.
bool xcc_add_memory(xcc_parser *p, U16 size);

/// @brief Adds a variable (automatic local storage) symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_var(const token &tok, xcc_parser *p);

/// @brief Adds a variable (static global storage) symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_svar(const token &tok, xcc_parser *p);

/// @brief Adds a parameter symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_param(const token &tok, xcc_parser *p);

/// @brief Adds a literal symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param value The initial value of the symbol.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_lit(const token &tok, U16 value, xcc_parser *p);

/// @brief Adds a function symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_fn(const token &tok, xcc_parser *p);

/// @brief Adds a label symbol to the topmost symbol scope.
/// @param tok The token containing the name of the symbol to be added.
/// @param p The parser.
/// @return The added symbol. Null if there was an error.
/// @note The parser receives an error if there is an internal error when adding a symbol.
/// @note Reserved keywords and other tokens are implemented by having the alias regex last in the token list. That way a potential alias matches against a keyword first and the parser fails to recognize the token as an alias, thereby preventing it from being registered as a symbol.
xcc_symbol *xcc_add_lbl(const token &tok, xcc_parser *p);

/// @brief Returns the size (in words) of the topmost symbol scope.
/// @param p The parser containing the symbol stack.
/// @return The size (in words) of the topmost symbol scope.
U16 xcc_top_scope_stack_size(const xcc_parser *p);

/// @brief Gets the stack size (in words) of the scopes from top to specified index.
/// @param p The parser containing the symbol stack.
/// @param scope_index The scope index to measure size down to.
/// @return The stack size (in words) of the scopes from top to specified index.
U16 xcc_loop_stack_size(const xcc_parser *p, U16 loop_scope);

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

/// @brief A structure representing a path to a directory or file.
struct xcc_path
{
	static constexpr uint32_t MAXPATH = 256;

	char     str[MAXPATH]; // The path characters.
	uint32_t len;          // The length of the path.

	/// @brief Constructs a default object instance.
	xcc_path( void );
	
	/// @brief Copies a path.
	/// @param NA The object to copy.
	xcc_path(const xcc_path&) = default;
	
	/// @brief Copies a path.
	/// @param NA The object to copy.
	/// @return Reference to the destination instance.
	xcc_path &operator=(const xcc_path&) = default;
};

/// @brief Takes a path and uses a relative path as a navigation agent to end up with another path.
/// @param out The current path, and the resulting path.
/// @param rwd The relative path.
/// @return False if the resulting path does not fit the path character limits.
bool xcc_set_path(xcc_path &out, const chars::view &rwd);

/// @brief Returns the path in a short form (max 31 characters).
/// @param path The path.
/// @return The short form of the path.
chars xcc_short_path(const chars::view &path);

/// @brief Manages the parser state so that it can roll back on failure.
/// @note When matching matterns, use manage_state to and new_state to create a new parser_state.
/// @sa xcc_manage_state
/// @sa xcc_new_state
struct xcc_parser_state
{
	xcc_parser  *p;             // The main parser.
	xcc_parser   restore_point; // The restore point if the current parsing fails.
	xcc_path     cwd;           // The current working directory.
	chars::view  swd;           // The standard working directory (never changes).
	unsigned     end;           // The end token to know if the parser has reached an end.
	unsigned     break_ip;      // The relative instruction address of the CNJMP instruction of last entered loop.
	unsigned     continue_ip;   // The relative instruction address to the first instruction of the test of the last loop.
	unsigned     loop_scope;    // The index of the scope right outside the loop.
};

/// @brief Constructs a new parser state from a current parser state and an end token.
/// @param ps The previous parser state to base the new one on.
/// @param end A token user type representing the end of the token stream.
/// @param break_ip The relative instruction address of the CNJMP instruction of last entered loop.
/// @param continue_ip The relative instruction address to the first instruction of the test of the last loop.
/// @param loop_scope The scope index of the scope outside the last loop.
/// @return A new parser state.
xcc_parser_state xcc_new_state(const xcc_parser_state &ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope);

/// @brief Constructs a new parser state from a current parser and an end token.
/// @param p The parser to base the new state on.
/// @param ps The previous parser state.
/// @param end A token user type representing the end of the token stream.
/// @param break_ip The relative instruction address of the CNJMP instruction of last entered loop.
/// @param continue_ip The relative instruction address to the first instruction of the test of the last loop.
/// @param loop_scope The scope index of the scope outside the last loop.
/// @return A new parser state.
xcc_parser_state xcc_new_state(xcc_parser *p, const xcc_parser_state *ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope);

/// @brief Manages the parser state so it properly rewinds if the parsing fails.
/// @param ps The current parser state.
/// @param success The success of the parsing.
/// @return The success of the parsing.
bool xcc_manage_state(xcc_parser_state &ps, bool success);

#endif
