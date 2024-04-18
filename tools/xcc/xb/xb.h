#ifndef XB_H
#define XB_H

#include "../xcc.h"

/// @brief Compiles XUN B source code with the XUN instruction set.
/// @param l The lexer to use, loaded with the code to lex.
/// @param std_lib_path The relationship between the XB binary compiling the source code, and the location of 'libb' folder. Must end with a separator if not empty.
/// @param mem The memory to output to.
/// @param sym_capacity The maximum amount of symbols that can be stored at any one given time.
/// @param file_capacity The maximum number of files that can be compiled in a single call.
/// @return The binary output.
/// @sa init_lexer
xcc_out xb(lexer l, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity = XCC_DEFAULT_SYM_CAPACITY, const unsigned file_capacity = XCC_DEFAULT_FILE_CAPACITY);

/// @brief Compiles XUN B source code with the XUN instruction set.
/// @param source_files The input source files (filenames).
/// @param num_source_files The number of input files.
/// @param std_lib_path The relationship between the XB binary compiling the source code, and the location of 'libb' folder. Must end with a separator if not empty.
/// @param mem The memory to output to.
/// @param sym_capacity The maximum amount of symbols that can be stored at any one given time.
/// @param file_capacity The maximum number of files that can be compiled in a single call.
/// @return The binary output.
/// @note There is no need to include header files in the source files parameter.
xcc_out xb(const chars::view *source_files, U16 num_source_files, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity = XCC_DEFAULT_SYM_CAPACITY, const unsigned file_capacity = XCC_DEFAULT_FILE_CAPACITY);

#endif
