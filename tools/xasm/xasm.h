#ifndef XASM_H
#define XASM_H

#include "../xcc/xcc.h"

// LAYOUT OF BINARY
//
// [JUMP TO MAIN] <-- EP needs to point here at the start of the execution (set 6 erel at erel jmp)
// [DATA]
//    [MAIN ADDRESS VARIABLE]
//    [GRAPHICS]                   -|
//    [FUNCTION ADDRESS VARIABLES]  |---< Can be any order, can be any data. XASM uses a "BIN" or "DATA" keyword to store raw data in the binary (this is not an instruction on the processor).
//    [GLOBAL VARIABLES]            |
//    etc...                       -|
// [INSTRUCTIONS]
//    [FUNCTIONS BEFORE MAIN]
//    [MAIN]                  <-- IP needs to point here at the start of execution by using JMP at start of binary
//    [FUNCTIONS AFTER MAIN]
// [STACK]                    <-- CP needs to point here (SP is 0)
//    [ELEMENTS]              <-- SP points to top here as an offset from CP

/// @brief XASM token identifier.
struct xtoken
{
	/// @brief The various constants representing an XASM token identifier.
	/// @note The token constants do not correspond to the XIS constants.
	enum tokentype
	{
		KEYWORD_INSTRUCTION = token::KEYWORD | (1<<8),
			KEYWORD_INSTRUCTION_NOP,
			KEYWORD_INSTRUCTION_SET,
			KEYWORD_INSTRUCTION_PUT,
			KEYWORD_INSTRUCTION_EREL,
			KEYWORD_INSTRUCTION_CREL,
			KEYWORD_INSTRUCTION_AT,
			KEYWORD_INSTRUCTION_JMP,
			KEYWORD_INSTRUCTION_CJMP,
			KEYWORD_INSTRUCTION_SKIP,
			KEYWORD_INSTRUCTION_CSKIP,
			KEYWORD_INSTRUCTION_ADD,
			KEYWORD_INSTRUCTION_SUB,
			KEYWORD_INSTRUCTION_MUL,
			KEYWORD_INSTRUCTION_DIV,
			KEYWORD_INSTRUCTION_MOD,
			KEYWORD_INSTRUCTION_IADD,
			KEYWORD_INSTRUCTION_ISUB,
			KEYWORD_INSTRUCTION_IMUL,
			KEYWORD_INSTRUCTION_IDIV,
			KEYWORD_INSTRUCTION_IMOD,
			KEYWORD_INSTRUCTION_INEG,
			KEYWORD_INSTRUCTION_LSH,
			KEYWORD_INSTRUCTION_RSH,
			KEYWORD_INSTRUCTION_AND,
			KEYWORD_INSTRUCTION_OR,
			KEYWORD_INSTRUCTION_XOR,
			KEYWORD_INSTRUCTION_NOT,
			KEYWORD_INSTRUCTION_EQ,
			KEYWORD_INSTRUCTION_NE,
			KEYWORD_INSTRUCTION_LE,
			KEYWORD_INSTRUCTION_GE,
			KEYWORD_INSTRUCTION_LT,
			KEYWORD_INSTRUCTION_GT,
			KEYWORD_INSTRUCTION_IEQ,
			KEYWORD_INSTRUCTION_INE,
			KEYWORD_INSTRUCTION_ILE,
			KEYWORD_INSTRUCTION_IGE,
			KEYWORD_INSTRUCTION_ILT,
			KEYWORD_INSTRUCTION_IGT,
			KEYWORD_INSTRUCTION_CEP,
			KEYWORD_INSTRUCTION_REP,
			KEYWORD_INSTRUCTION_CCP,
			KEYWORD_INSTRUCTION_RCP,
			KEYWORD_INSTRUCTION_HALT,
			KEYWORD_INSTRUCTION_PUSH,
			KEYWORD_INSTRUCTION_POP,
			KEYWORD_INSTRUCTION_TOSS,
			KEYWORD_INSTRUCTION_MOVD,
			KEYWORD_INSTRUCTION_MOVU,
			KEYWORD_INSTRUCTION_PEEK,
			KEYWORD_INSTRUCTION_CLOCK,

		KEYWORD_DIRECTIVE = token::KEYWORD | (2<<8), // Directives are instructions intended for the assembler itself so that it can emit context sensitive code.
			KEYWORD_DIRECTIVE_EVAL,    // Evaluates an expression of literals into a single literal.
			KEYWORD_DIRECTIVE_SIZE,    // Evaluates the size of a given type.
			KEYWORD_DIRECTIVE_BIN,     // Allows for the dump of data inside the binary.
			KEYWORD_DIRECTIVE_SCOPE,   // Allows for the creation of variables with automatic storage duration.
			KEYWORD_DIRECTIVE_SYNTAX,  // Switch syntax modes (raw is the most verbose, but most efficient).
			KEYWORD_DIRECTIVE_FUNC,    // Declare and define a function.
			KEYWORD_DIRECTIVE_CALL,    // Call a function.
			KEYWORD_DIRECTIVE_RETURN,  // Return from a function.
			KEYWORD_DIRECTIVE_RAW,     // Raw syntax mode. Each token corresponds to either an instruction or a literal/alias.
			KEYWORD_DIRECTIVE_SUGAR,   // Simplified syntax mode (default). Instructions can be repeated by providing several parameters. Less efficient than raw.
			KEYWORD_DIRECTIVE_NOSTACK, // No-stack syntax mode. Gets rid of manual control of the stack. Most instructions take a destination and source parameter. Less efficient than sugar.
			KEYWORD_DIRECTIVE_LIT,     // Defines a literal under a given alias.
			KEYWORD_DIRECTIVE_HERE,    // Emits the absolute address of IP.
			KEYWORD_DIRECTIVE_TOP,     // Emits the top stack value of SP
			KEYWORD_DIRECTIVE_FRAME,   // Emtis the top stack value of the frame.
			KEYWORD_DIRECTIVE_BASE,    // Emits the top stack value of the program stack entry point (the bottom at program start).

		OPERATOR_DIRECTIVE = token::OPERATOR | (3<<8),
			OPERATOR_DIRECTIVE_AT,
			OPERATOR_DIRECTIVE_ADDR,
			OPERATOR_DIRECTIVE_LABEL,
			OPERATOR_DIRECTIVE_DOLLAR,
		
		OPERATOR_ENCLOSE = token::OPERATOR | (4<<8),
			OPERATOR_ENCLOSE_PARENTHESIS = OPERATOR_ENCLOSE | (1<<4),
				OPERATOR_ENCLOSE_PARENTHESIS_L,
				OPERATOR_ENCLOSE_PARENTHESIS_R,
			
			OPERATOR_ENCLOSE_BRACKET = OPERATOR_ENCLOSE | (2<<4),
				OPERATOR_ENCLOSE_BRACKET_L,
				OPERATOR_ENCLOSE_BRACKET_R,
			
			OPERATOR_ENCLOSE_BRACE = OPERATOR_ENCLOSE | (3<<4),
				OPERATOR_ENCLOSE_BRACE_L,
				OPERATOR_ENCLOSE_BRACE_R,
		
		OPERATOR_STOP = token::OPERATOR | (5<<8),
		OPERATOR_COMMA,
		OPERATOR_COLON,

		LITERAL_INT = token::LITERAL | (6<<8)
	};
};

/// @brief Lexes XASM assembly code.
/// @param l The lexer.
/// @return The lexed token.
token xasm_lex(lexer *l);

/// @brief Assembles extended assembly language in the form of input tokens.
/// @param max_tokens The maximum number of tokens that is allowed to be generated. Generally corresponds to the number of elements in the tokens parameter. If the assembler exceeds this limit the assembly fails.
/// @param tokens The input token stream. 
/// @param max_binary_body The maximum number of output words that is allowed to be generated. Generally corresponds to the number of elements in the body parameter. If the assember exceeds this limit the assembly fails.
/// @param body The output binary write target.
/// @return Metadata relating to the output assembly.
/// @sa xlex
xcc_out xasm(U16 max_tokens, const token *tokens, U16 max_binary_body, XWORD *body);

/// @brief Assembles extended assembly language in the form of input tokens.
/// @param l The lexer to be used to read code.
/// @param memory The memory to write to.
/// @return Metadata relating to the output assembly.
/// @sa xlex
xcc_out xasm(lexer l, xcc_binary memory);

#endif