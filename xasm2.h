#ifndef XASM2_H
#define XASM2_H

#include <list>

#include "xbin.h"
#include "xsrc.h"
#include "lib/parsec/lex.h"

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

// $here -> instruction pointer
// $top -> stack pointer
// $frame -> call point stack pointer
// $prog -> program point stack pointer

struct xtoken
{
	enum tokentype
	{
		KEYWORD_INSTRUCTION = token::KEYWORD | (1<<8),
			KEYWORD_INSTRUCTION_NOP,
			KEYWORD_INSTRUCTION_PUT,
			KEYWORD_INSTRUCTION_EREL,
			KEYWORD_INSTRUCTION_CREL,
			KEYWORD_INSTRUCTION_AT,
			KEYWORD_INSTRUCTION_JMP,
			KEYWORD_INSTRUCTION_SKIP,
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
			KEYWORD_INSTRUCTION_DO,
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

		KEYWORD_DIRECTIVE = token::KEYWORD | (2<<8),
			KEYWORD_DIRECTIVE_EVAL,
			KEYWORD_DIRECTIVE_SCOPE,
			KEYWORD_DIRECTIVE_HERE,
			KEYWORD_DIRECTIVE_LIT,

		ALIAS_VAR = token::ALIAS | (1<<8),
		ALIAS_FUNC = token::ALIAS | (2<<8),
		//ALIAS_TYPE, // the name of the type, not the instantiated variable (the instantiated variable would be an ALIAS_VAR)

		OPERATOR_ARITHMETIC = token::OPERATOR | (1<<8),
			OPERATOR_ARITHMETIC_ADD,
			OPERATOR_ARITHMETIC_SUB,
			OPERATOR_ARITHMETIC_MUL,
			OPERATOR_ARITHMETIC_DIV,
			OPERATOR_ARITHMETIC_MOD,

		OPERATOR_DIRECTIVE = token::OPERATOR | (2<<8),
			OPERATOR_DIRECTIVE_AT,
			OPERATOR_DIRECTIVE_ADDR,
			OPERATOR_DIRECTIVE_DOLLAR,
		
		OPERATOR_ENCLOSE = token::OPERATOR | (3<<8),
			OPERATOR_ENCLOSE_PARENTHESIS = OPERATOR_ENCLOSE | (1<<4),
				OPERATOR_ENCLOSE_PARENTHESIS_L,
				OPERATOR_ENCLOSE_PARENTHESIS_R,
			
			OPERATOR_ENCLOSE_BRACKET = OPERATOR_ENCLOSE | (2<<4),
				OPERATOR_ENCLOSE_BRACKET_L,
				OPERATOR_ENCLOSE_BRACKET_R,
			
			OPERATOR_ENCLOSE_BRACE = OPERATOR_ENCLOSE | (3<<4),
				OPERATOR_ENCLOSE_BRACE_L,
				OPERATOR_ENCLOSE_BRACE_R,
		
		OPERATOR_STOP = token::OPERATOR | 1,
		OPERATOR_COMMA,
		OPERATOR_COLON,

		LITERAL_INT = token::LITERAL | (1<<8)
	};
};

/// @brief Lexes XASM code.
/// @param l The lexer.
/// @return The lexed token.
token xlex(lexer *l);

// assemble_xasm
// Assembles extended assembly language. Lots of syntactic sugar.
U16 assemble_xasm(U16 max_tokens, const token *tokens, U16 max_binary_body, XWORD *body);

#endif