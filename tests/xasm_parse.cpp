/*#include "tests.h"

CC0_UTEST_BEGIN(xasm_parse_nop)
{
	const int TOKEN_COUNT = 3;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("nop", 3, xtoken::KEYWORD_INSTRUCTION_NOP),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 4;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::NOP, XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_nop, false)

CC0_UTEST_BEGIN(xasm_parse_put_lit)
{
	const int TOKEN_COUNT = 4;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 5;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_lit, false)

CC0_UTEST_BEGIN(xasm_parse_two_instructions)
{
	const int TOKEN_COUNT = 7;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_literal ("200", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 7;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::PUT, U16(200), XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_two_instructions, false)

CC0_UTEST_BEGIN(xasm_parse_put_lit_ref)
{
	const int TOKEN_COUNT = 5;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("@",   1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 6;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::AT, XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_lit_ref, false)

CC0_UTEST_BEGIN(xasm_parse_put_lit_addr)
{
	const int TOKEN_COUNT = 5;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("&",   1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 0; // We expect this to fail.
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
}
CC0_UTEST_END(xasm_parse_put_lit_addr, false)

CC0_UTEST_BEGIN(xasm_parse_put_lit_refref)
{
	const int TOKEN_COUNT = 7;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("@",   1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_operator("@",   1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 7;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::AT, XIS::AT, XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_lit_refref, false)

CC0_UTEST_BEGIN(xasm_parse_put_lit_refrefaddr)
{
	const int TOKEN_COUNT = 7;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("@",   1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_operator("@",   1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_operator("&",   1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 0;
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
}
CC0_UTEST_END(xasm_parse_put_lit_refrefaddr, false)

CC0_UTEST_BEGIN(xasm_parse_put_var)
{
	const int TOKEN_COUNT = 10;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("var",   3,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_alias   ("var",   3,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 13;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_var, false)

CC0_UTEST_BEGIN(xasm_parse_put_var_addr)
{
	const int TOKEN_COUNT = 11;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("var",   3,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("&",     1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_alias   ("var",   3,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 12;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_var_addr, false)

CC0_UTEST_BEGIN(xasm_parse_put_var_refrefaddr)
{
	const int TOKEN_COUNT = 13;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",      1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope",  5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",      1, xtoken::OPERATOR_COLON),
		new_alias   ("var",    3,  token::ALIAS),
		new_operator("{",      1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("put",    3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("@",      1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_operator("@",      1, xtoken::OPERATOR_DIRECTIVE_AT),
		new_operator("&",      1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_alias   ("var",    3,  token::ALIAS),
		new_operator(".",      1, xtoken::OPERATOR_STOP),
		new_operator("}",      1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 14;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::AT,
		XIS::AT,
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_var_refrefaddr, false)

CC0_UTEST_BEGIN(xasm_parse_put_var_lit)
{
	const int TOKEN_COUNT = 12;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("var",   3,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_alias   ("var",   3,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_literal ("100",   3, xtoken::LITERAL_INT),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 15;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(100),
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_put_var_lit, false)

CC0_UTEST_BEGIN(xasm_parse_scope_mem)
{
	const int TOKEN_COUNT = 9;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("100",   3, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 9;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(100),
		XIS::PUSH,
		XIS::PUT, U16(100),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_scope_mem, false)

CC0_UTEST_BEGIN(xasm_parse_scope_arr)
{
	const int TOKEN_COUNT = 16;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("128",   3, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("64",    2, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 13;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(128),
		XIS::PUSH,
		XIS::PUT, U16(64),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(128),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_scope_arr, false)

CC0_UTEST_BEGIN(xasm_parse_scope_five_params)
{
	const int TOKEN_COUNT = 29;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("100",   3, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("b",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("c",     1,  token::ALIAS),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("100",   3, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("d",     1,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword("put",    3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("b",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("c",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("d",     1,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 25;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(203),
		XIS::PUSH,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(101),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(102),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(202),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(203),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_scope_five_params, false)

CC0_UTEST_BEGIN(xasm_parse_add_and_sub)
{
	const int TOKEN_COUNT = 12;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_literal ("200", 3, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		
		new_keyword ("add", 3, xtoken::KEYWORD_INSTRUCTION_ADD),
		new_literal ("100", 3, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("50",  2, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		
		new_keyword ("sub", 3, xtoken::KEYWORD_INSTRUCTION_SUB),
		new_literal ("75",  2, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 14;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(200),
		XIS::PUT, U16(100),
		XIS::ADD,
		XIS::PUT, U16(50),
		XIS::ADD,
		XIS::PUT, U16(75),
		XIS::SUB,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_add_and_sub, false)

CC0_UTEST_BEGIN(xasm_parse_size)
{
	const int TOKEN_COUNT = 17;
	const token TOKENS[TOKEN_COUNT] = {

		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("array", 5,  token::ALIAS),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("128",   3, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",     2, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("size",  4, xtoken::KEYWORD_DIRECTIVE_SIZE),
		new_operator("(",     1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
		new_alias   ("array", 5,  token::ALIAS),
		new_operator(")",     1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		
		new_eof     ()
	};
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(128),
		XIS::PUSH,
		XIS::PUT, U16(128),
		XIS::PUT, U16(128),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_size, false)

CC0_UTEST_BEGIN(xasm_parse_bin)
{
	const int TOKEN_COUNT = 19;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",   1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("bin", 3, xtoken::KEYWORD_DIRECTIVE_BIN),
		new_literal ("128", 3, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("64",  2, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("32",  2, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("16",  2, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("8",   1, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("4",   1, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("2",   1, xtoken::LITERAL_INT),
		new_operator(",",   1, xtoken::OPERATOR_COMMA),
		new_literal ("1",   1, xtoken::LITERAL_INT),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 14;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(8),
		XIS::SKIP,
		U16(128),
		U16(64),
		U16(32),
		U16(16),
		U16(8),
		U16(4),
		U16(2),
		U16(1),
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_bin, false)

CC0_UTEST_BEGIN(xasm_parse_decl_lit)
{
	const int TOKEN_COUNT = 10;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("lit",   3, xtoken::KEYWORD_DIRECTIVE_LIT),
		new_alias   ("CONST", 5,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_literal ("123",   3, xtoken::LITERAL_INT),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_alias   ("CONST", 5,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 5;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB, XIS::PUT, U16(123), XIS::LDB, XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_decl_lit, false)

CC0_UTEST_BEGIN(xasm_parse_set_one)
{
	const int TOKEN_COUNT = 13;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("set",   3, xtoken::KEYWORD_INSTRUCTION_SET),
		new_operator("&",     1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_literal ("12",    2, xtoken::LITERAL_INT),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 15;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,

		XIS::PUT, U16(1),
		XIS::PUSH,

		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::PUT, U16(12),
		XIS::MOVD,

		XIS::PUT, U16(1),
		XIS::POP,

		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_set_one, false)

CC0_UTEST_BEGIN(xasm_parse_set_two)
{
	const int TOKEN_COUNT = 21;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("b",     1,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_keyword ("set",   3, xtoken::KEYWORD_INSTRUCTION_SET),
		new_operator("&",     1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_literal ("12",    2, xtoken::LITERAL_INT),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_keyword ("set",   3, xtoken::KEYWORD_INSTRUCTION_SET),
		new_operator("&",     1, xtoken::OPERATOR_DIRECTIVE_ADDR),
		new_alias   ("b",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof     ()
	};
	const int EXPECT_COUNT = 23;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,

		XIS::PUT, U16(2),
		XIS::PUSH,

		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::PUT, U16(12),
		XIS::MOVD,

		XIS::PUT, U16(1),
		XIS::RLC,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::AT,
		XIS::MOVD,

		XIS::PUT, U16(2),
		XIS::POP,

		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_set_two, false)

CC0_UTEST_BEGIN(xasm_parse_here)
{
	const int TOKEN_COUNT = 5;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put",  3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",    1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("here", 4, xtoken::KEYWORD_DIRECTIVE_HERE),
		new_operator(".",    1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 4;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB, XIS::PUTI, XIS::LDB, XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_here, false)

CC0_UTEST_BEGIN(xasm_parse_here_index)
{
	const int TOKEN_COUNT = 8;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put",  3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",    1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("here", 4, xtoken::KEYWORD_DIRECTIVE_HERE),
		new_operator("[",    1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("3",    1, xtoken::LITERAL_INT),
		new_operator("]",    1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(".",    1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 7;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTI,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_here_index, false)

CC0_UTEST_BEGIN(xasm_parse_top)
{
	const int TOKEN_COUNT = 5;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",   1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("top", 3, xtoken::KEYWORD_DIRECTIVE_TOP),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 4;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTS,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_top, false)

CC0_UTEST_BEGIN(xasm_parse_top_index)
{
	const int TOKEN_COUNT = 8;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put", 3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",   1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("top", 3, xtoken::KEYWORD_DIRECTIVE_TOP),
		new_operator("[",   1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("3",   1, xtoken::LITERAL_INT),
		new_operator("]",   1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(".",   1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 7;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTS,
		XIS::PUT, U16(3),
		XIS::SUB,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_top_index, false)

CC0_UTEST_BEGIN(xasm_parse_frame_index)
{
	const int TOKEN_COUNT = 8;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("frame", 5, xtoken::KEYWORD_DIRECTIVE_FRAME),
		new_operator("[",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("3",     1, xtoken::LITERAL_INT),
		new_operator("]",     1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 9;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_frame_index, false)

CC0_UTEST_BEGIN(xasm_parse_bottom_index)
{
	const int TOKEN_COUNT = 8;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put",    3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_operator("$",      1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("base",   4, xtoken::KEYWORD_DIRECTIVE_BASE),
		new_operator("[",      1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
		new_literal ("3",      1, xtoken::LITERAL_INT),
		new_operator("]",      1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
		new_operator(".",      1, xtoken::OPERATOR_STOP),
		new_eof     ()
	};
	const int EXPECT_COUNT = 9;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0),
		XIS::RLB,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_bottom_index, false)

CC0_UTEST_BEGIN(xasm_parse_label_param)
{
	const int TOKEN_COUNT = 5;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("jmp",   3, xtoken::KEYWORD_INSTRUCTION_JMP),
		new_operator("%",     1, xtoken::OPERATOR_DIRECTIVE_LABEL),
		new_alias   ("label", 5,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_eof()
	};
	const int EXPECT_COUNT = 6;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0),
		XIS::JMP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_label_param, false)

CC0_UTEST_BEGIN(xasm_parse_label_jmp)
{
	const int TOKEN_COUNT = 11;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("put",   3, xtoken::KEYWORD_INSTRUCTION_PUT),
		new_literal ("0",     1, xtoken::LITERAL_INT),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("%",     1, xtoken::OPERATOR_DIRECTIVE_LABEL),
		new_alias   ("label", 5,  token::ALIAS),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_keyword ("jmp",   3, xtoken::KEYWORD_INSTRUCTION_JMP),
		new_operator("%",     1, xtoken::OPERATOR_DIRECTIVE_LABEL),
		new_alias   ("label", 5,  token::ALIAS),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_eof()
	};
	const int EXPECT_COUNT = 8;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0),
		XIS::PUT, U16(3),
		XIS::JMP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_label_jmp, false)

CC0_UTEST_BEGIN(xasm_parse_collapse_scope_with_lit)
{
	const int TOKEN_COUNT = 16;
	const token TOKENS[TOKEN_COUNT] = {
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("scope", 5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_alias   ("a",     1,  token::ALIAS),
		new_operator("{",     1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
		new_operator("%",     1, xtoken::OPERATOR_DIRECTIVE_LABEL),
		new_alias   ("lbl",   3,  token::ALIAS),
		new_operator(":",     1, xtoken::OPERATOR_COLON),
		new_operator("$",     1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
		new_keyword ("lit",   3, xtoken::KEYWORD_DIRECTIVE_LIT),
		new_alias   ("L",     1,  token::ALIAS),
		new_operator(",",     1, xtoken::OPERATOR_COMMA),
		new_literal ("10",    2, xtoken::LITERAL_INT),
		new_operator(".",     1, xtoken::OPERATOR_STOP),
		new_operator("}",     1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
		new_eof()
	};
	const int EXPECT_COUNT = 9;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_collapse_scope_with_lit, false)

CC0_UTEST_BEGIN(xasm_parse_fwd_label)
{
	const int TOKEN_COUNT = 8;
	const token TOKENS[TOKEN_COUNT] = {
		new_keyword ("jmp",            3, xtoken::KEYWORD_INSTRUCTION_JMP),
		new_operator("%",              1, xtoken::OPERATOR_DIRECTIVE_LABEL),
		new_alias   ("end_get_pixel", 13,  token::ALIAS),
		new_operator(".",              1, xtoken::OPERATOR_STOP),
		new_operator("%",              1, xtoken::OPERATOR_DIRECTIVE_LABEL),
		new_alias   ("end_get_pixel", 13,  token::ALIAS),
		new_operator(":",              1, xtoken::OPERATOR_COLON),
		new_eof()
	};
	const int EXPECT_COUNT = 6;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(4),
		XIS::JMP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(TOKEN_COUNT, TOKENS, sizeof(binary) / sizeof(XWORD), binary).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_parse_fwd_label, false)*/
