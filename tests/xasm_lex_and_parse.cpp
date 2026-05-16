#include "tests.h"

CC0_UTEST_BEGIN(xasm_lex_and_parse_nop)
{
	const char CODE[] = "nop.";
	const int EXPECT_COUNT = 4;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::NOP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_nop, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit)
{
	const char CODE[] = "put 100.";
	const int EXPECT_COUNT = 5;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(100),
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit_hex)
{
	const char CODE[] = "put 0x100.";
	const int EXPECT_COUNT = 5;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0x100),
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit_hex, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit_oct)
{
	const char CODE[] = "put 0100.";
	const int EXPECT_COUNT = 5;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0100),
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit_oct, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_two_instructions)
{
	const char CODE[] = "put 100. put 200.";
	const int EXPECT_COUNT = 7;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::PUT, U16(200), XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_two_instructions, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit_ref)
{
	const char CODE[] = "put @100.";
	const int EXPECT_COUNT = 6;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::AT, XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit_ref, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit_addr)
{
	const char CODE[] = "put &100.";
	const int EXPECT_COUNT = 0; // We expect this to fail.
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)}).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit_addr, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit_refref)
{
	const char CODE[] = "put @@100.";
	const int EXPECT_COUNT = 7;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::PUT, U16(100), XIS::AT, XIS::AT, XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit_refref, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_lit_refrefaddr)
{
	const char CODE[] = "put @@&100.";
	const int EXPECT_COUNT = 0;
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	U16 n = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)}).binary.size;
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)n);
}
CC0_UTEST_END(xasm_lex_and_parse_put_lit_refrefaddr, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_var)
{
	const char CODE[] = "$scope: var { put var. }";
	const int EXPECT_COUNT = 13;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(1),
		XIS::RLC,
		XIS::AT,
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_var, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_var_addr)
{
	const char CODE[] = "$scope: var { put &var. }";
	const int EXPECT_COUNT = 12;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(1),
		XIS::RLC,
		XIS::PUT, U16(1),
		XIS::POP,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_var_addr, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_var_refrefaddr)
{
	const char CODE[] = "$scope: var { put @@&var. }";
	const int EXPECT_COUNT = 14;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(1),
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_var_refrefaddr, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_put_var_lit)
{
	const char CODE[] = "$scope: var { put var, 100. }";
	const int EXPECT_COUNT = 15;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(1),
		XIS::PUSH,
		XIS::PUT, U16(1),
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_put_var_lit, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_scope_mem)
{
	const char CODE[] = "$scope: [100] {}";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_scope_mem, false)

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_scope_arr)
{
	const char CODE[] = "$scope: a[128] { put a[64]. }";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_scope_arr, false)*/

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_scope_five_params)
{
	const char CODE[] = "$scope: a, [100], b, c[100], d { put a, b, c, d. }";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_scope_five_params, false)*/

CC0_UTEST_BEGIN(xasm_lex_and_parse_add_and_sub)
{
	const char CODE[] = "put 200, 100, 50. add. add. put 75. sub.";
	const int EXPECT_COUNT = 14;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(200),
		XIS::PUT, U16(100),
		XIS::PUT, U16(50),
		XIS::ADD,
		XIS::ADD,
		XIS::PUT, U16(75),
		XIS::SUB,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(out.binary.buffer, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_add_and_sub, false)

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_size)
{
	const char CODE[] = "$scope: array[128] { put $size(array). }";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_size, false)*/

CC0_UTEST_BEGIN(xasm_lex_and_parse_bin)
{
	const char CODE[] = "$bin 128, 64, 32, 16, 8, 4, 2, 1.";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(out.binary.buffer, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_bin, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_decl_lit)
{
	const char CODE[] = "$lit CONST, 123. put CONST.";
	const int EXPECT_COUNT = 5;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB, XIS::PUT, U16(123), XIS::LDB, XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_decl_lit, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_set_one)
{
	const char CODE[] = "$scope: a { set a, 12. }";
	const int EXPECT_COUNT = 15;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,

		XIS::PUT, U16(1),
		XIS::PUSH,

		XIS::PUT, U16(1),
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_set_one, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_set_two)
{
	const char CODE[] = "$scope: a, b { set a, 12. set b, a. }";
	const int EXPECT_COUNT = 23;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,

		XIS::PUT, U16(2),
		XIS::PUSH,

		XIS::PUT, U16(1),
		XIS::RLC,
		XIS::PUT, U16(12),
		XIS::MOVD,

		XIS::PUT, U16(2),
		XIS::RLC,
		XIS::PUT, U16(1),
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_set_two, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_here)
{
	const char CODE[] = "put $here.";
	const int EXPECT_COUNT = 4;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTI,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_here, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_here_index)
{
	const char CODE[] = "put $here[3].";
	const int EXPECT_COUNT = 8;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTI,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::AT,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_here_index, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_top)
{
	const char CODE[] = "put $top.";
	const int EXPECT_COUNT = 4;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTS,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_top, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_top_index)
{
	const char CODE[] = "put $top[3].";
	const int EXPECT_COUNT = 8;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUTS,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::AT,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_top_index, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_frame_index)
{
	const char CODE[] = "put $frame[3].";
	const int EXPECT_COUNT = 10;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0),
		XIS::RLC,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::AT,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_frame_index, false)

CC0_UTEST_BEGIN(xasm_lex_and_parse_bottom_index)
{
	const char CODE[] = "put $base[3].";
	const int EXPECT_COUNT = 10;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0),
		XIS::RLB,
		XIS::PUT, U16(3),
		XIS::ADD,
		XIS::AT,
		XIS::LDB,
		XIS::HALT
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_bottom_index, false)

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_label)
{
	const char CODE[] = "%label:";
	const int EXPECT_COUNT = 3;
	const U16 EXPECT[EXPECT_COUNT] = { XIS::SVB, XIS::LDB, XIS::HALT };
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_label, false)*/

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_label_param)
{
	const char CODE[] = "jmp %label.";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_label_param, false)*/

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_label_jmp)
{
	const char CODE[] = "put 0. %label: jmp %label.";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_label_jmp, false)*/

/*CC0_UTEST_BEGIN(xasm_lex_and_parse_fwd_label)
{
	const char CODE[] = " \
		jmp %end_get_pixel. \
		%end_get_pixel: \
	";
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
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary, sizeof(binary) / sizeof(XWORD)});
	print_err(out);
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xasm_lex_and_parse_fwd_label, false)*/
