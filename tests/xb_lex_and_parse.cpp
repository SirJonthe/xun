#include "tests.h"

CC0_UTEST_BEGIN(xb_lex_and_parse)
{}
CC0_UTEST_END(xb_lex_and_parse, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_var_decl)
{
	const char CODE[] = "auto x;";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(0),           // The global 'x' variable.
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_var_decl, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_var)
{
	const char CODE[] = "auto x = 123;";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(123),         // The global 'x' variable.
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_var, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_var_hex)
{
	const char CODE[] = "auto x = 0x123;";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(0x123),       // The global 'x' variable.
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_var_hex, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_var_oct)
{
	const char CODE[] = "auto x = 0123;";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(0123),        // The global 'x' variable.
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_var_oct, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_vars)
{
	const char CODE[] = "auto x = 123, y, z[512];";
	const int EXPECT_COUNT = 19;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                      // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,    // 'main' function pointer storage.
		XIS::PUT, U16(123),            // The global 'x' variable.
		XIS::PUT, U16(0),              // The global 'y' variable.
		XIS::PUT, U16(5), XIS::RLB,    // The address pointer to the 'z' array.
		XIS::PUT, U16(512), XIS::PUSH, // The global 'z' array (empty).
		XIS::PUT, U16(516), XIS::POP,  // Remove 'main', 'x', 'y', and 'z'.
		XIS::LDB,                      // Load the B stack state.
		XIS::HALT                      // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_vars, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_arr)
{
	const char CODE[] = "auto x[2] = { 123, 321 };";
	const int EXPECT_COUNT = 16;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB, // The address pointer to the 'x' array.
		XIS::PUT, U16(123),         // The global 'x' variable.
		XIS::PUT, U16(321),         // The global 'x' variable.
		XIS::PUT, U16(4), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_counted_arr)
{
	const char CODE[] = "auto x[] = { 123, 321 };";
	const int EXPECT_COUNT = 16;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB, // The address pointer to the 'x' array.
		XIS::PUT, U16(123),         // The global 'x' variable.
		XIS::PUT, U16(321),         // The global 'x' variable.
		XIS::PUT, U16(4), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_counted_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_explicit_zero_arr)
{
	const char CODE[] = "auto x[0];";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, ==, 1);
	CC0_UTEST_ASSERT(out.error.code, ==, xcc_error::ZERO);
}
CC0_UTEST_END(xb_lex_and_parse_explicit_zero_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_implicit_zero_arr)
{
	const char CODE[] = "auto x[] = {};";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, ==, 1);
	CC0_UTEST_ASSERT(out.error.code, ==, xcc_error::ZERO);
}
CC0_UTEST_END(xb_lex_and_parse_implicit_zero_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_explicit_zero_static_arr)
{
	const char CODE[] = "static x[0] = {};";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, ==, 1);
	CC0_UTEST_ASSERT(out.error.code, ==, xcc_error::ZERO);
}
CC0_UTEST_END(xb_lex_and_parse_explicit_zero_static_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_implicit_zero_static_arr)
{
	const char CODE[] = "static x[] = {};";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, ==, 1);
	CC0_UTEST_ASSERT(out.error.code, ==, xcc_error::ZERO);
}
CC0_UTEST_END(xb_lex_and_parse_implicit_zero_static_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_sizeof)
{
	const char CODE[] =
		"auto a = 0; \n\
		auto b[2] = { 0, 0 }; \n\
		auto c[] = { 0, 0, 0 }; \n\
		auto d[3] = \"asd\"; \n\
		auto e[] = \"asdf\"; \n\
		auto A = sizeof(a); \n\
		auto B = sizeof(b); \n\
		auto C = sizeof(c); \n\
		auto D = sizeof(d); \n\
		auto E = sizeof(e);";
	const int EXPECT_COUNT = 61;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                    // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,  // 'main' function pointer storage.
		
		XIS::PUT, U16(0),            // The global 'a' variable.

		XIS::PUT, U16(4), XIS::RLB,  // The address pointer to the 'b' array.
		XIS::PUT, U16(0),            // The global 'b' variable.
		XIS::PUT, U16(0),            // The global 'b' variable.
		
		XIS::PUT, U16(7), XIS::RLB,  // The address pointer to the 'c' array.
		XIS::PUT, U16(0),            // The global 'c' variable.
		XIS::PUT, U16(0),            // The global 'c' variable.
		XIS::PUT, U16(0),            // The global 'c' variable.
		
		XIS::PUT, U16(11), XIS::RLB, // The address pointer to the 'd' array.
		XIS::PUT, U16('a'),          // The global 'd' variable.
		XIS::PUT, U16('s'),          // The global 'd' variable.
		XIS::PUT, U16('d'),          // The global 'd' variable.
		XIS::PUT, U16(0),            // The global 'd' variable.
		
		XIS::PUT, U16(16), XIS::RLB, // The address pointer to the 'e' array.
		XIS::PUT, U16('a'),          // The global 'e' variable.
		XIS::PUT, U16('s'),          // The global 'e' variable.
		XIS::PUT, U16('d'),          // The global 'e' variable.
		XIS::PUT, U16('f'),          // The global 'e' variable.
		XIS::PUT, U16(0),            // The global 'e' variable.
		
		XIS::PUT, U16(1),            // Size of 'a'.
		XIS::PUT, U16(2),            // Size of 'b'.
		XIS::PUT, U16(3),            // Size of 'c'.
		XIS::PUT, U16(4),            // Size of 'd'.
		XIS::PUT, U16(5),            // Size of 'e'.
		
		XIS::PUT, U16(25), XIS::POP, // Remove 'main' (1), 'a' (1), 'b' (1 + 2), 'c' (1 + 3), 'd' (1 + 4), 'e' (1 + 5), 'A' (1), 'B' (1), 'C' (1), 'D' (1), 'E' (1)
		XIS::LDB,                    // Load the B stack state.
		XIS::HALT                    // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_sizeof, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn)
{
	const char CODE[] = "fn() {}";
	const int EXPECT_COUNT = 19;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(14), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_fn, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_decl)
{
	const char CODE[] = "fn();";
	const int EXPECT_COUNT = 12;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(0), XIS::RLA, // 'fn' function pointer storage.
		XIS::PUT, U16(2), XIS::POP, // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_fn_decl, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_decl_and_def)
{
	const char CODE[] = "fn(); fn() {}";
	const int EXPECT_COUNT = 19;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(14), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_fn_decl_and_def, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_local_var)
{
	const char CODE[] = "fn() { auto x = 123; }";
	const int EXPECT_COUNT = 24;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(19), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(123),                    // Put and save local variable with value 123 on stack.
		XIS::PUT, U16(1), XIS::POP,            // Remove local variable from stack.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_local_var, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_local_arr)
{
	const char CODE[] = "fn() { auto x[2] = { 123, 321 }; }";
	const int EXPECT_COUNT = 29;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(24), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(2), XIS::RLC,            // The address pointer to the 'x' array.
		XIS::PUT, U16(123),                    // Put and save local variable with value 123 on stack.
		XIS::PUT, U16(321),                    // Put and save local variable with value 321 on stack.
		XIS::PUT, U16(3), XIS::POP,            // Remove local variable from stack.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	// 16514
	// 384 0 17696
	// 384 11 17696
	// 384 27 17696 1856
	// 16770
	// 384 2 18208
	// 384 123
	// 384 321
	// 384 4 11392
	// 384 1 11392 17602 1856 384 2 11392 17346 12544
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_local_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_main)
{
	const char CODE[] = "main(argc, argv) {}";
	const int EXPECT_COUNT = 35;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                               // Save the B stack state.
		XIS::PUT, U16(8), XIS::RLA,             // 'main' function pointer storage.
		XIS::PUT, U16(14), XIS::RLA, XIS::JMP,  // Jump over 'main' body.
		XIS::SVC,                               // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,             // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(0),                       // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(13), XIS::ADD, // Save return address with offset to return after function call.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(1), XIS::RLB, XIS::AT,    // Put 'main' address on stack.
		XIS::JMP,                               // Jump to 'main'.
		XIS::TOSS,                              // Discard the return value upon returning.
		XIS::PUT, U16(1), XIS::POP,             // Pop 'main' pointer from stack.
		XIS::LDB,                               // Load the B stack state.
		XIS::HALT                               // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_main, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_call_fn)
{
	const char CODE[] = "fn() { fn(); }";
	const int EXPECT_COUNT = 31;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(26), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(0),                      // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(9), XIS::ADD, // Save return address with offset to return after function call.
		XIS::PUT, U16(2), XIS::RLB, XIS::AT,   // Put 'fn' address on stack.
		XIS::JMP,                              // Jump to 'fn'.
		XIS::TOSS,                             // Discard the return value upon returning.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_call_fn, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_params)
{
	const char CODE[] = "fn(a, b) {}";
	const int EXPECT_COUNT = 22;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(17), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,            // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_fn_params, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_decl_and_def_params)
{
	const char CODE[] = "fn(a, b); fn(a, b) {}";
	const int EXPECT_COUNT = 22;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(17), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,            // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_fn_decl_and_def_params, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_param_mismatch)
{
	const char CODE[] = "fn(a, b); fn a) {}";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
}
CC0_UTEST_END(xb_lex_and_parse_fn_param_mismatch, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_params_call)
{
	const char CODE[] = "fn(a, b) { fn(0xfefe, 0xfeef); }";
	const int EXPECT_COUNT = 38;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                               // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,             // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,            // 'fn' function pointer storage.
		XIS::PUT, U16(33), XIS::RLA, XIS::JMP,  // Jump over 'fn' body.
		XIS::SVC,                               // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,             // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT, U16(0),                       // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(13), XIS::ADD, // Save return address with offset to return after function call.
		XIS::PUT, U16(0xfefe),                  // Put parameter value on stack.
		XIS::PUT, U16(0xfeef),                  // Put parameter value on stack.
		XIS::PUT, U16(2), XIS::RLB, XIS::AT,    // Put 'fn' address on stack.
		XIS::JMP,                               // Jump to 'fn'.
		XIS::TOSS,                              // Discard the return value upon returning.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,             // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                               // Load the B stack state.
		XIS::HALT                               // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_fn_params_call, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_fn_syntax_err)
{
	const char CODE1[] = "fn(a,) {}";
	const char CODE2[] = "fn(, b) {}";
	const char CODE3[] = "fn(a,);";
	const char CODE4[] = "fn(, b);";
	const char CODE5[] = "fn(a) { fn(,a); }";
	const char CODE6[] = "fn(a) { fn(a,); }";
	const char CODE7[] = "fn(a, b); fn(a) {}";
	const char CODE8[] = "fn(a, b) { fn(1); }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out;
	out = xb(init_lexer(chars::view{CODE1, sizeof(CODE1), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE2, sizeof(CODE2), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE3, sizeof(CODE3), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE4, sizeof(CODE4), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE5, sizeof(CODE5), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE6, sizeof(CODE6), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE7, sizeof(CODE7), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
	out = xb(init_lexer(chars::view{CODE8, sizeof(CODE8), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, >, 0);
}
CC0_UTEST_END(xb_lex_and_parse_fn_syntax_err, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_return)
{
	const char CODE[] = "fn() { return 0xfefe; }";
	const int EXPECT_COUNT = 31;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                                        // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,                                      // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,                                     // 'fn' function pointer storage.
		XIS::PUT, U16(26), XIS::RLA, XIS::JMP,                           // Jump over 'fn' body.
		XIS::SVC,                                                        // Save C stack state.
		XIS::PUT, U16(0), XIS::RLC, XIS::AT, XIS::PUT, U16(1), XIS::SUB, // Get the address of the external return value.
		XIS::PUT, U16(0xfefe),                                           // Put the local return value on top of stack.
		XIS::MOVD,                                                       // Move the top value of the stack to the external return value address.
		XIS::LDC,                                                        // Load C stack state     (for explicit return).
		XIS::JMP,                                                        // Jump back to call site (for explicit return).
		XIS::LDC,                                                        // Load C stack state     (for default return).
		XIS::JMP,                                                        // Jump back to call site (for default return).
		XIS::PUT, U16(2), XIS::POP,                                      // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                                                        // Load the B stack state.
		XIS::HALT                                                        // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_return, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_array_ptr)
{
	const char CODE[] = "auto x[1] = { 0xfefe }; auto a = &x; auto b = x; auto c = *x; auto d = x[0];";
	const int EXPECT_COUNT = 34;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                                                 // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,                                               // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,                                               // 'x' pointer storage.
		XIS::PUT, U16(0xfefe),                                                    // 'x' array storage.
		XIS::PUT, U16(2), XIS::RLB,                                               // auto a = &x
		XIS::PUT, U16(2), XIS::RLB, XIS::AT,                                      // auto b = x
		XIS::PUT, U16(2), XIS::RLB, XIS::AT, XIS::AT,                             // auto c = *x
		XIS::PUT, U16(2), XIS::RLB, XIS::AT, XIS::PUT, U16(0), XIS::ADD, XIS::AT, // auto d = x[0]
		XIS::PUT, U16(7), XIS::POP,                                               // Pop 'main', 'x' (pointer and array), 'a', 'b', 'c', and 'd' from stack.
		XIS::LDB,                                                                 // Load the B stack state.
		XIS::HALT                                                                 // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_array_ptr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_lit_operator_order)
{
	const char CODE[] = "auto x[10 - 2 * 3 ^ 4 + 5 & 6 | 7 / 2 % 3];";
	const int EXPECT_COUNT = 15;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                                          // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,                                        // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,                                        // The address pointer to the 'x' array.
		XIS::PUT, U16(10 - 2 * 3 ^ 4 + 5 & 6 | 7 / 2 % 3), XIS::PUSH,      // The global 'x' array and pointer.
		XIS::PUT, U16((10 - 2 * 3 ^ 4 + 5 & 6 | 7 / 2 % 3) + 2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                                                          // Load the B stack state.
		XIS::HALT                                                          // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_lit_operator_order, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_char)
{
	const char CODE[] = "auto x = 'x';";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16('x'),         // The global 'x' variable.
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_char, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_str)
{
	const char CODE[] = "auto x[13] = \"Hello, World!\";";
	const int EXPECT_COUNT = 40;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                    // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,  // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,  // The address pointer to the 'x' array.
		XIS::PUT, U16('H'),          // The global 'x' variable.
		XIS::PUT, U16('e'),          // The global 'x' variable.
		XIS::PUT, U16('l'),          // The global 'x' variable.
		XIS::PUT, U16('l'),          // The global 'x' variable.
		XIS::PUT, U16('o'),          // The global 'x' variable.
		XIS::PUT, U16(','),          // The global 'x' variable.
		XIS::PUT, U16(' '),          // The global 'x' variable.
		XIS::PUT, U16('W'),          // The global 'x' variable.
		XIS::PUT, U16('o'),          // The global 'x' variable.
		XIS::PUT, U16('r'),          // The global 'x' variable.
		XIS::PUT, U16('l'),          // The global 'x' variable.
		XIS::PUT, U16('d'),          // The global 'x' variable.
		XIS::PUT, U16('!'),          // The global 'x' variable.
		XIS::PUT, U16(0),            // The global 'x' variable.
		XIS::PUT, U16(16), XIS::POP, // Remove 'main' (1) and 'x' (1 + 13 + 1).
		XIS::LDB,                    // Load the B stack state.
		XIS::HALT                    // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_str, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_global_counted_str)
{
	const char CODE[] = "auto x[] = \"Hello, World!\";";
	const int EXPECT_COUNT = 40;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                    // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,  // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,  // The address pointer to the 'x' array.
		XIS::PUT, U16('H'),          // The global 'x' variable.
		XIS::PUT, U16('e'),          // The global 'x' variable.
		XIS::PUT, U16('l'),          // The global 'x' variable.
		XIS::PUT, U16('l'),          // The global 'x' variable.
		XIS::PUT, U16('o'),          // The global 'x' variable.
		XIS::PUT, U16(','),          // The global 'x' variable.
		XIS::PUT, U16(' '),          // The global 'x' variable.
		XIS::PUT, U16('W'),          // The global 'x' variable.
		XIS::PUT, U16('o'),          // The global 'x' variable.
		XIS::PUT, U16('r'),          // The global 'x' variable.
		XIS::PUT, U16('l'),          // The global 'x' variable.
		XIS::PUT, U16('d'),          // The global 'x' variable.
		XIS::PUT, U16('!'),          // The global 'x' variable.
		XIS::PUT, U16(0),            // The global 'x' variable.
		XIS::PUT, U16(16), XIS::POP, // Remove 'main' (1) and 'x' (1 + 13 + 1).
		XIS::LDB,                    // Load the B stack state.
		XIS::HALT                    // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_global_counted_str, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_comments)
{
	const char CODE[] = "\
		// comment here\n \
		auto x[1] = { // comment here\n \
			// comment here\n \
			0xfefe // comment here\n \
			// comment here\n \
		}; // comment here\n \
		// comment here\n \
		auto a = &x; // comment here\n \
		// comment here\n \
		auto b = x; // comment here\n \
		// comment here\n \
		auto c = *x; // comment here\n \
		// comment here\n \
		auto d = x[0]; // comment here\n \
	";
	const int EXPECT_COUNT = 34;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                                                 // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,                                               // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,                                               // 'x' pointer storage.
		XIS::PUT, U16(0xfefe),                                                    // 'x' array storage.
		XIS::PUT, U16(2), XIS::RLB,                                               // auto a = &x
		XIS::PUT, U16(2), XIS::RLB, XIS::AT,                                      // auto b = x
		XIS::PUT, U16(2), XIS::RLB, XIS::AT, XIS::AT,                             // auto c = *x
		XIS::PUT, U16(2), XIS::RLB, XIS::AT, XIS::PUT, U16(0), XIS::ADD, XIS::AT, // auto d = x[0]
		XIS::PUT, U16(7), XIS::POP,                                               // Pop 'main', 'x' (pointer and array), 'a', 'b', 'c', and 'd' from stack.
		XIS::LDB,                                                                 // Load the B stack state.
		XIS::HALT                                                                 // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_comments, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_empty_comment)
{
	const char CODE[] = "\
		auto x = 123; \n \
		// \n \
		auto y = 321; \n \
	";
	const int EXPECT_COUNT = 13;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(123),         // The global 'x' variable.
		XIS::PUT, U16(321),         // The global 'y' variable.
		XIS::PUT, U16(3), XIS::POP, // Remove 'main', 'x', and 'y'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_empty_comment, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_inline_xasm)
{
	const char CODE[] = "fn() { asm { nop. nop. nop. nop. } }";
	const int EXPECT_COUNT = 23;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                               // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,             // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,            // 'fn' function pointer storage.
		XIS::PUT, U16(18), XIS::RLA, XIS::JMP,  // Jump over 'fn' body.
		XIS::SVC,                               // Save C stack state.
		XIS::NOP, XIS::NOP, XIS::NOP, XIS::NOP, // Inline assembly.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,             // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                               // Load the B stack state.
		XIS::HALT                               // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_inline_xasm, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_laddr)
{
	const char CODE[] = "auto a = 0x00fe; auto b = &a; fn() { *b = 0xfefe; }";
	const int EXPECT_COUNT = 31;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(0x00fe),                 // Put 0x00fe on stack and store it as value for 'a'.
		XIS::PUT, U16(2), XIS::RLB,            // Put the address of 'a' on stack and store it as value for 'b'.
		XIS::PUT, U16(16), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(26), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(3), XIS::RLB, XIS::AT,   // Dereference 'b' to yield address of 'a'.
		XIS::PUT, U16(0xfefe),                 // Put 0xfefe on stack.
		XIS::MOVD,                             // Move top stack value (0xfefe) to 'a'.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(4), XIS::POP,            // Pop 'main', 'fn', 'a', and 'b' from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_laddr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_scope_pop)
{
	const char CODE[] = "fn() { auto x = 0; if (0) { auto y = 0; } else { x = 0; } }";
	const int EXPECT_COUNT = 45;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,              // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,             // 'fn' function pointer storage.
		XIS::PUT, U16(40), XIS::RLA, XIS::JMP,   // Jump over 'fn' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT, U16(0),                        // Put value of 'x' on stack.
		XIS::PUT, U16(0),                        // Put condition on stack.
		XIS::PUT, U16(29), XIS::RLA, XIS::CNJMP, // Jump to the else statement if condition evaluates to 0.
		XIS::PUT, U16(0),                        // Put value 'y' on stack.
		XIS::PUT, U16(1), XIS::POP,              // Pop 'y' from stack.
		XIS::PUT, U16(35), XIS::RLA, XIS::JMP,   // Skip over the else clause.
		XIS::PUT, U16(1), XIS::RLC,              // Put address of 'x' on top of stack.
		XIS::PUT, U16(0), XIS::MOVD,             // Move 0 to memory location of 'x'.
		XIS::PUT, U16(1), XIS::POP,              // Pop 'x' from the stack.
		XIS::LDC,                                // Load C stack state.
		XIS::JMP,                                // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,              // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                                // Load the B stack state.
		XIS::HALT                                // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_scope_pop, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_array_index)
{
	const char CODE[] = "auto x = 0; auto y = x[123][234][345];";
	const int EXPECT_COUNT = 27;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                  // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,                // 'main' function pointer storage.
		XIS::PUT, U16(0),
		XIS::PUT, U16(2), XIS::RLB,
			XIS::AT, XIS::PUT, U16(123), XIS::ADD,
			XIS::AT, XIS::PUT, U16(234), XIS::ADD,
			XIS::AT, XIS::PUT, U16(345), XIS::ADD,
		XIS::AT,                                   // auto y = x[0][0][0]
		XIS::PUT, U16(3), XIS::POP,                // Pop 'main', 'x', and 'y' from stack.
		XIS::LDB,                                  // Load the B stack state.
		XIS::HALT                                  // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_array_index, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_include_relative)
{
	const char CODE[] = "#include \"xun/tests/src/test01.xb\" fn(a, b) {}";
	const int EXPECT_COUNT = 22;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(17), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,            // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_include_relative, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_include_standard)
{
	const int EXPECT_COUNT = 22;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // 'fn' function pointer storage.
		XIS::PUT, U16(17), XIS::RLA, XIS::JMP, // Jump over 'fn' body.
		XIS::SVC,                              // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,            // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::LDC,                              // Load C stack state.
		XIS::JMP,                              // Jump back to call site.
		XIS::PUT, U16(2), XIS::POP,            // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	const chars::view files[] = {
		chars::view{ "xun/tests/src/test02.xb", 23, 0 }
	};
	xcc_out out = xb(files, 1, chars::view{"xun/tests/src/", 14, 0}, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_include_standard, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_include_const_abs)
{
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	const chars::view files[] = {
		chars::view{ "xun/tests/src/test04.xb", 23, 0 }
	};
	xcc_out out = xb(files, 1, chars::view{"xun/tests/src/", 14, 0}, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_include_const_abs, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_include_const_rel)
{
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	const chars::view files[] = {
		chars::view{ "xun/tests/src/test05.xb", 23, 0 }
	};
	xcc_out out = xb(files, 1, chars::view{"xun/tests/src/", 14, 0}, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_include_const_rel, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_std_limits)
{
	const char CODE[] = "#include <limits>";
	XWORD binary[4096];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_std_limits, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_std_stdio)
{
	const char CODE[] = "#include <stdio>";
	XWORD binary[4096];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_std_stdio, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_std_stdlib)
{
	const char CODE[] = "#include <stdlib>";
	XWORD binary[4096];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_std_stdlib, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_std_string)
{
	const char CODE[] = "#include <string>";
	XWORD binary[4096];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_std_string, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_std_sys)
{
	const char CODE[] = "#include <sys>";
	XWORD binary[4096];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_std_sys, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_std_time)
{
	const char CODE[] = "#include <time>";
	XWORD binary[4096];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
}
CC0_UTEST_END(xb_lex_and_parse_std_time, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_scoped_variable)
{
	const char CODE[] =
		"main(argc, argv)\n\
		{\n\
			auto a = 0xf0f0;\n\
			{\n\
				auto b = 0x0e0e;\n\
				b = 0xfefe;\n\
			}\n\
		}";
	const int EXPECT_COUNT = 51;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                               // Save the B stack state.
		XIS::PUT, U16(8), XIS::RLA,             // 'main' function pointer storage.
		XIS::PUT, U16(30), XIS::RLA, XIS::JMP,  // Jump over 'main' body.
		XIS::SVC,                               // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,             // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT, U16(0xf0f0),                  // Create space on stack for 'a' and set it to 0xf0f0.
		XIS::PUT, U16(0x0e0e),                  // Create space on stack for 'b' and set it to 0x0e0e.
		XIS::PUT, U16(2), XIS::RLC,             // Put address of 'b' on stack.
		XIS::PUT, U16(0xfefe),                  // Put value 0xfefe on stack.
		XIS::MOVD,                              // Set 'b' to 0xfefe.
		XIS::PUT, U16(1), XIS::POP,             // Remove 'b' from stack.
		XIS::PUT, U16(1), XIS::POP,             // Remove 'a' from stack.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(0),                       // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(13), XIS::ADD, // Save return address with offset to return after function call.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(1), XIS::RLB, XIS::AT,    // Put 'main' address on stack.
		XIS::JMP,                               // Jump to 'main'.
		XIS::TOSS,                              // Discard the return value upon returning.
		XIS::PUT, U16(1), XIS::POP,             // Pop 'main' pointer from stack.
		XIS::LDB,                               // Load the B stack state.
		XIS::HALT                               // Halt.
	};
	// 16514             SVB
	// 384 8 17696       PUT 8 RLA
	// 384 30 17696 1856 PUT 30 RLA JMP
	// 16770             SVC
	// 384 2 2704        PUT 2 SUB
	// 384 61680         PUT 0xf0f0
	// 384 3598          PUT 0x0e0e
	// 384 1 18208       PUT 1 RLC
	// 384 65278         PUT 0xfefe
	// 11904             MOVD
	// 384 1 11392       PUT 1 POP
	// 384 1 11392       PUT 1 POP
	// 17602             LDC
	// 1856              JMP
	// 384 0             PUT 0
	// 898 384 13 2448   PUTI PUT 13 ADD
	// 384 0             PUT 0
	// 384 0             PUT 0
	// 384 1 17952 1568  PUT 1 RLB AT
	// 1856              JMP
	// 11648             TOSS
	// 384 1 11392       PUT 1 POP
	// 17346             LDB
	// 12544             HALT
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_scoped_variable, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_str_trailing_spaces)
{
	const char CODE[] = "auto x[] = \" a  b \t\"; auto size = sizeof(x);";
	const int EXPECT_COUNT = 30;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                    // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,  // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,  // The address pointer to the 'x' array.
		XIS::PUT, U16(' '),          // The global 'x' variable.
		XIS::PUT, U16('a'),          // The global 'x' variable.
		XIS::PUT, U16(' '),          // The global 'x' variable.
		XIS::PUT, U16(' '),          // The global 'x' variable.
		XIS::PUT, U16('b'),          // The global 'x' variable.
		XIS::PUT, U16(' '),          // The global 'x' variable.
		XIS::PUT, U16('\t'),         // The global 'x' variable.
		XIS::PUT, U16(0),            // The global 'x' variable.
		XIS::PUT, U16(8),            // Store the size of the 'x' string.
		XIS::PUT, U16(11), XIS::POP, // Remove 'main' (1), 'x' (1 + 7 + 1), and 'size'.
		XIS::LDB,                    // Load the B stack state.
		XIS::HALT                    // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_str_trailing_spaces, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_static_var)
{
	const char CODE[] = "static a = 0xfefe;";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::BIN, U16(0xfefe),      // Value of 'a'.
		XIS::PUT, U16(1), XIS::POP, // Remove 'main' (1).
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_static_var, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_static_counted_arr)
{
	const char CODE[] = "static b[] = { 0x00fe, 0xfe00 };";
	const int EXPECT_COUNT = 18;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // The address pointer to the 'b' array.
		XIS::PUT, U16(13), XIS::RLA, XIS::JMP, // Jump over static values.
		U16(0x00fe), U16(0xfe00),              // Values in 'b' array.
		XIS::PUT, U16(2), XIS::POP,            // Remove 'main' (1), 'b' pointer (1).
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_static_counted_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_static_arr)
{
	const char CODE[] = "static c[2] = { 0x00fe, 0xfe00 };";
	const int EXPECT_COUNT = 18;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // The address pointer to the 'c' array.
		XIS::PUT, U16(13), XIS::RLA, XIS::JMP, // Jump over static values.
		U16(0x00fe), U16(0xfe00),              // Values in 'c' array.
		XIS::PUT, U16(2), XIS::POP,            // Remove 'main' (1), 'c' pointer (1).
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_static_arr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_static_counted_str)
{
	const char CODE[] = "static b[] = \"AB\";";
	const int EXPECT_COUNT = 19;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // The address pointer to the 'b' array.
		XIS::PUT, U16(14), XIS::RLA, XIS::JMP, // Jump over static values.
		U16('A'), U16('B'), U16(0),            // Values in 'b' array.
		XIS::PUT, U16(2), XIS::POP,            // Remove 'main' (1), 'b' pointer (1).
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_static_counted_str, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_static_str)
{
	const char CODE[] = "static c[2] = \"AB\";";
	const int EXPECT_COUNT = 19;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                              // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,           // The address pointer to the 'c' array.
		XIS::PUT, U16(14), XIS::RLA, XIS::JMP, // Jump over static values.
		U16('A'), U16('B'), U16(0),            // Values in 'c' array.
		XIS::PUT, U16(2), XIS::POP,            // Remove 'main' (1), 'c' pointer (1).
		XIS::LDB,                              // Load the B stack state.
		XIS::HALT                              // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_static_str, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_set_static)
{
	const char CODE[] = 
		"main(a, b) {\n\
			static c = 10+75-2*3;\n\
			c = 12;\n\
		}";
	const int EXPECT_COUNT = 43;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                               // Save the B stack state.
		XIS::PUT, U16(8), XIS::RLA,             // 'main' function pointer storage.
		XIS::PUT, U16(22), XIS::RLA, XIS::JMP,  // Jump over 'main' body.
		XIS::SVC,                               // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,             // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::BIN, U16(79),                      // Create space for 'c' and set its value to 79
		XIS::PUT, U16(13), XIS::RLA,            // Put address of 'c' on the stack.
		XIS::PUT, U16(12),                      // Put 12 on the stack.
		XIS::MOVD,                              // Move value 12 to address of 'c'.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(0),                       // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(13), XIS::ADD, // Save return address with offset to return after function call.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(1), XIS::RLB, XIS::AT,    // Put 'main' address on stack.
		XIS::JMP,                               // Jump to 'main'.
		XIS::TOSS,                              // Discard the return value upon returning.
		XIS::PUT, U16(1), XIS::POP,             // Pop 'main' pointer from stack.
		XIS::LDB,                               // Load the B stack state.
		XIS::HALT                               // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_set_static, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_static_symbol_offset)
{
	const char CODE[] = 
		"static INPUT[] = \"a\";\n\
		foo( void ) {}\n\
		auto bar = foo();";

	// auto:   16514 384 0 17696 384 3 17952 384 97 384 0 384 18 17696 384 21 17696 1856 16770 17602 1856 384 0 898 384 9 2448 384 5 17952 1568 1856 384 6 11392 17346 12544
	// static: 16514 384 0 17696 384 11 17696 384 13 17696 1856 97 0 384 20 17696 384 23 17696 1856 16770 17602 1856 384 0 898 384 9 2448 384 2 17952 1568 1856 384 4 11392 17346 12544

	const int EXPECT_COUNT = 39;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(0), XIS::RLA,                    // 'main' maybe?
		XIS::PUT, U16(11), XIS::RLA,                   // Another variable pointing to something in the binary
		XIS::PUT, U16(13), XIS::RLA, XIS::JMP,         // Jump over static string
		U16('a'), U16(0),                              // The static string.
		XIS::PUT, U16(20), XIS::RLA,                   // Move the function pointer 'foo' to the stack.
		XIS::PUT, U16(23), XIS::RLA, XIS::JMP,         // Jump over the 'foo' function.
		XIS::SVC,                                      // Save C offset pointer.
		XIS::LDC,                                      // Restore C offset pointer.
		XIS::JMP,                                      // Jump back to call site.
		XIS::PUT, U16(0),                              // Create space on the stack for 'bar'.
		XIS::PUTI, XIS::PUT, U16(9), XIS::ADD,         // Put a return address on the stack.
		XIS::PUT, U16(3), XIS::RLB, XIS::AT, XIS::JMP, // Go to the function.
		XIS::PUT, U16(4), XIS::POP,                    // Remove all stack variabled.
		XIS::LDB,                                      // Restore B offset pointer.
		XIS::HALT                                      // Stop the program.
	};

	// NOTE:
	// I maybe know the issue.
	// When we have a static array we have one part AUTO and one part STATIC (AUTO=array pointer, STATIC=array memory).
	// However, the symbol is set as static even though it has both aspects.
	// Since we omit static symbols from stack incrementing to get the next address to reserve variables on 'foo' thinks it occupies memory location 2 instead of 3.
	// In this case, it means we emit the array pointer, dereference it and start executing the array contents in the binary.

	// static:
	// 16514                 SVB
	// 384 0 17696           PUT 0 RLA
	// 384 11 17696          PUT 11 RLA
	// 384 13 17696 1856     PUT 13 RLA JMP
	// 97 0                  'a' 0
	// 384 20 17696          PUT 20 RLA
	// 384 23 17696 1856     PUT 23 RLA JMP
	// 16770                 SVC
	// 17602                 LDC
	// 1856                  JMP
	// 384 0                 PUT 0
	// 898 384 9 2448        PUTI PUT 9 ADD
	// 384 2 17952 1568 1856 PUT 2 RLB AT JMP
	// 384 4 11392           PUT 4 POP
	// 17346                 LDB
	// 12544                 HALT

	// auto:
	// 16514                 SVB
	// 384 0 17696           PUT 0 RLA
	// 384 3 17952           PUT 2 RLB
	// 384 97                PUT 'a'
	// 384 0                 PUT 0
	// 384 18 17696          PUT 18 RLA
	// 384 21 17696 1856     PUT 21 RLA JMP
	// 16770                 SVC
	// 17602                 LDC
	// 1856                  JMP
	// 384 0                 PUT 0
	// 898 384 9 2448        PUTI PUT 9 ADD
	// 384 5 17952 1568 1856 PUT 5 RLB AT JMP
	// 384 6 11392           PUT 6 POP
	// 17346                 LDB
	// 12544                 HALT
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_static_symbol_offset, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_const_symbol_offset)
{
	const char CODE[] =
		"const A = 0;\n\
		foo() {}\n\
		main(argc, argv) { foo(); }";
	const int EXPECT_COUNT = 57;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                               // Save the B stack state.
		XIS::PUT, U16(18), XIS::RLA,            // 'main' function pointer storage.
		XIS::PUT, U16(11), XIS::RLA,            // 'foo' function pointer storage.
		XIS::PUT, U16(14), XIS::RLA, XIS::JMP,  // Jump over 'foo' body.
		XIS::SVC,                               // Save C stack state.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(36), XIS::RLA, XIS::JMP,  // Jump over 'main' body.
		XIS::SVC,                               // Save C stack state.
		XIS::PUT, U16(2), XIS::SUB,             // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT, U16(0),                       // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(9), XIS::ADD,  // Save return address with offset to return after function call.
		XIS::PUT, U16(2), XIS::RLB, XIS::AT,    // Put 'foo' address on stack. <-- ERROR: Puts 1 here (address of 'main') instead of 2 (address of 'foo')
		XIS::JMP,                               // Jump to 'foo'.
		XIS::TOSS,                              // Discard the return value upon returning.
		XIS::LDC,                               // Load C stack state.
		XIS::JMP,                               // Jump back to call site.
		XIS::PUT, U16(0),                       // Make room for function return value.
		XIS::PUTI, XIS::PUT, U16(13), XIS::ADD, // Save return address with offset to return after function call.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(0),                       // Put function parameters on stack.
		XIS::PUT, U16(1), XIS::RLB, XIS::AT,    // Put 'main' address on stack.
		XIS::JMP,                               // Jump to 'main'.
		XIS::TOSS,                              // Discard the return value upon returning.
		XIS::PUT, U16(2), XIS::POP,             // Pop 'main' and 'foo' pointer from stack.
		XIS::LDB,                               // Load the B stack state.
		XIS::HALT                               // Halt.
	};

	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_const_symbol_offset, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_lit_div_0)
{
	const char CODE1[] = "const A = 0 / 0;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE1, sizeof(CODE1), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, !=, 0);
	CC0_UTEST_ASSERT(out.error.code, ==, xcc_error::ZERO);

	const char CODE2[] = "const A = 0 % 0;";
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	out = xb(init_lexer(chars::view{CODE2, sizeof(CODE2), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	CC0_UTEST_ASSERT(out.errors, !=, 0);
	CC0_UTEST_ASSERT(out.error.code, ==, xcc_error::ZERO);
}
CC0_UTEST_END(xb_lex_and_parse_lit_div_0, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_ext_str)
{
	const char CODE[] = "auto x[] = \"\\#efa\";";
	const int EXPECT_COUNT = 16;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                    // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,  // 'main' function pointer storage.
		XIS::PUT, U16(3), XIS::RLB,  // The address pointer to the 'x' array.
		XIS::PUT, U16(0xef00 + 'a'), // The global 'x' variable.
		XIS::PUT, U16(0),            // The null terminator.
		XIS::PUT, U16(4), XIS::POP,  // Remove 'main' (1) and 'x' (1 + 1 + 1).
		XIS::LDB,                    // Load the B stack state.
		XIS::HALT                    // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_ext_str, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_ext_char)
{
	const char CODE[] = "auto x = '\\#efx';";
	const int EXPECT_COUNT = 11;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                    // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,  // 'main' function pointer storage.
		XIS::PUT, U16(0xef00 + 'x'), // The global 'x' variable.
		XIS::PUT, U16(2), XIS::POP,  // Remove 'main' and 'x'.
		XIS::LDB,                    // Load the B stack state.
		XIS::HALT                    // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_ext_char, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_reverse_search)
{
	const char CODE[] = " \
		auto x = 12; \n\
		fn() { \n\
			auto x = 23; \n\
			return x + ::x; \n\
		}";
	const int EXPECT_COUNT = 45;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                                                        // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA,                                      // 'main' function pointer storage.
		XIS::PUT, U16(12),                                               // The global 'x' variable.
		XIS::PUT, U16(13), XIS::RLA,                                     // 'fn' function pointer storage.
		XIS::PUT, U16(40), XIS::RLA, XIS::JMP,                           // Jump over 'fn' body.
		XIS::SVC,                                                        // Save C stack state.
		XIS::PUT, U16(23),                                               // Put and save local variable with value 23 on stack.
		XIS::PUT, U16(0), XIS::RLC, XIS::AT, XIS::PUT, U16(1), XIS::SUB, // Get the address of the external return value.
		XIS::PUT, U16(1), XIS::RLC, XIS::AT,                             // Put the value of the local 'x' on the stack.
		XIS::PUT, U16(2), XIS::RLB, XIS::AT,                             // Put the value of the gloal 'x' on the stack.
		XIS::ADD,                                                        // Put the local return value on top of stack.
		XIS::MOVD,                                                       // Move the top value of the stack to the external return value address.
		XIS::LDC,                                                        // Load C stack state     (for explicit return).
		XIS::JMP,                                                        // Jump back to call site (for explicit return).
		XIS::PUT, U16(1), XIS::POP,                                      // Remove local variable 'x' from stack.
		XIS::LDC,                                                        // Load C stack state     (for default return).
		XIS::JMP,                                                        // Jump back to call site (for default return).
		XIS::PUT, U16(3), XIS::POP,                                      // Remove 'main', 'x', and 'fn' from stack.
		XIS::LDB,                                                        // Load the B stack state.
		XIS::HALT                                                        // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_reverse_search, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_unsigned_expr)
{
	const char CODE[] = "auto x = unsigned(111 + 222 - 333 * 444 / 555 % 666);";
	const int EXPECT_COUNT = 26;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(111),         // The global 'x' variable, set as 111.
		XIS::PUT, U16(222),         // 222
		XIS::ADD,                   // Addition
		XIS::PUT, U16(333),         // 333
		XIS::PUT, U16(444),         // 444
		XIS::MUL,                   // Multiply
		XIS::PUT, U16(555),         // 555
		XIS::DIV,                   // Division
		XIS::PUT, U16(666),         // 666
		XIS::MOD,                   // Modulus
		XIS::SUB,                   // Subtract
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	// 16514
	// 384 0 17696
	// 384 111
	// 384 222
	// 2448
	// 384 333
	// 384 444
	// 2960
	// 384 555
	// 3216
	// 384 666
	// 3472
	// 2704
	// 384 2 11392
	// 17346
	// 12544
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_unsigned_expr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_signed_expr)
{
	const char CODE[] = "auto x = signed(111 + 222 - 333 * 444 / 555);";
	const int EXPECT_COUNT = 23;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(111),         // The global 'x' variable, set as 111.
		XIS::PUT, U16(222),         // 222
		XIS::IADD,                  // Addition
		XIS::PUT, U16(333),         // 333
		XIS::PUT, U16(444),         // 444
		XIS::IMUL,                  // Multiply
		XIS::PUT, U16(555),         // 555
		XIS::IDIV,                  // Division
		XIS::ISUB,                  // Subtract
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_signed_expr, false)

CC0_UTEST_BEGIN(xb_lex_and_parse_typed_expr)
{
	const char CODE[] = "auto x = 111 + 222 - unsigned(signed(333 - 444) * 555);";
	const int EXPECT_COUNT = 23;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,                   // Save the B stack state.
		XIS::PUT, U16(0), XIS::RLA, // 'main' function pointer storage.
		XIS::PUT, U16(111),         // The global 'x' variable, set as 111.
		XIS::PUT, U16(222),         // 222
		XIS::ADD,                   // Addition, implicit by default
		XIS::PUT, U16(333),         // 333
		XIS::PUT, U16(444),         // 444
		XIS::ISUB,                  // subtract, signed
		XIS::PUT, U16(555),         // 555
		XIS::MUL,                   // Multiply, explicit unsigned
		XIS::SUB,                   // Subtract, implicit unsigned
		XIS::PUT, U16(2), XIS::POP, // Remove 'main' and 'x'.
		XIS::LDB,                   // Load the B stack state.
		XIS::HALT                   // Halt.
	};
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(binary, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		if (binary[i].u != EXPECT[i]) {
			print_bin(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}
}
CC0_UTEST_END(xb_lex_and_parse_typed_expr, false)
