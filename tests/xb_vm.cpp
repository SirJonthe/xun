#include "tests.h"

CC0_UTEST_BEGIN(xb_vm_global_var_assignment)
{
	const char CODE[] = "auto a = 0xfefe;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_global_var_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (1) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_early_exit_assignment_true)
{
	const char CODE[] = "auto x = 0; auto i = 0; const N = 16; main(a,b) { if (i >= N) { x = 1; return; } x = 2; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 2)
}
CC0_UTEST_END(xb_vm_if_early_exit_assignment_true, false)

CC0_UTEST_BEGIN(xb_vm_if_early_exit_assignment_false)
{
	const char CODE[] = "auto x = 0; auto i = 16; const N = 16; main(a,b) { if (i >= N) { x = 1; return; } x = 2; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 1)
}
CC0_UTEST_END(xb_vm_if_early_exit_assignment_false, false)

CC0_UTEST_BEGIN(xb_vm_if_true_assignment)
{
	const char CODE[] = "auto x = 0x00fe; auto y = 10; auto z = 20; main(a,b) { if (y < z) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_true_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_false_assignment)
{
	const char CODE[] = "auto x = 0x00fe; auto y = 10; auto z = 20; main(a,b) { if (y > z) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0x00fe)
}
CC0_UTEST_END(xb_vm_if_false_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_true_fn_assignment)
{
	const char CODE[] = "auto x = 0x00fe; y() { return 10; } z() { return 20; } main(a,b) { if (y() < z()) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_true_fn_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_false_fn_assignment)
{
	const char CODE[] = "auto x = 0x00fe; y() { return 10; }; z() { return 20; }; main(a,b) { if (y() > z()) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0x00fe)
}
CC0_UTEST_END(xb_vm_if_false_fn_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_and11_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (1&&1) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_and11_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_and10_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (1&&0) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0x00fe)
}
CC0_UTEST_END(xb_vm_if_and10_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_and01_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (0&&1) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0x00fe)
}
CC0_UTEST_END(xb_vm_if_and01_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_and00_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (0&&0) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0x00fe)
}
CC0_UTEST_END(xb_vm_if_and00_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_or11_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (1||1) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_or11_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_or10_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (1||0) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_or10_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_or01_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (0||1) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_if_or01_assignment, false)

CC0_UTEST_BEGIN(xb_vm_if_or00_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (0||0) { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0x00fe)
}
CC0_UTEST_END(xb_vm_if_or00_assignment, false)

CC0_UTEST_BEGIN(xb_vm_while)
{
	const char CODE[] = "auto x = 0xfefe - 10; main(a,b) { a = 10; while (a > 0) { x = x + 1; a = a - 1; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_while, false)

CC0_UTEST_BEGIN(xb_vm_else_assignment)
{
	const char CODE[] = "auto x = 0x00fe; main(a,b) { if (0) { x = 0xf0f0; } else { x = 0xfe00; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_else_assignment, false)

CC0_UTEST_BEGIN(xb_vm_global_operator_order)
{
	const char CODE[] = "auto a = 10 - 2 * 3 ^ 4 + 5 & 6 | 7 / 2 % 3;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, (10 - 2 * 3 ^ 4 + 5 & 6 | 7 / 2 % 3))
}
CC0_UTEST_END(xb_vm_global_operator_order, false)

CC0_UTEST_BEGIN(xb_vm_global_var_index)
{
	const char CODE[] = "auto x[2] = { 0x00fe, 0xfe00 }; auto a = x[0]; auto b = x[1];";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 7).u, ==, 0x00fe)
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 8).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_global_var_index, false)

CC0_UTEST_BEGIN(xb_vm_local_var_index_reassign)
{
	const char CODE[] = "auto x[2] = { 0, 0 }; main(a,b) { x[0] = 0x00fe; x[1] = 0xfe00; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0x00fe)
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 6).u, ==, 0xfe00)
}
CC0_UTEST_END(xb_vm_local_var_index_reassign, false)

CC0_UTEST_BEGIN(xb_vm_global_arr_assignment)
{
	const char CODE[] = "auto a[2] = { 0xfefe, 0xefef };";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0xfefe)
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 6).u, ==, 0xefef)
}
CC0_UTEST_END(xb_vm_global_arr_assignment, false)

CC0_UTEST_BEGIN(xb_vm_fn_call_return)
{
	const char CODE[] = "fn() {} main(a,b) { fn(); }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);

	U16 FLOW[] = {
		// Meta instructions
		XIS::SVB,                                // Program start.
		XIS::PUT, XIS::RLA,                      // 'main' function pointer storage.
		
		// Now we go to 'fn' head.
		XIS::PUT, XIS::RLA,                      // 'fn' function pointer storage.
		XIS::PUT, XIS::RLA, XIS::JMP,            // Jump over 'fn' body.

		// Now we jump to 'main' head.
		XIS::PUT, XIS::RLA, XIS::JMP,            // Jump over 'main' body.

		// Now we jump to 'main' call.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT,                                // Put function parameters on stack.
		XIS::PUT,                                // Put function parameters on stack.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'main' address on stack.
		XIS::JMP,                                // Jump to 'main'.

		// Now we go to 'main' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT, XIS::SUB,                      // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'fn' address on stack.
		XIS::JMP,                                // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                // Save C stack state.
		XIS::LDC,                                // Load C stack state.
		XIS::JMP,                                // Jump back to call site.

		// Now we return to 'fn' call site in 'main'.
		XIS::TOSS,                               // Discard the return value upon returning.
		XIS::LDC,                                // Load C stack state.
		XIS::JMP                                 // Jump back to call site.

		// There are a few additional instructions being executed, but not very important for this test.
	};
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	for (unsigned i = 0; i < sizeof(FLOW) / sizeof(U16); ++i) {
		m.Cycle();
		const U16 I = m.Instruction();
		if (I != FLOW[i]) {
			print_instr(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(I, ==, FLOW[i]);
	}
}
CC0_UTEST_END(xb_vm_fn_call_return, false)

CC0_UTEST_BEGIN(xb_vm_fn_params_call_return)
{
	const char CODE[] = "fn(a,b) {} main(a,b) { fn(a,b); }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);

	U16 FLOW[] = {
		// Meta instructions
		XIS::SVB,                                // Program start.
		XIS::PUT, XIS::RLA,                      // 'main' function pointer storage.
		
		// Now we go to 'fn' head.
		XIS::PUT, XIS::RLA,                      // 'fn' function pointer storage.
		XIS::PUT, XIS::RLA, XIS::JMP,            // Jump over 'fn' body.

		// Now we jump to 'main' head.
		XIS::PUT, XIS::RLA, XIS::JMP,            // Jump over 'main' body.

		// Now we jump to 'main' call.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT,                                // Put function parameters on stack.
		XIS::PUT,                                // Put function parameters on stack.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'main' address on stack.
		XIS::JMP,                                // Jump to 'main'.

		// Now we go to 'main' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT, XIS::SUB,                      // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLC, XIS::AT,             // Pass 'a' as parameter to function.
		XIS::PUT, XIS::RLC, XIS::AT,             // Pass 'b' as parameter to function.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'fn' address on stack.
		XIS::JMP,                                // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT, XIS::SUB,                      // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::LDC,                                // Load C stack state.
		XIS::JMP,                                // Jump back to call site.

		// Now we return to 'fn' call site in 'main'.
		XIS::TOSS,                               // Discard the return value upon returning.
		XIS::LDC,                                // Load C stack state.
		XIS::JMP                                 // Jump back to call site.

		// There are a few additional instructions being executed, but not very important for this test.
	};
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	for (unsigned i = 0; i < sizeof(FLOW) / sizeof(U16); ++i) {
		m.Cycle();
		const U16 I = m.Instruction();
		if (I != FLOW[i]) {
			print_instr(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(I, ==, FLOW[i]);
	}
}
CC0_UTEST_END(xb_vm_fn_params_call_return, false)

CC0_UTEST_BEGIN(xb_vm_recursion)
{
	const char CODE[] = "fn() { fn(); } main(a,b) { fn(); }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);

	U16 FLOW[] = {
		// Meta instructions
		XIS::SVB,                                // Program start.
		XIS::PUT, XIS::RLA,                      // 'main' function pointer storage.
		
		// Now we go to 'fn' head.
		XIS::PUT, XIS::RLA,                      // 'fn' function pointer storage.
		XIS::PUT, XIS::RLA, XIS::JMP,            // Jump over 'fn' body.

		// Now we jump to 'main' head.
		XIS::PUT, XIS::RLA, XIS::JMP,            // Jump over 'main' body.

		// Now we jump to 'main' call.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT,                                // Put function parameters on stack.
		XIS::PUT,                                // Put function parameters on stack.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'main' address on stack.
		XIS::JMP,                                // Jump to 'main'.

		// Now we go to 'main' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT, XIS::SUB,                      // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'fn' address on stack.
		XIS::JMP,                                // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'fn' address on stack.
		XIS::JMP,                                // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'fn' address on stack.
		XIS::JMP,                                // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                // Save C stack state.
		XIS::PUT,                                // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,           // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLB, XIS::AT,             // Put 'fn' address on stack.
		XIS::JMP                                 // Jump to 'fn'.

		// The last 9 instructions will repeat forever.
	};
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	for (unsigned i = 0; i < sizeof(FLOW) / sizeof(U16); ++i) {
		m.Cycle();
		const U16 I = m.Instruction();
		if (I != FLOW[i]) {
			print_instr(binary, out.binary.size);
		}
		CC0_UTEST_ASSERT(I, ==, FLOW[i]);
	}
}
CC0_UTEST_END(xb_vm_recursion, false)

CC0_UTEST_BEGIN(xb_vm_return_stmt)
{
	const char CODE[] = "fn() { return 0xfefe; } auto x = fn();";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);

	U16 FLOW[] = {
		// Meta instructions
		XIS::SVB,                                        // Program start.
		XIS::PUT, XIS::RLA,                              // 'main' function pointer storage.
		
		// Now we go to 'fn' head.
		XIS::PUT, XIS::RLA,                              // 'fn' function pointer storage.
		XIS::PUT, XIS::RLA, XIS::JMP,                    // Jump over 'fn' body.

		// Now we go to global variable assignment.
		XIS::PUT,                                        // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,                   // Save return address with offset to return after function call.
		XIS::PUT, XIS::RLB, XIS::AT,                     // Put 'fn' address on stack.
		XIS::JMP,                                        // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                        // Save C stack state.
		XIS::PUT, XIS::RLC, XIS::AT, XIS::PUT, XIS::SUB, // Get the address of the external return value.
		XIS::PUT,                                        // Put the local return value on top of stack.
		XIS::MOVD,                                       // Move the top value of the stack to the external return value address.
		XIS::LDC,                                        // Load C stack state.
		XIS::JMP,                                        // Jump back to call site.

		// Now we end program.
		XIS::PUT, XIS::POP,                              // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,
		XIS::HALT
	};
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	for (unsigned i = 0; i < sizeof(FLOW) / sizeof(U16); ++i) {
		m.Cycle();
		const U16 I = m.Instruction();
		if (I != FLOW[i]) {
			//print_instr(binary, out.binary.size);
			std::cout << "expected=" << xdebugger::decode(FLOW[i]).str << ", got=" << xdebugger::decode(I).str << std::endl;
		}
		CC0_UTEST_ASSERT(I, ==, FLOW[i]);
	}

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0xfefe);
}
CC0_UTEST_END(xb_vm_return_stmt, false)

CC0_UTEST_BEGIN(xb_vm_fn_param_addr)
{
	const char CODE[] = "fn(a,b) { return a+b; } auto x = fn(0xfe00,0x00fe);";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);

	U16 FLOW[] = {
		// Meta instructions
		XIS::SVB,                                        // Program start.
		XIS::PUT, XIS::RLA,                              // 'main' function pointer storage.
		
		// Now we go to 'fn' head.
		XIS::PUT, XIS::RLA,                              // 'fn' function pointer storage.
		XIS::PUT, XIS::RLA, XIS::JMP,                    // Jump over 'fn' body.

		// Now we go to global variable assignment.
		XIS::PUT,                                        // Make room for function return value.
		XIS::PUTI, XIS::PUT, XIS::ADD,                   // Save return address with offset to return after function call.
		XIS::PUT,                                        // Push 0xfe00 parameter.
		XIS::PUT,                                        // Push 0x00fe parameter.
		XIS::PUT, XIS::RLB, XIS::AT,                     // Put 'fn' address on stack.
		XIS::JMP,                                        // Jump to 'fn'.

		// Now we go to 'fn' body.
		XIS::SVC,                                        // Save C stack state.
		XIS::PUT, XIS::SUB,                              // Adjust saved C state to move it back to point to function parameters already on the stack.
		XIS::PUT, XIS::RLC, XIS::AT, XIS::PUT, XIS::SUB, // Get the address of the external return value.
		XIS::PUT, XIS::RLC, XIS::AT,                     // Put 'a' on top of the stack.
		XIS::PUT, XIS::RLC, XIS::AT,                     // Put 'b' on top of the stack.
		XIS::ADD,                                        // Compute return value.
		XIS::MOVD,                                       // Move the top value of the stack to the external return value address.
		XIS::LDC,                                        // Load C stack state.
		XIS::JMP,                                        // Jump back to call site.

		// Now we end program.
		XIS::PUT, XIS::POP,                              // Pop 'main' and 'fn' pointers from stack.
		XIS::LDB,
		XIS::HALT
	};
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	for (unsigned i = 0; i < sizeof(FLOW) / sizeof(U16); ++i) {
		m.Cycle();
		const U16 I = m.Instruction();
		if (I != FLOW[i]) {
			//print_instr(binary, out.binary.size);
			std::cout << "expected=" << xdebugger::decode(FLOW[i]).str << ", got=" << xdebugger::decode(I).str << std::endl;
		}
		CC0_UTEST_ASSERT(I, ==, FLOW[i]);
	}

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0xfefe);
}
CC0_UTEST_END(xb_vm_fn_param_addr, false)

CC0_UTEST_BEGIN(xb_vm_laddr)
{
	const char CODE[] = "auto a = 0x00fe; auto b = &a; main(argc,argv) { *b = 0xfefe; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfefe);
}
CC0_UTEST_END(xb_vm_laddr, false)

CC0_UTEST_BEGIN(xb_vm_pre_inc)
{
	const char CODE[] = "auto a = 10; auto b = ++a;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 11);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 11);
}
CC0_UTEST_END(xb_vm_pre_inc, false)

CC0_UTEST_BEGIN(xb_vm_post_inc)
{
	const char CODE[] = "auto a = 10; auto b = a++;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 11);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 10);
}
CC0_UTEST_END(xb_vm_post_inc, false)

CC0_UTEST_BEGIN(xb_vm_pre_dec)
{
	const char CODE[] = "auto a = 10; auto b = --a;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 9);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 9);
}
CC0_UTEST_END(xb_vm_pre_dec, false)

CC0_UTEST_BEGIN(xb_vm_post_dec)
{
	const char CODE[] = "auto a = 10; auto b = a--;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 9);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 10);
}
CC0_UTEST_END(xb_vm_post_dec, false)

CC0_UTEST_BEGIN(xb_vm_comp_add)
{
	const char CODE[] = "auto x = 123; main(a,b) { x += 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123+5)
}
CC0_UTEST_END(xb_vm_comp_add, false)

CC0_UTEST_BEGIN(xb_vm_comp_sub)
{
	const char CODE[] = "auto x = 123; main(a,b) { x -= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123 - 5)
}
CC0_UTEST_END(xb_vm_comp_sub, false)

CC0_UTEST_BEGIN(xb_vm_comp_mul)
{
	const char CODE[] = "auto x = 123; main(a,b) { x *= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123*5)
}
CC0_UTEST_END(xb_vm_comp_mul, false)

CC0_UTEST_BEGIN(xb_vm_comp_div)
{
	const char CODE[] = "auto x = 123; main(a,b) { x /= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123/5)
}
CC0_UTEST_END(xb_vm_comp_div, false)

CC0_UTEST_BEGIN(xb_vm_comp_mod)
{
	const char CODE[] = "auto x = 123; main(a,b) { x %= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123%5)
}
CC0_UTEST_END(xb_vm_comp_mod, false)

CC0_UTEST_BEGIN(xb_vm_comp_and)
{
	const char CODE[] = "auto x = 123; main(a,b) { x &= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123&5)
}
CC0_UTEST_END(xb_vm_comp_and, false)

CC0_UTEST_BEGIN(xb_vm_comp_or)
{
	const char CODE[] = "auto x = 123; main(a,b) { x |= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123|5)
}
CC0_UTEST_END(xb_vm_comp_or, false)

CC0_UTEST_BEGIN(xb_vm_comp_xor)
{
	const char CODE[] = "auto x = 123; main(a,b) { x ^= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123^5)
}
CC0_UTEST_END(xb_vm_comp_xor, false)

CC0_UTEST_BEGIN(xb_vm_comp_lsh)
{
	const char CODE[] = "auto x = 123; main(a,b) { x <<= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123<<5)
}
CC0_UTEST_END(xb_vm_comp_lsh, false)

CC0_UTEST_BEGIN(xb_vm_comp_rsh)
{
	const char CODE[] = "auto x = 123; main(a,b) { x >>= 5; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 123>>5)
}
CC0_UTEST_END(xb_vm_comp_rsh, false)

CC0_UTEST_BEGIN(xb_vm_while_break)
{
	const char CODE[] = "auto x = 0xfefe; main(a,b) { a = 10; while (a > 0) { break; x = x + 1; a = a - 1; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_while_break, false)

CC0_UTEST_BEGIN(xb_vm_while_continue)
{
	const char CODE[] = "auto x = 0xfefe; main(a,b) { a = 10; while (a > 0) { a = a - 1; continue; x = x + 1; } }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_while_continue, false)

CC0_UTEST_BEGIN(xb_vm_def_fn)
{
	const char CODE[] = "fn() { return 0xfefe; } auto x = fn();";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_def_fn, false)

CC0_UTEST_BEGIN(xb_vm_undef_fn)
{
	const char CODE[] = "fn(); auto x = fn(); fn() { return 0xfefe; }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_undef_fn, false)

CC0_UTEST_BEGIN(xb_vm_static_arr)
{
	const char CODE[] =
		"static INPUT[] = \"aaaaaaaaaaaaaaaaaaaaaaaa\"; \n\
		auto n = 0xfefe;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);

	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 5).u, ==, 0xfefe)
}
CC0_UTEST_END(xb_vm_static_arr, false)

/*CC0_UTEST_BEGIN(xb_vm_pre_inc_redir)
{
	const char CODE[] = "auto a = 10; auto b = &a; auto c = ++*b;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 11);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 6).u, ==, 11);
}
CC0_UTEST_END(xb_vm_pre_inc_redir, false)

CC0_UTEST_BEGIN(xb_vm_post_inc_redir)
{
	const char CODE[] = "auto a = 10; auto b = &a; auto c = *b++;";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xb(init_lexer(chars::view{CODE, sizeof(CODE), 0}), LIBB, xcc_binary{binary, sizeof(binary) / sizeof(XWORD), 0});
	print_err(out);
	CC0_UTEST_ASSERT(out.errors, ==, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);
	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 4).u, ==, 11);
	CC0_UTEST_ASSERT(m.Peek(out.binary.size + 6).u, ==, 10);
}
CC0_UTEST_END(xb_vm_post_inc_redir, false)*/
