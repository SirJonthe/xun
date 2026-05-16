#include "tests.h"

CC0_UTEST_BEGIN(xasm_run_add_3_and_5)
{
	const char CODE[] = "put 3, 5. add. mov 0.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);
	CC0_UTEST_ASSERT(out.binary.size, >, 0);
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 8);
}
CC0_UTEST_END(xasm_run_add_3_and_5, false)

CC0_UTEST_BEGIN(xasm_run_mov)
{
	const char CODE[] = "put 123, 234, 345. mov 100, 101, 102.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	const int EXPECT_COUNT = 18;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,
		XIS::PUT, U16(123),
		XIS::PUT, U16(234),
		XIS::PUT, U16(345),
		XIS::PUT, U16(100),
		XIS::MOVU,
		XIS::PUT, U16(101),
		XIS::MOVU,
		XIS::PUT, U16(102),
		XIS::MOVU,
		XIS::LDB,
		XIS::HALT
	};

	CC0_UTEST_ASSERT(out.binary.size, ==, U16(EXPECT_COUNT));

	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(100).u, ==, 345);
	CC0_UTEST_ASSERT(m.Peek(101).u, ==, 234);
	CC0_UTEST_ASSERT(m.Peek(102).u, ==, 123);
}
CC0_UTEST_END(xasm_run_mov, false)

CC0_UTEST_BEGIN(xasm_run_addr)
{
	const char CODE[] = "$scope: a, b, c { put &a, &b, &c. mov 128, 256, 512. }";
	const int EXPECT_COUNT = 27;
	const U16 EXPECT[EXPECT_COUNT] = {
		XIS::SVB,

		XIS::PUT, U16(3),
		XIS::PUSH,

		XIS::PUT, U16(1),
		XIS::RLC,
		XIS::PUT, U16(2),
		XIS::RLC,
		XIS::PUT, U16(3),
		XIS::RLC,

		XIS::PUT, U16(128),
		XIS::MOVU,
		XIS::PUT, U16(256),
		XIS::MOVU,
		XIS::PUT, U16(512),
		XIS::MOVU,

		XIS::PUT, U16(3),
		XIS::POP,

		XIS::LDB,
		XIS::HALT
	};
	// 16514           SVC
	// 384 3 11136     PUT 3 PUSH
	// 384 384 1 18208 PUT PUT 1 RLC
	// 384 384 2 18208 PUT PUT 2 RLC
	// 384 384 3 18208 PUT PUT 3 RLC
	// 384 128 12160   PUT 128 MOVU
	// 384 256 12160   PUT 256 MOVU
	// 384 512 12160   PUT 512 MOVU
	// 384 3 11392     PUT 3 POP
	// 17346           LDB
	// 12544           HALT
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);
	if (EXPECT_COUNT != (int)out.binary.size) {
		print_bin(out.binary.buffer, out.binary.size);
	}
	CC0_UTEST_ASSERT(EXPECT_COUNT, ==, (int)out.binary.size);
	for (int i = 0; i < EXPECT_COUNT; ++i) {
		CC0_UTEST_ASSERT(binary[i].u, ==, EXPECT[i]);
	}

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(128).u, ==, EXPECT_COUNT+5);
	CC0_UTEST_ASSERT(m.Peek(256).u, ==, EXPECT_COUNT+4);
	CC0_UTEST_ASSERT(m.Peek(512).u, ==, EXPECT_COUNT+3);
}
CC0_UTEST_END(xasm_run_addr, false)

CC0_UTEST_BEGIN(xasm_run_ref)
{
	const char CODE[] = "$scope: a { set a, 256. set @a, 512. set 0, @a. }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 512);
}
CC0_UTEST_END(xasm_run_ref, false)

CC0_UTEST_BEGIN(xasm_run_skip_bin)
{
	const char CODE[] = "$bin 1,2,3,4,5,6. set 0, 123.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 123);
}
CC0_UTEST_END(xasm_run_skip_bin, false)

CC0_UTEST_BEGIN(xasm_run_cjmp_true)
{
	const char CODE[] = "set 0, 123. put 0, 1. gt. put $here[18]. cjmp. put @0, 200. add. mov 0.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	out.binary.size += 18;
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 323);
}
CC0_UTEST_END(xasm_run_cjmp_true, false)

CC0_UTEST_BEGIN(xasm_run_cjmp_false)
{
	const char CODE[] = "set 0, 123. put 1, 0. gt. put $here[18]. cjmp. put @0, 200. add. mov 0.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	out.binary.size += 18;
	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 123);
}
CC0_UTEST_END(xasm_run_cjmp_false, false)

CC0_UTEST_BEGIN(xasm_run_set)
{
	const char CODE[] = "set 0, 123.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 123);
}
CC0_UTEST_END(xasm_run_set, false)

CC0_UTEST_BEGIN(xasm_run_skip_instruction)
{
	const char CODE[] = "set 0, 123. put 5. skip. put 555. mov 0. put 234. mov 1.";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 123);
	CC0_UTEST_ASSERT(m.Peek(1).u, ==, 234);
}
CC0_UTEST_END(xasm_run_skip_instruction, false)

/*CC0_UTEST_BEGIN(xasm_run_array_values)
{
	const char CODE[] = "$scope: arr[3] { set &arr[0], 99. set &arr[1], 88. set &arr[2], 77. set 0, arr[0]. set 1, arr[1]. set 2, arr[2]. }";
	XWORD binary[128];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out out = xasm(init_lexer(chars::view{CODE, sizeof(CODE), 0}), xcc_binary{binary,sizeof(binary) / sizeof(XWORD),0 });
	print_err(out);

	CC0_UTEST_ASSERT(out.binary.size, >, U16(0));

	Computer m(true);
	m.PowerOn();
	m.BootDisk(binary, out.binary.size);

	m.Run(1000);
	CC0_UTEST_ASSERT(m.Peek(0).u, ==, 99);
	CC0_UTEST_ASSERT(m.Peek(1).u, ==, 88);
	CC0_UTEST_ASSERT(m.Peek(2).u, ==, 77);
}
CC0_UTEST_END(xasm_run_array_values, false)*/
