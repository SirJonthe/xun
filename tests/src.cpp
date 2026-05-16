#include "tests.h"

CC0_UTEST_BEGIN(xb_compile_bios)
{
	const chars::view source_files[] = {
		chars::view{"xun/bios/bios.xb", 17, 0}
	};
	XWORD binary[16384];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out n = xb(
		source_files, 1,
		chars::view{"xun/xos/lib/libb/", 17, 0},
		xcc_binary{ binary, sizeof(binary) / sizeof(XWORD) },
		256
	);

	print_err(n);
	CC0_UTEST_ASSERT(n.binary.size, >, 0);
}
CC0_UTEST_END(xb_compile_bios, false)
