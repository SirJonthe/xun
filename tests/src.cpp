#include "tests.h"

CC0_UTEST_BEGIN(xb_compile_firmware)
{
	const chars::view source_files[] = {
		chars::view{"xun/firmware/firmware.xb", 24, 0}
	};
	XWORD binary[16384];
	clear_mem(binary, sizeof(binary) / sizeof(XWORD));
	xcc_out n = xb(
		source_files, 1,
		LIBB,
		xcc_binary{ binary, sizeof(binary) / sizeof(XWORD) },
		256
	);

	print_err(n);
	CC0_UTEST_ASSERT(n.binary.size, >, 0);
}
CC0_UTEST_END(xb_compile_firmware, false)
