#ifndef TESTS_H
#define TESTS_H

#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include "../lib/utest/utest.h"
#include "../lib/tokn/tokn.h"
#include "../tools/xcc/xb/xb.h"
#include "../tools/xdb/xdb.h"
#include "../xun.h"

std::string buffer_file(const std::string &filename);

void clear_mem(XWORD *mem, unsigned size);

void print_bin(XWORD *binary, U16 binary_size);

void print_instr(XWORD *binary, U16 binary_size);

void print_err(xcc_out &out);

static const chars::view LIBB = chars::view{"xun/sw/libb/", 12, 0};

#endif
