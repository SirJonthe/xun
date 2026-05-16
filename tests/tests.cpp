#include "tests.h"

std::string buffer_file(const std::string &filename)
{
	std::string buffer;
	std::ostringstream sout;
	std::ifstream file(filename, std::ios::binary);
	if (file.is_open()) {
		sout << file.rdbuf();
	} else {
		std::cout << "Could not open file \'" << filename << "\'" << std::endl;
	}
	return sout.str();
}

void clear_mem(XWORD *mem, unsigned size)
{
	for (unsigned i = 0; i < size; ++i) {
		mem[i].u = XIS::HALT;
	}
}

void print_bin(XWORD *binary, U16 binary_size)
{
	std::cout << std::endl << "    ";
	for (int i = 0; i < binary_size; ++i) {
		std::cout << binary[i].u << " ";
	}
}

void print_instr(XWORD *binary, U16 binary_size)
{
	std::cout << std::endl;
	U16 p = XIS::NOP;
	for (int i = 0; i < binary_size; ++i) {
		if (p == XIS::PUT) {
			std::cout << " " << binary[i].u << std::endl;
			p = XIS::NOP;
		} else {
			if (i + 1 < 10) {
				std::cout << "    ";
			} else if (i + 1 < 100) {
				std::cout << "   ";
			} else if (i + 1 < 1000) {
				std::cout << "  ";
			} else if (i + 1 < 10000) {
				std::cout << " ";
			}
			std::cout << "    " << i+1 << " " << xdebugger::decode(binary[i].u).str;
			p = binary[i].u;
			if (p != XIS::PUT) {
				std::cout << std::endl;
			}
		}
	}
}

std::string error(U16 code)
{
	switch (code) {
	case xcc_error::NONE:       return "NONE";
	case xcc_error::MEMORY:     return "MEMORY";
	case xcc_error::UNDEF:      return "UNDEF";
	case xcc_error::REDEF:      return "REDEF";
	case xcc_error::VERIFY:     return "VERIFY";
	case xcc_error::INTERNAL:   return "INTERNAL";
	case xcc_error::UNEXPECTED: return "UNEXPECTED";
	case xcc_error::MISSING:    return "MISSING";
	case xcc_error::ZERO:       return "ZERO";
	}
	return "???";
}

void print_err(xcc_out &out)
{
	if (out.errors != 0) {
		std::cout << std::endl << "  error: fil=\'" << out.error.file.str << "\',tok=" << out.error.tok.index+1 << ",txt=\'" << out.error.tok.text.str << "\' (\"" << out.error.file.str << "\")" << " @ row=" << out.error.tok.row+1 << ",col=" << out.error.tok.col+1 << ",typ=" << error(out.error.code) << ",loc=" << out.error.ifile << "@" << out.error.iline;
	}
}
