#include <iostream>
#include "xdb.h"
#include "../xcc/xasm/xasm.h"

void print_padded_hex(U16 num)
{
	for (signed i = 3; i >= 0; --i) {
		unsigned x = (num & (0xf << (i*4))) >> (i*4);
		switch (x) {
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
			std::cout << x;
			break;
		case 10:
			std::cout << 'a';
			break;
		case 11:
			std::cout << 'b';
			break;
		case 12:
			std::cout << 'c';
			break;
		case 13:
			std::cout << 'd';
			break;
		case 14:
			std::cout << 'e';
			break;
		case 15:
			std::cout << 'f';
			break;
		}
	}
}

chars decode_instruction(U16 i)
{
	class RenderChars
	{
	private:
		int len(const char *c) {
			int l = 0;
			while (c[l] != 0) { ++l; }
			return l;
		}
	public:
		void operator()(chars &out, const char *c) {
			int l = len(c);
			int i = 0;
			for (; i < l; ++i) {
				out.str[i] = c[i];
			}
			for (; i < sizeof(out.str); ++i) {
				out.str[i] = 0;
			}
		}
	} render;

	chars c;
	switch (i) {
		case XIS::NOP:    render(c, "NOP   "); break;
		case XIS::PUT:    render(c, "PUT   "); break;
		case XIS::PUTS:   render(c, "PUTS  "); break;
		case XIS::PUTI:   render(c, "PUTI  "); break;
		case XIS::AT:     render(c, "AT    "); break;
		case XIS::JMP:    render(c, "JMP   "); break;
		case XIS::SKIP:   render(c, "SKIP  "); break;
		case XIS::ADD:    render(c, "ADD   "); break;
		case XIS::SUB:    render(c, "SUB   "); break;
		case XIS::MUL:    render(c, "MUL   "); break;
		case XIS::DIV:    render(c, "DIV   "); break;
		case XIS::MOD:    render(c, "MOD   "); break;
		case XIS::IADD:   render(c, "IADD  "); break;
		case XIS::ISUB:   render(c, "ISUB  "); break;
		case XIS::IMUL:   render(c, "IMUL  "); break;
		case XIS::IDIV:   render(c, "IDIV  "); break;
		case XIS::IMOD:   render(c, "IMOD  "); break;
		case XIS::INEG:   render(c, "INEG  "); break;
		case XIS::LSH:    render(c, "LSH   "); break;
		case XIS::RSH:    render(c, "RSH   "); break;
		case XIS::AND:    render(c, "AND   "); break;
		case XIS::OR:     render(c, "OR    "); break;
		case XIS::XOR:    render(c, "XOR   "); break;
		case XIS::NOT:    render(c, "NOT   "); break;
		case XIS::EQ:     render(c, "EQ    "); break;
		case XIS::NE:     render(c, "NE    "); break;
		case XIS::LE:     render(c, "LE    "); break;
		case XIS::GE:     render(c, "GE    "); break;
		case XIS::LT:     render(c, "LT    "); break;
		case XIS::GT:     render(c, "GT    "); break;
		case XIS::IEQ:    render(c, "IEQ   "); break;
		case XIS::INE:    render(c, "INE   "); break;
		case XIS::ILE:    render(c, "ILE   "); break;
		case XIS::IGE:    render(c, "IGE   "); break;
		case XIS::ILT:    render(c, "ILT   "); break;
		case XIS::IGT:    render(c, "IGT   "); break;
		case XIS::PUSH:   render(c, "PUSH  "); break;
		case XIS::POP:    render(c, "POP   "); break;
		case XIS::TOSS:   render(c, "TOSS  "); break;
		case XIS::MOVD:   render(c, "MOVD  "); break;
		case XIS::MOVU:   render(c, "MOVU  "); break;
		case XIS::PEEK:   render(c, "PEEK  "); break;
		case XIS::HALT:   render(c, "HALT  "); break;
		case XIS::CJMP:   render(c, "CJMP  "); break;
		case XIS::CSKIP:  render(c, "CSKIP "); break;
		case XIS::CNJMP:  render(c, "CNJMP "); break;
		case XIS::CNSKIP: render(c, "CNSKIP"); break;
		case XIS::DUP:    render(c, "DUP   "); break;
		case XIS::SVA:    render(c, "SVA   "); break;
		case XIS::SVB:    render(c, "SVB   "); break;
		case XIS::SVC:    render(c, "SVC   "); break;
		case XIS::LDA:    render(c, "LDA   "); break;
		case XIS::LDB:    render(c, "LDB   "); break;
		case XIS::LDC:    render(c, "LDC   "); break;
		case XIS::RLA:    render(c, "RLA   "); break;
		case XIS::RLB:    render(c, "RLB   "); break;
		case XIS::RLC:    render(c, "RLC   "); break;
		case XIS::CLOCK:  render(c, "CLOCK "); break;
		case XIS::BIN:    render(c, "BIN   "); break;
		case XIS::PORT:   render(c, "PORT  "); break;
		case XIS::POLL:   render(c, "POLL  "); break;
		case XIS::PASS:   render(c, "PASS  "); break;
		case XIS::CPUID:  render(c, "CPUID "); break;
		case XIS::PEND:   render(c, "PEND  "); break;
		case XIS::ACK:    render(c, "ACK   "); break;
		case XIS::ERR:    render(c, "ERR   "); break;
		case XIS::CERR:   render(c, "CERR  "); break;
		case XIS::FULL:   render(c, "FULL  "); break;
		default:          render(c, "???   "); break;
	}
	return c;
}

xdebugger::xdebugger(const xcc_binary &program) : m_computer(true)
{
	m_computer.PowerOn();
	m_computer.BootDisk(program.buffer, program.size);
}

bool xdebugger::step( void )
{
	m_computer.Cycle();
	return m_computer.IsPoweredOn();
}

void xdebugger::ui(unsigned rows) const
{
	const int i_page_width  = 4;
	const int i_page_height = rows;
	const int i_page_size   = i_page_width * i_page_height;
	
	const int start_i = (m_computer.InstructionPointer() / i_page_size) * i_page_size;
	
	int stack_size = signed(m_computer.StackPointer()) - m_computer.StackOffsetC();
	if (stack_size < 0) { stack_size = 0; }

    std::cout << "                                            ";
	print_padded_hex(stack_size);
	std::cout << std::endl;
	std::cout << " LIST   PROG                    INST    STK=";
	print_padded_hex(m_computer.StackPointer());
	std::cout << "  " << "A=";
	print_padded_hex(m_computer.StackOffsetA());
	std::cout << "  " << "B=";
	print_padded_hex(m_computer.StackOffsetB());
	std::cout << "  " << "C=";
	print_padded_hex(m_computer.StackOffsetC());
	std::cout << std::endl;
	for (int y = 0; y < i_page_height; ++y) {
		const U16 o = start_i + y * i_page_width;
		
		std::cout << " ";
		print_padded_hex(o);
		std::cout << ":";
		for (int x = 0; x < i_page_width; ++x) {
			const XWORD i = m_computer.Peek(o + x);
			if (o + x == m_computer.InstructionPointer()) {
				std::cout << " >";
			} else {
				std::cout << "  ";
			}
			print_padded_hex(i.u);
		}
		
		if (m_computer.InstructionPointer() >= o && m_computer.InstructionPointer() < o + i_page_width) {
			std::cout << "  " << decode_instruction(XIS::Enum(m_computer.Peek(m_computer.InstructionPointer()).u)).str;
		} else {
			std::cout << "        ";
		}

//		if (stack_size < i_page_height) {
		const int display_offset = stack_size < i_page_height ? i_page_height - stack_size : 0;
		if (y - display_offset >= 0) {
			if (y - display_offset == 0) {
				std::cout << " >";
			} else {
				std::cout << "  ";
			}
			print_padded_hex(m_computer.PeekTop(-(y - display_offset)).u);
		} else {
			std::cout << "      ";
		}
//		}
//		else {
//			std::cout << "  ";
//			print_padded_hex(m_computer.PeekTop(-y).u);
//		}

		std::cout << std::endl;
	}
}