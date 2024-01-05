#include <iostream>
#include "xmach.h"

#define AT(x)         RAM[x.u]
#define ATN(x,n)      RAM[x.u + n]

#define READI         RAM[IP.u++].u

#define TOP           RAM[uint16_t(SP.u + C.u)]
#define LST           RAM[uint16_t(SP.u + C.u - 1)]
#define SAT(n)        RAM[uint16_t(SP.u + C.u + n)]

#define POP_STACK(n)  SP.u -= (n)
#define PUSH_STACK(n) SP.u += (n)

Machine::Machine( void ) : Device("XERXES(tm) Unified Nanocontroller [XUN(tm)]", 0xffff), m_embedded_storage(1<<21), m_relay(16), m_power(false)
{
	Device::Connect(*this, m_relay);
	Connect(m_clock,            0);
	Connect(m_power_controller, 1);
	Connect(m_embedded_storage, 2);
}

// Flash memory with a program.
// Ideally should be an OS that can load other programs,
// but can be any program.
void Machine::Feed(const XWORD *bin, U16 bin_count, bool debug)
{
	// TODO:
	// This feeds a bootloader into STORAGE
	// When computer boots, the machine FIRMWARE (a small read-only program) pulls the main executable from STORAGE into RAM.
	// When done, executes program in RAM.

	SP.u = U_MAX;
	IP.u = 0;
	A.u = 0;
	for (C.u = 0; C.u < bin_count; ++C.u) {
		AT(C) = bin[C.u];
	}
	B.u = C.u;
	if (debug) {
		for (unsigned i = C.u; i < U_MAX; ++i) {
			AT(XWORD{U16(i)}).u = XIS::HALT;
		}
	}
}

void Machine::Poke(U16 addr, XWORD val)
{
	RAM[addr] = val;
}

void Machine::PokeA(U16 addr, XWORD val)
{
	RAM[addr + A.u] = val;
}

void Machine::PokeB(U16 addr, XWORD val)
{
	RAM[addr + B.u] = val;
}

void Machine::PokeC(U16 addr, XWORD val)
{
	RAM[addr + C.u] = val;
}

XWORD Machine::Peek(U16 addr)
{
	return RAM[addr];
}

XWORD Machine::PeekA(U16 addr)
{
	return RAM[addr + A.u];
}

XWORD Machine::PeekB(U16 addr)
{
	return RAM[addr + B.u];
}

XWORD Machine::PeekC(U16 addr)
{
	return RAM[addr + C.u];
}

XWORD Machine::Cycle( void )
{
//	std::cout << "\tI" << IP.u << "=";
//	if (AT(IP).u < XIS::COUNT) { std::cout << ISTR[AT(IP).u] << std::endl; }
//	else                       { std::cout << "ERR(" << AT(IP).u << ")" << std::endl; }
	switch (READI) {
	case XIS::NOP:
		break;
	case XIS::JMP:
		IP.u = TOP.u;
		POP_STACK(1);
		break;
	case XIS::SKIP:
		IP.u += TOP.u;
		POP_STACK(1);
		break;
	case XIS::AT:
		TOP = RAM[TOP.u];
		break;
	case XIS::ADD:
		LST.u += TOP.u;
		POP_STACK(1);
		break;
	case XIS::SUB:
		LST.u -= TOP.u;
		POP_STACK(1);
		break;
	case XIS::MUL:
		LST.u *= TOP.u;
		POP_STACK(1);
		break;
	case XIS::DIV:
		if (TOP.u != 0)      { LST.u /= TOP.u; }
		else if (LST.u != 0) { LST.u = U_MAX; ERR.u |= ERR_DIV0; }
		else                 { LST.u = 0; }
		POP_STACK(1);
		break;
	case XIS::IADD:
		LST.i += TOP.i;
		POP_STACK(1);
		break;
	case XIS::ISUB:
		LST.i -= TOP.i;
		POP_STACK(1);
		break;
	case XIS::IMUL:
		LST.i *= TOP.i;
		POP_STACK(1);
		break;
	case XIS::IDIV:
		if (TOP.i != 0)     { LST.i /= TOP.i; }
		else if (LST.i < 0) { LST.i = I_MIN; ERR.u |= ERR_DIV0; }
		else if (LST.i > 0) { LST.i = I_MAX; ERR.u |= ERR_DIV0; }
		else                { LST.i = 0; }
		POP_STACK(1);
		break;
	case XIS::INEG:
		TOP.i = -TOP.i;
		break;
	case XIS::LSH:
		LST.u <<= (TOP.u & 15);
		POP_STACK(1);
		break;
	case XIS::RSH:
		LST.u >>= (TOP.u & 15);
		POP_STACK(1);
		break;
	case XIS::AND:
		LST.u &= TOP.u;
		POP_STACK(1);
		break;
	case XIS::OR:
		LST.u |= TOP.u;
		POP_STACK(1);
		break;
	case XIS::XOR:
		LST.u ^= TOP.u;
		POP_STACK(1);
		break;
	case XIS::NOT:
		TOP.u = !TOP.u;
		break;
	case XIS::EQ:
		LST.u = LST.u == TOP.u ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::NE:
		LST.u = LST.u != TOP.u ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::LE:
		LST.u = LST.u <= TOP.u ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::GE:
		LST.u = LST.u >= TOP.u ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::LT:
		LST.u = LST.u < TOP.u ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::GT:
		LST.u = LST.u > TOP.u ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::IEQ:
		LST.u = LST.i == TOP.i ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::INE:
		LST.u = LST.i != TOP.i ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::ILE:
		LST.u = LST.i <= TOP.i ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::IGE:
		LST.u = LST.i >= TOP.i ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::ILT:
		LST.u = LST.i < TOP.i ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::IGT:
		LST.u = LST.i > TOP.i ? uint16_t(1) : uint16_t(0);
		POP_STACK(1);
		break;
	case XIS::HALT:
		--IP.u;
		Shutdown();
		break;
	case XIS::PUSH:
		PUSH_STACK(TOP.u - 1);
		break;
	case XIS::POP:
		POP_STACK(TOP.u + 1);
		break;
	case XIS::PUT:
		PUSH_STACK(1);
		TOP.u = READI;
		break;
	case XIS::PUTS:
		PUSH_STACK(1);
		TOP.u = SP.u + C.u;
		break;
	case XIS::PUTI:
		PUSH_STACK(1);
		TOP.u = IP.u;
		break;
	case XIS::TOSS:
		POP_STACK(1);
		break;
	case XIS::MOVD:
		RAM[LST.u] = TOP;
		POP_STACK(2);
		break;
	case XIS::MOVU:
		RAM[TOP.u] = LST;
		POP_STACK(2);
		break;
	case XIS::PEEK:
		RAM[TOP.u] = LST;
		POP_STACK(1);
		break;
	case XIS::CJMP:
		if (LST.u) {
			IP.u = TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::CSKIP:
		if (LST.u) {
			IP.u += TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::CNJMP:
		if (!LST.u) {
			IP.u = TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::CNSKIP:
		if (!LST.u) {
			IP.u += TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::DUP:
		PUSH_STACK(1);
		TOP.u = LST.u;
		break;
	case XIS::SVA:
		// [A][B][C][SP]
		//         ABC^
		PUSH_STACK(1);
		TOP.u = A.u;
		PUSH_STACK(1);
		TOP.u = B.u;
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u;
		A.u = SP.u;
		B.u = SP.u;
		C.u = SP.u;
		SP.u = 0;
		break;
	case XIS::SVB:
		// [ ]...[B][C][SP]
		// A^           BC^
		PUSH_STACK(1);
		TOP.u = B.u;
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u;
		B.u = SP.u;
		C.u = SP.u;
		SP.u = 0;
		break;
	case XIS::SVC:
		// [ ]...[ ]...[C][SP]
		// A^    B^        C^
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u;
		C.u = SP.u;
		SP.u = 0;
		break;
	case XIS::LDA:
		SP = ATN(A,  0);
		C  = ATN(A, -1);
		B  = ATN(A, -2);
		A  = ATN(A, -3);
		POP_STACK(4);
		break;
	case XIS::LDB:
		SP = ATN(A,  0);
		C  = ATN(A, -1);
		B  = ATN(A, -2);
		POP_STACK(3);
		break;
	case XIS::LDC:
		SP = ATN(A,  0);
		C  = ATN(A, -1);
		POP_STACK(2);
		break;
	case XIS::RLA:
		TOP.u = TOP.u + A.u;
		break;
	case XIS::RLB:
		TOP.u = TOP.u + B.u;
		break;
	case XIS::RLC:
		TOP.u = TOP.u + C.u;
		break;
	default:
		// Generate a hardware exception here that will allow a potential OS to recover.
		ERR.u |= ERR_UNDEF;
		break;
	}
//	std::cout << "\tS" << SP.u << "=" << TOP.u << std::endl;
//	std::cout << "\tE" << EP.u << "=" << AT(EP).u << std::endl;
//	std::cout << "\tC" << CP.u << "=" << AT(CP).u << std::endl;
	return RAM[IP.u];
}

void Machine::Run( void )
{
	while (Cycle().u != XIS::HALT) {}
}

bool Machine::IsAvailablePort(U8 port) const
{
	return m_relay[port] == nullptr;
}

void Machine::Connect(Device &device, U8 port)
{
	m_relay.RelayConnect(device, port);
}

void Machine::Disconnect(U8 port)
{
	m_relay.RelayDisconnect(port);
}
