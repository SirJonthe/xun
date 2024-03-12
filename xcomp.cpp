#include <iostream>
#include "xcomp.h"

#define AT(x)         RAM[x.u]
#define ATN(x,n)      RAM[U16(x.u + (n))]

#define READI         RAM[IP.u++].u

#define TOP           RAM[SP.u]
#define LST           RAM[U16(SP.u - 1)]
#define SAT(n)        RAM[U16(SP.u + (n))]

#define POP_STACK(n)  SP.u -= (n)
#define PUSH_STACK(n) SP.u += (n)

Computer::Computer( void ) : Device("XERXES(tm) Unified Nanocontroller [XUN(tm)]", 0xffff), m_storage(1<<21), m_relay(16), m_latency_counter(0), m_cycles_per_second(10000000), m_power(false)
{
	Device::Connect(*this, m_relay);
	Connect(m_clock,            0);
	Connect(m_power_controller, 1);
	Connect(m_storage,          2);
}

// Flash memory with a program.
// Ideally should be an OS that can load other programs,
// but can be any program.
void Computer::BootDisk(const XWORD *bin, U16 bin_count, bool debug)
{
	IP.u = 0;
	A.u  = 0;
	for (C.u = 0; C.u < bin_count; ++C.u) {
		AT(C) = bin[C.u];
	}
	AT(XWORD{C.u}).u = XIS::HALT; // Emit implicit HALT for safety.
	B.u  = C.u;
	SP.u = C.u;
	if (debug) {
		for (unsigned i = C.u + 1; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = XIS::HALT;
		}
	} else {
		for (unsigned i = C.u + 1; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = U16(rand());
		}
	}
}

void Computer::PowerOff( void )
{
	if (m_power) {
		m_power = false;
		A.u = B.u = C.u = SP.u = IP.u = ERR.u = 0;
		for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = 0;
		}
	}
}

void Computer::PowerOn( void )
{
	if (!m_power) {
		m_power = true;
		A.u = B.u = C.u = SP.u = IP.u = ERR.u = 0;
		for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = U16(rand());
		}
	}
}

void Computer::PowerToggle( void )
{
	if (!m_power) {
		PowerOn();
	} else {
		PowerOff();
	}
}

void Computer::PowerCycle( void )
{
	if (m_power) {
		PowerOff();
		PowerOn();
	}
}

void Computer::Poke(U16 addr, XWORD val)
{
	RAM[addr] = val;
}

void Computer::PokeA(U16 addr, XWORD val)
{
	RAM[U16(addr + A.u)] = val;
}

void Computer::PokeB(U16 addr, XWORD val)
{
	RAM[U16(addr + B.u)] = val;
}

void Computer::PokeC(U16 addr, XWORD val)
{
	RAM[U16(addr + C.u)] = val;
}

void Computer::PokeTop(U16 addr, XWORD val)
{
	RAM[U16(addr + SP.u)] = val;
}

XWORD Computer::Peek(U16 addr) const
{
	return RAM[addr];
}

XWORD Computer::PeekA(U16 addr) const
{
	return RAM[U16(addr + A.u)];
}

XWORD Computer::PeekB(U16 addr) const
{
	return RAM[U16(addr + B.u)];
}

XWORD Computer::PeekC(U16 addr) const
{
	return RAM[U16(addr + C.u)];
}

XWORD Computer::PeekTop(U16 addr) const
{
	return RAM[U16(addr + SP.u)];
}

XWORD Computer::Cycle( void )
{
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
		TOP.u = SP.u;
		break;
	case XIS::PUTI:
		PUSH_STACK(1);
		TOP.u = IP.u - 1;
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
		break;
	case XIS::SVB:
		// [ ]...[B][C][SP]
		// A^          BC^
		PUSH_STACK(1);
		TOP.u = B.u;
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u;
		B.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::SVC:
		// [ ]...[ ]...[C][SP]
		// A^    B^        C^
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::LDA:
		SP = ATN(A,  0);
		C  = ATN(A, -1);
		B  = ATN(A, -2);
		A  = ATN(A, -3);
		POP_STACK(4);
		break;
	case XIS::LDB:
		SP = ATN(B,  0);
		C  = ATN(B, -1);
		B  = ATN(B, -2);
		POP_STACK(3);
		break;
	case XIS::LDC:
		SP = ATN(C,  0);
		C  = ATN(C, -1);
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
	return RAM[IP.u];
}

void Computer::Run( void )
{
	while (Cycle().u != XIS::HALT) {}
}

void Computer::Run(uint32_t ms)
{
	int64_t cycles = (int64_t(m_cycles_per_second) * 1000) / ms;
	while (cycles-- > 0 && Cycle().u != XIS::HALT) {}
}

bool Computer::IsAvailablePort(U8 port) const
{
	return m_relay[port] == nullptr;
}

void Computer::Connect(Device &device, U8 port)
{
	m_relay.RelayConnect(device, port);
}

void Computer::Disconnect(U8 port)
{
	m_relay.RelayDisconnect(port);
}

U16 Computer::InstructionPointer( void ) const
{
	return IP.u;
}

U16 Computer::StackPointer( void ) const
{
	return SP.u;
}

U16 Computer::StackOffsetA( void ) const
{
	return A.u;
}

U16 Computer::StackOffsetB( void ) const
{
	return B.u;
}

U16 Computer::StackOffsetC( void ) const
{
	return C.u;
}
