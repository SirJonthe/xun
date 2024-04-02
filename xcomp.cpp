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

Computer::Computer( void ) : Device("XERXES(tm) Unified Nanocontroller [XUN(tm)]", 0xffff), m_storage(1<<21), m_clock_ps(0)
{
	SetCyclesPerSecond(10000000U);

	Device::Connect(m_ports[0], m_power_controller);
	Device::Connect(m_ports[1], m_storage);
}

// Flash memory with a program.
// Ideally should be an OS that can load other programs,
// but can be any program.
void Computer::BootDisk(const XWORD *bin, U16 bin_count, bool debug)
{
	IP.u = A.u = B.u = C.u = SP.u = 0;
	for (U16 i = 0; i < bin_count; ++SP.u, ++i) {
		AT(SP) = bin[i];
	}
	if (debug) {
		for (unsigned i = SP.u; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = XIS::HALT;
		}
	} else {
		for (unsigned i = SP.u; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = U16(rand());
		}
	}
	SP.u -= 1;
}

void Computer::PowerOff( void )
{
	if (IsPoweredOn()) {
		m_clock_ps = 0;
		A.u = B.u = C.u = SP.u = IP.u = ERR.u = 0;
		for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = 0;
		}
		for (uint32_t i = 0; i < 16; ++i) {
			m_ports[i].Shutdown();
		}
		Device::Shutdown();
	}
}

void Computer::PowerOn( void )
{
	if (IsPoweredOff()) {
		Device::Boot();
		for (uint32_t i = 0; i < 16; ++i) {
			m_ports[i].Boot();
		}
		A.u = B.u = C.u = SP.u = IP.u = ERR.u = 0;
		for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = U16(rand());
		}
	}
}

void Computer::PowerToggle( void )
{
	if (IsPoweredOff()) {
		PowerOn();
	} else {
		PowerOff();
	}
}

void Computer::PowerCycle( void )
{
	if (IsPoweredOn()) {
		PowerOff();
		PowerOn();
	}
}

void Computer::SetCyclesPerSecond(uint32_t hz)
{
	// TODO optimally we adjust 'hz' down to become an even multiple of 1000000000000
	if (uint64_t(hz) < 1000000000000ULL) {
		m_cycles_per_second = hz;
		m_ps_per_cycle = 1000000000000ULL / hz;
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
	m_clock_ps += m_ps_per_cycle;
	const U16 I = READI;
	switch (I) {
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
	case XIS::MOD:
		if (TOP.u != 0)      { LST.u %= TOP.u; }
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
	case XIS::IMOD:
		if (TOP.i != 0)     { LST.i %= TOP.i; }
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
		LST.u = LST.u == TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::NE:
		LST.u = LST.u != TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::LE:
		LST.u = LST.u <= TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::GE:
		LST.u = LST.u >= TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::LT:
		LST.u = LST.u < TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::GT:
		LST.u = LST.u > TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::IEQ:
		LST.u = LST.i == TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::INE:
		LST.u = LST.i != TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::ILE:
		LST.u = LST.i <= TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::IGE:
		LST.u = LST.i >= TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::ILT:
		LST.u = LST.i < TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::IGT:
		LST.u = LST.i > TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::PORT:
		P.u = TOP.u % 16;
		POP_STACK(1);
		break;
//	case XIS::POLL: break; // Receive data from device on selected port.
//	case XIS::PASS:
//		if (m_ports[P.u].IsValid()) {
//			(*m_ports[P.u].GetOther())->Respond(TOP);
//		}
//		POP_STACK(1);
//		break; // Send top word on stack to selected port.
	case XIS::HWID:
		PUSH_STACK(1);
		TOP.u = (m_ports[P.u].GetConnectedDevice() != nullptr) ? (m_ports[P.u].GetConnectedDevice()->GetHWID()) : 0;
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
	case XIS::CLOCK:
		PUSH_STACK(1);
		TOP.u = U16(m_clock_ps / 1000000000ULL);
		break;
	case XIS::BIN:
		READI;
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
		TOP.u = SP.u - 4;
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
		TOP.u = SP.u - 3;
		B.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::SVC:
		// [ ]...[ ]...[C][SP]
		// A^    B^        C^
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u - 2;
		C.u = SP.u;
		break;
	case XIS::LDA:
		SP = ATN(A,  0);
		C  = ATN(A, -1);
		B  = ATN(A, -2);
		A  = ATN(A, -3);
		//POP_STACK(4);
		break;
	case XIS::LDB:
		SP = ATN(B,  0);
		C  = ATN(B, -1);
		B  = ATN(B, -2);
		//POP_STACK(3);
		break;
	case XIS::LDC:
		SP = ATN(C,  0);
		C  = ATN(C, -1);
		//POP_STACK(2);
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
	return XWORD{I};
}

void Computer::Run( void )
{
	while (Cycle().u != XIS::HALT) {}
}

void Computer::Run(uint32_t ms)
{
	uint64_t cycles = (uint64_t(m_cycles_per_second) * 1000) / ms;
	while (cycles-- >= 1 && Cycle().u != XIS::HALT) {}
}

bool Computer::IsAvailablePort(U8 port) const
{
	return m_ports[port % 16].GetConnectedDevice() == nullptr;
}

void Computer::Connect(Device &device, U8 port)
{
	Disconnect(port);
	Device::Connect(m_ports[port % 16], device);
}

void Computer::Disconnect(U8 port)
{
	m_ports[port % 16].Disconnect();
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
