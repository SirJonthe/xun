#include "hw/xhwids.h"
#include "xcomp.h"

#define AT(x)         RAM[x.u]
#define ATN(x,n)      RAM[U16(x.u + (n))]

#define READI         RAM[IP.u++].u

#define TOP           RAM[SP.u]
#define LST           RAM[U16(SP.u - 1)]
#define SAT(n)        RAM[U16(SP.u + (n))]

#define POP_STACK(n)  SP.u -= (n)
#define PUSH_STACK(n) SP.u += (n)

#define XUN_NAME "XERXES(tm) Unified Nanocontroller [XUN(tm)]"

Computer::IOPort::IOPort( void ) : Device(XUN_NAME, XHWID_XUN)
{}

Computer::IOPort *Computer::GetPort(U16 index)
{
	return index < NUM_PORTS ? &m_ports[index] : NULL;
}

const Computer::IOPort *Computer::GetPort(U16 index) const
{
	return index < NUM_PORTS ? &m_ports[index] : NULL;
}

void Computer::SetError(U16 code)
{
	ERR.u |= U16(1 << code);
	// TODO: IRQ here maybe?
}

void Computer::ClearError(U16 code)
{
	ERR.u &= ~U16(1 << code);
}

void Computer::Output(U16 port_index, U16 header_addr, U16 data_addr)
{
	if (GetPort(port_index) != NULL) {
		Packet p;
		for (XWORD i = XWORD{0}; i.u < Packet::HEADER_WORD_SIZE; ++i.u) {
			p.header[i.u] = ATN(i, header_addr).u;
		}
		for (XWORD i = XWORD{0}; i.u < Packet::PAYLOAD_WORD_SIZE; ++i.u) {
			p.payload[i.u] = ATN(i, data_addr).u;
		}
		GetPort(port_index)->Output(p);
	} else {
		SetError(ERR_IO);
	}
}

void Computer::Ack(U16 port_index)
{
	if (GetPort(port_index) != NULL) {
		GetPort(port_index)->Ack();
	} else {
		SetError(ERR_IO);
	}
}

void Computer::Peek(U16 port_index, U16 header_addr, U16 data_addr)
{
	if (GetPort(port_index) != NULL) {
		Packet p = GetPort(port_index)->Peek();
		for (XWORD i = XWORD{0}; i.u < Packet::HEADER_WORD_SIZE; ++i.u) {
			ATN(i, header_addr).u = p.header[i.u];
		}
		for (XWORD i = XWORD{0}; i.u < Packet::PAYLOAD_WORD_SIZE; ++i.u) {
			ATN(i, data_addr).u = p.payload[i.u];
		}
	} else {
		SetError(ERR_IO);
	}
}

bool Computer::IsFull(U16 port_index)
{
	if (GetPort(port_index) != NULL && GetPort(port_index)->GetConnectedDevice() != NULL) {
		return GetPort(port_index)->GetConnectedDevice()->IsFull();
	} else {
		SetError(ERR_IO);
	}
	return true;
}

bool Computer::IsEmpty(U16 port_index)
{
	if (GetPort(port_index) != NULL) {
		return GetPort(port_index)->IsEmpty();
	} else {
		SetError(ERR_IO);
	}
	return true;
}


void Computer::DoPowerOff( void )
{
	A.u = B.u = C.u = SP.u = IP.u = ERR.u = 0;
	if (!m_debug) {
		I.u = 0;
		for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = 0;
		}
	}
	for (uint32_t i = 0; i < 16; ++i) {
		m_ports[i].PowerOff();
	}
}

void Computer::DoPowerOn( void )
{
	for (uint32_t i = 0; i < 16; ++i) {
		m_ports[i].PowerOn();
	}
	A.u = B.u = C.u = SP.u = IP.u = I.u = ERR.u = 0;
	for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
		AT(XWORD{U16(i)}).u = U16(rand());
	}
}

void Computer::DoCycle( void )
{
	I.u = READI;
	switch (I.u) {
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
		else if (LST.u != 0) { LST.u = U_MAX; SetError(ERR_DIV0); }
		else                 { LST.u = 0; }
		POP_STACK(1);
		break;
	case XIS::MOD:
		if (TOP.u != 0)      { LST.u %= TOP.u; }
		else if (LST.u != 0) { LST.u = U_MAX; SetError(ERR_DIV0); }
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
		else if (LST.i < 0) { LST.i = I_MIN; SetError(ERR_DIV0); }
		else if (LST.i > 0) { LST.i = I_MAX; SetError(ERR_DIV0); }
		else                { LST.i = 0; }
		POP_STACK(1);
		break;
	case XIS::IMOD:
		if (TOP.i != 0)     { LST.i %= TOP.i; }
		else if (LST.i < 0) { LST.i = I_MIN; SetError(ERR_DIV0); }
		else if (LST.i > 0) { LST.i = I_MAX; SetError(ERR_DIV0); }
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
		P.u = TOP.u;
		POP_STACK(1);
		break;
	case XIS::PEND:
		PUSH_STACK(1);
		TOP.u = IsEmpty(P.u) ? 0 : 1;
		break;
	case XIS::FULL:
		PUSH_STACK(1);
		TOP.u = IsFull(P.u) ? 1 : 0;
		break;
	case XIS::ACK:
		Ack(P.u);
		break;
	case XIS::POLL: {
		U16 d = TOP.u, h = LST.u;
		POP_STACK(2);
		Peek(P.u, h, d);
		break;
	}
	case XIS::PASS: {
		U16 d = TOP.u, h = LST.u;
		POP_STACK(2);
		Output(P.u, h, d);
		break;
	}
	case XIS::CPUID:
		PUSH_STACK(1);
		TOP.u = GetHWID();
		break;
	case XIS::DID:
		PUSH_STACK(1);
		TOP.u = GetDID();
		break;
	case XIS::HALT:
		--IP.u;
		PowerOff();
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
		TOP.u = GetClock();
		break;
	case XIS::TNS:
		PUSH_STACK(1);
		TOP.u = U16(GetHighPrecisionClock() % 1000ULL);
		break;
	case XIS::TUS:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 1000ULL) % 1000ULL);
		break;
	case XIS::TMS:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 1000000ULL) % 1000ULL);
		break;
	case XIS::TS:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 1000000000ULL) % 60ULL);
		break;
	case XIS::TM:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 60000000000ULL) % 60ULL);
		break;
	case XIS::TH:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 3600000000000ULL) % 24ULL);
		break;
	case XIS::TD:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 86400000000000ULL) % 7ULL);
		break;
	case XIS::TW:
		PUSH_STACK(1);
		TOP.u = U16(GetHighPrecisionClock() / 604800000000000ULL);
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
		// ...[A][B][C][SP]
		//            ABC^
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
		// ...[ ]...[B][C][SP]
		//    A^          BC^
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
		// ...[ ]...[ ]...[C][SP]
		//    A^    B^        C^
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
		break;
	case XIS::LDB:
		SP = ATN(B,  0);
		C  = ATN(B, -1);
		B  = ATN(B, -2);
		break;
	case XIS::LDC:
		SP = ATN(C,  0);
		C  = ATN(C, -1);
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
	case XIS::ERR:
		PUSH_STACK(1);
		TOP.u = ERR.u;
	case XIS::CERR:
		ClearError(TOP.u);
		POP_STACK(1);
	default:
		SetError(ERR_UNDEF);
		break;
	}
}

Computer::Computer(bool debug) : Device(XUN_NAME, XHWID_XUN), m_storage(1<<21), m_debug(debug)
{
	SetCyclesPerSecond(10000000U);

	Device::Connect(m_ports[0], m_PCU);
	Device::Connect(m_ports[1], m_tty);
	Device::Connect(m_ports[2], m_internal_reader);
	Device::Connect(m_ports[3], m_external_reader);

	m_PCU.PowerOn();
	m_tty.PowerOn();
	m_internal_reader.PowerOn();
	m_external_reader.PowerOn();

	m_internal_reader.Attach(&m_storage);
}

// Flash memory with a program.
// Ideally should be an OS that can load other programs,
// but can be any program.
void Computer::BootDisk(const XWORD *bin, U16 bin_count)
{
	IP.u = A.u = B.u = C.u = SP.u = I.u = 0;
	for (U16 i = 0; i < bin_count; ++SP.u, ++i) {
		AT(SP) = bin[i];
	}
	if (m_debug) {
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

bool Computer::AttachDisk(Disk &disk)
{
	return m_external_reader.Attach(&disk);
}

void Computer::EjectDisk( void )
{
	m_external_reader.Eject();
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

bool Computer::IsAvailablePort(U8 port) const
{
	return port < NUM_PORTS ? (m_ports[port % NUM_PORTS].GetConnectedDevice() == nullptr) : false;
}

void Computer::Connect(Device &device, U8 port)
{
	if (port < NUM_PORTS) {
		Disconnect(port);
		Device::Connect(m_ports[port % NUM_PORTS], device);
	}
}

void Computer::Disconnect(U8 port)
{
	if (port < NUM_PORTS) {
		m_ports[port % NUM_PORTS].Disconnect();
	}
}

const Device *Computer::GetDeviceAtPort(U8 port) const
{
	return port < NUM_PORTS && GetPort(port) != nullptr ? GetPort(port)->GetConnectedDevice() : nullptr;
}

U16 Computer::GetPortIndex( void ) const
{
	return P.u;
}

U16 Computer::InstructionPointer( void ) const
{
	return IP.u;
}

U16 Computer::Instruction( void ) const
{
	return I.u;
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