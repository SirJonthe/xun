#include <iostream>
#include <iomanip>
#include <fstream>
#include "xun.h"

#define ERR RAM[this->X4].u

#define S RAM[this->SP].u
#define VAR_S RAM[S].u
#define INC_S S++
#define DEC_S S--

#define I RAM[this->IP].u
#define INC_I I--
#define DEC_I I++
#define READ_I RAM[INC_I].u
#define PEEK_I RAM[I].u

#define READ1 X.u = READ_I
#define READ2 X.u = READ_I; Y.u = READ_I
#define READ3 X.u = READ_I; Y.u = READ_I; Z.u = READ_I
#define LIT_X X.u
#define VAR_X RAM[LIT_X].u
#define VAR_XS RAM[LIT_X].s
#define LIT_Y Y.u
#define VAR_Y RAM[LIT_Y].u
#define VAR_YS RAM[LIT_Y].s
#define LIT_Z Z.u
#define VAR_Z RAM[LIT_Z].u

#define PEEK1 X.u = RAM[I - 1].u
#define PEEK2 X.u = RAM[I - 1].u; Y.u = RAM[I - 2].u
#define PEEK3 X.u = RAM[I - 1].u; Y.u = RAM[I - 2].u; Z.u = RAM[I - 3].u

#define VIDEO_MEM 8192
#define FONT_MEM  760

enum ParamType
{
	VAR1 = 1,
	LIT1 = VAR1 << 1,
	VAR2 = VAR1 << 2,
	LIT2 = VAR1 << 3,
	VAR3 = VAR1 << 4,
	LIT3 = VAR1 << 5
};

void xerx::NanoController::ResetRegisters( void )
{
	ERR = 0;
	S = SP_START;
	I = IP_START;
}

void xerx::NanoController::PrintInstruction(const char *op, xerx::uword a, xerx::uword b, xerx::uword c, unsigned int params, bool fill_blank) const
{
	std::cout << std::setw(4) << op;
	xerx::uword reg[3] = { a, b, c };
	int i;
	for (i = 0; i < 3 && (params & 3) > 0; ++i) {
		if ((params & 3) == VAR1) {
			std::cout << std::setw(6) << RAM[reg[i]].u << "@" << std::setw(5) << reg[i];
		} else if ((params & 3) == LIT1) {
			std::cout << std::setw(12) << reg[i];
		}
		params >>= 2;
		if (params > 0) {
			std::cout << ",";
		}
	}
	if (fill_blank) {
		for (; i < 3; ++i) {
			std::cout << "             ";
		}
	}
}

void xerx::NanoController::PrintInstruction(const char *op, xerx::uword a, xerx::uword b, unsigned int params, bool fill_blank) const
{
	PrintInstruction(op, a, b, 0, params, fill_blank);
}

void xerx::NanoController::PrintInstruction(const char *op, xerx::uword a, unsigned int params, bool fill_blank) const
{
	PrintInstruction(op, a, 0, 0, params, fill_blank);
}

void xerx::NanoController::PrintInstruction(const char *op, bool fill_blank) const
{
	PrintInstruction(op, 0, 0, 0, 0, fill_blank);
}

void xerx::NanoController::PrintRegisters(xerx::uword dst) const
{
	PrintInstruction("DST=", dst, VAR1, false);
	std::cout << "; ";

	PrintInstruction("SP=", S, VAR1, false);
	std::cout << "; ";
	PrintInstruction("IP=", I, VAR1, false);
	std::cout << "; ";

	PrintInstruction("X1=", X1, VAR1, false);
	std::cout << "; ";
	PrintInstruction("X2=", X2, VAR1, false);
	std::cout << "; ";
	PrintInstruction("X3=", X3, VAR1, false);
	std::cout << "; ";
	PrintInstruction("X4=", X4, VAR1, false);

	// don't print A, B, C, since these are only modified by user
}

void xerx::NanoController::HACK_LoadFont( void )
{
	// This is a temporary hack
	// Ideally, the ROM detects a storage device and uses XASM to load the font to RAM
	// Or I provide a prebaked ROM image with the font in it, and XASM is used to load the font to RAM
	std::ifstream fin("BIOS/font.bin", std::ios::binary);
	if (fin.is_open()) {
		fin.read(((char*)RAM) + VIDEO_MEM, FONT_MEM);
	} else {
		std::cout << "Font not loaded" << std::endl;
	}
}

bool xerx::NanoController::Function(xerx::uword, xerx::uword&, xerx::word_t*)
{
	return false;
}

xerx::NanoController::NanoController( void ) : Device(0xFFFF, "Xerxes Unified Nanocontroller", xerx::Device::Compute), dev_clock()
{
	xerx::Device::Connect(*this, dev_clock);
}

void xerx::NanoController::HACK_Load(const xerx::Binary &bin)
{
	// this is something that the computer should do at boot, IN ASM, not in C++
	// assembling source should be done via command "XASM SRC.XASM"
	// running binary should be done via command "RUN PRG.BIN"
	xerx::udword b = 0;
	xerx::uword  m = IP_START;
	while (b < bin.GetSize()) {
		RAM[m--] = bin[b++];
	}
	ResetRegisters();
}

void xerx::NanoController::Reset( void )
{
	// Power on

	// Load BIOS in C++, BIOS contains backed in code and assets
	// Run machine iteratively
	// Startup routine involves clearing memory and going to command prompt

	for (unsigned int i = 0; i < MEM_SIZE; ++i) {
		RAM[i].u = 0;
	}
	ResetRegisters();
	RAM[I].u = (xerx::uword)xerx::InstructionSet::HALT;

	// Time for hacks
	HACK_LoadFont();
}

bool xerx::NanoController::Run( void )
{
	word_t X, Y, Z;
	switch (READ_I) {
	case xerx::InstructionSet::NOP:
		break;
//	case xerx::InstructionSet::INC:
//		READ1;
//		VAR_X += 1;
//		break;
//	case xerx::InstructionSet::DEC:
//		READ1;
//		VAR_X -= 1;
//		break;
//	case xerx::InstructionSet::JMP:
//		READ1;
//		I = VAR_X;
//		break;
//	case xerx::InstructionSet::SAV:
//		READ1;
//		INC_S;
//		VAR_S = VAR_X;
//		break;
//	case xerx::InstructionSet::USE:
//		READ1;
//		VAR_X = VAR_S;
//		DEC_S;
//		break;
	case xerx::InstructionSet::PUSH:
		READ1;
		S += LIT_X;
		break;
	case xerx::InstructionSet::POP:
		READ1;
		S -= LIT_X;
		break;
	case xerx::InstructionSet::NOT:
		READ1;
		VAR_X = ~VAR_X;
		break;
	case xerx::InstructionSet::NEG:
		READ1;
		VAR_XS = -VAR_XS;
		break;
	case xerx::InstructionSet::DREF:
		READ2;
		VAR_X = RAM[VAR_Y].u;
		break;
	case xerx::InstructionSet::SET:
		READ2;
		VAR_X = LIT_Y;
		break;
	case xerx::InstructionSet::MOV:
		READ2;
		VAR_X = VAR_Y;
		break;
//	case xerx::InstructionSet::DSET:
//		READ2;
//		RAM[VAR_X].u = LIT_Y;
//		break;
	case xerx::InstructionSet::DMOV:
		READ2;
		RAM[VAR_X].u = VAR_Y;
		break;
	case xerx::InstructionSet::AND:
		READ2;
		VAR_X &= VAR_Y;
		break;
	case xerx::InstructionSet::OR:
		READ2;
		VAR_X |= VAR_Y;
		break;
	case xerx::InstructionSet::XOR:
		READ2;
		VAR_X ^= VAR_Y;
		break;
	case xerx::InstructionSet::LSU:
		READ2;
		VAR_X <<= VAR_Y;
		break;
	case xerx::InstructionSet::RSU:
		READ2;
		VAR_X >>= VAR_Y;
		break;
	case xerx::InstructionSet::LSS:
		READ2;
		VAR_XS <<= VAR_YS;
		break;
	case xerx::InstructionSet::RSS:
		READ2;
		VAR_XS >>= VAR_YS;
		break;
	case xerx::InstructionSet::ADD:
		READ2;
		VAR_X += VAR_Y;
		break;
	case xerx::InstructionSet::SUB:
		READ2;
		VAR_X -= VAR_Y;
		break;
	case xerx::InstructionSet::MUL:
		READ2;
		VAR_X *= VAR_Y;
		break;
	case xerx::InstructionSet::DIV:
		READ2;
		if (VAR_Y != 0) { VAR_X /= VAR_Y; }
		else {
			ERR |= Err_DivZero;
			return false;
		}
		break;
	case xerx::InstructionSet::MOD:
		READ2;
		if (VAR_Y != 0) { VAR_X %= VAR_Y; }
		else {
			ERR |= Err_DivZero;
			return false;
		}
		break;
	case xerx::InstructionSet::INT:
		READ2;
		LIT_Z = VAR_Y & Device::FUNC_MASK; // function number
		LIT_Y = (VAR_Y & Device::PORT_MASK) >> 8; // port number
		if (!Port(LIT_Y).IsValid() || !Port(LIT_Y)->Operate(LIT_Z, VAR_X, RAM)) {
			if (LIT_Z >= 16) { // device specific functions generate error when
				ERR |= Err_Hardware;
			} else {  // standard functions return 0 when the port is not connected to a device
				VAR_X = 0;
			}
			return false;
		}
		break;
	case xerx::InstructionSet::JE:
		READ3;
		if (VAR_X == VAR_Y) { I = VAR_Z; }
		break;
	case xerx::InstructionSet::JNE:
		READ3;
		if (VAR_X != VAR_Y) { I = VAR_Z; }
		break;
	case xerx::InstructionSet::JL:
		READ3;
		if (VAR_X < VAR_Y) { I = VAR_Z; }
		break;
	case xerx::InstructionSet::JLE:
		READ3;
		if (VAR_X <= VAR_Y) { I = VAR_Z; }
		break;
	case xerx::InstructionSet::JG:
		READ3;
		if (VAR_X > VAR_Y) { I = VAR_Z; }
		break;
	case xerx::InstructionSet::JGE:
		READ3;
		if (VAR_X >= VAR_Y) { I = VAR_Z; }
		break;
	case xerx::InstructionSet::HALT:
		DEC_I;
		return false;
	default:
		ERR |= Err_Unknown;
		DEC_I;
		return false;
	}
	return true;
}

bool xerx::NanoController::DebugRun( void )
{
	xerx::word_t X, Y, Z;
	X.u = 0;
	const xerx::word_t *RAM = this->RAM; // make this constant to detect possible write errors
	switch (PEEK_I) {
	case xerx::InstructionSet::NOP:
		PrintInstruction("NOP");
		break;
//	case xerx::InstructionSet::INC:
//		PEEK1;
//		PrintInstruction("INC", LIT_X, VAR1);
//		break;
//	case xerx::InstructionSet::DEC:
//		PEEK1;
//		PrintInstruction("DEC", LIT_X, VAR1);
//		break;
//	case xerx::InstructionSet::JMP:
//		PEEK1;
//		PrintInstruction("JMP", LIT_X, VAR1);
//		break;
//	case xerx::InstructionSet::SAV:
//		PEEK1;
//		PrintInstruction("SAV", LIT_X, VAR1);
//		break;
//	case xerx::InstructionSet::USE:
//		PEEK1;
//		PrintInstruction("USE", LIT_X, VAR1);
//		break;
	case xerx::InstructionSet::PUSH:
		PEEK1;
		PrintInstruction("PUSH", LIT_X, LIT1);
		break;
	case xerx::InstructionSet::POP:
		PEEK1;
		PrintInstruction("POP", LIT_X, LIT1);
		break;
	case xerx::InstructionSet::NOT:
		PEEK1;
		PrintInstruction("NOT", LIT_X, VAR1);
		break;
	case xerx::InstructionSet::NEG:
		PEEK1;
		PrintInstruction("NEG", LIT_X, VAR1);
		break;
	case xerx::InstructionSet::DREF:
		PEEK2;
		LIT_Y = VAR_Y;
		PrintInstruction("DREF", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::SET:
		PEEK2;
		PrintInstruction("SET", LIT_X, LIT_Y, VAR1|LIT2);
		break;
	case xerx::InstructionSet::MOV:
		PEEK2;
		PrintInstruction("MOV", LIT_X, LIT_Y, VAR1|VAR2);
		break;
//	case xerx::InstructionSet::DSET:
//		PEEK2;
//		PrintInstruction("DSET", LIT_X, LIT_Y, VAR1|LIT2);
//		break;
	case xerx::InstructionSet::DMOV:
		PEEK2;
		LIT_X = VAR_X;
		PrintInstruction("DMOV", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::AND:
		PEEK2;
		PrintInstruction("AND", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::OR:
		PEEK2;
		PrintInstruction("OR", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::XOR:
		PEEK2;
		PrintInstruction("XOR", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::LSU:
		PEEK2;
		PrintInstruction("LSU", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::RSU:
		PEEK2;
		PrintInstruction("RSU", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::LSS:
		PEEK2;
		PrintInstruction("LSS", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::RSS:
		PEEK2;
		PrintInstruction("RSS", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::ADD:
		PEEK2;
		PrintInstruction("ADD", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::SUB:
		PEEK2;
		PrintInstruction("SUB", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::MUL:
		PEEK2;
		PrintInstruction("MUL", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::DIV:
		PEEK2;
		PrintInstruction("DIV", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::MOD:
		PEEK2;
		PrintInstruction("MOD", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::INT:
		PEEK2;
		PrintInstruction("INT", LIT_X, LIT_Y, VAR1|VAR2);
		break;
	case xerx::InstructionSet::JE:
		PEEK3;
		PrintInstruction("JE", LIT_X, LIT_Y, LIT_Z, VAR1|VAR2|VAR3);
		break;
	case xerx::InstructionSet::JNE:
		PEEK3;
		PrintInstruction("JNE", LIT_X, LIT_Y, LIT_Z, VAR1|VAR2|VAR3);
		break;
	case xerx::InstructionSet::JL:
		PEEK3;
		PrintInstruction("JL", LIT_X, LIT_Y, LIT_Z, VAR1|VAR2|VAR3);
		break;
	case xerx::InstructionSet::JLE:
		PEEK3;
		PrintInstruction("JLE", LIT_X, LIT_Y, LIT_Z, VAR1|VAR2|VAR3);
		break;
	case xerx::InstructionSet::JG:
		PEEK3;
		PrintInstruction("JG", LIT_X, LIT_Y, LIT_Z, VAR1|VAR2|VAR3);
		break;
	case xerx::InstructionSet::JGE:
		PEEK3;
		PrintInstruction("JGE", LIT_X, LIT_Y, LIT_Z, VAR1|VAR2|VAR3);
		break;
	case xerx::InstructionSet::HALT:
		PrintInstruction("HALT");
		break;
	default:
		PrintInstruction("ERROR");
		break;
	}

	bool ret_val = Run();

	std::cout << " [";
	PrintRegisters(X.u);
	std::cout << "]" << std::endl;

	return ret_val;
}

void xerx::NanoController::RunCommand(const mtlChars &cmd)
{
}

xerx::uword xerx::NanoController::ErrorState( void ) const
{
	return ERR;
}

xerx::word_t xerx::NanoController::Debug_RAM(xerx::uword i) const
{
	return RAM[i];
}

xerx::word_t &xerx::NanoController::Debug_RAM(xerx::uword i)
{
	return RAM[i];
}

void xerx::NanoController::Debug_PrintDevices( void ) const
{
	std::cout << GetIDStr() << ", " << std::hex << std::setfill('0') << std::setw(4) << GetID() << std::endl;
	for (int i = 0; i < MAX_CONNECTIONS; ++i) {
		std::cout << std::setfill(' ') << std::dec << std::setw(2) << i << ": ";
		if (Port(i).IsValid()) {
			std::cout << std::setw(32) << Port(i)->GetIDStr() << ", " << std::setfill('0') << std::setw(4) << std::hex << Port(i)->GetID();
		} else {
			std::cout << "NO DEVICE";
		}
		std::cout << std::endl;
	}
	std::cout << std::dec << std::setfill(' ');
}
