#ifndef XIS_H
#define XIS_H

#define TOSTR(X) #X

struct XIS
{
	enum Enum
	{
		// IDENTITY BITS
		// #1 Stack pointer manipulation
		// #2 Instruction pointer manipulation
		// #3 Address manipulation
		// #4 Arithmetic
		// #5 Tests
		// #6 Signed
		// #7 Register access
		// #8 UNUSED
		NOP    = 0,  // nothing
		PUT    = 0b10000000 + ( 1<<8), // put a constant on the stack (use EREL/CREL + AT if you want to transform a constant address to a value)
		PUTS   = 0b10000010 + ( 2<<8), // put the absolute address of the top stack element on the stack
		PUTI   = 0b10000010 + ( 3<<8), // put the absolute address of the current instruction on the stack
		EREL   = 0b00100000 + ( 4<<8), // transform constant on stack to address relative to EP
		CREL   = 0b00100000 + ( 5<<8), // transform constant on stack to address relative to CP
		AT     = 0b00100000 + ( 6<<8), // dereference top value on the stack as an absolute address (use EREL/CREL to transform relative address to absolute)
		JMP    = 0b01000000 + ( 7<<8), // jump to top value (absolute address) on stack (use EREL/CREL to transform relative address to absolute), collapse stack by 1
		SKIP   = 0b01000000 + ( 8<<8), // increment/decrement current IP by an amount
		ADD    = 0b10010000 + ( 9<<8), // add top and top-1, collapse stack by 1
		SUB    = 0b10010000 + (10<<8), // subtract top from top-1, collapse stack by 1
		MUL    = 0b10010000 + (11<<8), // multiply top and top-1, collapse stack by 1
		DIV    = 0b10010000 + (12<<8), // divide top by top-1, collapse stack by 1
		MOD    = 0b10010000 + (13<<8), // modulus top by top-1, collapse stack by 1
		IADD   = 0b10010100 + (14<<8), // add top and top-1, collapse stack by 1 (signed)
		ISUB   = 0b10010100 + (15<<8), // subtract top from top-1, collapse stack by 1 (signed)
		IMUL   = 0b10010100 + (16<<8), // multiply top and top-1, collapse stack by 1 (signed)
		IDIV   = 0b10010100 + (17<<8), // divide top by top-1, collapse stack by 1 (signed)
		IMOD   = 0b10010100 + (18<<8), // modulus top by top-1, collapse stack by 1 (signed)
		INEG   = 0b00010100 + (19<<8), // negate the top value (i.e. a = -a)
		LSH    = 0b10010000 + (20<<8), // left shift bits in top-1 by top, collapse stack by 1
		RSH    = 0b10010000 + (21<<8), // right shift buts in top-1 by top, collapse stack by 1
		AND    = 0b10010000 + (22<<8), // bitwise and top and top-1, collapse stack by 1
		OR     = 0b10010000 + (23<<8), // bitwise or top and top-1, collapse stack by 1
		XOR    = 0b10010000 + (24<<8), // bitwise xor top and top-1, collapse stack by 1
		NOT    = 0b00010000 + (25<<8), // bitwise invert the top value
		EQ     = 0b10001000 + (26<<8), // test top-1 == top, collapse stack by 1
		NE     = 0b10001000 + (27<<8), // test between top-1 != top, collapse stack by 1
		LE     = 0b10001000 + (28<<8), // test top-1 <= top, collapse stack by 1
		GE     = 0b10001000 + (29<<8), // test top-1 >= top, collapse stack by 1
		LT     = 0b10001000 + (30<<8), // test top-1 < top, collapse stack by 1
		GT     = 0b10001000 + (31<<8), // test top-1 > top, collapse stack by 1
		IEQ    = 0b10001100 + (32<<8), // test top-1 == top, collapse stack by 1 (signed)
		INE    = 0b10001100 + (33<<8), // test between top-1 != top, collapse stack by 1 (signed)
		ILE    = 0b10001100 + (34<<8), // test top-1 <= top, collapse stack by 1 (signed)
		IGE    = 0b10001100 + (35<<8), // test top-1 >= top, collapse stack by 1 (signed)
		ILT    = 0b10001100 + (36<<8), // test top-1 < top, collapse stack by 1 (signed)
		IGT    = 0b10001100 + (37<<8), // test top-1 > top, collapse stack by 1 (signed)
		DO     = 0b11001000 + (38<<8), // Skips ahead ONE instruction if top stack value is 0
		CEP    = 0b10000010 + (39<<8), // begins a new program (saves registers including program pointer) -> Create Entry Point (transforms top 4 stack elements as offsets to saved registers)
		REP    = 0b11000010 + (40<<8), // ends program by restoring all pointers (SP, IP, CP, EP)          -> Restore Entry Point
		CCP    = 0b10000010 + (41<<8), // create restore point (saves SP, IP, CP)                          -> Create Call Point (transforms top 3 stack elements as offsets to saved registers)
		RCP    = 0b11000010 + (42<<8), // restore from restore point (ends function)                       -> Restore Call Point
		PUSH   = 0b10000000 + (43<<8), // increases the SP by the top value - 1
		POP    = 0b10000000 + (44<<8), // decreases the SP by the top value
		TOSS   = 0b10000000 + (45<<8), // remove a constant from the stack
		MOVD   = 0b10000000 + (46<<8), // remove a constant from the stack and store it in specified absolute address (use EREL/CREL to transform relative address to absolute) by storing the top stack entry as a value at the address in the second topmost stack entry. Collapses the stack two entries.
		MOVU   = 0b10000000 + (47<<8), // remove a constant from the stack and store it in specified absolute address (use EREL/CREL to transform relative address to absolute) by storing the second topmost stack entry as a value at the address in the top stack entry. Collapses the stack two entries.
		PEEK   = 0b00000000 + (48<<8), // store top constant from stack in specified absolute address (use EREL/CREL to transform relative address to absolute)
		HALT   = 0b00000000 + (49<<8), // halt execution (deprecate with power controller)

		IP     = 0b00000010 + (50<<8), // puts PIP+IP on the top stack
		SP     = 0b00000010 + (51<<8), // puts PSP+FSP+SP on the top stack
		FSP    = 0b00000010 + (52<<8), // puts PSP+FSP on the top stack
		PSP    = 0b00000010 + (53<<8), // puts PSP on the top stack

		SAVP   = 0b10000010 + (54<<8), // save program register state
		RSTP   = 0b11000010 + (55<<8), // restore program register state
		SAVF   = 0b10000010 + (56<<8), // save frame register state
		RSTF   = 0b11000010 + (57<<8), // restore frame register state

		CJMP   = 0b01001000 + (58<<8), // Jump to the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.
		CSKIP  = 0b01001000 + (59<<8), // Skup ahead by an amount as given by the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.
		CNJMP  = 0b01001000 + (60<<8), // Jump to the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.
		CNSKIP = 0b01001000 + (61<<8), // Skup ahead by an amount as given by the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.

		DUP    = 0b10000000 + (62<<8), // Duplicates the top value on the stack and puts it on the top.

		COUNT = 63 // The number of instructions in the instruction set.
	};
};

static const char *ISTR[XIS::COUNT] = {
	TOSTR(NOP),
	TOSTR(PUT),
	TOSTR(PUTS),
	TOSTR(PUTI),
	TOSTR(EREL),
	TOSTR(CREL),
	TOSTR(AT),
	TOSTR(JMP),
	TOSTR(SKIP),
	TOSTR(ADD),
	TOSTR(SUB),
	TOSTR(MUL),
	TOSTR(DIV),
	TOSTR(MOD),
	TOSTR(IADD),
	TOSTR(ISUB),
	TOSTR(IMUL),
	TOSTR(IDIV),
	TOSTR(IMOD),
	TOSTR(INEG),
	TOSTR(LSH),
	TOSTR(RSH),
	TOSTR(AND),
	TOSTR(OR),
	TOSTR(XOR),
	TOSTR(NOT),
	TOSTR(EQ),
	TOSTR(NE),
	TOSTR(LE),
	TOSTR(GE),
	TOSTR(LT),
	TOSTR(GT),
	TOSTR(IEQ),
	TOSTR(INE),
	TOSTR(ILE),
	TOSTR(IGE),
	TOSTR(ILT),
	TOSTR(IGT),
	TOSTR(DO),
	TOSTR(CEP),
	TOSTR(REP),
	TOSTR(CCP),
	TOSTR(RCP),
	TOSTR(PUSH),
	TOSTR(POP),
	TOSTR(TOSS),
	TOSTR(MOVD),
	TOSTR(MOVU),
	TOSTR(PEEK),
	TOSTR(HALT),
	TOSTR(IP),
	TOSTR(SP),
	TOSTR(FSP),
	TOSTR(PSP),
	TOSTR(SAVP),
	TOSTR(RSTP),
	TOSTR(SAVF),
	TOSTR(RSTF),
	TOSTR(CJMP),
	TOSTR(CSKIP),
	TOSTR(CNJMP),
	TOSTR(CNSKIP),
	TOSTR(DUP)
};

#endif // XIS_H
