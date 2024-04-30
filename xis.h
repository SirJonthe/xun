#ifndef XIS_H
#define XIS_H

#define TOSTR(X) #X

struct XIS
{
	// TODO Huge issue. The switch-case in the Computer will most likely want to work on a sequential list of enums rather than values that are very far apart, since 0...n enums can be implemented as a jump table.
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
		// #8 I/O
		NOP    = 0,                    // nothing
		PUT    = 0b10000000 + ( 1<<8), // put a constant on the stack (use RLA/RLB/RLC + AT if you want to transform a constant address to a value)
		PUTS   = 0b10000010 + ( 2<<8), // put the absolute address of the top stack element on the stack
		PUTI   = 0b10000010 + ( 3<<8), // put the absolute address of the current instruction on the stack
		CLOCK  = 0b10000000 + ( 4<<8), // put the processor clock value on top of the stack. Expand stack by 1.
		BIN    = 0b00000000 + ( 5<<8), // skips reading the next instruction
		AT     = 0b00100000 + ( 6<<8), // dereference top value on the stack as an absolute address (use RLA/RLB/RLC to transform relative address to absolute)
		JMP    = 0b01000000 + ( 7<<8), // jump to top value (absolute address) on stack (use RLA/RLB/RLC to transform relative address to absolute), collapse stack by 1
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
		PORT   = 0b10001001 + (38<<8), // Select a port to perform I/O on. collapse stack by 1.
		POLL   = 0b10000001 + (39<<8), // Receive data from device on selected port. Uses two top values on stack as addresses to store packet header and packed payload. collapse stack by 2.
		PASS   = 0b10000001 + (40<<8), // Send top word on stack to selected port. Uses two top values on stack as addresses to read packet header and packed payload. collapse stack by 2.
		CPUID  = 0b10000000 + (41<<8), // Puts the CPU ID on the stack.
		PEND   = 0b10000001 + (42<<8), // Puts 1 on the stack if there is a pending message on the selected port, 0 otherwise.
		PUSH   = 0b10000000 + (43<<8), // increases the SP by the top value - 1
		POP    = 0b10000000 + (44<<8), // decreases the SP by the top value
		TOSS   = 0b10000000 + (45<<8), // remove a constant from the stack
		MOVD   = 0b10000000 + (46<<8), // remove a constant from the stack and store it in specified absolute address (use RLA/RLB/RLC to transform relative address to absolute) by storing the top stack entry as a value at the address in the second topmost stack entry. Collapses the stack two entries.
		MOVU   = 0b10000000 + (47<<8), // remove a constant from the stack and store it in specified absolute address (use RLA/RLB/RLC to transform relative address to absolute) by storing the second topmost stack entry as a value at the address in the top stack entry. Collapses the stack two entries.
		PEEK   = 0b00000000 + (48<<8), // store top constant from stack in specified absolute address (use RLA/RLB/RLC to transform relative address to absolute)
		HALT   = 0b00000000 + (49<<8), // halt execution (deprecate with power controller)
		ACK    = 0b10000001 + (50<<8), // Consume the top message on the current port.
		ERR    = 0b10000010 + (51<<8), // put the error register on the top of the stack. move stack by 1
		CERR   = 0b10000010 + (52<<8), // clear the error flag at the index indicated by the top value on the stack. collapse stack by 1.
		FULL   = 0b10000001 + (53<<8), // puts 1 on the stack if the device on the selected port's input buffer is full. 0 otherwise. Expand stack by 1.
		RES54  = 0b10000010 + (54<<8), // Reserved #54
		RES55  = 0b11000010 + (55<<8), // Reserved #55
		RES56  = 0b10000010 + (56<<8), // Reserved #56
		RES57  = 0b11000010 + (57<<8), // Reserved #57

		CJMP   = 0b01001000 + (58<<8), // Jump to the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.
		CSKIP  = 0b01001000 + (59<<8), // Skip ahead by an amount as given by the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.
		CNJMP  = 0b01001000 + (60<<8), // Jump to the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.
		CNSKIP = 0b01001000 + (61<<8), // Skip ahead by an amount as given by the top address if the top-1 value is not 0. Pop 2 values from the stack regardless.

		DUP    = 0b10000000 + (62<<8), // Duplicates the top value on the stack and puts it on the top.

		SVA    = 0b10000010 + (63<<8), // Save the A, B, C, and SP to the top of the stack. Set A, B, C to SP, and set SP to 0. Expand stack by 4.
		SVB    = 0b10000010 + (64<<8), // Save the B, C, and SP to the top of the stack.  Set B and C to SP, and set SP to 0. Expand stack by 3.
		SVC    = 0b10000010 + (65<<8), // Save the C and SP to the top of the stack.  Set A to SP, and set SP to 0. Expand stack by 2.
		LDA    = 0b11000010 + (66<<8), // Load A, B, C, and SP from the stack. Stack collapses by restored amount.
		LDB    = 0b11000010 + (67<<8), // Load B, C, and SP from the stack. Stack collapses by restored amount.
		LDC    = 0b11000010 + (68<<8), // Load C and SP from the stack. Stack collapses by restored amount.

		RLA    = 0b00100000 + (69<<8), // transform constant on stack to address relative to A
		RLB    = 0b00100000 + (70<<8), // transform constant on stack to address relative to B
		RLC    = 0b00100000 + (71<<8), // transform constant on stack to address relative to C

		TNS    = 0b10000000 + (72<<8), // Get nanoseconds (0-999). Expand stack by 1.
		TUS    = 0b10000000 + (73<<8), // Get microseconds (0-999). Expand stack by 1.
		TMS    = 0b10000000 + (74<<8), // Get milliseconds (0-999). Expand stack by 1.
		TS     = 0b10000000 + (75<<8), // Get seconds (0-59). Expand stack by 1.
		TM     = 0b10000000 + (76<<8), // Get minutes (0-59). Expand stack by 1.
		TH     = 0b10000000 + (77<<8), // Get hours (0-23). Expand stack by 1.
		TD     = 0b10000000 + (78<<8), // Get days (0-6). Expand stack by 1.
		TW     = 0b10000000 + (79<<8), // Get absolute number of weeks (0-30500 before wrap, approx. 600 years). Expans stack by 1.

		COUNT = 80                       // The number of instructions in the instruction set.
	};
};

#endif // XIS_H
