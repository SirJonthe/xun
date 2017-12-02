#ifndef XIS_H__
#define XIS_H__

namespace xerx
{

struct InstructionSet
{
	// LIT = 1234, 0xDEADBEEF, 01011b
	// VAR = @LIT
	// REF = @VAR or @@LIT
	// ... etc.

	enum Type
	{
		NOP, // nop. ; nothing
		HALT, // end. ; terminate program

//		INC, // inc VAR. ; VAR += 1
//		DEC, // dec VAR. ; VAR -= 1

//		JMP, // jmp 0xLIT. ; set I to LIT

//		SAV, // sav VAR. ; stores value of VAR in stack and increments S
//		USE, // use VAR. ; loads value at top of stack to VAR and decrements S

		PUSH, // push 0xLIT. ; adds LIT to S          - DEPRECATED; must implement relative addressing before this can be removed
		POP, // pop 0xLIT. ; subtracts LIT from S     - DEPRECATED; must implement relative addressing before this can be removed

		NOT, // not VAR. ; invert VAR bits
		NEG, // neg VAR. ; VAR = -VAR

		DREF, // dref VAR, VAR. ; dereference VAR B, store to VAR A (C equivalent: A = mem[B])

		SET, // set VAR, 0xLIT. ; sets the value at the adress VAR to LIT
		MOV, // mov VAR, VAR. ; set the value of VAR A to the value of VAR B
//		DSET, // dset VAR, 0xLIT. ; set the value at the address = value VAR A to LIT
		DMOV, // dmov VAR, VAR. ; set the value at the address = value VAR A to value of VAR B (C equivalent: mem[A] = B)

		AND, // and VAR, VAR.
		OR, // or VAR, VAR.
		XOR, // xor VAR, VAR.

		LSU, // lsu VAR, VAR. ; unsigned shift left
		RSU, // rsu VAR, VAR. ; unsigned shift right
		LSS, // lss VAR, VAR. ; signed shift left
		RSS, // rss VAR, VAR. ; signed shift right

		ADD, // add VAR, VAR.
		SUB, // sub VAR, VAR.
		MUL, // mul VAR, VAR.
		DIV, // div VAR, VAR.
		MOD, // mod VAR, VAR.

		INT, // int VAR, VAR. ; hand over control to hardware, where port and function is defined by VAR B, manipulate memory at VAR A

		JE, // je VAR, VAR, 0xLIT. ; set I to LIT if A = B
		JNE, // jne VAR, VAR, 0xLIT. ; set I to LIT if A != B
		JL,  // jl VAR, VAR, 0xLIT. ; set I to LIT if A < B
		JLE, // jle VAR, VAR, 0xLIT. ; set I to LIT if A <= B
		JG,  // jg VAR, VAR, 0xLIT. ; set I to LIT if A > B
		JGE, // jge VAR, VAR, 0xLIT. ; set I to LIT if A >= B

		LAST
	};
};

}

#endif // XIS_H__
