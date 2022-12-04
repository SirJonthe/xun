#ifndef XUN_H__
#define XUN_H__

#include "xis.h"
#include "xarch.h"
#include "xbin.h"
#include "xdev/xdev.h"
#include "xdev/xclock.h"

namespace xerx
{

// @data NanoController
// @info The central processing device with on-board memory.
class NanoController : public xerx::Device
{
public:
	// @data ErrorBits
	// @info The various error codes.
	enum ErrorBits
	{
		Err_Unknown   = 1,
		Err_Reserved  = Err_Unknown << 1,
		Err_DivZero   = Err_Unknown << 2,
		Err_Overflow  = Err_Unknown << 3,
		Err_Underflow = Err_Unknown << 4,
		Err_Hardware  = Err_Unknown << 5
	};
	
	// @data reg_t
	// @info The symbolic constants of the machine's various registers.
	enum reg_t
	{
		SP = (MEM_SIZE >> 1), IP,
		X1, X2, X3, X4,
		A, B, C
	};
	
	// @data SP_START
	// @info The starting address for the stack pointer.
	static const xerx::uword SP_START = SP - 1;
	
	// @data IP_START
	// @info The starting address for the instruction pointer.
	static const xerx::uword IP_START = SP - 1;
	
private:
	word_t              RAM[MEM_SIZE];
	static const word_t ROM[MEM_SIZE]; // store a small OS here, used for typing in commands
	xerx::Clock         dev_clock;

private:
	void ResetRegisters( void );
	void PrintInstruction(const char *op, xerx::uword a, xerx::uword b, xerx::uword c, unsigned int params, bool fill_blank = true) const;
	void PrintInstruction(const char *op, xerx::uword a, xerx::uword b, unsigned int params, bool fill_blank = true) const;
	void PrintInstruction(const char *op, xerx::uword a, unsigned int params, bool fill_blank = true) const;
	void PrintInstruction(const char *op, bool fill_blank = true) const;
	void PrintRegisters(xerx::uword dst) const;
	void HACK_LoadFont( void );
	bool Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM);
	
public:
	NanoController( void );

	// @algo HACK_Load
	// @info Loads a program off of the provided binary.
	// @note Should be considered a debug function until there is an bootloader and an OS and may be subject for removal.
	// @in bin -> The program binary.
	void          HACK_Load(const xerx::Binary &bin);
	
	// @algo Reset
	// @info Resets the machine and all of its state.
	void          Reset( void );
	
	// @algo Run
	// @info Runs one instruction on the machine.
	// @out TRUE as long as there are more instructions to execute in the program (i.e. END or unrecoverable error is not encountered).
	bool          Run( void );
	
	// @algo DebugRun
	// @info Runs one instruction on the machine and prints the results.
	// @out TRUE as long as there are more instructions to execute in the program (i.e. END or unrecoverable error is not encountered).
	bool          DebugRun( void );
	
	// @algo RunCommand
	// @info Sends the human readable command to the OS in the ROM.
	// @in cmd -> The command string.
	void          RunCommand(const mtlChars &cmd);
	
	// @algo ErrorState
	// @out The error code.
	xerx::uword   ErrorState( void ) const;
	
	// @algo Debug_RAM
	// @info Returns value at RAM location.
	// @note Should be considered a debug function and may be subject for removal.
	// @in i -> The RAM index.
	// @out Value at RAM location.
	xerx::word_t  Debug_RAM(xerx::uword i) const;
	xerx::word_t &Debug_RAM(uword i);
	
	// @algo Debug_PrintDevices
	// @info Prints the connected devices.
	void          Debug_PrintDevices( void ) const;
};

}

#endif

