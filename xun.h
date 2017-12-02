#ifndef XUN_H__
#define XUN_H__

#include "xis.h"
#include "xarch.h"
#include "xbin.h"
#include "xdev/xdev.h"
#include "xdev/xclock.h"

namespace xerx
{

class NanoController : public xerx::Device
{
public:
	enum ErrorBits
	{
		Err_Unknown   = 1,
		Err_Reserved  = Err_Unknown << 1,
		Err_DivZero   = Err_Unknown << 2,
		Err_Overflow  = Err_Unknown << 3,
		Err_Underflow = Err_Unknown << 4,
		Err_Hardware  = Err_Unknown << 5
	};
	enum reg_t
	{
		SP = (MEM_SIZE >> 1), IP,
		X1, X2, X3, X4,
		A, B, C
	};
	static const xerx::uword SP_START = SP - 1;
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

	void          HACK_Load(const xerx::Binary &bin);
	void          Reset( void );
	bool          Run( void );
	bool          DebugRun( void );
	void          RunCommand(const mtlChars &cmd);
	xerx::uword   ErrorState( void ) const;
	xerx::word_t  Debug_RAM(xerx::uword i) const;
	xerx::word_t &Debug_RAM(uword i);
	void          Debug_PrintDevices( void ) const;
};

}

#endif

