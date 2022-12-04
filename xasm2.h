#ifndef XASM2_H
#define XASM2_H

#include <unordered_map>
#include "MiniLib/MTL/mtlString.h"
#include "MiniLib/MTL/mtlStringMap.h"
#include "xbin.h"
#include "xfs.h"
#include "xis.h"

namespace xerx
{

class Assembler
{
private:
	struct Error
	{
		xerx::uword type;
		xerx::uword loc;
		mtlString   file;
		mtlString   msg;
	};
	struct Def
	{
		enum Address
		{
			Rel, // relative to stack pointer
			Abs // located in a fixed location (use instruction pointer)
		};
		mtlString   alias;
		xerx::uword val;
		Address     addr;
	};
	struct StackFrame
	{
		xerx::uword  start;
		xerx::uword  size;
		mtlList<Def> defs;
	};

private:
	Error                  m_error;
	mtlList<StackFrame>    m_frames;
	mtlList<xerx::word_t>  m_bin;
	xerx::FileSystem      *m_file_system;

private:
	void ClearState( void );
	bool BufferFile(const mtlChars &file, mtlString &out);
	bool Assemble(const mtlChars &body);
	bool AssembleInstruction(const mtlChars &instr, const mtlChars &params);
	void PushBinary(xerx::uword i);
	bool PrintToFile(xerx::Binary &out) const;

public:
	bool Assemble(const mtlChars &entry_file, xerx::Binary &out, xerx::FileSystem *file_system = nullptr);
};

}

#endif // XASM2_H
