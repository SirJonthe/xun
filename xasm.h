#ifndef XASM_H__
#define XASM_H__

#include "MiniLib/MTL/mtlString.h"
#include "MiniLib/MTL/mtlList.h"

#include "xbin.h"

namespace xerx
{

// @algo Assemble
// @info Assembles an XASM program from a file location and outputs a binary ready to execute.
// @in file -> The path to the XASM program.
// @inout out -> The created binary containing a working program.
// @out TRUE on successful assembly.
bool Assemble(const mtlChars &file, xerx::Binary &out);

}

#endif // XASM_H__
