#ifndef XASM_H__
#define XASM_H__

#include "MiniLib/MTL/mtlString.h"
#include "MiniLib/MTL/mtlList.h"

#include "xbin.h"

namespace xerx
{

bool Assemble(const mtlChars &file, xerx::Binary &out);

}

#endif // XASM_H__
