#ifndef XASM_H
#define XASM_H

#include <list>

#include "xbin.h"
#include "xsrc.h"

struct AssemblyError
{
	std::string message;
	uint32_t    location;
};

struct AssemblyResult
{
	std::list< AssemblyError > errors;
	Binary                     binary;
};

// AssembleXASM
// Assembles extended assembly language. Lots of syntactic sugar.
AssemblyResult AssembleXASM(const Source &file);

#endif // XASM_H
