#ifndef XDB_H
#define XDB_H

#include "../xasm/xasm.h"
#include "../../xcomp.h"

class xdebugger
{
public:
	Computer m_computer;

public:
	xdebugger(const xbinary &program);
	bool step( void );
	void ui( void ) const;
};

#endif
