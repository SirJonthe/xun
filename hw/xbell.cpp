#include <iostream>
#include "xbell.h"

Bell::Bell( void ) : Device("XERXES(tm) On-board Bell", 0x0005)
{}

void Bell::Alarm( void ) const
{
	std::cout << '\a' << std::flush;
}
