#include "xhwids.h"
#include "xkb.h"

Keyboard::Keyboard( void ) : Device("XERXES(tm) Keyboard Model N", XHWID_KB)
{
	SetCyclesPerSecond(100);
}
