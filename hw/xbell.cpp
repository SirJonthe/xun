#include <iostream>
#include "xhwids.h"
#include "xbell.h"

bool Bell::HandlePacket(const Device::Packet &msg)
{
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case TYPE_BEEP:
		Sound();
		return true;
	}
	return false;
}

Bell::Bell( void ) : Device("XERXES(tm) On-board Bell", XHWID_BELL)
{
	SetCyclesPerSecond(0);
}

void Bell::Sound( void ) const
{
	std::cout << "[" << GetClock() << "] " << GetName() << ": <BEEP>" << std::endl;
}
