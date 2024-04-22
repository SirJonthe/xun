#include <iostream>
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

Bell::Bell( void ) : Device("XERXES(tm) On-board Bell", 0x0005)
{
	SetCyclesPerSecond(1);
}

void Bell::Sound( void ) const
{
	std::cout << GetName() << ": <BEEP>" << std::endl;
}
