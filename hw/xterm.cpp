#include <iostream>
#include "xterm.h"

bool Terminal::HandlePacket(const Device::Packet &msg)
{
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case Device::Packet::TYPE_DATA:
		Print(msg.payload, msg.header[Device::Packet::HEADER_SIZE]);
		return true;
	}
	return false;
}

Terminal::Terminal( void ) : Device("XERXES(tm) Integrated Terminal", 0x0006)
{
	SetCyclesPerSecond(10);
}

void Terminal::Print(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size; ++i) {
		std::cout << (char)msg[i];
	}
	std::cout << std::flush;
}