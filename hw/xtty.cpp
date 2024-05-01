#include <iostream>
#include "xhwids.h"
#include "xtty.h"

bool Teletypewriter::HandlePacket(const Device::Packet &msg)
{
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case Device::Packet::TYPE_DATA:
		PrintChars(msg.payload, msg.header[Device::Packet::HEADER_SIZE]);
		return true;
	}
	return false;
}

Teletypewriter::Teletypewriter( void ) : Device("XERXES(tm) Integrated Teletypewriter", XHWID_TTY)
{
	SetCyclesPerSecond(0);
}

void Teletypewriter::PrintChars(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size; ++i) {
		std::cout << (char)msg[i];
	}
	std::cout << std::flush;
}

void Teletypewriter::PrintNums(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size; ++i) {
		std::cout << msg[i] << " ";
	}
	std::cout << std::flush;
}