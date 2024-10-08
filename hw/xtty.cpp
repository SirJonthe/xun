#include <iostream>
#include "xhwids.h"
#include "xtty.h"

bool Teleprinter::HandlePacket(const Device::Packet &msg)
{
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case Device::Packet::TYPE_DATA:
		PrintChars(msg.payload, msg.header[Device::Packet::HEADER_SIZE]);
		return true;
	}
	return Device::HandlePacket(msg);
}

Teleprinter::Teleprinter( void ) : Device("XERXES(tm) Integrated Teleprinter", XHWID_TTY)
{
	SetCyclesPerSecond(0);
}

void Teleprinter::PrintChars(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size && msg[i] != 0; ++i) {
		if (msg[i] == '\a') {
			Info("<BEEP>");
			SetExternalState(0, 1, 500);
		} else {
			std::cout << (char)msg[i];
		}
	}
	std::cout << std::flush;
}

void Teleprinter::PrintNums(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size; ++i) {
		std::cout << msg[i] << " ";
	}
	std::cout << std::flush;
}