#ifndef XTERM_H
#define XTERM_H

#include "../xdev.h"

class Terminal : public Device
{
protected:
	bool HandlePacket(const Device::Packet &msg);

public:
	Terminal( void );

	void Print(const U16 *msg, U16 size);
};

#endif