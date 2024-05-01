#ifndef XTTY_H
#define XTTY_H

#include "../xdev.h"

class Teletypewriter : public Device
{
protected:
	bool HandlePacket(const Device::Packet &msg);

public:
	Teletypewriter( void );

	void PrintChars(const U16 *msg, U16 size);
	void PrintNums(const U16 *msg, U16 size);
};

#endif