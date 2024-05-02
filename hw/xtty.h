#ifndef XTTY_H
#define XTTY_H

#include "../xdev.h"

class Teleprinter : public Device
{
public:
	enum {
		MSG_TYPE_INFO = Device::Packet::TYPE_COUNT,
		MSG_TYPE_WARN,
		MSG_TYPE_ERROR
	};

protected:
	bool HandlePacket(const Device::Packet &msg);

public:
	Teleprinter( void );

	void PrintChars(const U16 *msg, U16 size);
	void PrintNums(const U16 *msg, U16 size);
};

#endif