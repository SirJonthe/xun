#ifndef XBELL_H
#define XBELL_H

#include "../xdev.h"
#include "../xarch.h"

class Bell : public Device
{
public:
	enum {
		TYPE_BEEP = Device::Packet::TYPE_COUNT
	};

protected:
	bool HandlePacket(const Device::Packet &msg);

public:
	Bell( void );

	void Sound( void ) const;
};

#endif