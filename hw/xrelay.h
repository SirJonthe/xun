#ifndef XRELAY_H
#define XRELAY_H

#include <vector>

#include "../xdev.h"

class DeviceRelay : public Device // A machine is connected to this
{
private:
	std::vector< Device* > m_device_array;

public:
	DeviceRelay(U8 array_size);

	Device *operator[](U8 port) const;

	void RelayConnect(Device &device, U8 port);
	void RelayDisconnect(U8 port);

	// Makes communications between a device connected to several devices possible by forwarding messages.
	// Needs to reserve 1 message for destination switching
};

#endif // XRELAY_H
