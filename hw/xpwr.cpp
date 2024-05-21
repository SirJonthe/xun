#include "xhwids.h"
#include "xpwr.h"

bool PowerControlUnit::HandlePacket(const Device::Packet &msg)
{
	SetExternalState(0);
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case TYPE_OFF:
		if (GetConnectedDevice() != NULL) {
			GetConnectedDevice()->PowerOff();
		}
		return true;
	case TYPE_CYCLE:
		if (GetConnectedDevice() != NULL) {
			GetConnectedDevice()->PowerCycle();
		}
		return true;
	}
	return Device::HandlePacket(msg);
}

PowerControlUnit::PowerControlUnit( void ) : Device("XERXES(tm) Power Control Unit", XHWID_PWR)
{
	SetCyclesPerSecond(0);
}
