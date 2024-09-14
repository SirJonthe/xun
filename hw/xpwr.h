#ifndef XPWR_H
#define XPWR_H

#include "../xdev.h"

/// @brief A power control unit that has the ability to turn off or cycle a computer that is already turned on.
/// @note A PCU comes connected to a XUN machine by default.
class PowerControlUnit : public Device
{
public:
	enum {
		TYPE_OFF = Device::Packet::TYPE_COUNT, // Powers the computer off.
		TYPE_CYCLE                             // Cycles the computer power if the computer is on, resulting in a reboot.
	};

protected:
	// Gives power to the device.
	void DoPowerOn( void ) override;

	/// @brief Overloads the built-in HandlePacket with rules for how to handle incoming packets of certain types.
	/// @param msg The message.
	/// @return True if the package was recognized and was handled properly.
	bool HandlePacket(const Device::Packet &msg) override;

public:
	/// @brief Initializes the PowerControlUnit.
	PowerControlUnit( void );
};

#endif