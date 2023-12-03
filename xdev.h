#ifndef XDEV_H
#define XDEV_H

#include <string>

#include "xarch.h"

class Device
{
private:
	std::string  m_name;
	U16          m_HWID;

protected:
	Device *m_connection;

protected:
	virtual XWORD HandleHandshake( void );
	virtual void HandleDisconnect( void );
	virtual XWORD HandleCustomMessage(XWORD message);
	XWORD Respond(XWORD message); // Responds to a message from a connected device.

public:
	enum StdMessage
	{
		HANDSHAKE, // A device has been connected
		DATA, // Tells the receiving end that a bunch of data is about to be sent. The next message should contain a number indicating how many XWORD elements are to be sent.
		GET_NAME, // request a device to return its name via the Send. Response should be [DATA][count][count elements]
		GET_HWID, // request a device to return its HWID. Response should be [DATA][1][HWID]
		SET_INTERRUPT, // [SET_INTERRUPT][IRQ]
		INTERRUPT_REQUEST, // [INTERRUPT_REQUEST] valid for programmable machines
		DISCONNECT
	};

	enum StdReponse
	{
		NO_RESPONSE, // Reserved for when you try to send data to an unconnected device
		FINISHED,    // Finished
		ERROR        // Finished with error
	};

	Device(const std::string &name, U16 HWID);
	~Device( void );

	virtual XWORD Boot( void );
	virtual XWORD Cycle( void );
	virtual XWORD Shutdown( void );

	virtual XWORD Send(XWORD message); // Sends a message to connected device.

	bool IsConnected(const Device &device) const;
	void Disconnect( void );

	static void Connect(Device &a, Device &b);
};

#endif // XDEV_H
