#ifndef XDEV_H
#define XDEV_H

#include <string>

#include "xarch.h"

class Device
{
protected:
	/// @brief A queue for messages between devices.
	class MessageQueue
	{
	private:
		static constexpr uint32_t CAPACITY = 1024;
		XWORD    m_queue[CAPACITY]; // The message queue.
		uint32_t m_start;
		uint32_t m_end;
	
	public:
		/// @brief Constructs a default MessageQueue object.
		MessageQueue( void );

		/// @brief Puts a message on the queue.
		/// @param msg The message to put on the queue.
		/// @note If the queue is full then the message will be dropped.
		void Pass(XWORD msg);

		/// @brief Consumes the top message and returns it.
		/// @return The message.
		/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
		XWORD Poll( void );

		/// @brief Peeks at the top message without consuming it from the queue.
		/// @return The message.
		/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
		XWORD Peek( void ) const;

		/// @brief Gets the number of messages on the queue.
		/// @return The number of messages on the queue.
		uint32_t GetSize( void ) const;

		/// @brief Checks if the queue is full and can take no more messages.
		/// @return True if the queue is full.
		bool IsFull( void ) const;
	};

private:
	std::string  m_name;
	U16          m_HWID;
	bool         m_power;

protected:
	Device       *m_connection;
	MessageQueue  m_out_queue;

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

	bool IsPoweredOn( void ) const;
	bool IsPoweredOff( void ) const;

	Device *GetConnectedDevice( void );
	const Device *GetConnectedDevice( void ) const;

	/// @brief Returns the hardware ID.
	/// @return The hardware ID.
	U16 GetHWID( void ) const;

	/// @brief Checks if a given device is connected to this device.
	/// @param device The device to check connectivity to.
	/// @return True if connected.
	bool IsConnected(const Device &device) const;

	/// @brief Disconnects the currently connected device.
	void Disconnect( void );

	/// @brief Consumes the top message and returns it.
	/// @return The message.
	/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
	XWORD Poll( void );

	/// @brief Peeks at the top message without consuming it from the queue.
	/// @return The message.
	/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
	XWORD Peek( void ) const;

	/// @brief Checks if there are unconsumed messages on the out queue.
	/// @return True if there are messages on the out queue.
	bool Pending( void ) const;

	/// @brief Connects two devices together (disconnects previously connected devices if need be) and performs a handshake.
	/// @param a A device.
	/// @param b Another device.
	static void Connect(Device &a, Device &b);
};

#endif // XDEV_H
