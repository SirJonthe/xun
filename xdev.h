#ifndef XDEV_H
#define XDEV_H

#include <string>

#include "xarch.h"

/// @brief An abstraction class representing any hardware device that can connect to another device and send messages between them. Its main way of functioning is by calling a function that simulates the device for a given amount of non-real world time.
class Device
{
protected:
	/// @brief A queue for messages between devices.
	class MessageQueue
	{
	private:
		static constexpr uint32_t CAPACITY = 1024;   // The capacity of the queue.
		XWORD                     m_queue[CAPACITY]; // The message queue.
		uint32_t                  m_start;           // The start index of the messages.
		uint32_t                  m_end;             // The end index of the messages.
	
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

		/// @brief Checks if the queue is empty.
		/// @return True if the queue is empty.
		bool IsEmpty( void ) const;
	};

private:
	Device       *m_connection;        // The connected device.
	MessageQueue  m_in_queue;          // The input buffer where the connected device sends input messages.
	std::string   m_name;              // The name of the device.
	U16           m_HWID;              // The unique hardware ID of the device (remember that this has to be unique, otherwise computers may not be able to distinguish this device from another device).
	uint64_t      m_clock_ns;          // The clock in pico seconds.
	uint64_t      m_exec_ns;           // Internal state representing the remainder of execution time left over after a given time slice.
	uint64_t      m_ns_per_cycle;      // The number of pico seconds per cycle.
	uint32_t      m_cycles_per_second; // The number of cycles that can run per second.
	bool          m_power;             // The power state of the device.

protected:
	/// @brief A programmable function that lets devices handle handshakes in a way appropriate for the individual device.
	/// @return The response.
	virtual XWORD HandleHandshake( void );
	
	/// @brief A programmable function that lets devices handle disconnects in a way appropriate for the individual device.
	virtual void HandleDisconnect( void );
	
	/// @brief A programmable function that lets devices handle non-standard messages in a way appropriate for the individual device.
	/// @param message The message.
	/// @return The response.
	virtual XWORD HandleCustomMessage(XWORD message);
	
	/// @brief Contains a switch-case that jumps to an appropriate response given an input message in the queue.
	/// @param message The message to respond to.
	/// @return The response.
	/// @deprecated This should be done in the 'Cycle' function by manually polling the input queue.
	XWORD Respond(XWORD message); // Responds to a message from a connected device.

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

	/// @brief Sends a message to the connected device's input queue.
	/// @param msg The message.
	/// @warning Silently does nothing if there is no connected device or if the connected device has a full input buffer.
	void Output(XWORD msg);

public:
	/// @brief Contains standard messages that devices may send to eachother to establish a communications protocol.
	enum StdMessage
	{
		HANDSHAKE,         // A device has been connected
		DATA,              // Tells the receiving end that a bunch of data is about to be sent. The next message should contain a number indicating how many XWORD elements are to be sent.
		GET_NAME,          // request a device to return its name via the Send. Response should be [DATA][count][count elements]
		GET_HWID,          // request a device to return its HWID. Response should be [DATA][1][HWID]
		SET_INTERRUPT,     // [SET_INTERRUPT][IRQ]
		INTERRUPT_REQUEST, // [INTERRUPT_REQUEST] valid for programmable machines
		DISCONNECT         // tell the connected device that this device has been powered off or physically disconnected
	};

	/// @brief Contains standard responses to messages that devices may send upon receiving a message to establish a communications protocol.
	enum StdReponse
	{
		NO_RESPONSE, // Reserved for when you try to send data to an unconnected device
		FINISHED,    // Finished
		ERROR        // Finished with error
	};

	/// @brief Creates a new Device object.
	/// @param name The hardware name.
	/// @param HWID The hardware ID. Must be unique.
	Device(const std::string &name, U16 HWID);

	/// @brief Destroys the Device object.
	/// @note Also disconnects connected devices.
	~Device( void );

	/// @brief Powers the device on.
	virtual void PowerOn( void );

	/// @brief Runs a single simulated cycle of this device's function.
	virtual void Cycle( void );
	
	/// @brief Simulates the device for a given amount of simulated time.
	/// @param ms The amount of time to simulate the hardware for. Note that this does not require that the application spends the given amount of time in the real world, just that the device is simulated for that time in whatever time it takes to simulate that time in real-time.
	void Run(uint32_t ms);

	/// @brief Powers the device off.
	virtual void PowerOff( void );

	/// @brief If the power is on then turn off power, and vice versa.
	void PowerToggle( void );

	/// @brief If the power is on, turns off the power and then turns it on again.
	void PowerCycle( void );

	/// @brief Checks if the device is powered on.
	/// @return True if device is powered on.
	bool IsPoweredOn( void ) const;

	/// @brief Checks if the device is powered off.
	/// @return True if the device is powered off.
	bool IsPoweredOff( void ) const;

	/// @brief Sets the number of cycles to perform per second.
	/// @param hz The number of cycles per second.
	void SetCyclesPerSecond(uint32_t hz);

	/// @brief Returns the local time relative to device power on in picoseconds.
	/// @return The local time.
	uint64_t GetLocalClock( void ) const;

	/// @brief Returns the connected device.
	/// @return The connected device.
	Device *GetConnectedDevice( void );

	/// @brief Returns the connected device.
	/// @return The connected device.
	const Device *GetConnectedDevice( void ) const;

	/// @brief Returns the hardware ID.
	/// @return The hardware ID.
	U16 GetHWID( void ) const;

	/// @brief Returns the hardware name of the device.
	/// @return The hardware name of the device.
	std::string GetName( void ) const;

	/// @brief Returns the local time of the device.
	/// @return The local time of the device.
	U16 GetClock( void ) const;

	/// @brief Checks if a given device is connected to this device.
	/// @param device The device to check connectivity to.
	/// @return True if connected.
	bool IsConnected(const Device &device) const;

	/// @brief Checks if a device is connected to this device.
	/// @return True if connected.
	bool IsConnected( void ) const;

	/// @brief Disconnects the currently connected device.
	void Disconnect( void );

	/// @brief Passes a message to this device's input queue.
	/// @param msg The message.
	void Input(XWORD msg);

	/// @brief Checks if the queue is full and can take no more messages.
	/// @return True if the queue is full.
	bool IsFull( void ) const;

	/// @brief Checks if the queue is empty.
	/// @return True if the queue is empty.
	bool IsEmpty( void ) const;

	/// @brief Connects two devices together (disconnects previously connected devices if need be) and performs a handshake.
	/// @param a A device.
	/// @param b Another device.
	static void Connect(Device &a, Device &b);
};

struct Packet
{
	enum {
		TYPE_ERR,        // Error. The payload contains more information about the error.
		TYPE_CONNECT,    // Send to a device when there is a physical connection to that device. The payload contains the device name.
		TYPE_DISCONNECT, // Send to a device when the connection has been disrupted.
		TYPE_PING,       // When sent we expect the receiver to send PONG back.
		TYPE_PONG,       // Sent back as an acknowledgement of a PING.
		TYPE_DATA        // Data is found in the payload.
	};

	U16 id;          // The hardware ID of the sending device.
	U16 clock;       // The local time as reported by the sender.
	U16 type;        // The type of the packet.
	U16 seq;         // The packet sequence number if this packet is part of a series of packets.
	U16 size;        // The number of words in the payload.
	U16 irq;         // The IRQ code for the receiving device to call.
	U16 payload[26]; // The packet payload (user data).
};

#endif // XDEV_H
