#ifndef XDEV_H
#define XDEV_H

#include <string>

#include "xarch.h"

/// @brief An abstraction class representing any hardware device that can connect to another device and send messages between them. Its main way of functioning is by calling a function that simulates the device for a given amount of non-real world time.
class Device
{
public:
	struct Packet
	{
		enum {
			TYPE_ERR,        // Error. The payload contains more information about the error.
			TYPE_CONNECT,    // Send to a device when there is a physical connection to that device. The payload contains the device name.
			TYPE_DISCONNECT, // Send to a device when the connection has been disrupted.
			TYPE_PING,       // When sent we expect the receiver to send PONG back.
			TYPE_PONG,       // Sent back as an acknowledgement of a PING.
			TYPE_DATA,       // Raw data is found in the payload.
			TYPE_KEYVALS     // A series of user-defined keys and values are sent in the payload. The same effect can be achieved sending raw data, but a separate pckat type may help distinguish between situations.
		};
		enum {
			HEADER_ID,       // The ID of the sending device.
			HEADER_CLOCK,    // The clock as reported by the sending device.
			HEADER_TYPE,     // See TYPE_*.
			HEADER_SEQ,      // The index of a segmented message.
			HEADER_SIZE,     // The number of words in the payload.
			HEADER_IRQ,      // The IRQ to call on the receiving device.
			HEADER_MID,      // The message ID.
			HEADER_WORD_SIZE
		};
		static constexpr uint32_t PACKET_WORD_SIZE  = 32;
		static constexpr uint32_t PAYLOAD_WORD_SIZE = PACKET_WORD_SIZE - HEADER_WORD_SIZE;
		
		U16 header[HEADER_WORD_SIZE];
		U16 payload[PAYLOAD_WORD_SIZE]; // The packet payload (user data).
	};

protected:
	/// @brief A queue for messages between devices.
	class MessageQueue
	{
	private:
		static constexpr uint32_t CAPACITY = 256;    // The capacity of the queue.
		Packet                    m_queue[CAPACITY]; // The message queue.
		uint32_t                  m_start;           // The start index of the messages.
		uint32_t                  m_end;             // The end index of the messages.
	
	public:
		/// @brief Constructs a default MessageQueue object.
		MessageQueue( void );

		/// @brief Puts a message on the queue.
		/// @param msg The message to put on the queue.
		/// @note If the queue is full then the message will be dropped.
		void Pass(const Packet &msg);

		/// @brief Consumes the top message.
		void Ack( void );

		/// @brief Peeks at the top message without consuming it from the queue.
		/// @return The message.
		/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
		Packet Peek( void ) const;

		/// @brief Gets the number of messages on the queue.
		/// @return The number of messages on the queue.
		uint32_t GetSize( void ) const;

		/// @brief Checks if the queue is full and can take no more messages.
		/// @return True if the queue is full.
		bool IsFull( void ) const;

		/// @brief Checks if the queue is empty.
		/// @return True if the queue is empty.
		bool IsEmpty( void ) const;

		/// @brief Removes all messages from the queue.
		void Flush( void );
	};

private:
	Device       *m_connection;         // The connected device.
	MessageQueue  m_in_queue;           // The input buffer where the connected device sends input messages.
	std::string   m_name;               // The name of the device.
	U16           m_HWID;               // The unique hardware ID of the device (remember that this has to be unique, otherwise computers may not be able to distinguish this device from another device).
	uint64_t      m_clock_ns;           // The clock in pico seconds.
	uint64_t      m_exec_ns;            // Internal state representing the remainder of execution time left over after a given time slice.
	uint64_t      m_ns_per_cycle;       // The number of pico seconds per cycle.
	uint32_t      m_cycles_per_second;  // The number of cycles that can run per second.
	U16           m_message_id_counter; // A counter to give each message a unique ID. Note that messages split up over several packets carry the same message ID.
	bool          m_power;              // The power state of the device.

protected:
	/// @brief Consumes the top of the in-queue message.
	void Ack( void );

	/// @brief Peeks at the top message of the in-queue without consuming it from the queue.
	/// @return The top message.
	/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
	Packet Peek( void ) const;

	/// @brief Checks if there are unconsumed messages on the in-queue.
	/// @return True if there are messages on the in-queue.
	bool Pending( void ) const;

	/// @brief Sends a message to the connected device's input queue.
	/// @param msg The message.
	/// @warning Silently does nothing if there is no connected device or if the connected device has a full input buffer.
	void Output(const Packet &msg);

	/// @brief Creates a new packet.
	/// @param type The message type to attach to the new packet.
	/// @return The new packet.
	Packet NewPacket(U16 type);

	/// @brief Creates a new error packet.
	/// @return The error packet.
	Packet ErrorPacket( void );
	
	/// @brief Creates a new ping packet.
	/// @return The ping packet.
	Packet PingPacket( void );

	/// @brief Creates a new pong packet.
	/// @return The pong packet.
	Packet PongPacket( void );
	
	/// @brief Creates a new connect packet.
	/// @return The connect packet.
	Packet ConnectPacket( void );
	
	/// @brief Creates a new disconnect packet.
	/// @return The disconnect packet.
	Packet DisconnectPacket( void );

	/// @brief Automatically grabs top packet, acknowledges it, then passes the packet to a custom function for handling.
	/// @return False if the message failed to be handled properly.
	/// @note Packet types TYPE_PING and TYPE_PONG are handled automatically.
	/// @sa HandlePacket
	bool Poll( void );

	/// @brief Abstract function that is intended to handle message types.
	/// @param msg The incoming message.
	/// @return False if the custom message is not recognized by the device.
	/// @note Overload this to handle the incoming packet.
	virtual bool HandlePacket(const Packet &msg);

public:
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
	void Input(const Packet &msg);

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

#endif // XDEV_H
