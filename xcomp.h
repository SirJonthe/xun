#ifndef XCOMP_H
#define XCOMP_H

#include "xarch.h"
#include "xis.h"
#include "xdev.h"
#include "hw/xdisk.h"
#include "hw/xpwr.h"
#include "hw/xbell.h"

/// @brief A programmable computer.
/// @note Use XASM or XB to generate binaries of custom software.
/// @sa xb
/// @sa xasm
class Computer : public Device
{
private:
	/// @brief An I/O port with its own input buffer.
	class IOPort : public Device
	{
		friend class Computer;

	public:
		/// @brief Constructs an IOPort object.
		IOPort( void );
	};

	/// @brief Returns the port at the given index.
	/// @param index The port index.
	/// @return The port at the given index.
	/// @note Returns NULL if the index is out of the valid bounds.
	IOPort *GetPort(U16 index);

	/// @brief Flips the error register flag as indicated by the error code.
	/// @param code The error code.
	void SetError(U16 code);

	/// @brief Unflips the error register flag as indicated by the error code.
	/// @param code The error code.
	void ClearError(U16 code);

	/// @brief Puts a message on the queue of the device at the other end of a given port.
	/// @param port_index The index of the port.
	/// @param header_addr The pointer to the location in memory holding the packet header data.
	/// @param data_addr The pointer to the location in memory holding the payload data.
	/// @note If the queue is full then the message will be dropped.
	/// @note If the port index does not exist, sets the error flag.
	void Output(U16 port_index, U16 header_addr, U16 data_addr);

	/// @brief Consumes the top message from the given port.
	/// @param port_index The port index.
	/// @note If the port index does not exist, sets the error flag.
	void Ack(U16 port_index);

	/// @brief Peeks at the top message without consuming it from the port queue.
	/// @param port_index The port index.
	/// @param header_addr The pointer to the location in memory to store the packet header data.
	/// @param data_addr The pointer to the location in memory to store the payload data.
	/// @warning Make sure there are messages to peek. If there is not, garbage data will be displayed.
	/// @note If the port index does not exist, sets the error flag.
	void Peek(U16 port_index, U16 header_addr, U16 data_addr);

	/// @brief Checks if the queue is full and can take no more messages.
	/// @param port_index The index of the port to 
	/// @return True if the queue is full.
	/// @note If the port index does not exist, sets the error flag.
	bool IsFull(U16 port_index);

	/// @brief Checks if the queue is empty.
	/// @param port_index The index of the port to 
	/// @return True if the queue is empty.
	/// @note If the port index does not exist, sets the error flag.
	bool IsEmpty(U16 port_index);

private:
	XWORD
		// Offset pointers.
		A,   // Offset pointer to the start of the program (access labels).
		B,   // Offset pointer to the start of the stack in a program (access global variables).
		C,   // Offset pointer to the start of the stack in a local function (access local variables).
		
		// Main pointers
		SP,  // Stack Pointer. Absolute.
		IP,  // Instruction Pointer. Absolute. Stored addresses are relative however, so need to be adjusted IP = addr + A for global binary labels, IP = addr + B for global stack labels, IP = addr + C for local stack labels.

		// Instruction registers
		I, // The currently executing instruction.

		// I/O registers
		P, // Port selector. I/O instructions target this port number.
		
		// Error registers
		ERR; // Error register.
	static constexpr uint32_t ROM_SIZE = 4096;     // The size, in words of the ROM.
	static XWORD              ROM[ROM_SIZE];       // BIOS lives here. At boot, the BIOS is flashed over to RAM.
	XWORD                     RAM[MEM_SIZE_MAX];   // The internal working memory of the computer. Non-persistent.
	DataStorage               m_storage;           // Built-in, small persistent memory bank.
	PowerController           m_power_controller;  // Can physically turn power off.
	Bell                      m_bell;
	
	// DataStorageReader     m_external_reader;                      // 
	// DataStorageReader     m_internal_reader;

	static constexpr uint32_t NUM_PORTS = 16;      // The number of I/O ports on the computer.
	IOPort                    m_ports[NUM_PORTS];  // The I/O ports of the computer.
	bool                      m_debug;

public:
	/// @brief Constructs a new Computer object.
	/// @param debug Sets debug mode where memory is not cleared on power off, and HALT instructions are emitted when loading a binary from disk.
	explicit Computer(bool debug = false);

	/// @brief Copies a Computer object.
	/// @param NA The object to copy.
	Computer(const Computer&) = default;

	/// @brief Copies a Computer object.
	/// @param NA The object to copy.
	/// @return Self.
	Computer &operator=(const Computer&) = default;

	/// @brief Flashes the memory with a specified binary, enabling that program to run on the machine. The function also resets the registers so previous execution will be wiped out.
	/// @param bin The binary.
	/// @param bin_count The number of words in the binary.
	/// @note If debug mode is true, writes HALT instruction to all memory locations not occupied by the given program.
	/// @deprecated This function will be replaced by a more proper boot sequence when powering on.
	void BootDisk(const XWORD *bin, U16 bin_count);

	// DiskReader &GetDiskReader();

	/// @brief Turns the power off.
	/// @note Not in use.
	void PowerOff( void );

	/// @brief Turns the power on.
	/// @note Not in use.
	void PowerOn( void );

	/// @brief Turns the power on if off, and off if on.
	/// @note Not in use.
	void PowerToggle( void );

	/// @brief Turns the computer off, then on again.
	/// @note Not in use.
	void PowerCycle( void );

	void  Poke(U16 addr, XWORD val);
	void  PokeA(U16 addr, XWORD val);
	void  PokeB(U16 addr, XWORD val);
	void  PokeC(U16 addr, XWORD val);
	void  PokeTop(U16 addr, XWORD val);
	XWORD Peek(U16 addr) const;
	XWORD PeekA(U16 addr) const;
	XWORD PeekB(U16 addr) const;
	XWORD PeekC(U16 addr) const;
	XWORD PeekTop(U16 addr) const;
	
	/// @brief Executes a single cycle on the machine.
	/// @return The last executed instruction.
	void Cycle( void );

	/// @brief Checks if a given port is available to be connected.
	/// @param port The port number.
	/// @return The availability of the port. True means available.
	bool IsAvailablePort(U8 port) const;

	/// @brief Connects a device to the given port.
	/// @param device The device.
	/// @param port The port number.
	/// @note Disconnects a device currently connected to the same port.
	void Connect(Device &device, U8 port);
	
	/// @brief Disconnects a device from the given port.
	/// @param port The port number.
	void Disconnect(U8 port);

	U16 InstructionPointer( void ) const;
	U16 Instruction( void ) const;
	U16 StackPointer( void ) const;
	U16 StackOffsetA( void ) const;
	U16 StackOffsetB( void ) const;
	U16 StackOffsetC( void ) const;
};

#endif // XCOMP_H
