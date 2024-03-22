#ifndef XCOMP_H
#define XCOMP_H

#include "xarch.h"
#include "xis.h"
#include "xdev.h"
#include "hw/xdisk.h"
#include "hw/xrelay.h"
#include "hw/xclock.h"
#include "hw/xpwr.h"

class Computer : public Device
{
private:
	XWORD
		// Offset pointers.
		A,   // Offset pointer to the start of the program (access labels).
		B,   // Offset pointer to the start of the stack in a program (access global variables).
		C,   // Offset pointer to the start of the stack in a local function (access local variables).
		
		// Main pointers
		SP,  // Stack Pointer. Absolute.
		IP,  // Instruction Pointer. Absolute. Stored addresses are relative however, so need to be adjusted IP = addr + A for global binary labels, IP = addr + B for global stack labels, IP = addr + C for local stack labels.
		
		// Error pointers
		ERR; // Error register.
	XWORD             RAM[MEM_SIZE_MAX];
	PersistentStorage m_storage;                   // Built-in, small persistent memory bank.
	Clock             m_clock;                     // System clock.
	PowerController   m_power_controller;          // Can physically turn power off.
	DeviceRelay       m_relay;                     // Connection to other devices. Drive and Clock connect to this automatically.
	// DiskReader     m_reader;                      // 
	
	uint32_t          m_cycles_per_second;         // The number of cycles that can run per second.

	bool              m_power;                     // The power state of the computer.

public:
	Computer( void );
	Computer(const Computer&) = default;
	Computer &operator=(const Computer&) = default;

	/// @brief Flashes the memory with a specified binary, enabling that program to run on the machine. The function also resets the registers so previous execution will be wiped out.
	/// @param bin The binary.
	/// @param bin_count The number of words in the binary.
	/// @param debug If true, writes HALT instruction to all memory locations not occupied by the given program.
	/// @deprecated This function will be replaced by a more proper boot sequence when powering on.
	void  BootDisk(const XWORD *bin, U16 bin_count, bool debug = false);

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
	XWORD Cycle( void );
	
	/// @brief Runs the machine until the program halts or until the power is shut off.
	/// @warning May indefinitely lock up the thread as the machine runs until a halting condition is fulfulled.
	void Run( void );

	/// @brief Runs the machine for a specified amount of time.
	/// @param ms The number of miliseconds to run the machine. Time is specified in machine-local time, i.e. not real-time, meaning the actual run time may be more or less than the specified time as the actual run time depends on the machine's clocks per second.
	void Run(uint32_t ms);

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
	U16 StackPointer( void ) const;
	U16 StackOffsetA( void ) const;
	U16 StackOffsetB( void ) const;
	U16 StackOffsetC( void ) const;
};

#endif // XCOMP_H
