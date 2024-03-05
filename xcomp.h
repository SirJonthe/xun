#ifndef XCOMP_H
#define XCOMP_H

#include "xarch.h"
#include "xis.h"
#include "xdev.h"
#include "dev/xdisk.h"
#include "dev/xrelay.h"
#include "dev/xclock.h"
#include "dev/xpwr.h"

class Computer : public Device
{
private:
	XWORD
		// Offset pointers.
		A, // Offset pointer to the start of the program (access labels).
		B, // Offset pointer to the start of the stack in a program (access global variables).
		C, // Offset pointer to the start of the stack in a local function (access local variables).
		
		// Main pointers
		SP,  // Stack Pointer. Absolute.
		IP,  // Instruction Pointer. Absolute. Stored addresses are relative however, so need to be adjusted IP = addr + A for global binary labels, IP = addr + B for global stack labels, IP = addr + C for local stack labels.
		
		// Error pointers
		ERR; // Error register.
	XWORD             RAM[MEM_SIZE_MAX];
	PersistentStorage m_storage; // Built-in, small persistent memory bank
	Clock             m_clock; // System clock
	PowerController   m_power_controller;
	DeviceRelay       m_relay; // Connection to other devices. Drive and Clock connect to this automatically.
	
	U16               m_latency_table[XIS::COUNT]; // Determines the latency in cycles for each instruction.
	int32_t           m_latency_counter;           // The current latency that must tick down to 0 before executing next instruction.
	uint32_t          m_cycles_per_second;         // The number of cycles that can run per second.

	bool              m_power;

public:
	Computer( void );

	void  BootDisk(const XWORD *bin, U16 bin_count, bool debug = false);
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
	XWORD Cycle( void );
	void  Run( void );
	void  Run(uint32_t ms);
	bool  IsAvailablePort(U8 port) const;
	void  Connect(Device &device, U8 port);
	void  Disconnect(U8 port);

	U16 InstructionPointer( void ) const;
	U16 StackPointer( void ) const;
	U16 StackOffsetA( void ) const;
	U16 StackOffsetB( void ) const;
	U16 StackOffsetC( void ) const;
};

#endif // XCOMP_H
