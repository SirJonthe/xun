#ifndef XMACH_H
#define XMACH_H

#include "xarch.h"
#include "xis.h"
#include "xbin.h"
#include "xdev.h"
#include "dev/xdisk.h"
#include "dev/xrelay.h"
#include "dev/xclock.h"
#include "dev/xpwr.h"

class Machine : public Device
{
private:
	XWORD
		// Offset pointers
		PSP, // Program Stack Pointer. Absolute. The offset from 0 where the stack pointer was located at the start of program load.
		FSP, // Frame Stack Pointer. Absolute. The offset from 0 where the stack pointer was located at the start of frame load.
		PIP, // Program Instruction Pointer. Absolute. The offset from 0 where the instruction pointer was located at the start of program load.

		// Register restore pointers
		PRP, // Program Restore Pointer. Absolute. The stack location of the previous program restore point.
		FRP, // Frame Restore Pointer. Absolute. The stack location of the previous frame restore point.

		// TODO: Replace with PSP and PIP
		EP,  // Entry Pointer. Absolute. Generally points to the start of where instructions are added in RAM.
		// TODO: Replace with FSP
		CP,  // Call Pointer. Absolute. Generally points to the start on the stack where a function frame begins.
		
		// Main pointers
		SP,  // Stack Pointer. Relative. Needs EP or CP to access elements in memory. SP + EP for global functions, variables, labels, and data. SP + CP for local variables, and labels.
		IP,  // Instruction Pointer. Absolute. Stored addresses are relative however, so need to be adjusted IP = addr + EP for global labels, IP = addr + CP for local labels.
		
		// Error pointers
		ERR; // Error register.
	XWORD             RAM[MEM_SIZE_MAX];
	PersistentStorage m_embedded_storage; // Built-in, small persistent memory bank
	Clock             m_clock; // System clock
	PowerController   m_power_controller;
	DeviceRelay       m_relay; // Connection to other devices. Drive and Clock connect to this automatically.
	
	U16               m_latency_table[XIS::COUNT]; // Determines the latency in cycles for each instruction.
	int32_t           m_latency_counter;           // The current latency that must tick down to 0 before executing next instruction.
	uint32_t          m_cycles_per_second;         // The number of cycles that can run per second.

	bool              m_power;

public:
	Machine( void );

	void  Feed(const Binary &bin, bool debug = false); // Flash with OS (that can load other programs) or the only program that will ever run on the machine.
	void  Feed(const XWORD *bin, U16 bin_count, bool debug = false);
	void  PokeAbs(U16 addr, XWORD val);
	void  PokeEP(U16 addr, XWORD val);
	void  PokeCP(U16 addr, XWORD val);
	XWORD PeekAbs(U16 addr);
	XWORD PeekEP(U16 addr);
	XWORD PeekCP(U16 addr);
	XWORD Cycle( void );
	void  Run( void );
	bool  IsAvailablePort(U8 port) const;
	void  Connect(Device &device, U8 port);
	void  Disconnect(U8 port);
};

#endif // XMACH_H
