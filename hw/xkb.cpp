#include "xhwids.h"
#include "xkb.h"

void Keyboard::ShiftState( void )
{
	for (uint32_t i = 0; i < KB_COUNT; ++i) {
		m_state[i] = (m_state[i] << 1) & 0xf;
	}
}

void Keyboard::Clear( void )
{
	for (uint32_t i = 0; i < KB_COUNT; ++i) {
		m_state[i] = 0;
	}
}

void Keyboard::DoCycle( void )
{
	ShiftState();
}

void Keyboard::DoPowerOn( void )
{
	Clear();
}

void Keyboard::DoPowerOff( void )
{
	Clear();
}

Keyboard::Keyboard( void ) : Device("XERXES(tm) Keyboard Model N", XHWID_KB)
{
	SetCyclesPerSecond(100);
}

void Keyboard::SetActive(uint32_t key)
{
	if (key < KB_COUNT) {
		m_state[key] &= 1;
	}
}

uint8_t Keyboard::GetState(uint32_t key) const
{
	return (key < KB_COUNT) ? (m_state[key] & 0xf) : STATE_UP;
}
