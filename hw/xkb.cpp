#include "xhwids.h"
#include "xkb.h"

void Keyboard::ShiftState( void )
{
	for (uint32_t i = 0; i < KB_COUNT; ++i) {
		if ((m_state[i] & 1) == 1) {
			m_state[i] = ((m_state[i] << 1) & 0xf) | 1;
		} else {
			m_state[i] = (m_state[i] << 1) & 0xf;
		}
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
	if (m_delta) {
		// TODO: Pressing all buttons at once will lead to overflow on the payload...
		Device::Packet delta = NewPacket(MSG_KBDELTA);
		delta.header[Device::Packet::HEADER_IRQ] = IRQ_STATECHANGE;
		for (uint32_t i = 0; i < KB_COUNT; ++i) {
			if (m_state[i] != 0xf && m_state[i] != 0x0) {
				if (delta.header[Device::Packet::HEADER_SIZE] + 2 > Device::Packet::PAYLOAD_WORD_CAP) {
					Error("State overflow"); // Shitty solution...
					break;
				}
				delta.payload[delta.header[Device::Packet::HEADER_SIZE]++] = i;
				delta.payload[delta.header[Device::Packet::HEADER_SIZE]++] = (m_state[i] & 1);
			}
		}
		if (delta.header[Device::Packet::HEADER_SIZE] > 0) {
			Output(delta);
		}
		m_delta = false;
	}
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

Keyboard::Keyboard( void ) : Device("XERXES(tm) Keyboard Model N", XHWID_KB), m_delta(false)
{
	SetCyclesPerSecond(100);
}

void Keyboard::SetActive(uint32_t key)
{
	if (key < KB_COUNT && (m_state[key] & 1) == 0) {
		m_state[key] |= 1;
		m_delta = true;
	}
}

void Keyboard::SetInactive(uint32_t key)
{
	if (key < KB_COUNT && (m_state[key] & 1) == 1) {
		m_state[key] &= U8(~1);
		m_delta = true;
	}
}

uint8_t Keyboard::GetState(uint32_t key) const
{
	return (key < KB_COUNT) ? (m_state[key] & 0xf) : STATE_UP;
}
