#include <iostream>
#include "xdev.h"

Device::MessageQueue::MessageQueue( void ) : m_queue(), m_start(0), m_end(0)
{}

void Device::MessageQueue::Pass(const Device::Packet &msg)
{
	if (!IsFull()) {
		m_queue[m_end] = msg;
		m_end = (m_end + 1) % CAPACITY;
	}
}

void Device::CountDownExternalState(uint32_t ms)
{
	for (uint32_t i = 0; i < 32; ++i) {
		if (m_external_state_reset[i] > 0 && m_external_state_reset[i] < STATE_TIMER_FOREVER) {
			if (ms >= m_external_state_reset[i]) {
				SetExternalState(i, false, 0);
			} else {
				m_external_state_reset[i] -= ms;
			}
		}
	}
}

void Device::ClearExternalState( void )
{
	m_external_state = 0;
	for (uint32_t i = 0; i < 32; ++i) {
		m_external_state_reset[i] = 0;
	}
}

void Device::MessageQueue::Ack( void )
{
	if (m_end != m_start) {
		m_start = (m_start + 1) % CAPACITY;
	}
}

Device::Packet Device::MessageQueue::Peek( void ) const
{
	return m_queue[m_start];
}

uint32_t Device::MessageQueue::GetSize( void ) const
{
	if (m_end < m_start) {
		return CAPACITY - m_start + m_end;
	}
	return m_end - m_start;
}

bool Device::MessageQueue::IsFull( void ) const
{
	return GetSize() == CAPACITY;
}

bool Device::MessageQueue::IsEmpty( void ) const
{
	return GetSize() == 0;
}

void Device::MessageQueue::Flush( void )
{
	m_start = 0;
	m_end = 0;
}

void Device::Info(const char *msg) const
{
	std::cout << GetClock() << " INFO  " << GetHWID() << ": " << msg << std::endl;
}

void Device::Warn(const char *msg) const
{
	std::cout << GetClock() << " WARN  " << GetHWID() << ": " << msg << std::endl;
}

void Device::Error(const char *msg) const
{
	std::cout << GetClock() << " ERROR " << GetHWID() << ": " << msg << std::endl;
}

void Device::Ack( void )
{
	return m_in_queue.Ack();
}

Device::Packet Device::Peek( void ) const
{
	return m_in_queue.Peek();
}

bool Device::Pending( void ) const
{
	return m_in_queue.GetSize() > 0;
}

void Device::Output(const Device::Packet &msg)
{
	if (m_connection != nullptr) {
		m_connection->Input(msg);
	}
}

Device::Packet Device::NewPacket(U16 type)
{
	Device::Packet p = {
		{
			GetHWID(),
			GetClock(),
			type,
			0,
			0,
			0,
			m_message_id_counter++
		}
	};
	for (uint32_t i = 0; i < sizeof(p.payload) / sizeof(U16); ++i) {
		p.payload[i] = 0;
	}
	return p;
}

bool Device::Poll( void )
{
	if (Pending()) {
		Packet p = Peek();
		Ack();
		if (p.header[Packet::HEADER_TYPE] == Packet::TYPE_PING) {
			Output(NewPacket(Packet::TYPE_PONG));
		}
		if (p.header[Packet::HEADER_TYPE] == Packet::TYPE_MULTIPACK) {
			bool ret = true;
			U16 i = 0;
			const U16 MAX_PAYLOAD = p.header[Packet::HEADER_SIZE] <= Packet::PAYLOAD_WORD_SIZE ? p.header[Packet::HEADER_SIZE] : Packet::PAYLOAD_WORD_SIZE;
			while (i < MAX_PAYLOAD) {
				Packet n;
				for (U16 j = 0; j < Packet::HEADER_WORD_SIZE; ++j) {
					n.header[j] = p.header[j];
				}
				n.header[Packet::HEADER_TYPE] = p.payload[i++];
				n.header[Packet::HEADER_SIZE] = p.payload[i++];
				n.header[Packet::HEADER_SIZE] = n.header[Packet::HEADER_SIZE] < MAX_PAYLOAD - i ? n.header[Packet::HEADER_SIZE] : MAX_PAYLOAD - i;
				for (U16 j = 0; j < n.header[Packet::HEADER_SIZE]; ++j) {
					n.payload[j] = p.payload[i++];
				}
				ret = ret & HandlePacket(n);
			}
			return ret;
		} else {
			return HandlePacket(p);
		}
	}
	return false;
}

bool Device::HandlePacket(const Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        /*Info("Got ERR");*/        return true;
		case Packet::TYPE_CONNECT:    /*Info("Got CONNECT");*/    return true;
		case Packet::TYPE_DISCONNECT: /*Info("Got DISCONNECT");*/ return true;
		case Packet::TYPE_PING:       /*Info("Got PING");*/       return true;
		case Packet::TYPE_PONG:       /*Info("Got PONG");*/       return true;
		case Packet::TYPE_DATA:       /*Info("Got DATA");*/       return true;
		case Packet::TYPE_MULTIPACK:  /*Info("Got MULTIPACK");*/  return true;
	}
	return false;
}

void Device::DoCycle( void )
{}

void Device::DoPowerOn( void )
{}

void Device::DoPowerOff( void )
{}

void Device::SetExternalState(uint32_t bit, bool state, uint32_t timer_ms)
{
	if (IsPoweredOn()) {
		bit = bit % 32;
		if (state) {
			m_external_state = m_external_state | (uint32_t(1) << bit);
		} else {
			m_external_state = m_external_state & ~(uint32_t(1) << bit);
		}
		m_external_state_reset[bit] = timer_ms;
	}
}

/// @brief Returns the next step in a Gray Code Sequence, which seemingly returns an index from a permuted sequence from 0..65535
/// @return The Gray code.
static U16 GrayCode( void )
{
	static U16 current = 0;
	U16 gray = current ^ (current << 3);
	++current;
	return gray;
}

Device::Device(const std::string &name, U16 HWID) : m_connection(nullptr), m_in_queue(), m_name(name), m_HWID(HWID), m_DID(GrayCode()), m_clock_ns(0), m_exec_ns(0), m_external_state(0), m_message_id_counter(0), m_power(false)
{
	ClearExternalState();
	SetCyclesPerSecond(60);
}

Device::~Device( void )
{
	Disconnect();
}

void Device::PowerOn( void )
{
	if (IsPoweredOff()) {
		m_power = true;
		m_clock_ns = 0;
		m_exec_ns = 0;
		m_message_id_counter = 0;
		m_in_queue.Flush();
		m_external_state = 0;
		ClearExternalState();
		Output(NewPacket(Device::Packet::TYPE_CONNECT));
		DoPowerOn();
	}
}

void Device::Cycle( void )
{
	if (m_cycles_per_second > 0 && IsPoweredOn()) {
		Poll();
		DoCycle();
		m_clock_ns += m_ns_per_cycle;
	}
}

void Device::Run(uint32_t ms)
{
	if (m_cycles_per_second > 0 && IsPoweredOn()) {
		m_exec_ns += uint64_t(ms) * 1000000ULL;
		while (m_cycles_per_second > 0 && m_exec_ns >= m_ns_per_cycle && IsPoweredOn()) {
			Poll();
			DoCycle();
			m_clock_ns += m_ns_per_cycle;
			m_exec_ns -= m_ns_per_cycle;
		}
		if (m_cycles_per_second == 0) {
			m_exec_ns = 0;
		}
		CountDownExternalState(ms);
	}
}

void Device::PowerOff( void )
{
	if (IsPoweredOn()) {
		DoPowerOff();
		Output(NewPacket(Device::Packet::TYPE_DISCONNECT)); // Do not formally call Disconnect since devices may still be physically connected.
		m_clock_ns = 0;
		m_exec_ns = 0;
		m_message_id_counter = 0;
		m_in_queue.Flush();
		m_external_state = 0;
		m_power = false;
		ClearExternalState();
	}
}

void Device::PowerToggle( void )
{
	if (IsPoweredOff()) {
		PowerOn();
	} else {
		PowerOff();
	}
}

void Device::PowerCycle( void )
{
	if (IsPoweredOn()) {
		PowerOff();
		PowerOn();
	}
}

bool Device::IsPoweredOn( void ) const
{
	return m_power;
}

bool Device::IsPoweredOff( void ) const
{
	return !m_power;
}

void Device::SetCyclesPerSecond(uint32_t hz)
{
	// TODO optimally we adjust 'hz' down to become an even multiple of 1000000000
	static constexpr uint64_t NS_PER_S = 1000000000ULL;
	hz = (uint64_t(hz) < NS_PER_S) ? hz : NS_PER_S;
	m_cycles_per_second = hz;
	m_ns_per_cycle = hz > 0 ? (((NS_PER_S * 10ULL) / uint64_t(hz)) + 5ULL) / 10ULL : 0ULL;
}

uint32_t Device::GetCyclesPerSecond( void ) const
{
	return m_cycles_per_second;
}

uint64_t Device::GetLocalClock( void ) const
{
	return m_clock_ns;
}

Device *Device::GetConnectedDevice( void )
{
	return m_connection;
}

const Device *Device::GetConnectedDevice( void ) const
{
	return m_connection;
}

U16 Device::GetHWID( void ) const
{
	return m_HWID;
}

U16 Device::GetDID( void ) const
{
	return m_DID;
}

std::string Device::GetName( void ) const
{
	return m_name;
}

U16 Device::GetClock( void ) const
{
	return U16(m_clock_ns / 1000000ULL);
}

uint64_t Device::GetHighPrecisionClock( void ) const
{
	return m_clock_ns;
}

bool Device::IsConnected(const Device &device) const
{
	return m_connection != &device;
}

bool Device::IsConnected( void ) const
{
	return m_connection != nullptr;
}

void Device::Disconnect( void )
{
	Output(NewPacket(Device::Packet::TYPE_DISCONNECT));
	Device *dev = m_connection;
	m_connection = nullptr;
	if (dev != nullptr) {
		dev->Disconnect();
	}
}

void Device::Input(const Device::Packet &msg)
{
	if (IsPoweredOn()) {
		if (IsFull()) { Warn("Input buffer full"); }
		m_in_queue.Pass(msg);
		if (m_cycles_per_second == 0) {
			m_clock_ns = msg.header[Packet::HEADER_CLOCK] * 1000000ULL;
			Poll();
		}
	}
}

bool Device::IsFull( void ) const
{
	return m_in_queue.IsFull();
}

bool Device::IsEmpty( void ) const
{
	return m_in_queue.IsEmpty();
}

uint32_t Device::GetExternalState( void ) const
{
	return IsPoweredOn() ? m_external_state : 0;
}

void Device::Connect(Device &a, Device &b)
{
	a.Disconnect();
	b.Disconnect();
	a.m_connection = &b;
	b.m_connection = &a;
	a.Output(a.NewPacket(Device::Packet::TYPE_CONNECT));
	b.Output(b.NewPacket(Device::Packet::TYPE_CONNECT));
}
