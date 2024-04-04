#include "xdev.h"

Device::MessageQueue::MessageQueue( void ) : m_queue(), m_start(0), m_end(0)
{}

void Device::MessageQueue::Pass(XWORD msg)
{
	if (!IsFull()) {
		m_queue[m_end] = msg;
		m_end = (m_end + 1) % CAPACITY;
	}
}

XWORD Device::MessageQueue::Poll( void )
{
	XWORD msg = Peek();
	if (m_end != m_start) {
		m_start = (m_start + 1) % CAPACITY;
	}
	return msg;
}

XWORD Device::MessageQueue::Peek( void ) const
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

XWORD Device::HandleHandshake( void ) { return XWORD{ U16(FINISHED) }; }
void Device::HandleDisconnect( void ) {}
XWORD Device::HandleCustomMessage(XWORD) { return XWORD{ U16(ERROR) }; }

XWORD Device::Respond(XWORD message)
{
	switch (message.u) {
	case HANDSHAKE:
		return HandleHandshake();

	case GET_NAME:
		Output(XWORD{ DATA });
		Output(XWORD{ U16(m_name.size()) });
		for (size_t i = 0; i < m_name.size(); ++i) {
			Output(XWORD{ U16(m_name[i]) });
		}
		return XWORD{ U16(FINISHED) };

	case GET_HWID:
		Output(XWORD{ DATA });
		Output(XWORD{ 1 });
		Output(XWORD{ m_HWID });
		return XWORD{ U16(FINISHED) };

	case DISCONNECT:
		HandleDisconnect();
		return XWORD{ U16(FINISHED) };

	default:
		return HandleCustomMessage(message);
	}
	return XWORD{ U16(ERROR) };
}

XWORD Device::Poll( void )
{
	return m_in_queue.Poll();
}

XWORD Device::Peek( void ) const
{
	return m_in_queue.Peek();
}

bool Device::Pending( void ) const
{
	return m_in_queue.GetSize() > 0;
}

void Device::Output(XWORD msg)
{
	Device *dev = m_connection;
	if (dev != nullptr) {
		dev->Input(msg);
	}
}

Device::Device(const std::string &name, U16 HWID) : m_connection(nullptr), m_in_queue(), m_name(name), m_HWID(HWID), m_clock_ps(0), m_exec_ps(0), m_power(false)
{
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
		m_clock_ps = 0;
		m_exec_ps = 0;
		Output(XWORD{ HANDSHAKE });
	}
}

void Device::Cycle( void )
{
	m_clock_ps += m_ps_per_cycle;
}
#include <iostream>
void Device::Run(uint32_t ms)
{
	uint32_t cycles = 0;
	m_exec_ps += uint64_t(ms) * 1000000000ULL;
	std::cout << " (" << m_exec_ps << "/" << m_ps_per_cycle << ",";
	while (m_exec_ps >= m_ps_per_cycle && IsPoweredOn()) {
		Cycle();
		++cycles;
		m_exec_ps -= m_ps_per_cycle;
	}
	std::cout << cycles << ") ";
}

void Device::PowerOff( void )
{
	if (IsPoweredOn()) {
		Output(XWORD{ DISCONNECT }); // Do not formally call Disconnect since devices may still be physically connected.
		m_clock_ps = 0;
		m_exec_ps = 0;
		m_power = false;
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
	// TODO optimally we adjust 'hz' down to become an even multiple of 1000000000000
	static constexpr uint64_t PS_PER_S = 1000000000000ULL;
	hz = (uint64_t(hz) < PS_PER_S) ? hz : PS_PER_S;
	m_cycles_per_second = hz;
	m_ps_per_cycle = PS_PER_S / uint64_t(hz); // BUG Dividing this by 60 becomes 3781764778 instead of 16666666666

// max:    18446744073709551615
// target:          16666666666
// actual:           3781764778
}

uint64_t Device::GetLocalClock( void ) const
{
	return m_clock_ps;
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

std::string Device::GetName( void ) const
{
	return m_name;
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
	Output(XWORD{ DISCONNECT });
	Device *dev = m_connection;
	m_connection = nullptr;
	if (dev != nullptr) {
		dev->Disconnect();
	}
}

void Device::Input(XWORD msg)
{
	if (IsPoweredOn()) {
		m_in_queue.Pass(msg);
	}
}

bool Device::IsFull( void ) const
{
	return m_in_queue.IsFull();
}

void Device::Connect(Device &a, Device &b)
{
	a.Disconnect();
	b.Disconnect();
	a.m_connection = &b;
	b.m_connection = &a;
	a.Output(XWORD{ HANDSHAKE });
	b.Output(XWORD{ HANDSHAKE });
}
