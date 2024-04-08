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
	Device *dev = m_connection;
	if (dev != nullptr) {
		dev->Input(msg);
	}
}

Device::Packet Device::NewPacket(U16 type) const
{
	Device::Packet p = {
		{
			GetHWID(),
			GetClock(),
			type,
			0,
			0,
			0
		}
	};
	for (uint32_t i = 0; i < sizeof(p.payload) / sizeof(U16); ++i) {
		p.payload[i] = 0;
	}
	return p;
}

Device::Packet Device::ErrorPacket( void ) const
{
	return NewPacket(Packet::TYPE_ERR);
}

Device::Packet Device::PingPacket( void ) const
{
	return NewPacket(Packet::TYPE_PING);
}

Device::Packet Device::PongPacket( void ) const
{
	return NewPacket(Packet::TYPE_PONG);
}

Device::Packet Device::ConnectPacket( void ) const
{
	return NewPacket(Packet::TYPE_CONNECT);
}

Device::Packet Device::DisconnectPacket( void ) const
{
	return NewPacket(Packet::TYPE_DISCONNECT);
}

Device::Device(const std::string &name, U16 HWID) : m_connection(nullptr), m_in_queue(), m_name(name), m_HWID(HWID), m_clock_ns(0), m_exec_ns(0), m_power(false)
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
		m_clock_ns = 0;
		m_exec_ns = 0;
		Output(ConnectPacket());
	}
}

void Device::Cycle( void )
{
	m_clock_ns += m_ns_per_cycle;
}

void Device::Run(uint32_t ms)
{
	uint32_t cycles = 0;
	m_exec_ns += uint64_t(ms) * 1000000ULL;
	while (m_exec_ns >= m_ns_per_cycle && IsPoweredOn()) {
		Cycle();
		++cycles;
		m_exec_ns -= m_ns_per_cycle;
	}
}

void Device::PowerOff( void )
{
	if (IsPoweredOn()) {
		Output(DisconnectPacket()); // Do not formally call Disconnect since devices may still be physically connected.
		m_clock_ns = 0;
		m_exec_ns = 0;
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
	// TODO optimally we adjust 'hz' down to become an even multiple of 1000000000
	static constexpr uint64_t NS_PER_S = 1000000000ULL;
	hz = (uint64_t(hz) < NS_PER_S) ? hz : NS_PER_S;
	m_cycles_per_second = hz;
	m_ns_per_cycle = hz > 0 ? (((NS_PER_S * 10ULL) / uint64_t(hz)) + 5ULL) / 10ULL : 0ULL;
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

std::string Device::GetName( void ) const
{
	return m_name;
}

U16 Device::GetClock( void ) const
{
	return U16(m_clock_ns / 1000000000ULL);
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
	Output(DisconnectPacket());
	Device *dev = m_connection;
	m_connection = nullptr;
	if (dev != nullptr) {
		dev->Disconnect();
	}
}

void Device::Input(const Device::Packet &msg)
{
	if (IsPoweredOn()) {
		m_in_queue.Pass(msg);
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

void Device::Connect(Device &a, Device &b)
{
	a.Disconnect();
	b.Disconnect();
	a.m_connection = &b;
	b.m_connection = &a;
	a.Output(b.ConnectPacket());
	b.Output(a.ConnectPacket());
}
