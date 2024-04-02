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
		m_out_queue.Pass(XWORD{ DATA });
		m_out_queue.Pass(XWORD{ U16(m_name.size()) });
		for (size_t i = 0; i < m_name.size(); ++i) {
			m_out_queue.Pass(XWORD{ U16(m_name[i]) });
		}
		return XWORD{ U16(FINISHED) };

	case GET_HWID:
		m_out_queue.Pass(XWORD{ DATA });
		m_out_queue.Pass(XWORD{ 1 });
		m_out_queue.Pass(XWORD{ m_HWID });
		return XWORD{ U16(FINISHED) };

	case DISCONNECT:
		HandleDisconnect();
		return XWORD{ U16(FINISHED) };

	default:
		return HandleCustomMessage(message);
	}
	return XWORD{ U16(ERROR) };
}

Device::Device(const std::string &name, U16 HWID) : m_name(name), m_HWID(HWID), m_power(false), m_connection(nullptr)
{}

Device::~Device( void )
{
	Disconnect();
}

XWORD Device::Boot( void )
{
	if (!m_power) {
		m_power = true;
		m_out_queue.Pass(XWORD{ HANDSHAKE });
	}
	return XWORD{0};
}

XWORD Device::Cycle( void )
{
	return XWORD{0};
}

XWORD Device::Shutdown( void )
{
	if (m_power) {
		m_out_queue.Pass(XWORD{ DISCONNECT }); // Do not formally call Disconnect since devices may still be physically connected.
		m_power = false;
	}
	return XWORD{0};
}

bool Device::IsPoweredOn( void ) const
{
	return m_power;
}

bool Device::IsPoweredOff( void ) const
{
	return !m_power;
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

bool Device::IsConnected(const Device &device) const
{
	return m_connection != &device;
}

void Device::Disconnect( void )
{
	m_out_queue.Pass(XWORD{DISCONNECT});
}

XWORD Device::Poll( void )
{
	return m_out_queue.Poll();
}

XWORD Device::Peek( void ) const
{
	return m_out_queue.Peek();
}

bool Device::Pending( void ) const
{
	return m_out_queue.GetSize() > 0;
}

void Device::Connect(Device &a, Device &b)
{
	a.Disconnect();
	b.Disconnect();
	a.m_connection = &b;
	b.m_connection = &a;
	a.m_out_queue.Pass(XWORD{ HANDSHAKE });
	b.m_out_queue.Pass(XWORD{ HANDSHAKE });
}
