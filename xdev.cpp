#include "xdev.h"

XWORD Device::HandleHandshake( void ) { return XWORD{ U16(FINISHED) }; }
void Device::HandleDisconnect( void ) {}
XWORD Device::HandleCustomMessage(XWORD) { return XWORD{ U16(ERROR) }; }

XWORD Device::Respond(XWORD message)
{
	switch (message.u) {
	case HANDSHAKE:
		return HandleHandshake();

	case GET_NAME:
		Send(XWORD{ DATA });
		Send(XWORD{ U16(m_name.size()) });
		for (size_t i = 0; i < m_name.size(); ++i) {
			Send(XWORD{ U16(m_name[i]) });
		}
		return XWORD{ U16(FINISHED) };

	case GET_HWID:
		Send(XWORD{ DATA });
		Send(XWORD{ 1 });
		Send(XWORD{ m_HWID });
		return XWORD{ U16(FINISHED) };

	case DISCONNECT:
		HandleDisconnect();
		return XWORD{ U16(FINISHED) };

	default:
		return HandleCustomMessage(message);
	}
	return XWORD{ U16(ERROR) };
}

Device::Device(const std::string &name, U16 HWID) : m_name(name), m_HWID(HWID), m_connection(nullptr)
{}

Device::~Device( void )
{
	Disconnect();
}

XWORD Device::Boot( void )
{
	Send(XWORD{ HANDSHAKE }); // A device may already be connected, but powered off
	return XWORD{ 0 };
}

XWORD Device::Cycle( void )
{
	return XWORD{ 0 };
}

XWORD Device::Shutdown( void )
{
	Send(XWORD{ DISCONNECT }); // Do not formally call Disconnect since devices may still be physically be connected.
	return XWORD{ 0 };
}

XWORD Device::Send(XWORD msg)
{
	if (m_connection != nullptr) {
		return m_connection->Respond(msg);
	}
	return XWORD{ NO_RESPONSE };
}

bool Device::IsConnected(const Device &device) const
{
	return m_connection != &device;
}

void Device::Disconnect( void )
{
	if (m_connection != nullptr) {
		Device *d = m_connection;
		m_connection->m_connection = nullptr;
		m_connection = nullptr;
		Respond(XWORD{ DISCONNECT });
		d->Respond(XWORD{ DISCONNECT });
	}
}

void Device::Connect(Device &a, Device &b)
{
	a.Disconnect();
	b.Disconnect();
	a.m_connection = &b;
	b.m_connection = &a;
	a.Send(XWORD{ HANDSHAKE });
	b.Send(XWORD{ HANDSHAKE });
}
