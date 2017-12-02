#include "MiniLib/MML/mmlMath.h"
#include "xdev.h"

bool xerx::Device::Connect(xerx::Device &a, xerx::Device &b)
{
	for (int i = 0; i < MAX_CONNECTIONS; ++i) {
		for (int j = 0; j < MAX_CONNECTIONS; ++j) {
			if (a.Port(i).IsConnected(b.Port(j))) { return true; }
		}
	}
	int i = 0;
	for (i = 0; i < MAX_CONNECTIONS; ++i) {
		if (!a.Port(i).IsValid()) { break; }
	}
	if (i == MAX_CONNECTIONS) { return false; }
	int j = 0;
	for (j = 0; j < MAX_CONNECTIONS; ++j) {
		if (!b.Port(j).IsValid()) { break; }
	}
	if (j == MAX_CONNECTIONS) { return false; }

	a.Port(i).Connect(b.Port(j));
	return true;
}

void xerx::Device::Disconnect(xerx::Device &a, xerx::Device &b)
{
	for (int i = 0; i < MAX_CONNECTIONS; ++i) {
		for (int j = 0; j < MAX_CONNECTIONS; ++j) {
			if (a.Port(i).IsConnected(b.Port(j))) { a.Port(i).Disconnect(); }
		}
	}
}

xerx::Device::Device(xerx::uword _id, const mtlChars &_str, xerx::Device::Type _type) : m_id(_id), m_type(_type)
{
	const int MIN = mmlMin(_str.GetSize(), STR_BUF - 1);
	for (int i = 0; i < MIN; ++i) {
		m_str[i] = _str.GetChars()[i];
	}
	for (int i = MIN; i < STR_BUF; ++i) {
		m_str[i] = '\0';
	}
	for (int i = 0; i < MAX_CONNECTIONS; ++i) {
		m_ports[i] = new Connection(this);
	}
}

xerx::Device::~Device( void )
{
	for (int i = 0; i < MAX_CONNECTIONS; ++i) {
		delete m_ports[i];
	}
}

bool xerx::Device::Operate(xerx::uword func, xerx::uword &param, xerx::word_t *RAM)
{
	switch (func) {
	case 0: // return ID
		param = m_id;
		break;
	case 1: // return STR
		for (int i = 0; i < STR_BUF; i += 2) {
			RAM[param + (i >> 1)].u = ((xerx::uword)(m_str[i]) << 8) | ((xerx::uword)(m_str[i]));
		}
		break;
	case 2:
		param = (xerx::uword)m_type;
		break;
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
	case 10:
	case 11:
	case 12:
	case 13:
	case 14:
	case 15:
		return true; // Reserved functions common for all XUN compatible devices
	default:
		return Function(func, param, RAM);
	}
	return true;
}

xerx::uword xerx::Device::GetID( void ) const
{
	return m_id;
}

const char *xerx::Device::GetIDStr( void ) const
{
	return m_str;
}

xerx::Device::Connection &xerx::Device::Port(xerx::uword port)
{
	return *(m_ports[port]);
}

const xerx::Device::Connection &xerx::Device::Port(xerx::uword port) const
{
	return *(m_ports[port]);
}
