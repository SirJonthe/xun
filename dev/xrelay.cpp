#include "xrelay.h"

DeviceRelay::DeviceRelay(U8 array_size) : Device("XERXES(tm) Device Array Relay Node [DARN(tm)]", 0x0003), m_device_array(size_t(array_size))
{
	for (size_t i = 0; i < m_device_array.size(); ++i) {
		m_device_array[i] = nullptr;
	}
}

Device *DeviceRelay::operator[](U8 port) const
{
	if (m_device_array.size() == 0) { return nullptr; }
	return m_device_array[port % m_device_array.size()];
}

void DeviceRelay::RelayConnect(Device &device, U8 port)
{
	if (m_device_array.size() == 0) { return; }

	port = port % m_device_array.size();

	Device *d = m_connection;
	Device::Connect(*this, device);
	m_connection = d;

	RelayDisconnect(port);
	m_device_array[port] = &device;
}

void DeviceRelay::RelayDisconnect(U8 port)
{
	if (m_device_array.size() == 0) { return; }

	port = port % m_device_array.size();
	if (m_device_array[port] != nullptr) {
		m_device_array[port]->Disconnect();
		m_device_array[port] = nullptr;
	}
}
