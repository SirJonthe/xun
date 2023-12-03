#include "xdisk.h"


PersistentStorage::PersistentStorage(uint32_t size) : Device("XERXES(tm) Persistent Data Storage Solid-State Drive", 0x0002)
{
	m_data.Create(size);
}

XWORD PersistentStorage::Read(U16 i)
{
	return m_data[i % m_data.GetSize()];
}

void PersistentStorage::Write(U16 i, XWORD data)
{
	m_data[i % m_data.GetSize()] = data;
}
