#include "xdisk.h"


PersistentStorage::PersistentStorage(uint32_t size) : Device("XERXES(tm) Persistent Data Storage Solid-State Drive", 0x0002)
{
	m_word_count = size;
	m_data = new XWORD[size];
}

PersistentStorage::~PersistentStorage( void )
{
	delete [] m_data;
}

XWORD PersistentStorage::Read(U16 i)
{
	return m_data[i % m_word_count];
}

void PersistentStorage::Write(U16 i, XWORD data)
{
	m_data[i % m_word_count] = data;
}
