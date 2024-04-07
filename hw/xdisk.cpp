#include "xdisk.h"

DataStorage::DataStorage(uint32_t size) : Device("XERXES(tm) Persistent Data Storage Solid-State Drive", 0x0002)
{
	m_word_count = size;
	m_data = new XWORD[size];
}

DataStorage::~DataStorage( void )
{
	delete [] m_data;
}

XWORD DataStorage::Read(U16 i)
{
	return m_data[i % m_word_count];
}

void DataStorage::Write(U16 i, XWORD data)
{
	m_data[i % m_word_count] = data;
}
