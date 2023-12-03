#include "xbin.h"

Binary::Binary( void ) :
	m_bin(nullptr), m_size(0)
{}

Binary::Binary(const Binary &bin) :
	Binary()
{
	Copy(bin);
}

Binary::Binary(Binary &&bin) :
	Binary()
{
	m_bin = bin.m_bin;
	m_size = bin.m_size;
	bin.m_bin = nullptr;
	bin.m_size = 0;
}

Binary::~Binary( void )
{
	Destroy();
}

Binary &Binary::operator=(const Binary &bin)
{
	Copy(bin);
	return *this;
}

void Binary::Destroy( void )
{
	delete [] m_bin;
	m_bin = nullptr;
	m_size = 0;
}

void Binary::Copy(const Binary &bin)
{
	if (&bin != this) {
		Create(bin.m_size);
		for (uint16_t i = 0; i < m_size; ++i) {
			m_bin[i] = bin.m_bin[i];
		}
	}
}

void Binary::Create(uint16_t size)
{
	Destroy();
	try {
		m_bin = new XWORD[size];
		m_size = size;
	} catch (...) {
		m_bin = nullptr;
		m_size = 0;
	}
}

uint16_t Binary::GetSize( void ) const
{
	return m_size;
}

Binary::operator XWORD*( void )
{
	return m_bin;
}
Binary::operator const XWORD*( void ) const
{
	return m_bin;
}
