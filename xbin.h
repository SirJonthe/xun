#ifndef XBIN_H
#define XBIN_H

#include "xarch.h"

class Binary
{
private:
	XWORD    *m_bin;
	uint16_t  m_size;

public:
	Binary( void );
	Binary(const Binary &bin);
	Binary(Binary &&bin);
	~Binary( void );
	Binary &operator=(const Binary &bin);

	void Destroy( void );
	void Copy(const Binary &bin);
	void Create(uint16_t size);
	uint16_t GetSize( void ) const;

	operator XWORD*( void );
	operator const XWORD*( void ) const;
};

#endif // XBIN_H
