#ifndef XBIN_H__
#define XBIN_H__

#include <cstdlib>
#include "xarch.h"

namespace xerx
{

class Binary
{
private:
	word_t *bin;
	udword  size;

public:
	Binary( void ) : bin(NULL), size(0) {}
	~Binary( void ) { delete [] bin; }

	xerx::udword GetSize( void ) const { return size; }

	bool Allocate(udword size_) {
		delete [] bin;
		bin = NULL;
		size = 0;
		if (size <= xerx::MEM_SIZE) {
			size = size_;
			bin = new word_t[size];
		}
		return bin != NULL;
	}
	void Delete( void )
	{
		delete [] bin;
		bin = NULL;
		size = 0;
	}
	word_t operator[](uword i) const { return bin[i]; }
	word_t &operator[](uword i) { return bin[i]; }
};

}

#endif // XBIN_H__
