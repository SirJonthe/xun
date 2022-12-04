#ifndef XBIN_H__
#define XBIN_H__

#include <cstdlib>
#include "xarch.h"

namespace xerx
{

// @data Binary
// @info Contains instructions and data used by the machine to run a program.
class Binary
{
private:
	word_t *bin;
	udword  size;

public:
	Binary( void ) : bin(nullptr), size(0) {}
	~Binary( void ) { delete [] bin; }

	// @algo GetSize
	// @out The number of architecture words that make up the program.
	xerx::udword GetSize( void ) const { return size; }

	// @algo Allocate
	// @info Allocates a binary with a given number of words.
	// @note The binary word count may not exceed MEM_SIZE.
	// @in size_ -> The number of words in the binary.
	// @out TRUE on success.
	bool Allocate(udword size_) {
		delete [] bin;
		bin = nullptr;
		size = 0;
		if (size <= xerx::MEM_SIZE) {
			size = size_;
			bin = new word_t[size];
		}
		return bin != nullptr;
	}
	
	// @algo Delete
	// @info Frees resources held by binary.
	void Delete( void )
	{
		delete [] bin;
		bin = nullptr;
		size = 0;
	}
	
	// @algo []
	// @info Returns a word at a given index.
	// @in i -> The index of the word.
	// @out The word.
	word_t operator[](uword i) const { return bin[i]; }
	word_t &operator[](uword i) { return bin[i]; }
};

}

#endif // XBIN_H__
