#ifndef XDISK_H
#define XDISK_H

#include <string>
#include <list>

#include "../xdev.h"
#include "../xarch.h"

/// @brief Represents a device that can store data persistently.
class DataStorage : public Device
{
private:
	XWORD    *m_data;       // The data.
	uint32_t  m_word_count; // The number of words in the data array.

public:
	/// @brief Initializes a DataStorage.
	/// @param size The number of words in the data storage.
	explicit DataStorage(uint32_t size);

	/// @brief Frees resources of DataStorage.
	~DataStorage( void );

	/// @brief Reads from the data storage and returns the data read.
	/// @param i The memory location to read from.
	/// @return The 16-bit value that was read.
	XWORD Read(U16 i);

	/// @brief Writes data to the data storage.
	/// @param i The memory location to write to.
	/// @param data The data to write to the speficied memory location.
	void Write(U16 i, XWORD data);
};

#endif // XDISK_H
