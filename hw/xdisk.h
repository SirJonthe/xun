#ifndef XDISK_H
#define XDISK_H

#include <string>
#include <list>

#include "../xdev.h"
#include "../xarch.h"

/// @brief Holds data.
class Disk
{
private:
	XWORD    *m_data;       // The data.
	uint32_t  m_word_count; // The number of words in the data array.

public:
	/// @brief Initializes a Disk.
	/// @param size The number of words in the data storage.
	explicit Disk(uint32_t size);

	/// @brief Cleans up used resources.
	~Disk( void );

	/// @brief Reads from the data storage and returns the data read.
	/// @param i The memory location to read from.
	/// @return The 16-bit value that was read.
	XWORD Read(uint32_t i);

	/// @brief Writes data to the data storage.
	/// @param i The memory location to write to.
	/// @param data The data to write to the speficied memory location.
	void Write(uint32_t i, XWORD data);

	/// @brief Returns the capacity of the disk in words.
	/// @return The capacity of the disk in words.
	uint32_t GetCapacity( void ) const;
};

/// @brief Represents a device that can store data persistently.
class DiskReader : public Device
{
public:
	enum {
		MSG_TYPE_READ  = 0xf00d,
		MSG_TYPE_WRITE = 0xfeed,
		MSG_TYPE_INFO  = 0xbead
	};

private:
	Disk *m_attachment;

protected:
	/// @brief 
	/// @param msg 
	/// @return 
	bool HandlePacket(const Device::Packet &msg);

public:
	/// @brief Initializes a DiskReader instance.
	DiskReader( void );

	/// @brief Reads from the data storage and returns the data read.
	/// @param i The memory location to read from.
	/// @return The 16-bit value that was read.
	XWORD Read(uint32_t i);

	/// @brief Writes data to the data storage.
	/// @param i The memory location to write to.
	/// @param data The data to write to the speficied memory location.
	void Write(uint32_t i, XWORD data);

	/// @brief Determines if there is an attachment in this reader.
	/// @return True if there is an attachment.
	bool HasAttachment( void ) const;

	/// @brief Attaches a disk. Fails if there is already 
	/// @param disk The disk to attach.
	/// @return True if the disk was successfully attached. False if there was already another disk attached.
	bool Attach(Disk *disk);

	/// @brief Ejects the attached disk.
	void Eject( void );

	/// @brief Returns the capacity of the attached disk in words.
	/// @return The capacity of the attached disk in words.
	uint32_t GetCapacity( void ) const;
};

#endif // XDISK_H
