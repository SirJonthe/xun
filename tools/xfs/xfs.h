#ifndef XFS_H
#define XFS_H

#include <map>
#include <string>
#include <vector>
#include "../../hw/xdisk.h"

/// @brief A utility designed to handle the XFS file system, such as writing a disk image.
class XFSUtility
{
private:
	/// @brief The type of a folder entry.
	enum EntryType
	{
		ENTRYTYPE_NONE,   // The entry is unoccupied and can be re-used.
		ENTRYTYPE_FOLDER, // The entry refers to a folder.
		ENTRYTYPE_FILE    // The entry refers to a file.
	};

	/// @brief An entry inside a folder. An entry contains metadata about a file or folder contained within a folder.
	struct Entry
	{
		Addr32   parent;    // The address of the first byte in the first block of the containing folder.
		char     name[64];  // The name of the file/folder.
		Addr32   loc;       // The location of the file/folder.
		uint32_t size;      // The total size of the file/folder.
		uint8_t  type;      // The type of the file/folder (i.e. file or folder).
		char     unused[3]; // Unused padding (may use this for flags later).
	};

	/// @brief A block of data which the entire XFS file system is divided into.
	struct XFSBlock
	{
		/// @brief A block header containing metadata for the block.
		struct Header
		{
			Addr32 next;      // The location of the next block. 0 indicates that there is no next block.
			Addr32 prev;      // The location of the previous block. 0 indicates that there is no previous block.
			Addr32 entry;     // The location of the index entry in the parent folder.
			U16    size;      // The occupied space of the current block excluding the header. This will be 0 if the block is unoccupied.
			char   unused[2]; // Unused padding (may use this for flags later).
		} header; // The header of the block.
		union {
			uint8_t bin[512 - sizeof(Header)];        // File data.
			Entry   rec[sizeof(bin) / sizeof(Entry)]; // Folder data.
		} data;
	};

	struct Folder;

	/// @brief An intermediate representation of a file. External formats, like TAR, are converted into this format, and then this format will be converted into XFS.
	struct File
	{
		Folder      *parent;   // The parent folder that contains this file.
		std::string  contents; // The contents of the file.
	};

	/// @brief An intermediate representation of a folder. External formats, like TAR, are converted into this format, and then this format will be converted into XFS.
	struct Folder
	{
		Folder                        *parent;  // The parent folder that contains this folder.
		std::map<std::string, Folder>  folders; // The folders contained within this folder.
		std::map<std::string, File>    files;   // The files contained within this folder.

		/// @brief Inserts a folder recursively, creating new folders as needed depending on the structure of the path.
		/// @param path The path. Does not support navigation.
		/// @return The inserted folder.
		Folder &InsertFolder(const std::string &path);
		
		/// @brief Inserts a file recursively, creating new folders as needed depending on the structure of the path.
		/// @param path The path. Does not support navigation.
		/// @return The inserted file.
		File &InsertFile(const std::string &path);
		
		/// @brief Prints the contents of the folder.
		/// @param level Indendation level. Contents of a folder will be indented an additional level to make it visually distinct.
		void Info(uint32_t level) const;
	};

private:
	Disk     *m_disk;       // The disk to be manipulated.
	XFSBlock *m_blocks;     // A pointer to the disk memory as XFS blocks.
	uint32_t  m_num_blocks; // The number of full XFS blocks that fits on the disk.
	uint32_t  m_block_i;    // The current block index.

private:
	/// @brief Parses a human readable octal number and converts it to a binary representation.
	/// @param str The octal number.
	/// @param byte_count The number of characters in the octal number.
	/// @return The resulting binary representation of the octal number.
	uint32_t ParseOctal(const char *str, uint32_t byte_count) const;
	
	/// @brief Calculates the null terminated length of a string. If the string is not null terminated, limits the size to a given maximum.
	/// @param s The string.
	/// @param max The maximum length of the string.
	/// @return The length of the string.
	uint32_t StrLen(const char *s, uint32_t max) const;

	/// @brief Constructs a complete path given a prefix and name.
	/// @param prefix A TAR path prefix.
	/// @param name A TAR path name.
	/// @return The final path.
	std::string GetPath(const char *prefix, const char *name) const;
	
	/// @brief Creates a new block.
	/// @return True if there is space to allocate a new block on disk.
	bool NewBlock( void );

	/// @brief Writes a data structure to the disk.
	/// @tparam type_t The type of the data to write.
	/// @param data The data to write.
	/// @param p_entry The entry of this folder inside the parent folder data.
	/// @return True if there is space to write to the disk.
	template < typename type_t >
	bool Write(const type_t &data, Entry *p_entry);
	
	/// @brief Writes a single byte to the disk.
	/// @param x The byte to write.
	/// @param p_entry The entry of this folder inside the parent folder data.
	/// @return True if there is space to write to the disk.
	bool Write(uint8_t x, Entry *p_entry);
	
	/// @brief Writes an intermediary file representation to disk in XFS format.
	/// @param file The file to write.
	/// @param p_entry The entry of this folder inside the parent folder data.
	/// @return True if there is space to write the file to disk.
	bool WriteFile(const File &file, Entry *p_entry);
	
	/// @brief Creates a new blank entry.
	/// @param name The name of the entry (max 64 characters).
	/// @param type The type of the entry (file/folder/none).
	/// @param parent_fold_loc The location of the first byte of the parent folder.
	/// @return The newly created blank entry.
	Entry NewEntry(const std::string &name, uint8_t type, Addr32 parent_fold_loc) const;

	/// @brief Writes an intermediary folder representation as the root-level to disk in XFS format.
	/// @param root The root-level folder to write.
	/// @return True if there is space to write the folder to disk.
	bool WriteRoot(const Folder &root);

	/// @brief Writes an intermediary folder representation to disk in XFS format.
	/// @param folder The folder to write.
	/// @param p_entry The entry of this folder inside the parent folder data.
	/// @return True if there is space to write the folder to disk.
	bool WriteFolder(const Folder &folder, Entry *p_entry);
	
	/// @brief Prints the contents of a block to standard output.
	/// @param b The block to print.
	/// @param type The type of the block.
	/// @param level The indentation level to display the block.
	/// @sa EntryType
	void List(const XFSBlock &b, uint8_t type, uint32_t level) const;

	/// @brief Returns an absolute pointer into disk memory given an address offset.
	/// @tparam type_t The type to cast the absolute pointer to.
	/// @param  addr The relative offset into the memory, in bytes.
	/// @return The absolute pointer.
	template < typename type_t >
	type_t *GetPtr(uint32_t addr);

	/// @brief Returns an absolute pointer into disk memory given an address offset.
	/// @tparam type_t The type to cast the absolute pointer to.
	/// @param  addr The relative offset into the memory, in bytes.
	/// @return The absolute pointer.
	template < typename type_t >
	const type_t *GetPtr(uint32_t addr) const;

	/// @brief Generate a relative pointer from an absolute pointer.
	/// @param ptr The absolute pointer.
	/// @return The relative pointer.
	uint32_t RelPtr(const void *ptr) const;

	/// @brief Convert a 32-bit number into a bank and location.
	/// @param addr The 32-bit number.
	/// @return The bank and location.
	Addr32 ToAddr32(uint32_t addr) const;

	/// @brief
	/// @param folder
	/// @param p_entry
	/// @param addr
	/// @return
	bool HealthCheckFolder(const XFSBlock *folder, const Entry *p_entry, std::vector<Addr32> &addr) const;

	/// @brief
	/// @param file
	/// @param p_entry
	/// @param addr
	/// @return
	bool HealthCheckFile(const XFSBlock *file, const Entry *p_entry, std::vector<Addr32> &addr) const;

	bool HealthCheckLinkedBlocksForward(const XFSBlock *block, const Entry *p_entry, const XFSBlock *prev, uint32_t accum_size) const;

	bool HealthCheckLinkedBlocksBackwards(const XFSBlock *block, const Entry *p_entry, const XFSBlock *prev, uint32_t accum_size) const;

	struct IntegrityNode
	{
		IntegrityNode  *prev;
		const XFSBlock *block;
	};

public:
	/// @brief Creates a new XFSUtility object capable of manipulate a given disk.
	/// @param disk The disk to manipulate.
	XFSUtility(Disk &disk);

	/// @brief Wipes data from the disk and replaces it with zeroes.
	void Wipe( void );

	/// @brief Takes a TAR file format and writes it as XFS file system to the disk.
	/// @param tar_binary The TAR file in raw binary format.
	/// @param byte_count The number of bytes in the TAR binary.
	/// @return True if there is space to convert the 
	/// @note Only straight files and folders are supported in the TAR file. Everything else will not be converted to XFS format.
	/// @warning Wipes the data on the disk before writing. The data is lost regardless of if the write process succeeded.
	bool WriteTAR(const uint8_t *tar_binary, uint32_t byte_count);

	/// @brief Prints the contents of the disk to standard output.
	void List( void ) const;

	/// @brief Analyzes the disk and reports any obvious data corruption issues.
	/// @return True if there were no errors detected.
	bool HealthCheck( void ) const;
};

template < typename type_t >
bool XFSUtility::Write(const type_t &data, Entry *p_entry)
{
	// TODO: We need to specialize this on built-in data types to take endianness into account.
	const uint8_t *bytes = reinterpret_cast<const uint8_t*>(&data);
	for (uint32_t i = 0; i < sizeof(data); ++i) {
		if (!Write(bytes[i], p_entry)) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
type_t *XFSUtility::GetPtr(uint32_t addr)
{
	if (m_blocks == nullptr || sizeof(XFSBlock) * m_num_blocks < addr) { return nullptr; }
	return reinterpret_cast<type_t*>(m_disk->GetData() + addr);
}

template < typename type_t >
const type_t *XFSUtility::GetPtr(uint32_t addr) const
{
	if (m_blocks == nullptr || sizeof(XFSBlock) * m_num_blocks < addr) { return nullptr; }
	return reinterpret_cast<type_t*>(m_disk->GetData() + addr);
}

#endif
