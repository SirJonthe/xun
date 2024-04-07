#ifndef XDISK_H
#define XDISK_H

#include <string>
#include <list>

#include "../xdev.h"
#include "../xarch.h"

class DataStorage : public Device
{
private:
	XWORD    *m_data;
	uint32_t  m_word_count;

public:
	explicit DataStorage(uint32_t size);
	~DataStorage( void );

	XWORD Read(U16 i);
	void  Write(U16 i, XWORD data);

	// Cycle -> Move head, even if we are not reading or writing
};


// NOTE:
// For a complete VM and better portability, this is better to implement in XASM inside an OS as an interface between Machine and Disk
/*class FileSystem
{
	// reinterpret disk memory as a set of pages
	// index (pointers to files)
	// actual files

	struct Page
	{
		// only one file can be contained within
		// if a file is split over multiple pages, some of the memory points to the next page

		U16 data[512];
		enum {
			FORMAT, // 0 = FILE, 1 = FOLDER
			NEXT,   // 0 = No more data, N = FILE/FOLDER continues on next Page index N
			PREV    // != 0, then
		};
	};

	Page        *m_fs; // reinterpret_cast disk data to this format
	U16          m_page_count;
	Page        *m_current_page;
	std::string  m_cwd;

public:
	void CreateFolder(const std::string &name);
	void RemoveFolder(const std::string &name);

	void CreateFile(const std::string &name);
	void RemoveFile(const std::string &name);
	U16 OpenFile(const std::string &name, U16 mode);

	void Read(U16 num_bytes, void *out);
	void Write(U16 num_bytes, void *data);

	const std::string &GetCurrentDirectory( void ) const;

	void ChangeDirectory(const std::string &rel_dir);
};*/

#endif // XDISK_H
