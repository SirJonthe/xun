#ifndef XFS_H__
#define XFS_H__

#include "xbin.h"

namespace xerx
{

class FileSystem
{
private:
	class Dir;
	class Item
	{
	private:
		enum Type
		{
			DIR,
			BIN
		};
		
	protected:
		mtlChars name;
		Dir      *parent;
		Type      type;
	};
	class Dir : public Item
	{
	public:
		mtlList<Item> items;
	};
	class Bin : public Item
	{
	private:
		Binary data;
	};

	Dir tree;
	
public:
	// All paths must be absolute
	// Handle relative paths and working directories on the OS level

	xerx::uword GetHandle(const mtlChars &bin) const; // 0 = file not found
	bool        GetSize(xerx::uword hnd, xerx::udword &size);
	
	bool        CreateBin(const mtlChars &bin);
	bool        DeleteBin(xerx::uword hnd);
	
	bool        BeginReadBin(xerx::uword hnd, xerx::udword start, xerx::udword size, Binary &out); // locks file from write by others
	void        EndReadBin(xerx::uword hnd);
	
	bool        BeginWriteBin(xerx::uword hnd, Binary &out); // locks file from read/write by others
	void        EndWriteBin(xerx::uword hnd);
	
	bool        Save(const mtlChars &file); // save to physical HDD
	bool        Load(const mtlChars &file); // load from physical HDD
};

}

#endif

// File system format, serialized
//IDX
//{
//	"NAME.EXT", BIN , SIZE @ LOC, HANDLE
//	"NAME", DIR , SIZE @ LOC
//}
//"NAME.EXT", BIN, SIZE, HANDLE
//	01010101010111010110101110101101 etc.
//"NAME", DIR, SIZE
//	IDX
//	{
//		"NAME.EXT", BIN, SIZE @ LOC, HANDLE
//		"NAME", DIR, SIZE @ LOC
//	}
//	etc.
