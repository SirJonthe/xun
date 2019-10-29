#ifndef XFS_H__
#define XFS_H__

#include "xbin.h"

namespace xerx
{

// @data FileSystem
// @info Contains a virtual file system for use by the virtual machine.
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

	// @algo GetHandle
	// @info Gets a unique handle (UUID) of a binary file.
	// @in bin -> The absolute path to the virtual binary file.
	// @out The unique handle (UUID). 0 = file not found.
	xerx::uword GetHandle(const mtlChars &bin) const; // 0 = file not found
	
	// @algo GetSize
	// @info Returns the size (in words) of a specified item.
	// @in hnd -> The handle of the virtual binary file.
	// @inout size -> The size (in words) of the specified item.
	// @out SUCCESS if the handle is valid and points to a binary item.
	bool        GetSize(xerx::uword hnd, xerx::udword &size);
	
	// @algo CreateBin
	// @info Creates a binary virtual file in the virtual file system.
	// @in bin -> The absolute path of the file created.
	// @out TRUE on success.
	bool        CreateBin(const mtlChars &bin);
	
	// @algo DeleteBin
	// @info Deletes a binary. May fail if file does not exist or is locked.
	// @in hnd -> The virtual file handle.
	// @out TRUE on successful delete.
	bool        DeleteBin(xerx::uword hnd);
	
	// @algo BeginReadBin
	// @info Begins a read session on a virtual file and locks file from write by others
	// @in
	//   hnd -> The virtual file handle.
	//   start -> The starting location of the read.
	//   size -> The number of words to read.
	// @inout out -> The binary data read.
	// @out TRUE on successful read.
	bool        BeginReadBin(xerx::uword hnd, xerx::udword start, xerx::udword size, Binary &out);
	
	// @algo EndReadBin
	// @info Ends a read session on a virtual file.
	// @in hnd -> The virtual file handle.
	void        EndReadBin(xerx::uword hnd);
	
	// @algo BeginWriteBin
	// @info Begins write session on a virtual file and locks file from read/write by others.
	// @in
	//   hnd -> The virtual file handle.
	// @inout out -> The binary to write. (?)
	// @out TRUE on successful write.
	bool        BeginWriteBin(xerx::uword hnd, Binary &out);
	
	// @algo EndWriteBin
	// @info Closes the writing session.
	// @in hnd -> The virtual file handle.
	void        EndWriteBin(xerx::uword hnd);
	
	// @algo Save
	// @info Saves the state of the virtual file system to the HDD.
	// @in file -> The file to create or overwrite.
	// @out TRUE on write success.
	bool        Save(const mtlChars &file); // save to physical HDD
	
	// @algo Load
	// @info Loads a virtual HDD file from disk and stores in virtual file system.
	// @in file -> The path (on disk) to the HDD file.
	// @out TRUE on read operation success.
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
