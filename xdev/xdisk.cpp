#include <fstream>
#include "xdisk.h"

bool xerx::Disk::BufferFile(const char *file)
{
	std::ifstream fin(file, std::ios::ate|std::ios::binary);
	if (!fin.is_open()) { return false; }
	std::streamsize size = fin.tellg();
	if (fin.tellg() > (std::streamsize)MEM_SIZE * sizeof(xerx::uword)) {
		size = (std::streamsize)MEM_SIZE * sizeof(xerx::uword);
	}
	m_buffer_size = size / sizeof(xerx::uword) + (size % sizeof(xerx::uword));
	fin.seekg(0, std::ios::beg);
	return !fin.read((char*)m_buffer, size).bad();
}

bool xerx::Disk::TestOpenFile(const char *file)
{
	std::ifstream fin(file, std::ios::ate|std::ios::binary);
	if (!fin.is_open()) { return false; }
	return true;
}

void xerx::Disk::FlushBuffer(const char *file)
{
	std::ofstream fout(file, std::ios::binary);
	if (!fout.is_open()) { return; }
	fout.write((char*)m_buffer, m_buffer_size * sizeof(xerx::uword));
}

bool xerx::Disk::Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM)
{
	switch (func) {
	case 16:
		OpenRead((const char*)(RAM + param));
		break;
	case 17:
		OpenOverwrite((const char*)(RAM + param));
		break;
	case 18:
		OpenAppendWrite((const char*)(RAM + param));
		break;
	case 19:
		Close();
		break;
	case 20:
		param = Read();
		break;
	case 21:
		Write(param);
		break;
	default: return false;
	}
	return true;
}

xerx::uword xerx::Disk::OpenRead(const char *file)
{
	m_mode = StreamMode_Read;
	bool ret_val = BufferFile(file);
	m_reader = 0;
	return ret_val ? 1 : 0;
}

xerx::uword xerx::Disk::OpenOverwrite(const char *file)
{
	m_mode        = StreamMode_Write;
	m_buffer_size = 0;
	m_reader      = 0;
	return TestOpenFile(file) ? 1 : 0;
}

xerx::uword xerx::Disk::OpenAppendWrite(const char *file)
{
	m_mode = StreamMode_Write;
	bool ret_val = BufferFile(file);
	m_reader = m_buffer_size;
	return ret_val ? 1 : 0;
}

void xerx::Disk::Close( void )
{
	m_mode = StreamMode_Closed;
	//FlushBuffer(file);
	m_buffer_size = 0;
	m_reader = 0;
}

xerx::uword xerx::Disk::Read( void )
{
	return m_buffer[m_reader++];
}

void xerx::Disk::Write(xerx::uword data)
{
	m_buffer[m_reader++] = data;
	m_buffer_size = m_reader;
}

xerx::Disk::Disk(const mtlChars &mount_point) : Device(0x1EAD, "Xerxes Storage Disk", xerx::Device::Storage), /*m_fs(mount_point),*/ m_mode(StreamMode_Closed), m_buffer_size(0), m_reader(0)
{
}
