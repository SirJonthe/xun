#ifndef XDISK_H__
#define XDISK_H__

#include <fstream>

#include "xdev.h"
#include "xfs.h"

namespace xerx
{

	class Disk : public xerx::Device
	{
	private:
		enum StreamMode
		{
			StreamMode_Closed,
			StreamMode_Read,
			StreamMode_Write
		};

	private:
		xerx::FileSystem  m_fs;
		StreamMode        m_mode;
		xerx::uword       m_buffer[xerx::MEM_SIZE];
		xerx::uword       m_buffer_size;
		xerx::uword       m_reader;

	private:
		bool BufferFile(const char *file);
		bool TestOpenFile(const char *file);
		void FlushBuffer(const char *file);
	
	private:
		bool        Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM);
		xerx::uword OpenRead(const char *file);
		xerx::uword OpenOverwrite(const char *file);
		xerx::uword OpenAppendWrite(const char *file);
		void        Close( void );
		xerx::uword Read( void );
		void        Write(xerx::uword data);
		
	public:
		Disk(const mtlChars &mount_point);
	};

}



#endif
