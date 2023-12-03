#include <fstream>
#include "lib/MiniLib/MTL/mtlString.h"
#include "xsrc.h"

void Source::SetSource(const std::string &code)
{
	m_src = code;
}

bool Source::ReadSource(const std::string &file)
{
	std::ifstream fin(file, std::ios::ate|std::ios::binary);
	if (!fin.is_open()) {
		m_src.clear();
		return false;
	}
	m_src.resize(fin.tellg());
	char *buffer = new char[fin.tellg()];
	fin.seekg(0, std::ios::beg);
	fin.read(buffer, m_src.size()).bad();
	for (size_t i = 0; i < m_src.size(); ++i) {
		m_src[i] = buffer[i];
	}
	delete [] buffer;

	return true;
}

const std::string &Source::GetSource( void ) const
{
	return m_src;
}
