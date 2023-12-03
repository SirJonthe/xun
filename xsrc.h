#ifndef XSRC_H
#define XSRC_H

#include <string>

class Source
{
private:
	std::string m_src;

public:
	void SetSource(const std::string &code);
	bool ReadSource(const std::string &file);

	const std::string &GetSource( void ) const;
};

#endif // XSRC_H
