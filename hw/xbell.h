#ifndef XBELL_H
#define XBELL_H

#include "../xdev.h"
#include "../xarch.h"

class Bell : public Device
{
public:
	Bell( void );

	void Alarm( void ) const;
};

#endif