#ifndef XPWRCTRL_H__
#define XPWRCTRL_H__

#include "xdev.h"

namespace xerx
{

	class PowerController : public xerx::Device
	{
	private:
		bool Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM);
		void Reboot( void );
		void Shutdown( void );

	public:
		PowerController( void );
	};

}

#endif
