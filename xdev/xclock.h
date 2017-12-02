#ifndef XCLOCK__
#define XCLOCK__

#include "xdev.h"

namespace xerx
{

	class Clock : public xerx::Device
	{
	private:
		xerx::uword GetMS( void ) const;
		xerx::uword GetS( void ) const;
		xerx::uword GetH( void ) const;
		xerx::uword GetY( void ) const;
		void        Halt(xerx::uword time_ms) const;
		bool Function(uword func, uword &param, word_t *RAM);

	public:
		Clock( void );
	};

}

#endif // XCLOCK__
