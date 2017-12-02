#ifndef XDEV_H__
#define XDEV_H__

#include "MiniLib/MTL/mtlString.h"
#include "MiniLib/MTL/mtlDuplex.h"
#include "xarch.h"

namespace xerx
{
	class Device
	{
	public:
		enum Type
		{
			Compute,
			Storage,
			Display,
			Keyboard,
			Audio,
			Other
		};
		typedef mtlDuplex<Device,Device> Connection;

		static bool Connect(Device &a, Device &b);
		static void Disconnect(Device &a, Device &b);

		static const xerx::uword MAX_CONNECTIONS = 16;
		static const xerx::uword PORT_MASK       = (MAX_CONNECTIONS - 1) << 8;
		static const xerx::uword FUNC_MASK       = 0xff;
		static const xerx::uword STR_BUF         = 32;

	private:
		char         m_str[STR_BUF];
		Connection  *m_ports[MAX_CONNECTIONS];
		xerx::uword  m_id;
		Type         m_type;

	protected:
		virtual bool Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM) = 0;

	public:
					Device(xerx::uword _id, const mtlChars &_str, Type _type);
		virtual    ~Device( void );
		bool        Operate(xerx::uword func, xerx::uword &param, xerx::word_t *RAM);
		xerx::uword GetID( void ) const;
		const char *GetIDStr( void ) const;
		Connection &Port(xerx::uword port);
		const Connection &Port(xerx::uword port) const;
	};
}

#endif // XDEV_H__
