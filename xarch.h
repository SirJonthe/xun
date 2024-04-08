#ifndef XARCH_H
#define XARCH_H

#include <cstdint>
#include <limits>
#include "xis.h"

typedef uint16_t U16;
typedef int16_t  I16;
typedef uint8_t  U8;
typedef int8_t   I8;

static constexpr U16  HILO_IDX = 0x0100; // [0x01][0x00] but let the architecture format it according to its endianess.
static const     U8  *HILO_IDX8 = reinterpret_cast<const uint8_t*>(&HILO_IDX);

// LO8 uses indirection due to unknown endianess. If index 0 points to high byte, then value of high byte is 1, i.e. pointing to the other, low, byte.
#define LO8 HILO_IDX8[0]
// HI8 uses indirection due to unknown endianess. If index 1 points to low byte, then value of low byte is 0, i.e. pointing to the other, high, byte.
#define HI8 HILO_IDX8[1]

enum ERRMASK
{
	ERR_DIV0,
	ERR_OVERFLOW,
	ERR_UNDERFLOW,
	ERR_UNDEF,
	ERR_IO
};

union XBYTE
{
	U8 u;
	I8 i;
};

union XWORD
{
	U16   u;
	I16   i;
	XBYTE hilo[2];
};

constexpr U16 UB_MAX = std::numeric_limits<U8 >::max();
constexpr I16 IB_MAX = std::numeric_limits<I8>::max();
constexpr I16 IB_MIN = std::numeric_limits<I8>::min();
constexpr U16 U_MAX  = std::numeric_limits<U16>::max();
constexpr I16 I_MAX  = std::numeric_limits<I16>::max();
constexpr I16 I_MIN  = std::numeric_limits<I16>::min();

constexpr uint32_t MEM_SIZE_MAX = uint32_t(U_MAX) + 1;

#endif // XARCH_H
