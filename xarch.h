#ifndef XARCH_H
#define XARCH_H

#include <cstdint>
#include <limits>
#include "xis.h"

typedef uint16_t U16; // A 16-bit unsigned integer.
typedef int16_t  I16; // A 16-bit signed integer.
typedef uint8_t  U8;  // An 8-bit unsigned integer.
typedef int8_t   I8;  // An 8-bit signed integer.

static constexpr U16  HILO_IDX = 0x0100;                                       // A series of byte values ([0x01][0x00]) used to help re-order encoded endianness into a specific endian. The host architecture will order this according to its endianness, i.e. most likely into 0x01,0x00 or 0x00,0x01.
static const     U8  *HILO_IDX8 = reinterpret_cast<const uint8_t*>(&HILO_IDX); // The HILO_IDX reinterpreted as a byte array.

// LO8 uses indirection due to unknown endianess. If index 0 points to high byte, then value of high byte is 1, i.e. pointing to the other, low, byte.
#define LO8 HILO_IDX8[0]
// HI8 uses indirection due to unknown endianess. If index 1 points to low byte, then value of low byte is 0, i.e. pointing to the other, high, byte.
#define HI8 HILO_IDX8[1]

/// @brief Contains the bit index values of the error bits used in a XUN machine.
enum ERRMASK
{
	ERR_DIV0,      // Error: Division by zero.
	ERR_OVERFLOW,  // Error: Arithmetic overflow.
	ERR_UNDERFLOW, // Error: Arithmetid underflow.
	ERR_UNDEF,     // Error: Undefined error.
	ERR_IO         // Error: Generic I/O.
};

/// @brief A byte value that can be used to access both signed and unsigned versions of itself.
union XBYTE
{
	U8 u; // The unsigned value.
	I8 i; // The signed value.
};

/// @brief A 16-bit value used in the XUN architecture. This is the both the smallest addressable unit and the working word size. Can be used to access both signed, unsigned, and individual bytes.
union XWORD
{
	U16   u;       // The unsigned value.
	I16   i;       // The signed value.
	XBYTE hilo[2]; // The individual bytes of the value.
};

constexpr U16 UB_MAX = std::numeric_limits<U8 >::max(); // The maximum range of an 8-bit unsigned integer.
constexpr I16 IB_MAX = std::numeric_limits<I8>::max();  // The minimum range of an 8-bit signed integer.
constexpr I16 IB_MIN = std::numeric_limits<I8>::min();  // The maximum range of an 8-bit signed integer.
constexpr U16 U_MAX  = std::numeric_limits<U16>::max(); // The maximum rage of a 16-bit unsigned integer.
constexpr I16 I_MAX  = std::numeric_limits<I16>::max(); // The minimum range of a 16-bit signed integer.
constexpr I16 I_MIN  = std::numeric_limits<I16>::min(); // The maximum range of a 16-bit signed integer.

constexpr uint32_t MEM_SIZE_MAX = uint32_t(U_MAX) + 1; // The maximum number of addressable words in the 16-bit XUN architecture.

/// @brief A data structure used to access 32-bit memory locations.
struct Addr32
{
	U16 bank; // The memory bank.
	U16 loc;  // The memory location inside the given bank.

	uint32_t Flat( void ) const { return (uint32_t(bank) << 16) + uint32_t(loc); }
};

#endif // XARCH_H
