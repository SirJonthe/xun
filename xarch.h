#ifndef XARCH_H__
#define XARCH_H__

namespace xerx
{

// @data uword
// @info The architecture unsigned word (16 bits).
typedef unsigned short uword;

// @data sword
// @info The architecture signed word (16 bits).
typedef signed   short sword;
	
// @data udword
// @info The architecture unsigned double word (32 bits).
typedef unsigned int   udword;

// @data word_t
// @info The architecture signed/unsigned word (16 bits).
union word_t
{
	uword u;
	sword s;
};

// @data UMAX
// @info The architecture unsigned word maximum value.
static const uword  UMAX = (uword)-1;
	
// @data SMAX
// @info The architecture signed word maximum value.
static const sword  SMAX = (sword)(UMAX>>1);

// @data SMIN
// @info The architecture signed word minimum value.
static const sword  SMIN = ~SMAX;

// @data MEM_SIZE
// @info The architecture maximum addressable memory locations.
static const udword MEM_SIZE = (udword)UMAX + 1;

// @data WRD_BITS
// @info The architecture bit count.
static const uword  WRD_BITS = 16;

}

#endif // XARCH_H__
