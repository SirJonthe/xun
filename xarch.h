#ifndef XARCH_H__
#define XARCH_H__

namespace xerx
{

typedef unsigned short uword;
typedef signed   short sword;
typedef unsigned int   udword;

union word_t
{
	uword u;
	sword s;
};

static const uword  UMAX = (uword)-1;
static const sword  SMAX = (sword)(UMAX>>1);
static const sword  SMIN = ~SMAX;
static const udword MEM_SIZE = (udword)UMAX + 1;
static const uword  WRD_BITS = 16;

}

#endif // XARCH_H__
