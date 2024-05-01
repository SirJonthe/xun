#include "xhwids.h"
#include "xpwr.h"

PowerController::PowerController( void ) : Device("XERXES(tm) Power Controller Circuit", XHWID_PWR)
{
	SetCyclesPerSecond(0);
}
