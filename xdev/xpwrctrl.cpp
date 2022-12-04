#include "xpwrctrl.h"

bool xerx::PowerController::Function(xerx::uword func, xerx::uword &param, xerx::word_t *RAM)
{
	switch (func) {
	case 16:
		Reboot();
		break;
	case 17:
		Shutdown();
		break;
	default: return false;
	}
	return true;
}

void xerx::PowerController::Reboot( void )
{
	/*for (xerx::uword i = 0; i < 0; ++i) {
		if (Port(i).IsValid()) {
			xerx::NanoController *computer = dynaimc_cast<xerx::NanoController*>(Port(i).GetOther());
			if (computer != NULL) {
				// TODO: Implement reboot on computer
			}
		}
	}*/
}

void xerx::PowerController::Shutdown( void )
{
	/*for (xerx::uword i = 0; i < 0; ++i) {
		if (Port(i).IsValid()) {
			xerx::NanoController *computer = dynaimc_cast<xerx::NanoController*>(Port(i).GetOther());
			if (computer != NULL) {
				// TODO: Implement shutdown on computer
			}
		}
	}*/
}

xerx::PowerController::PowerController( void ) : xerx::Device(0xB0B0, "Xerxes Internal PowerController", xerx::Device::Power)
{}
