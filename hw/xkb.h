#ifndef XKB_H
#define XKB_H

#include "../xdev.h"

/// @brief A keyboard.
class Keyboard : public Device
{
public:
	enum {
		KB_SPACE, KB_BACKSPACE, KB_ENTER,
		KB_A, KB_B, KB_C, KB_D, KB_E, KB_F, KB_G, KB_H, KB_I, KB_J,
		KB_K, KB_L, KB_M, KB_N, KB_O, KB_P, KB_Q, KB_R, KB_S, KB_T,
		KB_U, KB_V, KB_W, KB_X, KB_Y, KB_Z,
		KB_0, KB_1, KB_2, KB_3, KB_4, KB_5, KB_6, KB_7, KB_8, KB_9,
		
		// Keys that modify the behavior of other keys
		KB_MOD_SHIFT,
		
		KB_COUNT
	};

	enum {
		STATE_UP,      // 00
		STATE_PRESS,   // 01
		STATE_RELEASE, // 10
		STATE_HOLD     // 11
	};

	static constexpr U16 MSG_KBDELTA = 0xdf0e;

	static constexpr U16 IRQ_STATECHANGE = 26;

private:
	U8   m_state[KB_COUNT];
	bool m_delta;

private:
	/// @brief Shifts the state of the keyboard.
	void ShiftState( void );

	/// @brief Clears the keyboard state.
	void Clear( void );

protected:
	/// @brief Updates the keyboard state.
	void DoCycle( void ) override;

	/// @brief Clears the keyboard state.
	void DoPowerOn( void ) override;

	/// @brief Clears the keyboard state.
	void DoPowerOff( void ) override;

public:
	/// @brief Initializes a Keyboard.
	Keyboard( void );

	/// @brief Sets the active bit on the key to 1.
	/// @param key The key index.
	void SetActive(uint32_t key);

	/// @brief Sets the active bit on the key to 0.
	/// @param key The key index.
	void SetInactive(uint32_t key);

	/// @brief Returns the state of a key.
	/// @param key The key index to return the state of.
	/// @return The state of the key.
	uint8_t GetState(uint32_t key) const;
};

#endif
