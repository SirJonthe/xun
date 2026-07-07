#ifndef XKB_H
#define XKB_H

#include "../xdev.h"

/// @brief A keyboard.
class Keyboard : public Device
{
public:
	enum {
		KB_SPACE, KB_SHIFT, KB_BACKSPACE, KB_ENTER,
		KB_A, KB_B, KB_C, KB_D, KB_E, KB_F, KB_G, KB_H, KB_I, KB_J,
		KB_K, KB_L, KB_M, KB_N, KB_O, KB_P, KB_Q, KB_R, KB_S, KB_T,
		KB_U, KB_V, KB_W, KB_X, KB_Y, KB_Z,
		KB_0, KB_1, KB_2, KB_3, KB_4, KB_5, KB_6, KB_7, KB_8, KB_9,
		KB_COUNT
	};

	enum {
		STATE_UP,      // 00
		STATE_PRESS,   // 01
		STATE_RELEASE, // 10
		STATE_HOLD     // 11
	};

private:
	U8 m_state[KB_COUNT];

private:
	/// @brief Shifts the state of the keyboard.
	void ShiftState( void );

	/// @brief Clears the keyboard state.
	void Clear( void );

protected:
	/// @brief Updates the keyboard state.
	virtual void DoCycle( void );

	/// @brief Clears the keyboard state.
	virtual void DoPowerOn( void );

	/// @brief Clears the keyboard state.
	virtual void DoPowerOff( void );

public:
	/// @brief Initializes a Keyboard.
	Keyboard( void );

	/// @brief Sets the active bit on the key.
	void SetActive(uint32_t key);

	/// @brief Returns the state of a key.
	/// @param key The key index to return the state of.
	/// @return The state of the key.
	uint8_t GetState(uint32_t key) const;
};

#endif
