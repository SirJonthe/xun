#include <cstdlib>
#include "xb.h"
#include "../../../xis.h"
#include "../../../xarch.h"
#include "../../../lib/tokn/tokn.h"
#include "../xasm/xasm.h"

using namespace cc0::tokn;

/// @brief Constructs a new parser state from an end token.
/// @param end A token user type representing the end of the token stream.
/// @return A new parser state.
#define new_state(end) xcc_new_state(ps, end, ps.break_ip, ps.continue_ip, ps.loop_scope)

/// @brief Manages the parser state so it properly rewinds if the parsing fails.
/// @param success The success of the parsing.
/// @return The success of the parsing.
#define manage_state(success) xcc_manage_state(ps, ps.p->error.code == xcc_error::NONE && (success))

/// @brief Sets an error in the parser.
/// @param p The parser.
/// @param t The token causing the error.
/// @param code The error code.
#define set_error(p, t, code) xcc_set_error(p, t, code, p->file, __FILE__, __LINE__)

/// @brief Converts a series of characters representing a human-readable hexadecimal string into a binary number.
/// @param nums The characters representing the human-readable hexadecimal string.
/// @param len The length of the input string.
/// @return The binary number.
/// @warning This function does not verify the correctness of the input characters. The output of an incorrect input is undefined.
/// @note The input string is assumed to be prepended with an "0x".
static unsigned hex2u(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 2; i < len; ++i) {
		if (nums[i] >= '0' && nums[i] <= '9') {
			h = h  * 16 + nums[i] - '0';
		} else if (nums[i] >= 'a' && nums[i] <= 'f') {
			h = h  * 16 + nums[i] - 'a' + 10;
		} else if (nums[i] >= 'A' && nums[i] <= 'F') {
			h = h  * 16 + nums[i] - 'A' + 10;
		}
	}
	return h;
}

/// @brief Converts a series of characters representing a human-readable octal string into a binary number.
/// @param nums The characters representing the human-readable octal string.
/// @param len The length of the input string.
/// @return The binary number.
/// @warning This function does not verify the correctness of the input characters. The output of an incorrect input is undefined.
/// @note The input string is assumed to be prepended with an "0".
static unsigned oct2u(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 1; i < len; ++i) {
		if (nums[i] >= '0' && nums[i] <= '7') {
			h = h  * 8 + nums[i] - '0';
		}
	}
	return h;
}

/// @brief Token data specific to the XB programming language.
struct xbtoken
{
	/// @brief Token types specific to the XB programming language.
	enum tokentype
	{
		KEYWORD_TYPE_AUTO,
		KEYWORD_TYPE_CONST,
		KEYWORD_TYPE_STATIC,
		KEYWORD_TYPE_VOID,
		KEYWORD_TYPE_UNSIGNED,
		KEYWORD_TYPE_SIGNED,
		KEYWORD_CONTROL_RETURN,
		KEYWORD_CONTROL_IF,
		KEYWORD_CONTROL_ELSE,
		KEYWORD_CONTROL_WHILE,
		KEYWORD_CONTROL_BREAK,
		KEYWORD_CONTROL_CONTINUE,
		KEYWORD_INTRINSIC_ASM,
		KEYWORD_NAMESPACE,
		KEYWORD_INCLUDE,
		KEYWORD_ENUM,
		OPERATOR_ARITHMETIC_ADD,
		OPERATOR_ARITHMETIC_SUB,
		OPERATOR_ARITHMETIC_MUL,
		OPERATOR_ARITHMETIC_DIV,
		OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ARITHMETIC_INC,
		OPERATOR_ARITHMETIC_DEC,
		OPERATOR_ASSIGNMENT_SET,
		OPERATOR_ARITHMETIC_ASSIGNMENT_ADD,
		OPERATOR_ARITHMETIC_ASSIGNMENT_SUB,
		OPERATOR_ARITHMETIC_ASSIGNMENT_MUL,
		OPERATOR_ARITHMETIC_ASSIGNMENT_DIV,
		OPERATOR_ARITHMETIC_ASSIGNMENT_MOD,
		OPERATOR_BITWISE_ASSIGNMENT_AND,
		OPERATOR_BITWISE_ASSIGNMENT_OR,
		OPERATOR_BITWISE_ASSIGNMENT_XOR,
		OPERATOR_BITWISE_ASSIGNMENT_LSHIFT,
		OPERATOR_BITWISE_ASSIGNMENT_RSHIFT,
		OPERATOR_ENCLOSE_PARENTHESIS_L,
		OPERATOR_ENCLOSE_PARENTHESIS_R,
		OPERATOR_ENCLOSE_BRACKET_L,
		OPERATOR_ENCLOSE_BRACKET_R,
		OPERATOR_ENCLOSE_BRACE_L,
		OPERATOR_ENCLOSE_BRACE_R,
		OPERATOR_ENCLOSE_SINGLEQUOTE,
		OPERATOR_ENCLOSE_DOUBLEQUOTE,
		OPERATOR_MACRO,
		OPERATOR_LOGICAL_LESS,
		OPERATOR_LOGICAL_LESSEQUAL,
		OPERATOR_LOGICAL_GREATER,
		OPERATOR_LOGICAL_GREATEREQUAL,
		OPERATOR_LOGICAL_EQUAL,
		OPERATOR_LOGICAL_NOTEQUAL,
		OPERATOR_LOGICAL_AND,
		OPERATOR_LOGICAL_OR,
		OPERATOR_LOGICAL_NOT,
		OPERATOR_BITWISE_AND,
		OPERATOR_BITWISE_OR,
		OPERATOR_BITWISE_XOR,
		OPERATOR_BITWISE_NOT,
		OPERATOR_BITWISE_LSHIFT,
		OPERATOR_BITWISE_RSHIFT,
		OPERATOR_SEMICOLON,
		OPERATOR_COLON,
		OPERATOR_COMMA,
		OPERATOR_HASH,
		OPERATOR_REVERSE_SEARCH,
		OPERATOR_SIZEOF,
		LITERAL_INT
		// LITERAL_FLOAT
	};
};

const signed XB_TOKEN_COUNT = 69; // The number of tokens defined for the programming language.
const token XB_TOKENS[XB_TOKEN_COUNT] = { // The tokens defined for the programming language.
	new_keyword ("return",                  6, xbtoken::KEYWORD_CONTROL_RETURN),
	new_keyword ("if",                      2, xbtoken::KEYWORD_CONTROL_IF),
	new_keyword ("else",                    4, xbtoken::KEYWORD_CONTROL_ELSE),
	new_keyword ("while",                   5, xbtoken::KEYWORD_CONTROL_WHILE),
	new_keyword ("break",                   5, xbtoken::KEYWORD_CONTROL_BREAK),
	new_keyword ("continue",                8, xbtoken::KEYWORD_CONTROL_CONTINUE),
	new_keyword ("asm",                     3, xbtoken::KEYWORD_INTRINSIC_ASM),
	new_keyword ("auto",                    4, xbtoken::KEYWORD_TYPE_AUTO),
	new_keyword ("const",                   5, xbtoken::KEYWORD_TYPE_CONST),
	new_keyword ("static",                  6, xbtoken::KEYWORD_TYPE_STATIC),
	new_keyword ("void",                    4, xbtoken::KEYWORD_TYPE_VOID),
	new_keyword ("unsigned",                8, xbtoken::KEYWORD_TYPE_SIGNED),
	new_keyword ("signed",                  6, xbtoken::KEYWORD_TYPE_SIGNED),
	new_keyword ("namespace",               9, xbtoken::KEYWORD_NAMESPACE),
	new_keyword ("include",                 7, xbtoken::KEYWORD_INCLUDE),
	new_keyword ("enum",                    4, xbtoken::KEYWORD_ENUM),
	new_keyword ("sizeof",                  6, xbtoken::OPERATOR_SIZEOF),
	new_operator("#",                       1, xbtoken::OPERATOR_HASH),
	new_operator("+=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_ADD),
	new_operator("-=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_SUB),
	new_operator("*=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MUL),
	new_operator("/=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_DIV),
	new_operator("%=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MOD),
	new_operator("&=",                      2, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_AND),
	new_operator("|=",                      2, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_OR),
	new_operator("^=",                      2, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_XOR),
	new_operator("<<=",                     3, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_LSHIFT),
	new_operator(">>=",                     3, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_RSHIFT),
	new_operator("<=",                      2, xbtoken::OPERATOR_LOGICAL_LESSEQUAL),
	new_operator(">=",                      2, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL),
	new_operator("==",                      2, xbtoken::OPERATOR_LOGICAL_EQUAL),
	new_operator("!=",                      2, xbtoken::OPERATOR_LOGICAL_NOTEQUAL),
	new_operator("&&",                      2, xbtoken::OPERATOR_LOGICAL_AND),
	new_operator("||",                      2, xbtoken::OPERATOR_LOGICAL_OR),
	new_operator("<<",                      2, xbtoken::OPERATOR_BITWISE_LSHIFT),
	new_operator(">>",                      2, xbtoken::OPERATOR_BITWISE_RSHIFT),
	new_operator("++",                      2, xbtoken::OPERATOR_ARITHMETIC_INC),
	new_operator("--",                      2, xbtoken::OPERATOR_ARITHMETIC_DEC),
	new_operator("::",                      2, xbtoken::OPERATOR_REVERSE_SEARCH),
	new_comment ("//",                      2),
	new_operator("!",                       1, xbtoken::OPERATOR_LOGICAL_NOT),
	new_operator("#",                       1, xbtoken::OPERATOR_MACRO),
	new_operator("\"",                      1, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE),
	new_operator("\'",                      1, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE),
	new_operator("(",                       1, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator("{",                       1, xbtoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, xbtoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("[",                       1, xbtoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, xbtoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("+",                       1, xbtoken::OPERATOR_ARITHMETIC_ADD),
	new_operator("-",                       1, xbtoken::OPERATOR_ARITHMETIC_SUB),
	new_operator("*",                       1, xbtoken::OPERATOR_ARITHMETIC_MUL),
	new_operator("/",                       1, xbtoken::OPERATOR_ARITHMETIC_DIV),
	new_operator("%",                       1, xbtoken::OPERATOR_ARITHMETIC_MOD),
	new_operator("=",                       1, xbtoken::OPERATOR_ASSIGNMENT_SET),
	new_operator(",",                       1, xbtoken::OPERATOR_COMMA),
	new_operator(";",                       1, xbtoken::OPERATOR_SEMICOLON),
	new_operator(":",                       1, xbtoken::OPERATOR_COLON),
	new_operator("<",                       1, xbtoken::OPERATOR_LOGICAL_LESS),
	new_operator(">",                       1, xbtoken::OPERATOR_LOGICAL_GREATER),
	new_operator("&",                       1, xbtoken::OPERATOR_BITWISE_AND),
	new_operator("|",                       1, xbtoken::OPERATOR_BITWISE_OR),
	new_operator("^",                       1, xbtoken::OPERATOR_BITWISE_XOR),
	new_operator("~",                       1, xbtoken::OPERATOR_BITWISE_NOT),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,   token::ALIAS),
	new_literal ("0[0-9]+",                 7, xbtoken::LITERAL_INT, oct2u),
	new_literal ("[0-9]+",                  6, xbtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xbtoken::LITERAL_INT, hex2u)
};

/// @brief Reads a token from the lexer and tries to match it against known token types defined for the current programming language.
/// @param l The lexer.
/// @return The token.
token xblex(lexer *l)
{
	return lex(l, XB_TOKENS, XB_TOKEN_COUNT);
}

/// @brief Reads a single character and outputs it as a token.
/// @param l The lexer.
/// @return The token.
/// @note May also output white spaces as tokens.
token xblex1(lexer *l)
{
	return chlex(l);
}

/// @brief Creates space in the binary for the creation of a new variable.
/// @param p The parser.
/// @param sym The symbol.
/// @return False if the output binary buffer is full. True otherwise.
static bool emit_empty_symbol_storage(xcc_parser *p, const xcc_symbol *sym)
{
	if (sym->storage != xcc_symbol::STORAGE_PARAM && sym->storage != xcc_symbol::STORAGE_LIT) {
		return 
			(sym->storage == xcc_symbol::STORAGE_STATIC ? xcc_write_word(p, XWORD{XIS::BIN}) : xcc_write_word(p, XWORD{XIS::PUT})) &&
			xcc_write_word(p, XWORD{0});
	}
	return true;
}

/// @brief Pops the scope stack, and emits instructions to restore the stack to its previous state.
/// @param p The parser.
/// @return False if the output binary buffer is full or the scope stack is empty. True otherwise.
static bool emit_pop_scope(xcc_parser *p)
{
	const U16 lsp = xcc_top_scope_stack_size(p);
	return
		(
			lsp == 0 ||
			(
				xcc_write_word(p, XWORD{XIS::PUT}) &&
				xcc_write_word(p, XWORD{lsp})      &&
				xcc_write_word(p, XWORD{XIS::POP})
			)
		) &&
		xcc_pop_scope(p);
}

/// @brief Peeks the next token without advancing the parser.
/// @param p The parser.
/// @return The token.
static token peek(xcc_parser *p)
{
	return xcc_peek(p, xblex);
}

/// @brief Peeks the next single character token without advancing the parser.
/// @param p The parser.
/// @return The token.
static token peek1(xcc_parser *p)
{
	return xcc_peek(p, xblex1);
}

/// @brief Tries to match the next token in the token sequence against an expected user type.
/// @param p The parser.
/// @param type The user type of the expected token.
/// @param out Stores the token that was read by the parser. Put null here if you do not want to have the output token (or do not supply the parameter at all).
/// @return True if there was a match.
/// @note Does not advance the parser if no match was detected.
static bool match(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xblex);
}

/// @brief Tries to match the next token in the token seqence against an expected user type. This version only reads a single character in the input token sequence and converts it to a token.
/// @param p The parser.
/// @param type The user type of the expected token.
/// @param out Stores the token that was read by the parser. Put null here if you do not want to have the output token (or do not supply the parameter at all).
/// @return True if there was a match.
/// @note Does not advance the parser if no match was detected.
static bool match1(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xblex1);
}

/// @brief Tries to match against a pattern function until the end token is reached.
/// @param ps The parser state.
/// @param try_fn The pattern function to call.
/// @return True if the pattern was successfully matched.
/// @note This function allows no instance of the pattern function to be called, i.e. if the first token after calling this function is the end token, then the function still returns true.
/// @note The function does not consume the end token, so care must be taken to consume that token outside of calling this function.
static bool until_end(xcc_parser_state ps, bool (*try_fn)(xcc_parser_state))
{
	token t;
	while ((t = peek(ps.p)).user_type != ps.end && t.user_type != token::STOP_EOF) {
		if (
			!manage_state(
				try_fn(new_state(ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_read_lit(xcc_parser_state ps, type_t &result);

/// @brief Converts a token to a literal and emits instructions to put it on the stack.
/// @param ps The parser state.
/// @return True if successful.
static bool try_put_lit(xcc_parser_state ps)
{
	U16 result = 0;
	if (
		manage_state(
			try_read_lit  (new_state(ps.end), result) &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})     &&
			xcc_write_word(ps.p, XWORD{result})
		)
	) {
		return true;
	}
	return false;
}

/// @brief Tries to read an alias and determine if the search should be reversed depending on preceding operator.
/// @param ps The parser state.
/// @param t The token that was read.
/// @param reverse_search The state of the search mode.
/// @return True if successful.
static bool try_alias(xcc_parser_state ps, token &t, bool &reverse_search)
{
	reverse_search = false;
	if (
		manage_state(
			(
				reverse_search = match(ps.p, xbtoken::OPERATOR_REVERSE_SEARCH) &&
				match(ps.p, token::ALIAS, &t)
			) ||
			match(ps.p, token::ALIAS, &t)
		)
	) {
		return true;
	}
	return false;
}

/// @brief Matches a token against an existing variable symbol and emits instructions to put its address on the stack.
/// @param ps The parser state.
/// @return True if successful.
static bool try_put_var_addr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			try_alias(new_state(ps.end), t, rs)            &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_fn_addr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			try_alias(new_state(ps.end), t, rs)           &&
			(sym = xcc_find_fn(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool try_rval(xcc_parser_state ps);

/// @brief Emits instructions to perform a redirect on a value to get the value at the address of the value.
/// @param ps The parser state
/// @return True if successful
static bool try_redir_val(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			try_rval      (new_state(ps.end))                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

/// @brief Matches a token against an existing variable symbol and emits instructions to put its value on the stack.
/// @param ps The parser state.
/// @return True if successful.
static bool try_put_var(xcc_parser_state ps)
{
	if (
		manage_state(
			try_redir_val(new_state(ps.end)) ||
			(
				try_put_var_addr(new_state(ps.end)) &&
				xcc_write_word(ps.p, XWORD{XIS::AT})
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_fn(xcc_parser_state ps)
{
	if (
		manage_state(
			try_redir_val(new_state(ps.end)) ||
			(
				try_put_fn_addr(new_state(ps.end)) &&
				xcc_write_word(ps.p, XWORD{XIS::AT})
			)
		)
	) {
		return true;
	}
	return false;
}

/// @brief Emits instructions to increment or decrement a token matching against a variable.
/// @param ps The parser state.
/// @return True if successful.
static bool try_post_incdec_var(xcc_parser_state ps)
{
	token t, op;
	xcc_symbol *sym;
	bool rs;
	if ( // NOTE: var++ var--
		manage_state(
			try_alias(new_state(ps.end), t, rs)            &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			(
				match(ps.p, xbtoken::OPERATOR_ARITHMETIC_INC, &op) ||
				match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DEC, &op)
			)                                              &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})           &&
			xcc_write_word(ps.p, XWORD{XIS::DUP})          &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})          &&
			xcc_write_word(ps.p, XWORD{1})                 &&
			(
				op.user_type == xbtoken::OPERATOR_ARITHMETIC_INC ?
					xcc_write_word(ps.p, XWORD{XIS::ADD}) :
					xcc_write_word(ps.p, XWORD{XIS::SUB})
			)                                             &&
			xcc_write_rel (ps.p, sym)                     &&
			xcc_write_word(ps.p, XWORD{XIS::MOVU})
		)
	) {
		return true;
	}
	return false;
}

static bool try_pre_incdec_var(xcc_parser_state ps)
{
	token t, op;
	xcc_symbol *sym;
	bool rs;
	if ( // NOTE: ++var --var
		manage_state(
			(
				match(ps.p, xbtoken::OPERATOR_ARITHMETIC_INC, &op) ||
				match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DEC, &op)
			)                                              &&
			try_alias(new_state(ps.end), t, rs)            &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})           &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})          &&
			xcc_write_word(ps.p, XWORD{1})                 &&
			(
				op.user_type == xbtoken::OPERATOR_ARITHMETIC_INC ?
					xcc_write_word(ps.p, XWORD{XIS::ADD}) :
					xcc_write_word(ps.p, XWORD{XIS::SUB})
			)                                              &&
			xcc_write_word(ps.p, XWORD{XIS::DUP})          &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVU})
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr(xcc_parser_state ps);

static bool try_put_fn_param(xcc_parser_state ps, U16 *param_count)
{
	if (
		manage_state(
			try_expr(new_state(ps.end))
		)
	) {
		if (param_count != NULL) {
			++(*param_count);
		}
		return true;
	}
	return false;
}

static bool try_put_fn_params(xcc_parser_state ps, U16 *param_count)
{
	if (
		manage_state(
			try_put_fn_param(new_state(ps.end), param_count) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_put_fn_params(new_state(ps.end), param_count)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_opt_fn_params(xcc_parser_state ps, const xcc_symbol *sym)
{
	U16 param_count = 0;
	if (
		manage_state(
			peek(ps.p).user_type == ps.end ||
			try_put_fn_params(new_state(ps.end), &param_count)
		)
	) {
		if (sym != NULL && sym->storage == xcc_symbol::STORAGE_FN && sym->param_count != param_count) {
			set_error(ps.p, sym->tok, xcc_error::VERIFY);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_call_fn_inline_arr(xcc_parser_state ps)
{
	// TODO
	// pre-parse the function
	// If there is an array or string as argument, then we parse recursively
	// foo("asd", {1,2,3})
	// becomes
	// { auto _p0[] = "asd", _p1[] = {1,2,3}; foo(_p0, _p1); }
	return false;
}

static bool try_call_fn(xcc_parser_state ps)
{
	if (!try_call_fn_inline_arr(new_state(ps.end))) {
		xcc_symbol *sym;
		U16 off_index = 0;
		token t;
		U16 result = 0;
		bool rs;
		if (
			manage_state(
				try_alias(new_state(ps.end), t, rs)                                            && // TODO Replace this with any value (can be alias, literal, indirection etc.)
				(
					(sym = xcc_find_symbol(t.text, ps.p, rs)) != NULL                          || // NOTE: find_symbol instead of find_fn. That way we can call anything as if it is a function!
					try_read_lit(new_state(ps.end), result)
				)                                                                              &&
				match                (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)           &&
				xcc_write_word       (ps.p, XWORD{XIS::PUT})                                   &&
				xcc_write_word       (ps.p, XWORD{0})                                          && // NOTE: Put return value memory on stack.
				xcc_write_word       (ps.p, XWORD{XIS::PUTI})                                  && // NOTE: Put return address on stack.
				xcc_write_word       (ps.p, XWORD{XIS::PUT})                                   &&
				(off_index = ps.p->out.size)                                                   &&
				xcc_write_word       (ps.p, XWORD{0})                                          && // NOTE: Unable to determine return address offset here, so just emit 0.
				xcc_write_word       (ps.p, XWORD{XIS::ADD})                                   && // NOTE: Adjust return address to move ahead of call site.
				try_put_opt_fn_params(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), sym) &&
				match                (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)           &&
				(ps.p->out.buffer[off_index].u = (ps.p->out.size - off_index) + 7)             && // NOTE: Return address offset can be determined. Adjust the previously emitted 0.
				(
					sym != NULL ?
						xcc_write_rel(ps.p, sym) :
						xcc_write_word(ps.p, XWORD{result})
				)                                                                              &&
				xcc_write_word       (ps.p, XWORD{XIS::AT})                                    &&
				xcc_write_word       (ps.p, XWORD{XIS::JMP})
			)
		) {
			// TODO Surely we can determine instruction pointer offset without constant 7...
			return true;
		}
		if (t.user_type == token::ALIAS && sym == NULL) {
			set_error(ps.p, t, xcc_error::UNDEF);
		}
	}
	return false;
}

template < typename type_t >
static bool eval_operation(xcc_parser *p, unsigned user_type, type_t &l, type_t r)
{
	switch (user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ADD:       l += r;                                                                                 return true;
	case xbtoken::OPERATOR_ARITHMETIC_SUB:       l -= r;                                                                                 return true;
	case xbtoken::OPERATOR_ARITHMETIC_MUL:       l *= r;                                                                                 return true;
	case xbtoken::OPERATOR_ARITHMETIC_DIV:       if (r != 0) { l /= r; } else { l = 0xffff; set_error(p, p->in.last, xcc_error::ZERO); } return true;
	case xbtoken::OPERATOR_ARITHMETIC_MOD:       if (r != 0) { l %= r; } else { l = 0xffff; set_error(p, p->in.last, xcc_error::ZERO); } return true;
	case xbtoken::OPERATOR_BITWISE_AND:
	case xbtoken::OPERATOR_LOGICAL_AND:          l &= r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_OR:
	case xbtoken::OPERATOR_LOGICAL_OR:           l |= r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_XOR:          l ^= r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_NOT:          l = ~r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_LSHIFT:       l <<= r;                                                                                return true;
	case xbtoken::OPERATOR_BITWISE_RSHIFT:       l >>= r;                                                                                return true;
	case xbtoken::OPERATOR_LOGICAL_LESS:         l = (l < r);                                                                            return true;
	case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:    l = (l <= r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_GREATER:      l = (l > r);                                                                            return true;
	case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL: l = (l >= r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_EQUAL:        l = (l == r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:     l = (l != r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_NOT:          l = !r;                                                                                 return true;
	}
	return false;
}

static bool try_escape_char(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			match1(ps.p, token::CHAR, &t)
		)
	) {
		switch (t.hash) {
		case '\\':               return true;
		case 't': t.hash = '\t'; return true;
		case 'r': t.hash = '\r'; return true;
		case 'a': t.hash = '\a'; return true;
		case 'n': t.hash = '\n'; return true;
		case 'b': t.hash = '\b'; return true;
		case 'f': t.hash = '\f'; return true;
		case '0': t.hash = '\0'; return true;
		}
		if (t.hash == ps.end) {
			switch (t.hash) {
			case '\'': return true;
			case '"':  return true;
			}
		}
	}
	return false;
}

static unsigned hexdig2dec(char hex)
{
	switch (hex) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		return hex - '0';
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
		return (hex - 'a') + 10;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
		return (hex - 'A') + 10;
	}
	return 0;
}

static bool try_encoded_char(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			match1(ps.p, token::CHAR, &t) &&
			(
				(t.hash == '\\') ?
					try_escape_char(new_state(ps.end), t) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_ext_escape_char(xcc_parser_state ps, token &t)
{
	token hi, lo;
	if (
		manage_state(
			match1(ps.p, token::CHAR, &t) &&
			t.hash == '#' &&
			match1(ps.p, token::CHAR, &hi) &&
			match1(ps.p, token::CHAR, &lo) &&
			try_encoded_char(new_state(ps.end), t)
		)
	) {
		t.hash = (((hexdig2dec(hi.hash) << 4) + hexdig2dec(lo.hash)) << 8) + t.hash;
		return true;
	}
	return false;
}

static bool try_ext_encoded_char(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			match1(ps.p, token::CHAR, &t) &&
			(
				(t.hash == '\\') ?
					try_ext_escape_char(new_state(ps.end), t) || try_escape_char(new_state(ps.end), t) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_read_char_lit(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE)         &&
			try_ext_encoded_char(new_state(xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE), t) &&
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_factor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_read_lit(xcc_parser_state ps, type_t &result)
{
	// TODO: Manage this state
	token t;
	bool rs;
	if (match(ps.p, xbtoken::LITERAL_INT, &t)) {
		result = t.hash;
		return true;
	} else if (try_alias(new_state(ps.end), t, rs)) {
		const xcc_symbol *sym = xcc_find_lit(t.text, ps.p, rs);
		if (sym != NULL) {
			result = sym->data.u;
			return true;
		}
	} else if (try_read_char_lit(new_state(ps.end), t)) {
		result = t.hash;
		return true;
	} else if (
		match    (ps.p, xbtoken::OPERATOR_SIZEOF)                            &&
		match    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)             &&
		try_alias(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), t, rs) &&
		match    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
	) {
		const xcc_symbol *sym = xcc_find_symbol(t.text, ps.p, rs);
		if (sym != NULL) {
			result = sym->size > 1 ? sym->size - 1 : sym->size;
			return true;
		}
	}
	return false;
}

template < typename type_t >
static bool try_lit_opt_factor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_factor(new_state(ps.end), r)    &&
				eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_rval(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_uni_bnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_BITWISE_NOT, &t) &&
			try_lit_rval  (new_state(ps.end), l)                    &&
			eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_uni_lnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_LOGICAL_NOT, &t) &&
			try_lit_rval  (new_state(ps.end), l)                    &&
			eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_rval(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_read_lit(new_state(ps.end), l) ||
			(
				(
					match       (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)         &&
					try_lit_expr(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), l) &&
					match       (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_uni_pos(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			match       (ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD) &&
			try_lit_rval(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_uni_neg(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			match       (ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB) &&
			try_lit_rval(new_state(ps.end), l)
		)
	) {
		l = -l;
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_factor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_uni_pos     (new_state(ps.end), l) ||
			try_lit_uni_neg     (new_state(ps.end), l) ||
			try_lit_uni_bnot_val(new_state(ps.end), l) ||
			try_lit_uni_lnot_val(new_state(ps.end), l) ||
			try_lit_rval        (new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_term(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_term(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_term  (new_state(ps.end), r)    &&
				eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_term(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_factor    (new_state(ps.end), l) &&
			try_lit_opt_factor(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_bitshift(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_bitshift(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_LSHIFT, &t) || match(ps.p, xbtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_bitshift(new_state(ps.end), r)    &&
				eval_operation  (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_bitshift(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_term    (new_state(ps.end), l) &&
			try_lit_opt_term(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_less_greater(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_less_greater(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_LESS, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATER, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_less_greater(new_state(ps.end), r)    &&
				eval_operation      (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_less_greater(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_bitshift    (new_state(ps.end), l) &&
			try_lit_opt_bitshift(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_equality(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_equality(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_equality(new_state(ps.end), r)    &&
				eval_operation  (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_equality(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_less_greater    (new_state(ps.end), l) &&
			try_lit_opt_less_greater(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_and(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_and(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_AND, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_and   (new_state(ps.end), r)    &&
				eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_and(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_equality    (new_state(ps.end), l) &&
			try_lit_opt_equality(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_xor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_xor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_XOR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_xor   (new_state(ps.end), r)    &&
				eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_xor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_and    (new_state(ps.end), l) &&
			try_lit_opt_and(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_or(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_or(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_OR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_or    (new_state(ps.end), r)    &&
				eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_or(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_xor    (new_state(ps.end), l) &&
			try_lit_opt_xor(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_logical_and(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_logical_and(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_AND, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_logical_and(new_state(ps.end), r)    &&
				eval_operation     (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_logical_and(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_or    (new_state(ps.end), l) &&
			try_lit_opt_or(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_logical_or(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_lit_opt_logical_or(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_OR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				try_lit_logical_or(new_state(ps.end), r)    &&
				eval_operation    (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_logical_or(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_logical_and    (new_state(ps.end), l) &&
			try_lit_opt_logical_and(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_expr(xcc_parser_state ps, type_t &l)
{

	if (
		manage_state(
			try_lit_logical_or    (new_state(ps.end), l) &&
			try_lit_opt_logical_or(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

static bool emit_operation(xcc_parser *p, unsigned user_type)
{
	switch (user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ADD:       return xcc_write_word(p, XWORD{XIS::ADD});
	case xbtoken::OPERATOR_ARITHMETIC_SUB:       return xcc_write_word(p, XWORD{XIS::SUB});
	case xbtoken::OPERATOR_ARITHMETIC_MUL:       return xcc_write_word(p, XWORD{XIS::MUL});
	case xbtoken::OPERATOR_ARITHMETIC_DIV:       return xcc_write_word(p, XWORD{XIS::DIV});
	case xbtoken::OPERATOR_ARITHMETIC_MOD:       return xcc_write_word(p, XWORD{XIS::MOD});
	case xbtoken::OPERATOR_BITWISE_AND:
	case xbtoken::OPERATOR_LOGICAL_AND:          return xcc_write_word(p, XWORD{XIS::AND});
	case xbtoken::OPERATOR_BITWISE_OR:
	case xbtoken::OPERATOR_LOGICAL_OR:           return xcc_write_word(p, XWORD{XIS::OR});
	case xbtoken::OPERATOR_BITWISE_XOR:          return xcc_write_word(p, XWORD{XIS::XOR});
	case xbtoken::OPERATOR_BITWISE_NOT:          return xcc_write_word(p, XWORD{XIS::NOT});
	case xbtoken::OPERATOR_BITWISE_LSHIFT:       return xcc_write_word(p, XWORD{XIS::LSH});
	case xbtoken::OPERATOR_BITWISE_RSHIFT:       return xcc_write_word(p, XWORD{XIS::RSH});
	case xbtoken::OPERATOR_LOGICAL_LESS:         return xcc_write_word(p, XWORD{XIS::LT});
	case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:    return xcc_write_word(p, XWORD{XIS::LE});
	case xbtoken::OPERATOR_LOGICAL_GREATER:      return xcc_write_word(p, XWORD{XIS::GT});
	case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL: return xcc_write_word(p, XWORD{XIS::LE});
	case xbtoken::OPERATOR_LOGICAL_EQUAL:        return xcc_write_word(p, XWORD{XIS::EQ});
	case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:     return xcc_write_word(p, XWORD{XIS::NE});
	case xbtoken::OPERATOR_LOGICAL_NOT:
		return 
			xcc_write_word(p, XWORD{XIS::PUT}) &&
			xcc_write_word(p, XWORD{0})        &&
			xcc_write_word(p, XWORD{XIS::EQ})
		;
	}
	return false;
}

static bool try_factor(xcc_parser_state ps);

static bool try_opt_factor(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		if (
			!manage_state(
				try_factor(new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_index_src(xcc_parser_state ps)
{
	if (
		manage_state(
			try_put_var_addr(new_state(ps.end)) ||
			try_put_lit     (new_state(ps.end)) ||
			(
				match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
				try_expr(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
				match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_opt_index(xcc_parser_state ps)
{
	if (peek(ps.p).user_type != xbtoken::OPERATOR_ENCLOSE_BRACKET_L) {
		return true;
	}
	if (
		manage_state(
			xcc_write_word(ps.p, XWORD{XIS::AT})                           &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			try_expr      (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)      &&
			xcc_write_word(ps.p, XWORD{XIS::ADD})                          &&
			try_opt_index (new_state(ps.end))
		)
	) {
			return true;
	}
	return false;
}

static bool try_put_index_addr(xcc_parser_state ps)
{
	if (
		manage_state(
			try_index_src (new_state(ps.end))                              &&
			xcc_write_word(ps.p, XWORD{XIS::AT})                           &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			try_expr      (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)      &&
			xcc_write_word(ps.p, XWORD{XIS::ADD})                          &&
			try_opt_index (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_index(xcc_parser_state ps)
{
	if (
		manage_state(
			try_put_index_addr(new_state(ps.end))    &&
			xcc_write_word    (ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_bnot_val(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_BITWISE_NOT, &t) &&
			try_rval      (new_state(ps.end))                       &&
			emit_operation(ps.p, t.user_type)
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_lnot_val(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_LOGICAL_NOT, &t) &&
			try_rval      (new_state(ps.end))                       &&
			emit_operation(ps.p, t.user_type)
		)
	) {
		return true;
	}
	return false;
}

// val ::= "*" val | name "(" exprs ")" | val "[" expr "]" | name | num
static bool try_rval(xcc_parser_state ps)
{
	if (
		manage_state(
			
			try_call_fn        (new_state(ps.end)) ||
			try_put_lit        (new_state(ps.end)) ||
			try_put_index      (new_state(ps.end)) ||
			try_post_incdec_var(new_state(ps.end)) ||
			try_put_var        (new_state(ps.end)) ||
			try_put_fn         (new_state(ps.end)) ||
			(
				(
					match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
					try_expr(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
					match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_addr(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_BITWISE_AND) &&
			try_put_var_addr(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_pos(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD) &&
			try_rval(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_neg(xcc_parser_state ps)
{
	if (
		manage_state(
			match     (ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB) &&
			try_rval  (new_state(ps.end))                      &&
			xcc_write_word(ps.p, XWORD{XIS::INEG})
		)
	) {
		return true;
	}
	return false;
}

// factor ::= "&" val | "+" val | "-" val | "(" expr ")"
static bool try_factor(xcc_parser_state ps)
{
	if (
		manage_state(
			try_uni_addr      (new_state(ps.end)) ||
			try_uni_pos       (new_state(ps.end)) ||
			try_uni_neg       (new_state(ps.end)) ||
			try_uni_bnot_val  (new_state(ps.end)) ||
			try_uni_lnot_val  (new_state(ps.end)) ||
			try_pre_incdec_var(new_state(ps.end)) ||
			try_rval          (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_term(xcc_parser_state ps);

static bool try_opt_term(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		if (
			!manage_state(
				try_term(new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_term(xcc_parser_state ps)
{
	if (
		manage_state(
			try_factor    (new_state(ps.end)) &&
			try_opt_factor(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_bitshift(xcc_parser_state ps);

static bool try_opt_bitshift(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_LSHIFT, &t) || match(ps.p, xbtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
		if (
			!manage_state(
				try_bitshift(new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_bitshift(xcc_parser_state ps)
{
	if (
		manage_state(
			try_term    (new_state(ps.end)) &&
			try_opt_term(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_less_greater(xcc_parser_state ps);

static bool try_opt_less_greater(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_LESS, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATER, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
		if (
			!manage_state(
				try_less_greater(new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_less_greater(xcc_parser_state ps)
{
	if (
		manage_state(
			try_bitshift    (new_state(ps.end)) &&
			try_opt_bitshift(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_equality(xcc_parser_state ps);

static bool try_opt_equality(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
		if (
			!manage_state(
				try_equality  (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_equality(xcc_parser_state ps)
{
	if (
		manage_state(
			try_less_greater    (new_state(ps.end)) &&
			try_opt_less_greater(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_and(xcc_parser_state ps);

static bool try_opt_and(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_AND, &t)) {
		if (
			!manage_state(
				try_and       (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_and(xcc_parser_state ps)
{
	if (
		manage_state(
			try_equality    (new_state(ps.end)) &&
			try_opt_equality(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_xor(xcc_parser_state ps);

static bool try_opt_xor(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_XOR, &t)) {
		if (
			!manage_state(
				try_xor       (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_xor(xcc_parser_state ps)
{
	if (
		manage_state(
			try_and    (new_state(ps.end)) &&
			try_opt_and(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_or(xcc_parser_state ps);

static bool try_opt_or(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_OR, &t)) {
		if (
			!manage_state(
				try_or        (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_or(xcc_parser_state ps)
{
	if (
		manage_state(
			try_xor    (new_state(ps.end)) &&
			try_opt_xor(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_logical_and(xcc_parser_state ps);

static bool try_opt_logical_and(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_AND, &t)) {
		U16 jmp_addr;
		if (
			!manage_state(
				xcc_write_word (ps.p, XWORD{XIS::DUP})          &&
				xcc_write_word (ps.p, XWORD{XIS::PUT})          &&
				(jmp_addr = ps.p->out.size)                     &&
				xcc_write_word (ps.p, XWORD{0})                 && // NOTE: Temp value
				xcc_write_word (ps.p, XWORD{XIS::RLA})          &&
				xcc_write_word (ps.p, XWORD{XIS::CNJMP})        &&
				try_logical_and(new_state(ps.end))              &&
				emit_operation (ps.p, t.user_type)              &&
				(ps.p->out.buffer[jmp_addr].u = ps.p->out.size)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_logical_and(xcc_parser_state ps)
{
	if (
		manage_state(
			try_or    (new_state(ps.end)) &&
			try_opt_or(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_logical_or(xcc_parser_state ps);

static bool try_opt_logical_or(xcc_parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_OR, &t)) {
		U16 jmp_addr;
		if (
			!manage_state(
				xcc_write_word(ps.p, XWORD{XIS::DUP})           &&
				xcc_write_word(ps.p, XWORD{XIS::PUT})           &&
				(jmp_addr = ps.p->out.size)                     &&
				xcc_write_word(ps.p, XWORD{0})                  && // NOTE: Temp value
				xcc_write_word(ps.p, XWORD{XIS::CJMP})          &&
				try_logical_or(new_state(ps.end))               &&
				emit_operation(ps.p, t.user_type)               &&
				(ps.p->out.buffer[jmp_addr].u = ps.p->out.size)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_logical_or(xcc_parser_state ps)
{
	if (
		manage_state(
			try_logical_and    (new_state(ps.end)) &&
			try_opt_logical_and(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr(xcc_parser_state ps)
{
	if (
		manage_state(
			try_logical_or    (new_state(ps.end)) &&
			try_opt_logical_or(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr_list(xcc_parser_state ps, U16 *count)
{
	
	if (
		manage_state(
			(
				try_expr(new_state(ps.end)) &&
				++(*count)                  &&
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) ?
						try_expr_list(new_state(ps.end), count) :
						true
				)
			) ||
			peek(ps.p).user_type == ps.end
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_var_list(xcc_parser_state ps);

static bool try_str_lit(xcc_parser_state ps, U16 *count)
{
	token t;
	while (((t = peek(ps.p)).user_type != ps.end || is_white(peek1(ps.p).hash)) && t.user_type != token::STOP_EOF) {
		if (!try_ext_encoded_char(new_state(ps.end), t)) {
			return false;
		}
		if (!xcc_write_word(ps.p, XWORD{XIS::PUT}) || !xcc_write_word(ps.p, XWORD{U16(t.hash)})) {
			return false;
		}
		++*count;
	}
	if (!xcc_write_word(ps.p, XWORD{XIS::PUT}) || !xcc_write_word(ps.p, XWORD{U16(0)})) {
		return false;
	}
	++*count;
	return true;
}

static bool try_opt_arr_def_expr(xcc_parser_state ps, xcc_symbol *sym)
{
	++sym->size;
	token p = peek(ps.p);
	if (!xcc_write_rel(ps.p, sym)) {
		return false;
	}
	++ps.p->out.buffer[ps.p->out.size - 2].u; // Make the 'xcc_write_rel' point to one address higher than the array pointer (where the array is located).
	if (p.user_type == ps.end || p.user_type == xbtoken::OPERATOR_COMMA) {
		return
			xcc_write_word(ps.p, XWORD{XIS::PUT})           &&
			xcc_write_word(ps.p, XWORD{U16(sym->size - 1)}) &&
			xcc_write_word(ps.p, XWORD{XIS::PUSH});
	}
	U16 count = 0;
	bool is_str = false;
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET) &&
			(
				(
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)              &&
					try_expr_list(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), &count) &&
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
				) ||
				(
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)                       &&
					(is_str = try_str_lit(new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), &count)) &&
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)
				)
			)
		)
	) {
		if (is_str) {
			++sym->size; // NOTE: Make room for the implicit null terminator.
		}
		if (sym->size - 1 != count) {
			set_error(ps.p, sym->tok, xcc_error::VERIFY);
			return false;
		}
		if (count == 0) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_arr_def_expr(xcc_parser_state ps, xcc_symbol *sym)
{
	token p = peek(ps.p);
	if (!xcc_write_rel(ps.p, sym)) {
		return false;
	}
	++ps.p->out.buffer[ps.p->out.size - 2].u; // Make the 'xcc_write_rel' point to one address higher than the array pointer (where the array is located).
	U16 count = 0;
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET) &&
			(
				(
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)              &&
					try_expr_list(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), &count) &&
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
				) ||
				(
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)              &&
					try_str_lit  (new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), &count) &&
					match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)
				)
			)
		)
	) {
		sym->size += count;
		if (count == 0) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_new_arr_item(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			match                       (ps.p, token::ALIAS, &t)                                    &&
			(sym = xcc_add_var(t, ps.p)) != NULL                                                    &&
			match                       (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)                 &&
			(
				(
					try_lit_expr        (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R), sym->size) &&
					match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					try_opt_arr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym)
				)                                                                                   ||
				(
					match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					try_arr_def_expr    (new_state(xbtoken::OPERATOR_SEMICOLON), sym)
				)
			)                                                                                       &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ?
					try_new_var_list(new_state(ps.end)) :
					true
			)
		)
	) {
		if (sym->size <= 1) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_opt_var_def_expr(xcc_parser_state ps)
{
	token p = peek(ps.p);
	if (p.user_type == ps.end || p.user_type == xbtoken::OPERATOR_COMMA) {
		return
			xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
			xcc_write_word(ps.p, XWORD{0});
	}
	if (
		manage_state(
			match   (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET) &&
			try_expr(new_state(xbtoken::OPERATOR_SEMICOLON))
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_var_item(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			match               (ps.p, token::ALIAS, &t)                 &&
			xcc_add_var         (t, ps.p) != NULL                        &&
			try_opt_var_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ?
					try_new_var_list(new_state(ps.end)) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_var_list(xcc_parser_state ps)
{
	if (
		manage_state(
			try_new_arr_item(new_state(ps.end)) ||
			try_new_var_item(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_vars(xcc_parser_state ps)
{
	if (
		manage_state(
			match           (ps.p, xbtoken::KEYWORD_TYPE_AUTO)       &&
			try_new_var_list(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			match           (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool try_sexpr(xcc_parser_state ps)
{
	U16 result = 0;
	if (
		manage_state(
			try_lit_expr(new_state(ps.end), result) &&
			xcc_write_word(ps.p, XWORD{result})
		)
	) {
		return true;
	}
	return false;
}

static bool try_sexpr_list(xcc_parser_state ps, U16 *count)
{
	if (
		manage_state(
			(
				try_sexpr(new_state(ps.end)) &&
				++(*count)                   &&
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) ?
						try_sexpr_list(new_state(ps.end), count) :
						true
				)
			) ||
			peek(ps.p).user_type == ps.end
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_svar_list(xcc_parser_state ps);

static bool try_sstr_lit(xcc_parser_state ps, U16 *count)
{
	token t;
	if (!xcc_write_word(ps.p, XWORD{XIS::PUT}) || !xcc_write_word(ps.p, XWORD{0}) || !xcc_write_word(ps.p, XWORD{XIS::RLA}) || !xcc_write_word(ps.p, XWORD{XIS::JMP})) {
		return false;
	}
	U16 jmp_addr_idx = ps.p->out.size - 3;
	while (((t = peek(ps.p)).user_type != ps.end || is_white(peek1(ps.p).hash)) && t.user_type != token::STOP_EOF) {
		if (!try_ext_encoded_char(new_state(ps.end), t)) {
			return false;
		}
		if (!xcc_write_word(ps.p, XWORD{U16(t.hash)})) {
			return false;
		}
		++*count;
	}
	if (!xcc_write_word(ps.p, XWORD{U16(0)})) {
		return false;
	}
	++*count;
	// ps.p->out.buffer[jmp_addr_idx].u = *count; // WRONG: This should be used for SKIP, not JMP
	ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
	return true;
}

static bool try_sexpr_def(xcc_parser_state ps, U16 &count)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			xcc_write_word(ps.p, XWORD{XIS::PUT})                                &&
			(jmp_addr_idx = ps.p->out.size)                                      &&
			xcc_write_word(ps.p, XWORD{0})                                       &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                                &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})                                &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)              &&
			try_sexpr_list(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), &count) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return true;
	}
	return false;
}

static bool try_sarr_def_expr(xcc_parser_state ps, xcc_symbol *sym, bool verify)
{
	++sym->size;
	U16 count = 0;
	bool is_str = false;
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)                                             &&
			(
				xcc_write_word            (ps.p, XWORD{XIS::PUT})                                     &&
				xcc_write_word            (ps.p, XWORD{U16(ps.p->out.size + 6)})                      && // NOTE: 6 is the number of instructions that will be emitted after this that we have to skip in order to point to the array data.
				xcc_write_word            (ps.p, XWORD{XIS::RLA})                                     &&
				try_sexpr_def             (new_state(ps.end), count)                                  ||
				(
					match                 (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)               &&
					(is_str = try_sstr_lit(new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), &count)) &&
					match                 (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)
				)
			)
		)
	) {
		if (is_str) {
			++sym->size;
		}
		if (!verify) {
			sym->size += count;
		} else if (sym->size - 1 != count) {
			set_error(ps.p, sym->tok, xcc_error::VERIFY);
			return false;
		}
		if (count == 0) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_new_sarr_item(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			match                    (ps.p, token::ALIAS, &t)                                    &&
			(sym = xcc_add_var(t, ps.p)) != NULL                                                 && // NOTE: Array pointers are still AUTO rather than STATIC. However, we need to change that type to STATIC later (not because the pointer is not auto, but because we need to be able to identify that it is pointing to a static array).
			match                    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)                 &&
			(
				(
					try_lit_expr     (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R), sym->size) &&
					match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					try_sarr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym, true)
				)                                                                                ||
				(
					match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					try_sarr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym, false)
				)
			)                                                                                    &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ?
					try_new_svar_list(new_state(ps.end)) :
					true
			)
		)
	) {
		sym->storage = xcc_symbol::STORAGE_STATIC; // NOTE: Static arrays are really hacky. They require a lot of backend work because we want to be able to have them mostly not affect the stack size since the actual array elements do not reside on the stack yet the array pointer does. This results in the symbol needing to be registered as AUTO initially, then switched over to STATIC, and have the XCC backend implement a lot of exceptions for how static arrays are handled.
		if (sym->size <= 1) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_svar_def_expr(xcc_parser_state ps)
{
	U16 result = 0;
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)         &&
			try_lit_expr  (new_state(xbtoken::OPERATOR_SEMICOLON), result) &&
			xcc_write_word(ps.p, XWORD{XIS::BIN})                          &&
			xcc_write_word(ps.p, XWORD{result})
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_svar_item(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			match            (ps.p, token::ALIAS, &t)                 &&
			xcc_add_svar     (t, ps.p) != NULL                        &&
			try_svar_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ?
					try_new_svar_list(new_state(ps.end)) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_svar_list(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			try_new_sarr_item(new_state(ps.end)) ||
			try_new_svar_item(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_svars(xcc_parser_state ps)
{
	if (
		manage_state(
			match            (ps.p, xbtoken::KEYWORD_TYPE_STATIC)     &&
			try_new_svar_list(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			match            (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool try_const_def_expr(xcc_parser_state ps, U16 &result)
{
	result = 0;
	if (
		manage_state(
			match       (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)         &&
			try_lit_expr(new_state(xbtoken::OPERATOR_SEMICOLON), result)
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_const_list(xcc_parser_state ps);

static bool try_new_const_item(xcc_parser_state ps)
{
	token t;
	U16 result = 0;
	if (
		manage_state(
			match             (ps.p, token::ALIAS, &t)                         &&
			try_const_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), result) &&
			xcc_add_lit       (t, result, ps.p) != NULL                        &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ?
					try_new_const_list(new_state(ps.end)) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_const_list(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			try_new_const_item(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_consts(xcc_parser_state ps)
{
	if (
		manage_state(
			match             (ps.p, xbtoken::KEYWORD_TYPE_CONST)      &&
			try_new_const_list(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			match             (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool try_fn_param(xcc_parser_state ps, xcc_symbol *param)
{
	token t;
	if (
		manage_state(
			match(ps.p, token::ALIAS, &t)
		)
	) {
		param->param = xcc_add_param(t, ps.p);
		if (param->param == NULL) {
			return false;
		}
		return emit_empty_symbol_storage(ps.p, param->param);
	}
	return false;
}

static bool try_fn_params(xcc_parser_state ps, xcc_symbol *param)
{
	if (
		manage_state(
			try_fn_param(new_state(ps.end), param)                  &&
			(
				peek(ps.p).user_type == ps.end                      ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA)            &&
					try_fn_params (new_state(ps.end), param->param)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static void set_param_addr(xcc_symbol *fn)
{
	xcc_symbol *p = fn->param;
	U16 i = 0;
	while (p != NULL) {
		p->data.u = i - fn->param_count - 1;
		p = p->param;
		++i;
	}
}

static bool try_fn_noparam(xcc_parser_state ps)
{
	if (
		manage_state(
			(peek(ps.p).user_type == ps.end)            ||
			(
				match(ps.p, xbtoken::KEYWORD_TYPE_VOID) &&
				peek(ps.p).user_type == ps.end
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_opt_fn_params(xcc_parser_state ps, bool verify_params)
{
	if (ps.p->fn == NULL) {
		set_error(ps.p, ps.p->in.last, xcc_error::INTERNAL);
		return false;
	}
	const xcc_symbol fn = *ps.p->fn;
	if (
		manage_state(
			try_fn_noparam(new_state(ps.end))           ||
			try_fn_params (new_state(ps.end), ps.p->fn)
		)
	) {
		ps.p->fn->param_count = 0;
		const xcc_symbol *p = ps.p->fn->param;
		while (p != NULL) {
			++ps.p->fn->param_count;
			p = p->param;
		}
		if (verify_params && ps.p->fn->param_count != fn.param_count) {
			set_error(ps.p, ps.p->fn->tok, xcc_error::VERIFY);
			return false;
		}
		set_param_addr(ps.p->fn);
		return true;
	}
	*ps.p->fn = fn;
	return false;
}

static bool try_statements(xcc_parser_state ps);
static bool try_statement(xcc_parser_state ps);

static bool try_else(xcc_parser_state ps)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_ELSE) &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})               &&
			(jmp_addr_idx = ps.p->out.size)                     &&
			xcc_write_word(ps.p, XWORD{0})                      &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})               &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})               &&
			xcc_push_scope(ps.p, false)                         &&
			try_statement (new_state(ps.end))                   &&
			emit_pop_scope(ps.p)
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return true;
	}
	return false;
}

static bool try_if(xcc_parser_state ps)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_IF)                  &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			try_expr      (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                              &&
			(jmp_addr_idx = ps.p->out.size)                                    &&
			xcc_write_word(ps.p, XWORD{0})                                     &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                              &&
			xcc_write_word(ps.p, XWORD{XIS::CNJMP})                            &&
			xcc_push_scope(ps.p, false)                                        &&
			try_statement (new_state(ps.end))                                  &&
			emit_pop_scope(ps.p)                                               &&
			(ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size)                &&
			(
				!try_else(new_state(ps.end)) ||
				(ps.p->out.buffer[jmp_addr_idx].u += 4) // NOTE: A successful try_else emits an unconditional jump (PUT ADDR RLA JMP) that we want to skip over to get into the ELSE body.
			)

		)
	) {
		return true;
	}
	return false;
}

static bool try_while(xcc_parser_state ps)
{
	U16 jmp_addr_idx   = 0;
	U16 return_jmp_idx = ps.p->out.size;
	ps.loop_scope = ps.p->scopes.scope;
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_WHILE)               &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			(ps.continue_ip = ps.p->out.size)                                  &&
			try_expr      (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			(ps.break_ip = ps.p->out.size)                                     &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                              &&
			(jmp_addr_idx = ps.p->out.size)                                    &&
			xcc_write_word(ps.p, XWORD{0})                                     &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                              &&
			xcc_write_word(ps.p, XWORD{XIS::CNJMP})                            &&
			xcc_push_scope(ps.p, false)                                        &&
			try_statement (new_state(ps.end))                                  &&
			emit_pop_scope(ps.p)                                               &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                              &&
			xcc_write_word(ps.p, XWORD{return_jmp_idx})                        &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                              &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})                              &&
			(ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size)
		)
	) {
		return true;
	}
	return false;
}

static bool try_statements(xcc_parser_state ps);

static bool try_scope(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)      &&
			xcc_push_scope(ps.p, false)                                  &&
			try_statements(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)      &&
			emit_pop_scope(ps.p)
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			try_expr      (new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
			xcc_write_word(ps.p, XWORD{XIS::TOSS})
		)
	) {
		return true;
	}
	return false;
}

static bool try_redir_lval(xcc_parser_state ps)
{
	if (
		manage_state(
			match   (ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			try_rval(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_lval(xcc_parser_state ps)
{
	if (
		manage_state(
			try_put_index_addr(new_state(ps.end)) ||
			try_put_var_addr  (new_state(ps.end)) ||
			try_redir_lval    (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_lexpr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			try_lval(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_comp_ass(xcc_parser_state ps, token &op)
{
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_ADD, &op) ||
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_SUB, &op) ||
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MUL, &op) ||
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_DIV, &op) ||
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MOD, &op) ||
			match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_AND,    &op) ||
			match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_OR,     &op) ||
			match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_XOR,    &op) ||
			match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_LSHIFT, &op) ||
			match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_RSHIFT, &op)
		)
	) {
		return true;
	}
	return false;
}

static bool emit_comp_ass(xcc_parser_state ps, const token &op)
{
	switch (op.user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_ADD: return xcc_write_word(ps.p, XWORD{XIS::ADD});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_SUB: return xcc_write_word(ps.p, XWORD{XIS::SUB});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MUL: return xcc_write_word(ps.p, XWORD{XIS::MUL});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_DIV: return xcc_write_word(ps.p, XWORD{XIS::DIV});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MOD: return xcc_write_word(ps.p, XWORD{XIS::MOD});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_AND:    return xcc_write_word(ps.p, XWORD{XIS::AND});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_OR:     return xcc_write_word(ps.p, XWORD{XIS::OR});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_XOR:    return xcc_write_word(ps.p, XWORD{XIS::XOR});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_LSHIFT: return xcc_write_word(ps.p, XWORD{XIS::LSH});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_RSHIFT: return xcc_write_word(ps.p, XWORD{XIS::RSH});
	}
	return false;
}

static bool try_comp_ass_var_stmt(xcc_parser_state ps)
{
	token op;
	if (
		manage_state(
			try_lexpr     (new_state(ps.end))                      && // BUG: We need to determine the compound assignment before we have read it. Unsure if just using the previous end token will work here...
			xcc_write_word(ps.p, XWORD{XIS::DUP})                  &&
			xcc_write_word(ps.p, XWORD{XIS::AT})                   &&
			try_comp_ass  (new_state(ps.end), op)                  &&
			try_expr      (new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			emit_comp_ass (new_state(ps.end), op)                  &&
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_set_var_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			try_lexpr     (new_state(xbtoken::OPERATOR_ASSIGNMENT_SET)) &&
			match         (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)      &&
			try_expr      (new_state(xbtoken::OPERATOR_SEMICOLON))      &&
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON)           &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_reass_var_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			try_set_var_stmt     (new_state(ps.end)) ||
			try_comp_ass_var_stmt(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_opt_expr(xcc_parser_state ps)
{
	if (
		manage_state(
			(peek(ps.p).user_type == ps.end) ?
				(
					xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
					xcc_write_word(ps.p, XWORD{0})
				) :
				try_expr(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_break_stmt(xcc_parser_state ps)
{
	// TODO Also note that break gets a different meaning when used in switch-case.
	U16 lsp = xcc_loop_stack_size(ps.p, ps.loop_scope);
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_BREAK) &&
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON)    &&
			(
				lsp > 0 ?
					xcc_write_word(ps.p, XWORD{XIS::PUT})        &&
					xcc_write_word(ps.p, XWORD{lsp})             &&
					xcc_write_word(ps.p, XWORD{XIS::POP}) :
					true
			)                                                    &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                &&
			xcc_write_word(ps.p, XWORD{0})                       &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                &&
			xcc_write_word(ps.p, XWORD{U16(ps.break_ip)})        &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})
		)
	) {
		if (ps.loop_scope == 0) {
			set_error(ps.p, ps.p->in.last, xcc_error::UNEXPECTED);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_continue_stmt(xcc_parser_state ps)
{
	U16 lsp = xcc_loop_stack_size(ps.p, ps.loop_scope);
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_CONTINUE) &&
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON)       &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                   &&
			xcc_write_word(ps.p, XWORD{U16(ps.continue_ip)})        &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                   &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})
		)
	) {
		if (ps.loop_scope == 0) {
			set_error(ps.p, ps.p->in.last, xcc_error::UNEXPECTED);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_return_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_RETURN)  &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                  &&
			xcc_write_word(ps.p, XWORD{0})                         &&
			xcc_write_word(ps.p, XWORD{XIS::RLC})                  &&
			xcc_write_word(ps.p, XWORD{XIS::AT})                   &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                  &&
			xcc_write_word(ps.p, XWORD{1})                         &&
			xcc_write_word(ps.p, XWORD{XIS::SUB})                  &&
			try_opt_expr  (new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})                 && // NOTE: Address of return value is top value. Move expression result to external return memory.
			xcc_write_word(ps.p, XWORD{XIS::LDC})                  &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})                     // NOTE: Return address is top value. Jump back to call site
		)
	) {
		return true;
	}
	return false;
}

static bool try_asm_instr(xcc_parser_state ps)
{
	if (manage_state(xasm_inline(new_state(ps.end)))) {
		return true;
	}
	return false;
}

static bool try_asm_block(xcc_parser_state ps)
{
	if (
		manage_state(
			match    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                     &&
			until_end(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), try_asm_instr) &&
			match    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		return true;
	}
	return false;
}

static bool try_asm_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xbtoken::KEYWORD_INTRINSIC_ASM) &&
			(
				try_asm_block(new_state(ps.end)) ||
				try_asm_instr(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_enum_val(xcc_parser_state ps, U16 &val)
{
	token t;
	U16 new_val = 0;
	bool set_val = false;
	if (
		match(ps.p, token::ALIAS, &t) &&
		(
			(set_val = match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL)) ? try_lit_expr(new_state(ps.end), new_val) : true
		) &&
		xcc_add_lit(t, set_val ? new_val : val, ps.p)
	) {
		if (set_val) {
			val = new_val;
		}
		return true;
	}
	return false;
}

static bool try_enum_vals(xcc_parser_state ps, U16 val)
{
	if (
		manage_state(
			try_enum_val(new_state(ps.end), val) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_enum_vals(new_state(ps.end), val + 1)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_enum(xcc_parser_state ps)
{
	if (
		manage_state(
			match        (ps.p, xbtoken::KEYWORD_ENUM)                     &&
			match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)         &&
			try_enum_vals(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), 0) &&
			match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)         &&
			match        (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool try_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			match             (ps.p, xbtoken::OPERATOR_SEMICOLON) ||
			try_enum          (new_state(ps.end))                 ||
			try_new_vars      (new_state(ps.end))                 ||
			try_new_svars     (new_state(ps.end))                 ||
			try_new_consts    (new_state(ps.end))                 ||
			try_if            (new_state(ps.end))                 ||
			try_while         (new_state(ps.end))                 ||
			try_return_stmt   (new_state(ps.end))                 ||
			try_break_stmt    (new_state(ps.end))                 ||
			try_continue_stmt (new_state(ps.end))                 ||
			try_scope         (new_state(ps.end))                 ||
			try_reass_var_stmt(new_state(ps.end))                 ||
			try_expr_stmt     (new_state(ps.end))                 ||
			try_asm_stmt      (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_statements(xcc_parser_state ps)
{
	if (
		manage_state(
			until_end(new_state(ps.end), try_statement)
		)
	) {
		return true;
	}
	return false;
}

static bool adjust_fn_rel_ptr(xcc_parser_state ps)
{
	const xcc_symbol *p = ps.p->fn->param;
	U16 params = 0;
	while (p != NULL) {
		++params;
		p = p->param;
	}
	if (params == 0) {
		return true;
	}
	return
		xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
		xcc_write_word(ps.p, XWORD{params})   &&
		xcc_write_word(ps.p, XWORD{XIS::SUB});
}

static bool try_filepath(xcc_parser_state ps, chars::view &fp)
{
	fp = { ps.p->in.code.str + ps.p->in.head, 0, 0 }; // BUG: If we implement paging this will not work.
	token t;
	while (peek(ps.p).user_type != ps.end) {
		if (!match1(ps.p, token::CHAR, &t)) {
			set_error(ps.p, t, xcc_error::UNEXPECTED);
			return false;
		}
		++fp.len;
	}
	return true;
}

static bool try_load_text(xcc_parser_state &ps, const chars::view &source_file, xcc_text &text)
{
	if (!xcc_load_text(source_file, text)) {
		set_error(ps.p, ps.p->in.last, xcc_error::MISSING);
		return false;
	}
	
	if (xcc_add_filesum(ps.p->fsums, text.sum)) {
		ps.p->in = init_lexer(chars::view{ text.txt, text.len, 0 });
	} else {
		ps.p->in = init_lexer(chars::view{ "", 0, 0 });
	}
	ps.p->max = ps.p->in.last;
	return true;
}

static bool try_global_statement(xcc_parser_state ps);
static bool try_global_statements(xcc_parser_state ps);

static bool try_file(xcc_parser_state ps, const chars::view &wd, const chars::view &source_file, const chars::view &ext)
{
	for (unsigned i = 0; i < wd.len; ++i) {
		ps.cwd.str[i] = wd.str[i];
	}
	ps.cwd.len = wd.len;
	if (!xcc_set_path(ps.cwd, source_file)) {
		set_error(ps.p, ps.p->in.last, xcc_error::MEMORY);
		return false;
	}
	for (unsigned i = 0; i < ext.len; ++i) {
		if (ps.cwd.len == xcc_path::MAXPATH - 1) {
			set_error(ps.p, ps.p->in.last, xcc_error::MEMORY);
			return false;
		}
		ps.cwd.str[ps.cwd.len] = ext.str[i];
		++ps.cwd.len;
	}

	xcc_text text;
	if (
		manage_state(
			try_load_text        (ps, chars::view{ps.cwd.str, ps.cwd.len, 0}, text) &&
			try_global_statements(new_state(token::STOP_EOF))
		)
	) {
		// BUG: We need to restore more state inside the parser than just the lexer. We can probably use ps.restore_point for that. If we do not, then file tracking with be wrong.
		ps.p->fn = ps.restore_point.fn;
		ps.p->in = ps.restore_point.in;
		return true;
	}
	return false;
}

static bool try_include_relative_filepath(xcc_parser_state ps)
{
	chars::view fp;
	if (
		manage_state(
			match       (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)          &&
			try_filepath(new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), fp) &&
			match       (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)          &&
			try_file    (new_state(ps.end), chars::view{ ps.cwd.str, ps.cwd.len, 0 }, fp, chars::view{ NULL, 0, 0 })
		)
	) {
		return true;
	}
	return false;
}

static bool try_include_standard_filepath(xcc_parser_state ps)
{
	chars::view fp;
	if (
		manage_state(
			match       (ps.p, xbtoken::OPERATOR_LOGICAL_LESS)                    &&
			try_filepath(new_state(xbtoken::OPERATOR_LOGICAL_GREATER), fp)        &&
			match       (ps.p, xbtoken::OPERATOR_LOGICAL_GREATER)                 &&
			try_file    (new_state(ps.end), ps.swd, fp, chars::view{".xh", 3, 0}) &&
			try_file    (new_state(ps.end), ps.swd, fp, chars::view{".xb", 3, 0})
		)
	) {
		return true;
	}
	return false;
}

static bool try_include(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xbtoken::OPERATOR_HASH)   &&
			match(ps.p, xbtoken::KEYWORD_INCLUDE) &&
			(
				try_include_relative_filepath(new_state(ps.end)) ||
				try_include_standard_filepath(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_fn_def(xcc_parser_state ps)
{
	U16   guard_jmp_idx = 0;
	token t;
	bool verify_params = false;
	if (
		manage_state(
			match            (ps.p,  token::ALIAS, &t)                                           &&
			(
				(verify_params = ((ps.p->fn = xcc_find_fn(t.text, ps.p)) != NULL)) ||
				(
					(ps.p->fn = xcc_add_fn(t, ps.p)) != NULL  &&
					emit_empty_symbol_storage(ps.p, ps.p->fn) &&
					(ps.p->fn->link = ps.p->out.size - 1)     &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				)
			)                                                                                    &&
			xcc_write_word   (ps.p, XWORD{XIS::PUT})                                             && // 1/5 Set up function guard to jump over function body.
			(guard_jmp_idx = ps.p->out.size)                                                     && // 2/5 Record jump constant index so it can be modified later.
			xcc_write_word   (ps.p, XWORD{0})                                                    && // 3/5 The jump-to address. Insert any value temporarily. This will be changed later.
			xcc_write_word   (ps.p, XWORD{XIS::RLA})                                             && // 4/5 Make the jump-to address relative to A.
			xcc_write_word   (ps.p, XWORD{XIS::JMP})                                             && // 5/5 Perform jump.
			(ps.p->fn->link == 0 || (ps.p->out.buffer[ps.p->fn->link].u = ps.p->out.size))       && // 1/1 Record first instruction address in function variable store.
			xcc_write_word   (ps.p, XWORD{XIS::SVC})                                             &&
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                     &&
			xcc_push_scope   (ps.p)                                                              && // 1/1 Push a parameter scope.
			try_opt_fn_params(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), verify_params) && // 1/1 Scan for parameters, and only verify number against a forward declared function.
			adjust_fn_rel_ptr(new_state(ps.end))                                                 &&
			xcc_push_scope   (ps.p)                                                              && // 1/1 Push a local variable scope.
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                     &&
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                           &&
			try_statements   (new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R))                      &&
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)                           && // 1/1 Pop the local variable scope.
			emit_pop_scope   (ps.p)                                                              && // 1/1 Pop the parameter scope.
			emit_pop_scope   (ps.p)                                                              &&
			xcc_write_word   (ps.p, XWORD{XIS::LDC})                                             && // NOTE: We do not explicitly set a return value since the call site sets it to 0 by default.
			xcc_write_word   (ps.p, XWORD{XIS::JMP})                                                // 1/1 Jump back to call site
		)
	) {
		ps.p->out.buffer[guard_jmp_idx].u = ps.p->out.size;
		ps.p->fn->link = 0;
		ps.p->fn = NULL;
		return true;
	}
	return false;
}

static bool try_count_fn_param(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			match(ps.p, token::ALIAS, &t)
		)
	) {
		++ps.p->fn->param_count;
		return true;
	}
	return false;
}

static bool try_count_fn_params(xcc_parser_state ps)
{
	if (
		manage_state(
			try_count_fn_param(new_state(ps.end)) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_count_fn_params(new_state(ps.end))
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_count_opt_fn_params(xcc_parser_state ps)
{
	ps.p->fn->param_count = 0;
	if (
		manage_state(
			try_fn_noparam     (new_state(ps.end)) ||
			try_count_fn_params(new_state(ps.end))
		)
	) {
		return true;
	}
	ps.p->fn->param_count = 0;
	return false;
}

static bool try_fn_decl(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			match                    (ps.p, token::ALIAS, &t)                             &&
			(ps.p->fn = xcc_add_fn(t, ps.p)) != NULL                                      &&
			emit_empty_symbol_storage(ps.p, ps.p->fn)                                     &&
			(ps.p->fn->link = ps.p->out.size - 1)                                         &&
			xcc_write_word           (ps.p, XWORD{XIS::RLA})                              &&
			match                    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			try_count_opt_fn_params  (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match                    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			match                    (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		ps.p->fn = NULL;
		return true;
	}
	return false;
}

static bool try_global_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_SEMICOLON) ||
			try_include   (new_state(ps.end))                 ||
			try_enum      (new_state(ps.end))                 ||
			try_fn_def    (new_state(ps.end))                 ||
			try_fn_decl   (new_state(ps.end))                 ||
			try_new_vars  (new_state(ps.end))                 ||
			try_new_consts(new_state(ps.end))                 ||
			try_new_svars (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_global_statements(xcc_parser_state ps)
{
	if (
		manage_state(
			until_end(new_state(ps.end), try_global_statement)
		)
	) {
		return true;
	}
	return false;
}

static bool add_main(xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_fn(new_token("main",4,token::ALIAS,token::ALIAS), p);
	if (sym == NULL) {
		return false;
	}
	sym->param_count = 2; // argc, argv
	return
		emit_empty_symbol_storage(p, sym)  &&
		(sym->link = p->out.size - 1)      &&
		xcc_write_word(p, XWORD{XIS::RLA});
}

static bool emit_call_main(xcc_parser *op)
{
	xcc_symbol *sym = xcc_find_fn(to_chars("main",4), op);
	if (sym == NULL || sym->param == NULL) { // NOTE: param will be null if there is no formal entry point defined.
		return true;
	}

	xcc_parser p = *op;
	// TODO We need to think about how we pass parameters to programs, as well as return values from programs to the calling program. Use function calls as base.
	// TODO According to B manual "main(); exit();" are implicitly called in that order. Since we have parameters and return values from programs we can do the below:
	// p.in.l = init_lexer(chars::view{"exit(main(*0x01,*0x02);", 23})); // 0x00 is the return value address, 0x01 is 'argc', and 0x02 is 'argv' (array of pointers).
	// TODO We should break out "exit" function into a small header so we do not need to include the entire 'libb' for simple programs.
	p.in = init_lexer(chars::view{ "main(0,0);", 10 });
	//p.in = init_lexer(chars::view{ "#include<_exit>exit(main(0,0));", 31 });
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	if (
		manage_state(
//			try_include  (new_state(ps.end)) &&
			try_statement(new_state(ps.end))
		)
	) {
		op->out = p.out;
		return true;
	}
	return false;
}

static bool try_single_program(xcc_parser_state ps)
{
	if (
		manage_state(
			xcc_write_word       (ps.p, XWORD{XIS::SVB}) &&
			add_main             (ps.p)                  &&
			try_global_statements(new_state(ps.end))     &&
			emit_call_main       (ps.p)
		)
	) {
		// NOTE: xcc_pop_scope not possible since we are at index 0 here.
		// TODO: Make push and pop scope work here since we rely on some if its logic
		const U16 lsp = xcc_top_scope_stack_size(ps.p);
		return
			(
				(
					lsp == 0 ||
					(
						xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
						xcc_write_word(ps.p, XWORD{lsp})      &&
						xcc_write_word(ps.p, XWORD{XIS::POP})
					)
				) &&
				xcc_write_word(ps.p, XWORD{XIS::LDB})  &&
				xcc_write_word(ps.p, XWORD{XIS::HALT})
			);
	}
	return false;
}

xcc_out xb(lexer l, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity, const unsigned file_capacity)
{
	xcc_symbol       sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	xcc_filesum      sum_mem[file_capacity];
	xcc_parser       p  = xcc_init_parser(l, mem, sym_mem, sym_capacity, sum_mem, file_capacity);
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	ps.swd = std_lib_path;

	if (
		manage_state(
			try_single_program(new_state(ps.end))
		)
	) {
		return xcc_out{ p.in, p.out, p.max, p.error.code != xcc_error::NONE, p.error };
	}
	set_error(ps.p, p.max, xcc_error::UNEXPECTED);
	return xcc_out{ p.in, p.out, p.max, 1, p.error };
}

static bool try_files(xcc_parser_state ps, const chars::view *source_files, U16 num_source_files)
{
	for (unsigned i = 0; i < num_source_files; ++i) {
		if (
			!manage_state(
				try_file(new_state(ps.end), chars::view{ NULL, 0, 0 }, source_files[i], chars::view{ NULL, 0, 0 })
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_program(xcc_parser_state ps, const chars::view *source_files, U16 num_source_files)
{
	if (
		manage_state(
			xcc_write_word(ps.p, XWORD{XIS::SVB})                             &&
			add_main      (ps.p)                                              &&
			try_files     (new_state(ps.end), source_files, num_source_files) &&
			emit_call_main(ps.p)
		)
	) {
		// NOTE: xcc_pop_scope not possible since we are at index 0 here.
		// TODO: Make push and pop scope work here since we rely on some if its logic
		const U16 lsp = xcc_top_scope_stack_size(ps.p);
		return
			(
				(
					lsp == 0 ||
					(
						xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
						xcc_write_word(ps.p, XWORD{lsp})      &&
						xcc_write_word(ps.p, XWORD{XIS::POP})
					)
				) &&
				xcc_write_word(ps.p, XWORD{XIS::LDB})  &&
				xcc_write_word(ps.p, XWORD{XIS::HALT})
			);
	}
	return false;
}

xcc_out xb(const chars::view *source_files, U16 num_source_files, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity, const unsigned file_capacity)
{
	xcc_symbol       sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	xcc_filesum      sum_mem[file_capacity];
	xcc_parser       p  = xcc_init_parser(init_lexer(chars::view{NULL,0,0}), mem, sym_mem, sym_capacity, sum_mem, file_capacity);
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	ps.swd = std_lib_path;

	if (
		manage_state(
			try_program(new_state(ps.end), source_files, num_source_files)
		)
	) {
		return xcc_out{ p.in, p.out, p.max, p.error.code != xcc_error::NONE, p.error };
	}
	set_error(ps.p, p.max, xcc_error::UNEXPECTED);
	return xcc_out{ p.in, p.out, p.max, 1, p.error };
}

#undef new_state
#undef manage_state
#undef set_error
