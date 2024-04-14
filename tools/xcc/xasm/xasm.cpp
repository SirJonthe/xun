#include <cstddef>
#include "xasm.h"

/// @brief Constructs a new xcc_parser state from an end token.
/// @param end A token user type representing the end of the token stream.
/// @return A new xcc_parser state.
#define new_state(end) xcc_new_state(ps, end, ps.break_ip, ps.continue_ip, ps.loop_scope)

/// @brief Manages the xcc_parser state so it properly rewinds if the parsing fails.
/// @param success The success of the parsing.
/// @return The success of the parsing.
#define manage_state(success) xcc_manage_state(ps, ps.p->error.code == xcc_error::NONE && (success))

/// @brief Sets an error in the xcc_parser.
/// @param p The xcc_parser.
/// @param code The error code.
#define set_error(p, code) xcc_set_error(p, code, p->file, __LINE__)

/// @brief Converts a human-readable hexadecimal string to a number.
/// @param nums The characters representing the human-readable hexadecimal number. The input is assumed to be prepended with "0x".
/// @param len The number of characters in the input string.
/// @return The resulting number.
/// @warning The function does not verify inputs. Invalid input gives invalid output.
/// @note The input string is assumed to be prepended with "0x".
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

/// @brief XASM token identifier.
struct xtoken
{
	/// @brief The various constants representing an XASM token identifier.
	/// @note The token constants do not correspond to the XIS constants.
	enum tokentype
	{
		KEYWORD_INSTRUCTION_SET = 1,

		KEYWORD_DIRECTIVE_BIN,     // Allows for the dump of data inside the binary.
		KEYWORD_DIRECTIVE_SCOPE,   // Allows for the creation of variables with automatic storage duration.
		
		KEYWORD_DIRECTIVE_LIT,     // Defines a literal under a given alias.
		KEYWORD_DIRECTIVE_HERE,    // Emits the absolute address of IP.
		KEYWORD_DIRECTIVE_TOP,     // Emits the top stack address of SP
		KEYWORD_DIRECTIVE_FRAME,   // Emtis the top stack address of the frame (C relative).
		KEYWORD_DIRECTIVE_BASE,    // Emits the top stack address of the program stack entry point (the bottom at program start) (B relative).
		KEYWORD_DIRECTIVE_ENTRY,   // Emits the top address of (A relative)
		KEYWORD_DIRECTIVE_ERR,     // Emit the error register

		OPERATOR_DIRECTIVE_AT,
		OPERATOR_DIRECTIVE_ADDR,
		OPERATOR_DIRECTIVE_LABEL,
		OPERATOR_DIRECTIVE_DOLLAR,
	
		OPERATOR_ENCLOSE_PARENTHESIS_L,
		OPERATOR_ENCLOSE_PARENTHESIS_R,
	
		OPERATOR_ENCLOSE_BRACKET_L,
		OPERATOR_ENCLOSE_BRACKET_R,
	
		OPERATOR_ENCLOSE_BRACE_L,
		OPERATOR_ENCLOSE_BRACE_R,
		
		OPERATOR_ARITHMETIC_ADD,
		OPERATOR_ARITHMETIC_SUB,
		OPERATOR_ARITHMETIC_MUL,
		OPERATOR_ARITHMETIC_DIV,
		OPERATOR_ARITHMETIC_MOD,
		OPERATOR_BITWISE_AND,
		OPERATOR_LOGICAL_AND,
		OPERATOR_BITWISE_OR,
		OPERATOR_LOGICAL_OR,
		OPERATOR_BITWISE_XOR,
		OPERATOR_BITWISE_NOT,
		OPERATOR_BITWISE_LSHIFT,
		OPERATOR_BITWISE_RSHIFT,
		OPERATOR_LOGICAL_LESS,
		OPERATOR_LOGICAL_LESSEQUAL,
		OPERATOR_LOGICAL_GREATER,
		OPERATOR_LOGICAL_GREATEREQUAL,
		OPERATOR_LOGICAL_EQUAL,
		OPERATOR_LOGICAL_NOTEQUAL,
		OPERATOR_LOGICAL_NOT,

		OPERATOR_STOP,
		OPERATOR_COMMA,
		OPERATOR_COLON,

		LITERAL_INT
	};
};

const signed X_TOKEN_COUNT = 96;
const token X_TOKENS[X_TOKEN_COUNT] = {
	new_keyword ("nop",                     3, XIS::NOP),
	new_keyword ("at",                      2, XIS::AT),
	new_keyword ("set",                     3, xtoken::KEYWORD_INSTRUCTION_SET),
	new_keyword ("put",                     3, XIS::PUT),
	new_keyword ("add",                     3, XIS::ADD),
	new_keyword ("sub",                     3, XIS::SUB),
	new_keyword ("mul",                     3, XIS::MUL),
	new_keyword ("div",                     3, XIS::DIV),
	new_keyword ("mod",                     3, XIS::MOD),
	new_keyword ("iadd",                    4, XIS::IADD),
	new_keyword ("isub",                    4, XIS::ISUB),
	new_keyword ("imul",                    4, XIS::IMUL),
	new_keyword ("idiv",                    4, XIS::IDIV),
	new_keyword ("imod",                    4, XIS::IMOD),
	new_keyword ("lsh",                     3, XIS::LSH),
	new_keyword ("rsh",                     3, XIS::RSH),
	new_keyword ("and",                     3, XIS::AND),
	new_keyword ("or",                      2, XIS::OR),
	new_keyword ("xor",                     3, XIS::XOR),
	new_keyword ("mov",                     3, XIS::MOVU),
	new_keyword ("eq",                      2, XIS::EQ),
	new_keyword ("ne",                      2, XIS::NE),
	new_keyword ("gt",                      2, XIS::GT),
	new_keyword ("lt",                      2, XIS::LT),
	new_keyword ("ge",                      2, XIS::GE),
	new_keyword ("le",                      2, XIS::LE),
	new_keyword ("igt",                     3, XIS::IGT),
	new_keyword ("ilt",                     3, XIS::ILT),
	new_keyword ("ige",                     3, XIS::IGE),
	new_keyword ("ile",                     3, XIS::ILE),
	new_keyword ("jmp",                     3, XIS::JMP),
	new_keyword ("cjmp",                    4, XIS::CJMP),
	new_keyword ("skip",                    4, XIS::SKIP),
	new_keyword ("cskip",                   5, XIS::CSKIP),
	new_keyword ("clock",                   5, XIS::CLOCK),
	new_keyword("sva",                      3, XIS::SVA),
	new_keyword("lda",                      3, XIS::LDA),
	new_keyword("rla",                      3, XIS::RLA),
	new_keyword("svb",                      3, XIS::SVB),
	new_keyword("ldb",                      3, XIS::LDB),
	new_keyword("rlb",                      3, XIS::RLB),
	new_keyword("svc",                      3, XIS::SVC),
	new_keyword("ldc",                      3, XIS::LDC),
	new_keyword("rlc",                      3, XIS::RLC),
	new_keyword("port",                     4, XIS::PORT),
	new_keyword("poll",                     4, XIS::POLL),
	new_keyword("pass",                     4, XIS::PASS),
	new_keyword("pend",                     4, XIS::PEND),
	new_keyword("cpuid",                    5, XIS::CPUID),
	new_keyword("cerr",                     4, XIS::CERR),
	new_keyword("full",                     4, XIS::FULL),

	new_operator("@",                       1, xtoken::OPERATOR_DIRECTIVE_AT),
	new_operator("&",                       1, xtoken::OPERATOR_DIRECTIVE_ADDR),
	new_operator("$",                       1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
	
	new_keyword ("bin",                     3, xtoken::KEYWORD_DIRECTIVE_BIN),
	new_keyword ("scope",                   5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
	new_keyword ("here",                    4, xtoken::KEYWORD_DIRECTIVE_HERE),
	new_keyword ("top",                     3, xtoken::KEYWORD_DIRECTIVE_TOP),
	new_keyword ("frame",                   5, xtoken::KEYWORD_DIRECTIVE_FRAME),
	new_keyword ("base",                    4, xtoken::KEYWORD_DIRECTIVE_BASE),
	new_keyword ("entry",                   5, xtoken::KEYWORD_DIRECTIVE_ENTRY),
	new_keyword ("lit",                     3, xtoken::KEYWORD_DIRECTIVE_LIT),
	new_keyword ("err",                     3, xtoken::KEYWORD_DIRECTIVE_ERR),

	new_operator("+",                       1, xtoken::OPERATOR_ARITHMETIC_ADD),
	new_operator("-",                       1, xtoken::OPERATOR_ARITHMETIC_SUB),
	new_operator("*",                       1, xtoken::OPERATOR_ARITHMETIC_MUL),
	new_operator("/",                       1, xtoken::OPERATOR_ARITHMETIC_DIV),
	new_operator("%",                       1, xtoken::OPERATOR_ARITHMETIC_MOD),
	new_operator("&",                       1, xtoken::OPERATOR_BITWISE_AND),
	new_operator("&&",                      2, xtoken::OPERATOR_LOGICAL_AND),
	new_operator("|",                       1, xtoken::OPERATOR_BITWISE_OR),
	new_operator("||",                      2, xtoken::OPERATOR_LOGICAL_OR),
	new_operator("^",                       1, xtoken::OPERATOR_BITWISE_XOR),
	new_operator("~",                       1, xtoken::OPERATOR_BITWISE_NOT),
	new_operator("<<",                      2, xtoken::OPERATOR_BITWISE_LSHIFT),
	new_operator(">>",                      2, xtoken::OPERATOR_BITWISE_RSHIFT),
	new_operator("<",                       1, xtoken::OPERATOR_LOGICAL_LESS),
	new_operator("<=",                      2, xtoken::OPERATOR_LOGICAL_LESSEQUAL),
	new_operator(">",                       1, xtoken::OPERATOR_LOGICAL_GREATER),
	new_operator(">=",                      2, xtoken::OPERATOR_LOGICAL_GREATEREQUAL),
	new_operator("==",                      2, xtoken::OPERATOR_LOGICAL_EQUAL),
	new_operator("!=",                      2, xtoken::OPERATOR_LOGICAL_NOTEQUAL),
	new_operator("!",                       1, xtoken::OPERATOR_LOGICAL_NOT),

	new_operator(":",                       1, xtoken::OPERATOR_COLON),
	new_operator("[",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("{",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("(",                       1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator(",",                       1, xtoken::OPERATOR_COMMA),
	new_operator(".",                       1, xtoken::OPERATOR_STOP),
	new_comment ("//",                      2),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, xtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xtoken::LITERAL_INT, hex2u)
};

static token xasm_lex(lexer *l)
{
	return lex(l, X_TOKENS, X_TOKEN_COUNT);
}

static token peek(xcc_parser *p)
{
	return xcc_peek(p, xasm_lex);
}

static bool match(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xasm_lex);
}

static bool try_decl_var        (xcc_parser_state ps);
static bool try_decl_mem        (xcc_parser_state ps);
static bool try_decl_list       (xcc_parser_state ps);
static bool try_directive_scope (xcc_parser_state ps);
static bool try_directive_stmt  (xcc_parser_state ps);
static bool try_param           (xcc_parser_state ps);
static bool try_lparam          (xcc_parser_state ps);
static bool try_put_param       (xcc_parser_state ps);
static bool try_directive_at    (xcc_parser_state ps);
static bool try_instruction_put (xcc_parser_state ps);
static bool try_mov_param       (xcc_parser_state ps);
static bool try_repeat_mov_param(xcc_parser_state ps);
static bool try_instruction_mov (xcc_parser_state ps);
static bool try_instruction_stmt(xcc_parser_state ps);
static bool try_emit_lit_list   (xcc_parser_state ps);
static bool try_statements      (xcc_parser_state ps);
static bool try_program         (xcc_parser_state ps);

template < typename type_t >
static bool eval_operation(xcc_parser *p, unsigned user_type, type_t &l, type_t r)
{
	switch (user_type) {
	case xtoken::OPERATOR_ARITHMETIC_ADD:       l += r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_SUB:       l -= r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_MUL:       l *= r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_DIV:       l /= r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_MOD:       l %= r;       return true;
	case xtoken::OPERATOR_BITWISE_AND:
	case xtoken::OPERATOR_LOGICAL_AND:          l &= r;       return true;
	case xtoken::OPERATOR_BITWISE_OR:
	case xtoken::OPERATOR_LOGICAL_OR:           l |= r;       return true;
	case xtoken::OPERATOR_BITWISE_XOR:          l ^= r;       return true;
	case xtoken::OPERATOR_BITWISE_NOT:          l = ~r;       return true;
	case xtoken::OPERATOR_BITWISE_LSHIFT:       l <<= r;      return true;
	case xtoken::OPERATOR_BITWISE_RSHIFT:       l >>= r;      return true;
	case xtoken::OPERATOR_LOGICAL_LESS:         l = (l < r);  return true;
	case xtoken::OPERATOR_LOGICAL_LESSEQUAL:    l = (l <= r); return true;
	case xtoken::OPERATOR_LOGICAL_GREATER:      l = (l > r);  return true;
	case xtoken::OPERATOR_LOGICAL_GREATEREQUAL: l = (l >= r); return true;
	case xtoken::OPERATOR_LOGICAL_EQUAL:        l = (l == r); return true;
	case xtoken::OPERATOR_LOGICAL_NOTEQUAL:     l = (l != r); return true;
	case xtoken::OPERATOR_LOGICAL_NOT:          l = !r;       return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_factor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_read_lit(xcc_parser_state ps, type_t &result)
{
	token t;
	if (match(ps.p, xtoken::LITERAL_INT, &t)) {
		result = t.hash;
		return true;
	} else if (match(ps.p, token::ALIAS, &t)) {
		const xcc_symbol *sym = xcc_find_lit(t.text, ps.p);
		if (sym != NULL) {
			result = sym->data.u;
			return true;
		}
	}
	return false;
}

template < typename type_t >
static bool try_lit_opt_factor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (match(ps.p, xtoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, xtoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, xtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
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
static bool try_lit_bwnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_BITWISE_NOT, &t) &&
			try_lit_rval  (new_state(ps.end), l)                   &&
			eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_lnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_LOGICAL_NOT, &t) &&
			try_lit_rval  (new_state(ps.end), l)                   &&
			eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

// val ::= "*" val | name "(" exprs ")" | val "[" expr "]" | name | num
template < typename type_t >
static bool try_lit_rval(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_bwnot_val(new_state(ps.end), l) ||
			try_lit_lnot_val (new_state(ps.end), l) ||
			try_read_lit     (new_state(ps.end), l) ||
			(
				(
					match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)         &&
					try_lit_expr(new_state(xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), l) &&
					match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
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
			match       (ps.p, xtoken::OPERATOR_ARITHMETIC_ADD) &&
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
			match       (ps.p, xtoken::OPERATOR_ARITHMETIC_SUB) &&
			try_lit_rval(new_state(ps.end), l)
		)
	) {
		l = -l;
		return true;
	}
	return false;
}

// factor ::= "&" val | "+" val | "-" val | "(" expr ")"
template < typename type_t >
static bool try_lit_factor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			try_lit_uni_pos (new_state(ps.end), l) ||
			try_lit_uni_neg (new_state(ps.end), l) ||
			try_lit_rval    (new_state(ps.end), l)
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
	while (match(ps.p, xtoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, xtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_BITWISE_LSHIFT, &t) || match(ps.p, xtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_LOGICAL_LESS, &t) || match(ps.p, xtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || match(ps.p, xtoken::OPERATOR_LOGICAL_GREATER, &t) || match(ps.p, xtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_LOGICAL_EQUAL, &t) || match(ps.p, xtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_BITWISE_AND, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_BITWISE_XOR, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_BITWISE_OR, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_LOGICAL_AND, &t)) {
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
	while (match(ps.p, xtoken::OPERATOR_LOGICAL_OR, &t)) {
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

static bool try_decl_var(xcc_parser_state ps)
{
	const token t = peek(ps.p);
	if (
		manage_state(
			match(ps.p, token::ALIAS)
		)
	) {
		if (xcc_add_var(t, ps.p) == NULL) { return false; }
		return true;
	}
	return false;
}

static bool try_decl_mem(xcc_parser_state ps)
{
	U16 size;
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L)            &&
			try_read_lit  (new_state(xtoken::OPERATOR_ENCLOSE_BRACKET_R), size) &&
			match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)            &&
			xcc_add_memory(ps.p, size)
		)
	) {
		return true;
	}
	return false;
}

static bool try_decl_list(xcc_parser_state ps)
{
	if (
		manage_state(
			peek(ps.p).user_type == ps.end ||
			(
				(
					try_decl_var(new_state(ps.end)) ||
					try_decl_mem(new_state(ps.end))
				) &&
				(
					!match       (ps.p, xtoken::OPERATOR_COMMA) ||
					try_decl_list(new_state(ps.end))
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_directive_scope(xcc_parser_state ps)
{
	U16 stack_size;
	if (
		manage_state(
			match         (ps.p, xtoken::KEYWORD_DIRECTIVE_SCOPE)       &&
			match         (ps.p, xtoken::OPERATOR_COLON)                &&
			xcc_push_scope(ps.p)                                        &&
			try_decl_list (new_state(xtoken::OPERATOR_ENCLOSE_BRACE_L)) &&
			(
				(stack_size = xcc_top_scope_stack_size(ps.p)) == 0 ||
				(
					xcc_write_word(ps.p, XWORD{XIS::PUT})   &&
					xcc_write_word(ps.p, XWORD{stack_size}) &&
					xcc_write_word(ps.p, XWORD{XIS::PUSH})
				)
			) &&
			match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L)      &&
			try_statements(new_state(xtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R)      &&
			(
				stack_size == 0 ||
				(
					xcc_write_word(ps.p, XWORD{XIS::PUT})   &&
					xcc_write_word(ps.p, XWORD{stack_size}) &&
					xcc_write_word(ps.p, XWORD{XIS::POP})
				)
			) &&
			xcc_pop_scope(ps.p)
		)
	) {
		
		return true;
	}
	return false;
}

static bool try_directive_bin(xcc_parser_state ps)
{
	U16 ip;
	if (
		manage_state(
			match            (ps.p, xtoken::KEYWORD_DIRECTIVE_BIN) &&
			xcc_write_word   (ps.p, XWORD{XIS::PUT})               &&
			(ip = ps.p->out.size)                                  &&
			xcc_write_word   (ps.p, XWORD{0})                      &&
			xcc_write_word   (ps.p, XWORD{XIS::SKIP})              &&
			try_emit_lit_list(new_state(xtoken::OPERATOR_STOP))    &&
			match            (ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		ps.p->out.buffer[ip].u = ps.p->out.size - ip - 2;
		return true;
	}
	return false;
}

static bool try_directive_lit(xcc_parser_state ps)
{
	U16 val;
	token t;
	if (
		manage_state(
			match       (ps.p, xtoken::KEYWORD_DIRECTIVE_LIT)   &&
			match       (ps.p, token::ALIAS, &t)                &&
			match       (ps.p, xtoken::OPERATOR_COMMA)          &&
			try_lit_expr(new_state(xtoken::OPERATOR_STOP), val) &&
			xcc_add_lit (t, val, ps.p)                          &&
			match       (ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool try_directive_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) &&
			(
				try_directive_scope(new_state(ps.end)) ||
				try_directive_bin  (new_state(ps.end)) ||
				try_directive_lit  (new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_redir_param(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_DIRECTIVE_AT) &&
			try_param     (new_state(ps.end))                   &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_var(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool skip_redir = match(ps.p, xtoken::OPERATOR_DIRECTIVE_ADDR);
	if (
		manage_state(
			match        (ps.p, token::ALIAS, &t)      &&
			(sym = xcc_find_var(t.text, ps.p)) != NULL &&
			xcc_write_rel(ps.p, sym)                   &&
			(
				skip_redir ||
				xcc_write_word(ps.p, XWORD{XIS::AT})
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_lit(xcc_parser_state ps)
{
	U16 lit = 0;
	if (
		manage_state(
			try_lit_expr  (new_state(ps.end), lit) &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})  &&
			xcc_write_word(ps.p, XWORD{lit})
		)
	) {
		return true;
	}
	return false;
}

static bool try_opt_index(xcc_parser_state ps)
{
	const unsigned p = peek(ps.p).user_type;
	if (
		manage_state(
			(p == ps.end || p == xtoken::OPERATOR_COMMA) ||
			(
				match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
				try_put_lit   (new_state(xtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
				xcc_write_word(ps.p, XWORD{XIS::ADD})                         &&
				xcc_write_word(ps.p, XWORD{XIS::AT})                          &&
				match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_reg(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) &&
			(
				(
					match         (ps.p, xtoken::KEYWORD_DIRECTIVE_HERE)  &&
					xcc_write_word(ps.p, XWORD{XIS::PUTI})
				) ||
				(
					match         (ps.p, xtoken::KEYWORD_DIRECTIVE_TOP)   &&
					xcc_write_word(ps.p, XWORD{XIS::PUTS})
				) ||
				(
					match         (ps.p, xtoken::KEYWORD_DIRECTIVE_FRAME) &&
					xcc_write_word(ps.p, XWORD{XIS::PUT})                 &&
					xcc_write_word(ps.p, XWORD{0})                        &&
					xcc_write_word(ps.p, XWORD{XIS::RLC})
				) ||
				(
					match         (ps.p, xtoken::KEYWORD_DIRECTIVE_BASE)  &&
					xcc_write_word(ps.p, XWORD{XIS::PUT})                 &&
					xcc_write_word(ps.p, XWORD{0})                        &&
					xcc_write_word(ps.p, XWORD{XIS::RLB})
				) ||
				(
					match         (ps.p, xtoken::KEYWORD_DIRECTIVE_ENTRY) &&
					xcc_write_word(ps.p, XWORD{XIS::PUT})                 &&
					xcc_write_word(ps.p, XWORD{0})                        &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				) ||
				(
					match         (ps.p, xtoken::KEYWORD_DIRECTIVE_ERR)   &&
					xcc_write_word(ps.p, XWORD{XIS::ERR})
				)
			)
		)
	) {
		return true;
	}
	return false;
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

static bool try_put_lbl(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			match(ps.p, xtoken::OPERATOR_DIRECTIVE_LABEL)  &&
			match(ps.p, token::ALIAS, &t)                  &&
			(
				(sym = xcc_find_lbl(t.text, ps.p)) != NULL ||
				(
					(sym = xcc_add_lbl(t, ps.p)) != NULL   &&
					emit_empty_symbol_storage(ps.p, sym)   &&
					(sym->link = ps.p->out.size - 1)       &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				)
			)                                              &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_param(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				try_redir_param(new_state(ps.end)) ||
				try_put_var    (new_state(ps.end)) ||
				try_put_lit    (new_state(ps.end)) ||
				try_put_reg    (new_state(ps.end)) ||
				try_put_lbl    (new_state(ps.end))
			) &&
			try_opt_index(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_redir_lparam(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_DIRECTIVE_AT) &&
			try_lparam    (new_state(ps.end))                   &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_lvar(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			match        (ps.p, token::ALIAS, &t)      &&
			(sym = xcc_find_var(t.text, ps.p)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool try_lparam(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				try_redir_lparam(new_state(ps.end)) ||
				try_put_lvar    (new_state(ps.end)) ||
				try_put_lit     (new_state(ps.end))
			) &&
			try_opt_index(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_param(xcc_parser_state ps)
{
	if (
		manage_state(
			try_param(new_state(ps.end)) &&
			(
				!match       (ps.p, xtoken::OPERATOR_COMMA) ||
				try_put_param(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_instruction_put(xcc_parser_state ps)
{
	if (
		manage_state(
			match        (ps.p, XIS::PUT) &&
			try_put_param(new_state(xtoken::OPERATOR_STOP))
		)
	) {
		return true;
	}
	return false;
}

static bool try_mov_param(xcc_parser_state ps)
{
	if (
		manage_state(
			try_lparam    (new_state(ps.end))      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVU})
		)
	) {
		return true;
	}
	return false;
}

static bool try_toss_param(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_ARITHMETIC_MUL) &&
			xcc_write_word(ps.p, XWORD{XIS::TOSS})
		)
	) {
		return true;
	}
	return false;
}

static bool try_repeat_mov_param(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				try_mov_param (new_state(ps.end))  ||
				try_toss_param(new_state(ps.end))
			) &&
			(
				!match              (ps.p, xtoken::OPERATOR_COMMA) ||
				try_repeat_mov_param(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_instruction_mov(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, XIS::MOVU) &&
			(
				(
					peek(ps.p).user_type == ps.end &&
					xcc_write_word(ps.p, XWORD{XIS::MOVU})
				) ||
				try_repeat_mov_param(new_state(xtoken::OPERATOR_STOP))
			)
		)
	 ) {
		return true;
	}
	return false;
}

static bool try_instruction_all(xcc_parser_state ps)
{
	token i = peek(ps.p);
	if (
		manage_state(
			i.type      == token::KEYWORD                  &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_HERE  &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_FRAME &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_BASE  &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_ENTRY &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_TOP   &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_ERR   &&
			match         (ps.p, i.user_type)              &&
			xcc_write_word(ps.p, XWORD{U16(i.user_type)})
		)
	) {
		return true;
	}
	return false;
}

static bool try_instruction_set(xcc_parser_state ps)
{

	if (
		manage_state(
			match         (ps.p, xtoken::KEYWORD_INSTRUCTION_SET) &&
			try_lparam    (new_state(xtoken::OPERATOR_COMMA))     &&
			match         (ps.p, xtoken::OPERATOR_COMMA)          &&
			try_param     (new_state(xtoken::OPERATOR_STOP))      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_instruction_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				try_instruction_put(new_state(xtoken::OPERATOR_STOP)) ||
				try_instruction_mov(new_state(xtoken::OPERATOR_STOP)) ||
				try_instruction_set(new_state(xtoken::OPERATOR_STOP)) ||
				try_instruction_all(new_state(xtoken::OPERATOR_STOP)) // NOTE: Must be last condition
			) &&
			match(ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool try_emit_lit_list(xcc_parser_state ps)
{
	U16 lit = 0;
	while (
		manage_state(
			try_lit_expr(new_state(ps.end), lit) &&
			xcc_write_word(ps.p, XWORD{lit})
		)
	) {
		if (peek(ps.p).user_type == ps.end) {
			return true;
		} else if (!match(ps.p, xtoken::OPERATOR_COMMA)) {
			break;
		}
		lit = 0;
	}
	return false;
}

static bool try_lbl_def_stmt(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			match(ps.p, token::ALIAS, &t)       &&
			match(ps.p, xtoken::OPERATOR_COMMA) &&
			(
				(sym = xcc_find_lbl(t.text, ps.p)) != NULL ||
				(
					(sym = xcc_add_lbl(t, ps.p)) != NULL  &&
					emit_empty_symbol_storage(ps.p, sym)  &&
					(sym->link = ps.p->out.size - 1)      &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				)
			 ) &&
			(sym->link == 0 || (ps.p->out.buffer[sym->link].u = ps.p->out.size))
		)
	) {
		sym->link = 0;
		return true;
	}
	return false;
}

static bool try_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			try_directive_stmt  (new_state(ps.end)) ||
			try_instruction_stmt(new_state(ps.end)) ||
			try_lbl_def_stmt    (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_statements(xcc_parser_state ps)
{
	while (peek(ps.p).user_type != ps.end) {
		if (
			!manage_state(
				try_statement(new_state(ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_program(xcc_parser_state ps)
{
	if (
		manage_state(
			xcc_write_word(ps.p, XWORD{XIS::SVB})  &&
			try_statements(new_state(ps.end))      &&
			xcc_write_word(ps.p, XWORD{XIS::LDB})  &&
			xcc_write_word(ps.p, XWORD{XIS::HALT})
		)
	) {
		return true;
	}
	return false;
}

xcc_out xasm(lexer l, xcc_binary memory, const U16 sym_capacity)
{
	xcc_symbol       sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	xcc_parser       p  = xcc_init_parser(l, memory, sym_mem, sym_capacity);
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	if (manage_state(try_program(new_state(ps.end)))) {
		return xcc_out{ p.in, p.out, p.max, 0, p.error };
	}
	set_error(ps.p, xcc_error::UNEXPECTED);
	p.error.tok = p.max;
	return xcc_out{ p.in, p.out, p.max, 1, p.error };
}

bool xasm_inline(xcc_parser_state ps)
{
	if (
		manage_state(
			try_statement(new_state(ps.end))
		)
	) {
		return true;
	}
	set_error(ps.p, xcc_error::UNEXPECTED);
	ps.p->error.tok = ps.p->max;
	return false;
}

#undef new_state
#undef manage_state
#undef set_error
