#include <cstdlib>
#include "xb.h"
#include "../../../xis.h"
#include "../../../xarch.h"
#include "../../../lib/parsec/lex.h"
#include "../xasm/xasm.h"

// TODO
// [ ] Fix comments
// [ ] Arrays without explicit size
// [ ] Push a second scope after parameter scope in functions
// [ ] Include files (hard because it requires a virtual file system)
// [ ] static (variables stored in binary, RLA used to address)
// [ ] ++ +=
// [ ] namespace
// [ ] continue, break

/// @brief Constructs a new parser state from an end token.
/// @param end A token user type representing the end of the token stream.
/// @return A new parser state.
#define new_state(end) xcc_new_state(ps.p, end)

/// @brief Manages the parser state so it properly rewinds if the parsing fails.
/// @param success The success of the parsing.
/// @return The success of the parsing.
#define manage_state(success) xcc_manage_state(ps, ps.p->error.code == xcc_error::NONE && (success))

/// @brief Sets an error in the parser.
/// @param p The parser.
/// @param code The error code.
#define set_error(p, code) xcc_set_error(p, code, __LINE__)

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

static unsigned ch2u(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 1; i < len - 1; ++i) {
		h = h * 10 + U16(nums[i]);
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
		KEYWORD_CONTROL_RETURN,
		KEYWORD_CONTROL_IF,
		KEYWORD_CONTROL_ELSE,
		KEYWORD_CONTROL_WHILE,
		KEYWORD_CONTROL_BREAK,
		KEYWORD_CONTROL_CONTINUE,
		KEYWORD_INTRINSIC_ASM,
		KEYWORD_NAMESPACE,
		OPERATOR_ARITHMETIC_ADD,
		OPERATOR_ARITHMETIC_SUB,
		OPERATOR_ARITHMETIC_MUL,
		OPERATOR_ARITHMETIC_DIV,
		OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ASSIGNMENT_SET,
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
		LITERAL_INT
		// LITERAL_FLOAT
	};
};

const signed XB_TOKEN_COUNT = 49;
const token XB_TOKENS[XB_TOKEN_COUNT] = {
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
	new_keyword ("namespace",               9, xbtoken::KEYWORD_NAMESPACE),
	new_operator("<=",                      2, xbtoken::OPERATOR_LOGICAL_LESSEQUAL),
	new_operator(">=",                      2, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL),
	new_operator("==",                      2, xbtoken::OPERATOR_LOGICAL_EQUAL),
	new_operator("!=",                      2, xbtoken::OPERATOR_LOGICAL_NOTEQUAL),
	new_operator("&&",                      2, xbtoken::OPERATOR_LOGICAL_AND),
	new_operator("||",                      2, xbtoken::OPERATOR_LOGICAL_OR),
	new_operator("<<",                      2, xbtoken::OPERATOR_BITWISE_LSHIFT),
	new_operator(">>",                      2, xbtoken::OPERATOR_BITWISE_RSHIFT),
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
	new_literal ("[0-9]+",                  6, xbtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xbtoken::LITERAL_INT, hex2u),
	new_literal ("\'*\'",                   3, xbtoken::LITERAL_INT, ch2u)
};

token xblex(lexer *l)
{
	return lex(l, XB_TOKENS, XB_TOKEN_COUNT);
}

token xblex1(lexer *l)
{
	return chlex(l);
}

static bool emit_empty_symbol_storage(xcc_parser *p, const xcc_symbol *sym)
{
	if (sym->category != xcc_symbol::PARAM && sym->category != xcc_symbol::LIT) {
		return 
			(sym->category == xcc_symbol::SVAR ? xcc_write_word(p, XWORD{XIS::BIN}) : xcc_write_word(p, XWORD{XIS::PUT})) &&
			xcc_write_word(p, XWORD{0});
	}
	return true;
}

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
		xcc_pop_scope(p->scopes);
}

static token peek(xcc_parser *p)
{
	return xcc_peek(p, xblex);
}

static bool match(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xblex);
}

static bool match1(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xblex1);
}

static bool until_end(xcc_parser_state ps, bool (*try_fn)(xcc_parser_state))
{
	while (peek(ps.p).user_type != ps.end) {
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

static bool try_put_var_addr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			match(ps.p, token::ALIAS, &t)              &&
			(sym = xcc_find_var(t.text, ps.p)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_var(xcc_parser_state ps)
{
	if (
		manage_state(
			try_put_var_addr(new_state(ps.end)) &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
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
		if (sym != NULL && sym->category == xcc_symbol::FN && sym->param_count != param_count) {
			set_error(ps.p, xcc_error::VERIFY);
			return false;
		}
		return true;
	}
	return false;
}

static bool try_call_fn(xcc_parser_state ps)
{
	xcc_symbol *sym;
	U16 off_index = 0;
	token t;
	if (
		manage_state(
			match                (ps.p, token::ALIAS, &t)                                  && // TODO Replace this with any value (can be alias, literal, indirection etc.)
			(sym = xcc_find_symbol(t.text, ps.p)) != NULL                                  && // NOTE: find_symbol instead of find_fn. That way we can call anything as if it is a function!
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
			xcc_write_rel        (ps.p, sym)                                               &&
			xcc_write_word       (ps.p, XWORD{XIS::AT})                                    &&
			xcc_write_word       (ps.p, XWORD{XIS::JMP})
		)
	) {
		// TODO Surely we can determine instruction pointer offset without constant 7...
		return true;
	}
	return false;
}

template < typename type_t >
static bool eval_operation(xcc_parser *p, unsigned user_type, type_t &l, type_t r)
{
	switch (user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ADD:
		l += r;
		return true;
	case xbtoken::OPERATOR_ARITHMETIC_SUB:
		l -= r;
		return true;
	case xbtoken::OPERATOR_ARITHMETIC_MUL:
		l *= r;
		return true;
	case xbtoken::OPERATOR_ARITHMETIC_DIV:
		l /= r;
		return true;
	case xbtoken::OPERATOR_ARITHMETIC_MOD:
		l %= r;
		return true;
	case xbtoken::OPERATOR_BITWISE_AND:
	case xbtoken::OPERATOR_LOGICAL_AND:
		l &= r;
		return true;
	case xbtoken::OPERATOR_BITWISE_OR:
	case xbtoken::OPERATOR_LOGICAL_OR:
		l |= r;
		return true;
	case xbtoken::OPERATOR_BITWISE_XOR:
		l ^= r;
		return true;
	case xbtoken::OPERATOR_BITWISE_NOT:
		l = ~r;
		return true;
	case xbtoken::OPERATOR_BITWISE_LSHIFT:
		l <<= r;
		return true;
	case xbtoken::OPERATOR_BITWISE_RSHIFT:
		l >>= r;
		return true;
	case xbtoken::OPERATOR_LOGICAL_LESS:
		l = (l < r);
		return true;
	case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:
		l = (l <= r);
		return true;
	case xbtoken::OPERATOR_LOGICAL_GREATER:
		l = (l > r);
		return true;
	case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL:
		l = (l >= r);
		return true;
	case xbtoken::OPERATOR_LOGICAL_EQUAL:
		l = (l == r);
		return true;
	case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:
		l = (l != r);
		return true;
	case xbtoken::OPERATOR_LOGICAL_NOT:
		l = !r;
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_factor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool try_read_lit(xcc_parser_state ps, type_t &result)
{
	token t;
	if (match(ps.p, xbtoken::LITERAL_INT, &t)) {
		result = t.hash;
		return true;
	} else if (match(ps.p, token::ALIAS, &t)) {
		const xcc_symbol *sym = xcc_find_lit(t.text, ps.p);
		if (sym != NULL) {
			result = sym->data.u;
			return true;
		}
	} else if (match(ps.p, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE) && match1(ps.p, token::CHAR, &t) && match(ps.p, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE)) {
		result = t.hash;
		return true;
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
static bool try_lit_bwnot_val(xcc_parser_state ps, type_t &l)
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
static bool try_lit_lnot_val(xcc_parser_state ps, type_t &l)
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
	case xbtoken::OPERATOR_ARITHMETIC_ADD:
		if (xcc_write_word(p, XWORD{XIS::ADD})) { return true; }
		break;
	case xbtoken::OPERATOR_ARITHMETIC_SUB:
		if (xcc_write_word(p, XWORD{XIS::SUB})) { return true; }
		break;
	case xbtoken::OPERATOR_ARITHMETIC_MUL:
		if (xcc_write_word(p, XWORD{XIS::MUL})) { return true; }
		break;
	case xbtoken::OPERATOR_ARITHMETIC_DIV:
		if (xcc_write_word(p, XWORD{XIS::DIV})) { return true; }
		break;
	case xbtoken::OPERATOR_ARITHMETIC_MOD:
		if (xcc_write_word(p, XWORD{XIS::MOD})) { return true; }
		break;
	case xbtoken::OPERATOR_BITWISE_AND:
	case xbtoken::OPERATOR_LOGICAL_AND:
		if (xcc_write_word(p, XWORD{XIS::AND})) { return true; }
		break;
	case xbtoken::OPERATOR_BITWISE_OR:
	case xbtoken::OPERATOR_LOGICAL_OR:
		if (xcc_write_word(p, XWORD{XIS::OR})) { return true; }
		break;
	case xbtoken::OPERATOR_BITWISE_XOR:
		if (xcc_write_word(p, XWORD{XIS::XOR})) { return true; }
		break;
	case xbtoken::OPERATOR_BITWISE_NOT:
		if (xcc_write_word(p, XWORD{XIS::NOT})) { return true; }
		break;
	case xbtoken::OPERATOR_BITWISE_LSHIFT:
		if (xcc_write_word(p, XWORD{XIS::LSH})) { return true; }
		break;
	case xbtoken::OPERATOR_BITWISE_RSHIFT:
		if (xcc_write_word(p, XWORD{XIS::RSH})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_LESS:
		if (xcc_write_word(p, XWORD{XIS::LT})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:
		if (xcc_write_word(p, XWORD{XIS::LE})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_GREATER:
		if (xcc_write_word(p, XWORD{XIS::GT})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL:
		if (xcc_write_word(p, XWORD{XIS::LE})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_EQUAL:
		if (xcc_write_word(p, XWORD{XIS::EQ})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:
		if (xcc_write_word(p, XWORD{XIS::NE})) { return true; }
		break;
	case xbtoken::OPERATOR_LOGICAL_NOT:
		if (
			xcc_write_word(p, XWORD{XIS::PUT}) &&
			xcc_write_word(p, XWORD{0})        &&
			xcc_write_word(p, XWORD{XIS::EQ})
		) {
			return true;
		}
		break;
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

static bool try_rval(xcc_parser_state ps);

static bool try_redir_val(xcc_parser_state ps)
{
	if (
		manage_state(
			match     (ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			try_rval  (new_state(ps.end))                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_index_src(xcc_parser_state ps)
{
	if (
		manage_state(
			try_put_var_addr(new_state(ps.end)) ||
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

static bool try_put_index_addr(xcc_parser_state ps)
{
	if (
		manage_state(
			try_index_src (new_state(ps.end))                              &&
			xcc_write_word(ps.p, XWORD{XIS::AT})                           &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			try_expr      (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)      &&
			xcc_write_word(ps.p, XWORD{XIS::ADD})
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

static bool try_bwnot_val(xcc_parser_state ps)
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

static bool try_lnot_val(xcc_parser_state ps)
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
			try_redir_val(new_state(ps.end)) ||
			try_bwnot_val(new_state(ps.end)) ||
			try_lnot_val (new_state(ps.end)) ||
			try_call_fn  (new_state(ps.end)) ||
			try_put_lit  (new_state(ps.end)) ||
			try_put_index(new_state(ps.end)) ||
			try_put_var  (new_state(ps.end)) ||
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
			try_uni_addr(new_state(ps.end)) ||
			try_uni_pos (new_state(ps.end)) ||
			try_uni_neg (new_state(ps.end)) ||
			try_rval    (new_state(ps.end))
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
				try_equality(new_state(ps.end)) &&
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
				try_and(new_state(ps.end)) &&
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
				try_xor(new_state(ps.end)) &&
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
				try_or(new_state(ps.end)) &&
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
	++(*count);
	if (
		manage_state(
			try_expr(new_state(ps.end))
		)
	) {
		return match(ps.p, xbtoken::OPERATOR_COMMA) ? try_expr_list(new_state(ps.end), count) : true;
	}
	return false;
}

static bool try_new_var_list(xcc_parser_state ps);

static bool try_str_lit(xcc_parser_state ps, U16 *count)
{
	token t;
	while (peek(ps.p).user_type != ps.end) {
		if (!match1(ps.p, token::CHAR, &t)) {
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
			set_error(ps.p, xcc_error::VERIFY);
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
			match               (ps.p, token::ALIAS, &t)                                    &&
			(sym = xcc_add_var(t.text, ps.p)) != NULL                                       &&
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)                 &&
			try_lit_expr        (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R), sym->size) &&
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
			try_opt_arr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym)               &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ? try_new_var_list(new_state(ps.end)) : true
			)
		)
	) {
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
			xcc_add_var         (t.text, ps.p) != NULL                   &&
			try_opt_var_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ? try_new_var_list(new_state(ps.end)) : true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_var_list(xcc_parser_state ps)
{
	token t;
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

static bool try_const_def_expr(xcc_parser_state ps, U16 &result)
{
	result = 0;
	token p = peek(ps.p);
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
			xcc_add_lit       (t.text, result, ps.p) != NULL                   &&
			(
				match(ps.p, xbtoken::OPERATOR_COMMA) ? try_new_const_list(new_state(ps.end)) : true
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
		param->param = xcc_add_param(t.text, ps.p);
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
			try_fn_param(new_state(ps.end), param) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_fn_params(new_state(ps.end), param->param)
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

static bool try_opt_fn_params(xcc_parser_state ps, bool verify_params)
{
	if (ps.p->fn == NULL) {
		set_error(ps.p, xcc_error::INTERNAL);
		return false;
	}
	const xcc_symbol fn = *ps.p->fn;
	if (
		manage_state(
			peek(ps.p).user_type == ps.end ||
			try_fn_params(new_state(ps.end), ps.p->fn)
		)
	) {
		ps.p->fn->param_count = 0;
		const xcc_symbol *p = ps.p->fn->param;
		while (p != NULL) {
			++ps.p->fn->param_count;
			p = p->param;
		}
		if (verify_params && ps.p->fn->param_count != fn.param_count) {
			set_error(ps.p, xcc_error::VERIFY);
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
			xcc_write_word(ps.p, XWORD{XIS::JMP})               &&
			xcc_push_scope(ps.p->scopes)                        &&
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
	// TODO: Using CSKIP for conditionals makes binaries more resilient once I start using IP as an offset from the A pointer.

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
			xcc_write_word(ps.p, XWORD{XIS::CNJMP})                            &&
			xcc_push_scope(ps.p->scopes)                                       &&
			try_statement (new_state(ps.end))                                  &&
			emit_pop_scope(ps.p)
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return manage_state(
			(
				try_else(new_state(ps.end)) &&
				(ps.p->out.buffer[jmp_addr_idx].u += 3) // NOTE: A successful try_else emits an unconditional jump (PUT ADDR JMP) that we want to skip over to get into the ELSE body.
			) ||
			true
		);
	}
	return false;
}

static bool try_while(xcc_parser_state ps)
{
	// TODO: Using CSKIP for conditionals makes binaries more resilient once I start using IP as an offset from the A pointer.

	U16 jmp_addr_idx   = 0;
	U16 return_jmp_idx = ps.p->out.size;
	if (
		manage_state(
			match         (ps.p, xbtoken::KEYWORD_CONTROL_WHILE)               &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			try_expr      (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                              &&
			(jmp_addr_idx = ps.p->out.size)                                    &&
			xcc_write_word(ps.p, XWORD{0})                                     &&
			xcc_write_word(ps.p, XWORD{XIS::CNJMP})                            &&
			xcc_push_scope(ps.p->scopes)                                       &&
			try_statement (new_state(ps.end))                                  &&
			emit_pop_scope(ps.p)
		)
	) {
		return manage_state(
			xcc_write_word    (ps.p, XWORD{XIS::PUT})          &&
			xcc_write_word    (ps.p, XWORD{return_jmp_idx})    &&
			xcc_write_word    (ps.p, XWORD{XIS::JMP})          &&
			(ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size)
		);
	}
	return false;
}

static bool try_statements(xcc_parser_state ps);

static bool try_scope(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)      &&
			xcc_push_scope(ps.p->scopes)                                 &&
			try_statements(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		return emit_pop_scope(ps.p);
	}
	return false;
}

static bool try_expr_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			try_expr  (new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			match     (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
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
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
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

static bool try_reass_var_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			try_lexpr (new_state(xbtoken::OPERATOR_ASSIGNMENT_SET)) &&
			match     (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)      &&
			try_expr  (new_state(xbtoken::OPERATOR_SEMICOLON))      &&
			match     (ps.p, xbtoken::OPERATOR_SEMICOLON)           &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})
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
			(peek(ps.p).user_type == ps.end) ||
			try_expr(new_state(ps.end))
		)
	) {
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
			try_expr      (new_state(xbtoken::OPERATOR_SEMICOLON)) && // TODO: Should be try_opt_expr, but requires to mirror default return pattern at end of function.
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

static bool try_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			match             (ps.p, xbtoken::OPERATOR_SEMICOLON) ||
			try_new_vars      (new_state(ps.end))                 ||
			try_new_consts    (new_state(ps.end))                 ||
			try_if            (new_state(ps.end))                 ||
			try_while         (new_state(ps.end))                 ||
			try_return_stmt   (new_state(ps.end))                 ||
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
					(ps.p->fn = xcc_add_fn(t.text, ps.p)) != NULL &&
					emit_empty_symbol_storage(ps.p, ps.p->fn)
				)
			)                                                                                    &&
			xcc_write_rel    (ps.p, ps.p->fn)                                                    &&
			xcc_write_word   (ps.p, XWORD{XIS::PUTI})                                            &&
			xcc_write_word   (ps.p, XWORD{XIS::PUT})                                             &&
			xcc_write_word   (ps.p, XWORD{9})                                                    && // NOTE: 9 is the offset to get to SVC (the first instruction of the function body).
			xcc_write_word   (ps.p, XWORD{XIS::ADD})                                             &&
			xcc_write_word   (ps.p, XWORD{XIS::RLA})                                             &&
			xcc_write_word   (ps.p, XWORD{XIS::MOVD})                                            &&
			xcc_write_word   (ps.p, XWORD{XIS::PUT})                                             &&
			(guard_jmp_idx = ps.p->out.size)                                                     &&
			xcc_write_word   (ps.p, XWORD{0})                                                    &&
			xcc_write_word   (ps.p, XWORD{XIS::JMP})                                             &&
			xcc_write_word   (ps.p, XWORD{XIS::SVC})                                             &&
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                     &&
			xcc_push_scope   (ps.p->scopes)                                                      &&
			try_opt_fn_params(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), verify_params) &&
			adjust_fn_rel_ptr(new_state(ps.end))                                                 &&
			// TODO push anot scope here (don't forget to pop)
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                     &&
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                           &&
			try_statements   (new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R))                      &&
			match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)                           &&
			emit_pop_scope   (ps.p)                                                              &&
			xcc_write_word   (ps.p, XWORD{XIS::LDC})                                             && // NOTE: We do not explicitly set a return value since the call site sets it to 0 by default.
			xcc_write_word   (ps.p, XWORD{XIS::JMP})                                                // NOTE: Jump back to call site
		)
	) {
		ps.p->out.buffer[guard_jmp_idx].u = ps.p->out.size;
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
			peek(ps.p).user_type == ps.end ||
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
			(ps.p->fn = xcc_add_fn(t.text, ps.p)) != NULL                                 &&
			emit_empty_symbol_storage(ps.p, ps.p->fn)                                     &&
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
			try_fn_def    (new_state(ps.end))                 ||
			try_fn_decl   (new_state(ps.end))                 ||
			try_new_vars  (new_state(ps.end))                 ||
			try_new_consts(new_state(ps.end))               //||
			//try_new_svars (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool add_main(xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_fn(to_chars("main",4), p);
	if (sym == NULL) {
		return false;
	}
	sym->param_count = 2; // argc, argv
	return emit_empty_symbol_storage(p, sym);
}

static bool try_global_statements(xcc_parser_state ps)
{
	if (manage_state( until_end(new_state(ps.end), try_global_statement))) {
		return true;
	}
	return false;
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
	p.in.l = init_lexer(chars::view{ "main(0,0);", 10 });
	xcc_parser_state ps = xcc_new_state(&p, token::STOP_EOF);
	if (
		manage_state(
			try_statement(new_state(ps.end))
		)
	) {
		op->out = p.out;
		return true;
	}
	return false;
}

static bool try_program(xcc_parser_state ps)
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

xcc_out xb(lexer l, xcc_binary mem, const U16 sym_capacity)
{
	xcc_symbol       sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	xcc_parser       p  = xcc_init_parser(l, mem, sym_mem, sym_capacity);
	xcc_parser_state ps = xcc_new_state(&p, token::STOP_EOF);

	if (
		manage_state(
			try_program(new_state(ps.end))
		)
	) {
		return xcc_out{ p.in.l, p.out, p.max, 0, xcc_error{ p.max, xcc_error::NONE, 0 } };
	}
	return xcc_out{ p.in.l, p.out, p.max, 1, p.error };
}


#undef new_state
#undef manage_state
#undef set_error
