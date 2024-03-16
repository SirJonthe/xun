#include <cstdlib>
#include "xb.h"
#include "../../xis.h"
#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

// A much simplifed version of C with no typing (everything is an unsigned integer) - essentially the B programming language.
//	function(a, b)
//	{
//		return a + b;
//	}
//	
//	main() {
//		a := 1;
//		a = function(1, 1);
//		b := { 1, 1 };
//		if (1) {
//	
//		} else {
//	
//		}
//		while (1) {
//		}
//		asm {
//			nop.
//		}
//		return 1;
//	}

// TODO
// [ ] Push a second scope after parameter scope
// [ ] Loops
// [ ] Comparisons
// [ ] Declare empty arrays
// [ ] Bit operations
// [ ] Arrays are pointers to first element
// [ ] Indexing
// [ ] Inline assembly
// [ ] LHS expression

// dec ::= 0-9
// hex ::= 0x0-9a-fA-F
// num ::= dec | hex
// name ::= a-zA-Z0-9_

// fn ::= name "(" pms ")" "{" stmts "}"
// pms ::= E | pm,pms | pm
// pm ::= name
// glob_stmts ::= E | glob_stmt glob_stmts
// stmt ::= name ":=" expr ";" | name ":=" "{" exprs "}" ";" | fn
// stmts ::= E | stmt stmts
// stmt ::= name ":=" expr ";" | name ":=" "{" exprs "}" ";" | name "=" expr ";" | "{" stmts "}" | "if" "(" expr ")" stmt | "while" "(" expr ")" stmt | "return" expr ";" | "asm" asm_stmt | expr ";"
// asm_stmt ::= asm | "{" asm_stmts "}"
// asm_stmts ::= E | asm_stmt asm_stmts
// exprs ::= E | expr,exprs | expr
// expr ::= term | term (+|-) term
// term ::= factor | factor ("*" | "/") factor
// factor ::= "*" val | "&" val | "(" expr ")"
// val ::= "*" val | name "(" exprs ")" | val "[" expr "]" | name | num

#include <cstdlib>
#include "xb.h"
#include "../../xis.h"
#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

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

/// @brief Token data specific to the XB programming language.
struct xbtoken
{
	/// @brief Token types specific to the XB programming language.
	enum tokentype
	{
		KEYWORD_TYPE = token::KEYWORD | (1<<8),
			KEYWORD_TYPE_AUTO,
			KEYWORD_TYPE_CONST,
		KEYWORD_CONTROL = token::KEYWORD | (2<<8),
			KEYWORD_CONTROL_RETURN,
			KEYWORD_CONTROL_IF,
			KEYWORD_CONTROL_ELSE,
			KEYWORD_CONTROL_WHILE,
		KEYWORD_INTRINSIC = token::KEYWORD | (3<<8),
			KEYWORD_INTRINSIC_ASM,
		OPERATOR_ARITHMETIC = token::OPERATOR | (1<<8),
			OPERATOR_ARITHMETIC_ADD,
			OPERATOR_ARITHMETIC_SUB,
			OPERATOR_ARITHMETIC_MUL,
			OPERATOR_ARITHMETIC_DIV,
			OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ASSIGNMENT = token::OPERATOR | (2<<8),
			OPERATOR_ASSIGNMENT_SET,
		OPERATOR_ENCLOSE = token::OPERATOR | (3<<8),
			OPERATOR_ENCLOSE_PARENTHESIS = OPERATOR_ENCLOSE | (1<<4),
				OPERATOR_ENCLOSE_PARENTHESIS_L,
				OPERATOR_ENCLOSE_PARENTHESIS_R,
			OPERATOR_ENCLOSE_BRACKET = OPERATOR_ENCLOSE | (2<<4),
				OPERATOR_ENCLOSE_BRACKET_L,
				OPERATOR_ENCLOSE_BRACKET_R,
			OPERATOR_ENCLOSE_BRACE = OPERATOR_ENCLOSE | (3<<4),
				OPERATOR_ENCLOSE_BRACE_L,
				OPERATOR_ENCLOSE_BRACE_R,
		OPERATOR_LOGICAL = token::OPERATOR | (4<<8),
			OPERATOR_LOGICAL_LESS,
			OPERATOR_LOGICAL_LESSEQUAL,
			OPERATOR_LOGICAL_GREATER,
			OPERATOR_LOGICAL_GREATEREQUAL,
			OPERATOR_LOGICAL_EQUAL,
			OPERATOR_LOGICAL_NOTEQUAL,
			OPERATOR_LOGICAL_AND,
			OPERATOR_LOGICAL_OR,
		OPERATOR_BITWISE = token::OPERATOR | (5<<8),
			OPERATOR_BITWISE_AND,
			OPERATOR_BITWISE_OR,
			OPERATOR_BITWISE_XOR,
			OPERATOR_BITWISE_LSHIFT,
			OPERATOR_BITWISE_RSHIFT,
		OPERATOR_SEMICOLON = token::OPERATOR | 1,
		OPERATOR_COLON,
		OPERATOR_COMMA,

		LITERAL_INT = token::LITERAL | (1<<8),
		//LITERAL_FLOAT = token::LITERAL | (2<<8),
	};
};

const signed XB_TOKEN_COUNT = 37;
const token XB_TOKENS[XB_TOKEN_COUNT] = {
	new_keyword ("return",                  6, xbtoken::KEYWORD_CONTROL_RETURN),
	new_keyword ("if",                      2, xbtoken::KEYWORD_CONTROL_IF),
	new_keyword ("else",                    4, xbtoken::KEYWORD_CONTROL_ELSE),
	new_keyword ("while",                   5, xbtoken::KEYWORD_CONTROL_WHILE),
	new_keyword ("asm",                     3, xbtoken::KEYWORD_INTRINSIC_ASM),
	new_keyword ("auto",                    4, xbtoken::KEYWORD_TYPE_AUTO),
	new_operator("<=",                      2, xbtoken::OPERATOR_LOGICAL_LESSEQUAL),
	new_operator(">=",                      2, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL),
	new_operator("==",                      2, xbtoken::OPERATOR_LOGICAL_EQUAL),
	new_operator("!=",                      2, xbtoken::OPERATOR_LOGICAL_NOTEQUAL),
	new_operator("&&",                      2, xbtoken::OPERATOR_LOGICAL_AND),
	new_operator("||",                      2, xbtoken::OPERATOR_LOGICAL_OR),
	new_operator("<<",                      2, xbtoken::OPERATOR_BITWISE_LSHIFT),
	new_operator(">>",                      2, xbtoken::OPERATOR_BITWISE_RSHIFT),
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
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, xbtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xbtoken::LITERAL_INT, hex2u)
};

token xblex(lexer *l)
{
	return lex(l, XB_TOKENS, XB_TOKEN_COUNT);
}

/// @brief The input stream of tokens. If the user provides a pre-lexed array of tokens then the match function will consume from that array, while if the token array is null then the match function will instead lex code as attached to the lexer.
/// @sa match
struct input_tokens
{
	lexer        l;
	const token *tokens;
	U16          capacity;
	U16          index;
};

static bool write_word(xcc_binary &buf, XWORD data)
{
	if (buf.size >= buf.capacity) {
		// TODO FATAL ERROR
		// OUT OF MEMORY
		return false;
	}
	buf.buffer[buf.size++] = data;
	return true;
}

/// @brief The data structure containing meta data about a declared symbol in code, e.g. variables, constants, functions, structures, etc.
struct symbol
{
	enum {
		VAR,   // A modifiable value.
		PARAM, // A modifiable value, that does not modify the stack.
		LIT,   // An immediate constant.
		FN     // An immediate constant for use as a target for jump instructions.
	};
	chars   name;        // The name of the symbol.
	XWORD   data;        // The address of a variable/function, or the value of a literal.
	U16     category;    // VAR, PARAM, LIT, FN
	U16     scope_index; // The scope index this symbol is defined in.
	U16     param_count; // For functions, the number of parameters a function takes.
	symbol *param;       // For functions, this points to the first parameter. For parameters, this points to the next parameter.
};

/// @brief The data structure containing the current state of declared and defined symbols in the compiler.
struct symbol_stack
{
	symbol *symbols;
	U16     capacity;  // The maximum number of symbols that can be on the stack.
	U16     count;     // The number of symbols on the stack.
	U16     top_index; // The index of the top scope.
	U16     scope;     // The number of the topmost scope.
};

static U16 top_scope_stack_size(const symbol_stack &s)
{
	U16 size = 0;
	for (U16 i = s.top_index; i < s.count; ++i) {
		const symbol *sym = &s.symbols[i];
		if (sym->category != symbol::LIT && sym->category != symbol::PARAM) {
			++size;
		}
	}
	return size;
}

static U16 fn_scope_stack_size(const symbol *fn, const symbol_stack &s)
{
	U16 size        = 0;
	U16 scope_index = fn->scope_index + 1;
	U16 i           = 0;
	while (i < s.count && s.symbols[i].scope_index < scope_index) {
		++i;
	}
	while (i < s.count) {
		const symbol *sym = &s.symbols[i];
		if (sym->category != symbol::LIT && sym->category != symbol::PARAM) {
			++size;
		}
		++i;
	}
	return size;

}

static bool push_scope(symbol_stack &ss)
{
	++ss.scope;
	ss.top_index = ss.count;
	return true;
}

static bool pop_scope(symbol_stack &ss)
{
	--ss.scope;
	ss.count = ss.top_index;
	if (ss.scope > 0) {
		while (ss.top_index > 0 && ss.symbols[ss.top_index].scope_index > ss.scope) {
			--ss.top_index;
		}
	} else {
		ss.top_index = 0;
	}
	return true;
}

static bool strcmp(const char *a, unsigned a_count, const char *b, unsigned b_count)
{
	if (a_count != b_count) { return false; }
	for (unsigned i = 0; i < a_count; ++i) {
		if (a[i] != b[i]) {
			return false;
		}
	}
	return true;
}

static unsigned chcount(const char *s)
{
	unsigned n = 0;
	while (s[n] != 0) { ++n; }
	return n;
}

/// @brief The main data structure used for parsing C code.
struct parser
{
	input_tokens  in;     // The parser input.
	xcc_binary    out;    // The parser output.
	token         max;    // The maximally reached token.
	symbol_stack  scopes; // The symbols ordered into scopes.
	symbol       *fn;     // The current function being parsed.
	xcc_error     error;  // The first fatal error.
};

static symbol *find_symbol(const chars &name, parser *p)
{
	const unsigned name_char_count = chcount(name.str);
	for (signed i = p->scopes.count - 1; i >= 0; --i) {
		if (strcmp(name.str, name_char_count, p->scopes.symbols[i].name.str, chcount(p->scopes.symbols[i].name.str))) {
			return p->scopes.symbols + i;
		}
	}
	return NULL;
}

static symbol *find_var(const chars &name, parser *p)
{
	symbol *sym = find_symbol(name, p);
	if (sym != NULL && sym->category != symbol::VAR && sym->category != symbol::PARAM) {
		return NULL;
	}
	return sym;
}

static symbol *find_fn(const chars &name, parser *p)
{
	symbol *sym = find_symbol(name, p);
	if (sym != NULL && sym->category != symbol::FN) {
		return NULL;
	}
	return sym;
}

static symbol *add_symbol(const chars &name, unsigned category, parser *p, U16 value = 0)
{
	if (p->scopes.count >= p->scopes.capacity) {
		// TODO FATAL ERROR
		// OUT OF MEMORY
		return NULL;
	}
	const unsigned name_char_count = chcount(name.str);
	for (U16 i = p->scopes.top_index; i < p->scopes.count; ++i) {
		if (strcmp(p->scopes.symbols[i].name.str, chcount(p->scopes.symbols[i].name.str), name.str, name_char_count)) {
			// TODO NON-FATAL ERROR
			// REDEFINITION
			return NULL;
		}
	}

	symbol &sym = p->scopes.symbols[p->scopes.count];
	sym.name        = name;
	sym.category    = category;
	sym.scope_index = p->scopes.scope;
	sym.param_count = 0;
	sym.param       = NULL;
	if (category != symbol::PARAM) {
		if (category != symbol::LIT) {
			sym.data.u = top_scope_stack_size(p->scopes) + 1;
			if (
				!write_word(p->out, XWORD{XIS::PUT}) ||
				!write_word(p->out, XWORD{value})
			) {
				// TODO FATAL ERROR
				// OUT OF MEMORY
				return NULL;
			}
		} else {
			sym.data.u = value;
		}
	}
	++p->scopes.count;
	return &sym;
}

static symbol *add_var(const chars &name, parser *p)
{
	return add_symbol(name, symbol::VAR, p);
}

static symbol *add_param(const chars &name, parser *p)
{
	return add_symbol(name, symbol::PARAM, p);
}

static symbol *add_lit(const chars &name, U16 value, parser *p)
{
	return add_symbol(name, symbol::LIT, p, value);
}

static symbol *add_fn(const chars &name, parser *p)
{
	return add_symbol(name, symbol::FN, p);
}

static U16 top_scope_stack_size(const parser *p)
{
	return top_scope_stack_size(p->scopes);
}

static U16 fn_scope_stack_size(const parser *p)
{
	return fn_scope_stack_size(p->fn, p->scopes);
}

static bool emit_pop_scope(parser *p)
{
	const U16 lsp = top_scope_stack_size(p);
	return
		(
			lsp == 0 ||
			(
				write_word(p->out, XWORD{XIS::PUT}) &&
				write_word(p->out, XWORD{lsp})      &&
				write_word(p->out, XWORD{XIS::POP})
			)
		) &&
		pop_scope(p->scopes);
}

static token peek(parser *p)
{
	token t;
	if (p->in.tokens != NULL) {
		t = p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
	} else {
		lexer l = p->in.l;
		t = xblex(&l);
	}
	return t;
}

static bool match(parser *p, unsigned type, token *out = NULL)
{
	lexer l = p->in.l;

	// Read from token stream if it is available.
	if (p->in.tokens != NULL) {
		l.last = p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
		l.last.index = p->in.index;
	}
	// Otherwise read from lexer. This is done in a separate copy to avoid committing to reads of unexpected types.
	else {
		xblex(&l);
	}

	// Record the read token if requested.
	if (out != NULL) {
		*out = l.last;
	}

	// Record the read token if it is the furthest along in the token sequence.
	if (l.last.index >= p->max.index) {
		p->max = l.last;
	}

	// If the read token is generic STOP type then we count it as a match if the type we are looking for is STOP_EOF.
	if (l.last.type == token::STOP) {
		return type == token::STOP_EOF;
	}

	// If the token user type is the same as the type we are looking for, we advance the main lexer state before reporting that we got a match.
	if (type == l.last.user_type) {
		p->in.l = l;
		++p->in.index;
		return true;
	}

	// No match.
	return false;
}

/// @brief Manages the parser state so that it can roll back on failure.
/// @note When matching matterns, use manage_state to and new_state to create a new parser_state.
/// @sa manage_state
/// @sa new_state
struct parser_state
{
	parser   *p;
	parser    restore_point;
	unsigned  end;
};

static parser_state new_state(parser *p, unsigned end)
{
	parser_state ps = { p, *p, end };
	return ps;
}

static bool manage_state(parser_state &ps, bool success)
{
	if (!success) {
		token max = ps.p->max;
		*ps.p = ps.restore_point;
		if (max.index >= ps.p->max.index) {
			ps.p->max = max;
		}
	}
	return success;
}

static bool until_end(parser_state ps, bool (*try_fn)(parser_state))
{
	while (peek(ps.p).user_type != ps.end) {
		if (
			!manage_state(
				ps,
				try_fn(new_state(ps.p, ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_put_lit(parser_state ps)
{
	token t;
	if (match(ps.p, xbtoken::LITERAL_INT, &t)) {
		return
			write_word(ps.p->out, XWORD{XIS::PUT})    &&
			write_word(ps.p->out, XWORD{U16(t.hash)});
	}
	return false;
}

static bool write_rel(parser *p, const symbol *sym)
{
	if (sym->category == symbol::LIT) {
		return
			write_word(p->out, XWORD{XIS::PUT})    &&
			write_word(p->out, XWORD{sym->data.u});
	}
	return
		write_word(p->out, XWORD{XIS::PUT})    &&
		write_word(p->out, XWORD{sym->data.u}) &&
		sym->scope_index > 0 ?
			write_word(p->out, XWORD{XIS::RLC}) :
			write_word(p->out, XWORD{XIS::RLB});
}

static bool try_put_var_addr(parser_state ps)
{
	token t;
	symbol *sym;
	if (
		manage_state(
			ps,
			match(ps.p, token::ALIAS, &t)          &&
			(sym = find_var(t.text, ps.p)) != NULL &&
			write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_var(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_put_var_addr(new_state(ps.p, ps.end)) &&
			write_word(ps.p->out, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr(parser_state ps);

static bool try_put_fn_param(parser_state ps, U16 *param_count)
{
	if (
		manage_state(
			ps,
			try_expr(new_state(ps.p, ps.end))
		)
	) {
		if (param_count != NULL) {
			++(*param_count);
		}
		return true;
	}
	return false;
}

static bool try_put_fn_params(parser_state ps, U16 *param_count)
{
	if (
		manage_state(
			ps,
			try_put_fn_param(new_state(ps.p, ps.end), param_count) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_put_fn_params(new_state(ps.p, ps.end), param_count)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_opt_fn_params(parser_state ps, const symbol *sym)
{
	U16 param_count = 0;
	if (
		manage_state(
			ps,
			peek(ps.p).user_type == ps.end ||
			try_put_fn_params(new_state(ps.p, ps.end), &param_count)
		)
	) {
		if (sym != NULL && sym->category == symbol::FN && sym->param_count != param_count) {
			// TODO FATAL ERROR
			// PARAMETER COUNT MISMATCH
			return false;
		}
		return true;
	}
	return false;
}

static bool try_call_fn(parser_state ps)
{
	symbol *sym;
	U16 off_index = 0;
	token t;
	if (
		manage_state(
			ps,
			match                (ps.p, token::ALIAS, &t)                                        && // TODO Replace this with any value (can be alias, literal, indirection etc.)
			(sym = find_symbol(t.text, ps.p)) != NULL                                            && // NOTE: find_symbol instead of find_fn. That way we can call anything as if it is a function!
			match                (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                 &&
			write_word           (ps.p->out, XWORD{XIS::PUT})                                    &&
			write_word           (ps.p->out, XWORD{0})                                           && // NOTE: Put return value memory on stack.
			write_word           (ps.p->out, XWORD{XIS::PUTI})                                   && // NOTE: Put return address on stack.
			write_word           (ps.p->out, XWORD{XIS::PUT})                                    &&
			(off_index = ps.p->out.size)                                                         &&
			write_word           (ps.p->out, XWORD{0})                                           && // NOTE: Unable to determine return address offset here, so just emit 0.
			write_word           (ps.p->out, XWORD{XIS::ADD})                                    && // NOTE: Adjust return address to move ahead of call site.
			try_put_opt_fn_params(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), sym) &&
			match                (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                 &&
			(ps.p->out.buffer[off_index].u = (ps.p->out.size - off_index) + 7)                   && // NOTE: Return address offset can be determined. Adjust the previously emitted 0.
			write_rel            (ps.p, sym)                                                     &&
			write_word           (ps.p->out, XWORD{XIS::AT})                                     &&
			write_word           (ps.p->out, XWORD{XIS::JMP})
		)
	) {
		// TODO Surely we can determine instruction pointer offset without constant 7...
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_term(parser_state ps, type_t &result);

template < typename type_t >
static bool try_lit_opt_term(parser_state ps, type_t &result)
{
	token t;
	type_t l;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		if (!manage_state(ps, try_lit_term(new_state(ps.p, ps.end), l))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_ARITHMETIC_ADD:
			result += l;
			break;
		case xbtoken::OPERATOR_ARITHMETIC_SUB:
			result -= l;
			break;
		default:
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_factor(parser_state ps, type_t &result);

template < typename type_t >
static bool try_lit_opt_factor(parser_state ps, type_t &result)
{
	token t;
	type_t l;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		if (!manage_state(ps, try_lit_factor(new_state(ps.p, ps.end), l))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_ARITHMETIC_MUL:
			result *= l;
			break;
		case xbtoken::OPERATOR_ARITHMETIC_DIV:
			result /= l;
			break;
		case xbtoken::OPERATOR_ARITHMETIC_MOD:
			result %= l;
			break;
		default:
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool try_lit_expr(parser_state ps, type_t &result);

template < typename type_t >
static bool try_read_lit(parser_state ps, type_t &result)
{
	token t;
	if (match(ps.p, xbtoken::LITERAL_INT, &t)) {
		result = t.hash;
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_factor(parser_state ps, type_t &result)
{
	if (
		manage_state(
			ps,
			try_read_lit(new_state(ps.p, ps.end), result) ||
			(
				match       (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                    &&
				try_lit_expr(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), result) &&
				match       (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
			)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_term(parser_state ps, type_t &result)
{
	if (
		manage_state(
			ps,
			try_lit_factor    (new_state(ps.p, ps.end), result) &&
			try_lit_opt_factor(new_state(ps.p, ps.end), result)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool try_lit_expr(parser_state ps, type_t &result)
{
	if (
		manage_state(
			ps,
			try_lit_term    (new_state(ps.p, ps.end), result) &&
			try_lit_opt_term(new_state(ps.p, ps.end), result)
		)
	) {
		return true;
	}
	return false;
}

static bool try_factor(parser_state ps);

static bool try_opt_factor(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		if (!manage_state(ps, try_factor(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_ARITHMETIC_MUL:
			if (!write_word(ps.p->out, XWORD{XIS::MUL})) { return false; }
			break;
		case xbtoken::OPERATOR_ARITHMETIC_DIV:
			if (!write_word(ps.p->out, XWORD{XIS::DIV})) { return false; }
			break;
		case xbtoken::OPERATOR_ARITHMETIC_MOD:
			if (!write_word(ps.p->out, XWORD{XIS::MOD})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_rval(parser_state ps);

static bool try_redir_val(parser_state ps)
{
	if (
		manage_state(
			ps,
			match     (ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			try_rval  (new_state(ps.p, ps.end))                &&
			write_word(ps.p->out, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool try_index_src(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_put_var_addr(new_state(ps.p, ps.end)) ||
			(
				match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
				try_expr(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
				match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_index_addr(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_index_src(new_state(ps.p, ps.end))                              &&
			match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)            &&
			try_expr     (new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)            &&
			write_word   (ps.p->out, XWORD{XIS::ADD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_put_index(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_put_index_addr(new_state(ps.p, ps.end))   &&
			write_word        (ps.p->out, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

// val ::= "*" val | name "(" exprs ")" | val "[" expr "]" | name | num
static bool try_rval(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_redir_val(new_state(ps.p, ps.end)) ||
			try_call_fn  (new_state(ps.p, ps.end)) ||
			try_put_lit  (new_state(ps.p, ps.end)) ||
			try_put_index(new_state(ps.p, ps.end)) ||
			try_put_var  (new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_addr(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xbtoken::OPERATOR_BITWISE_AND) &&
			try_put_var_addr(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_pos(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD) &&
			try_rval(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_uni_neg(parser_state ps)
{
	if (
		manage_state(
			ps,
			match     (ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB) &&
			write_word(ps.p->out, XWORD{XIS::PUT})             &&
			write_word(ps.p->out, XWORD{0})                    &&
			try_rval  (new_state(ps.p, ps.end))                &&
			write_word(ps.p->out, XWORD{XIS::SUB})
		)
	) {
		return true;
	}
	return false;
}

// factor ::= "&" val | "+" val | "-" val | "(" expr ")"
static bool try_factor(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_uni_addr(new_state(ps.p, ps.end)) ||
			try_uni_pos (new_state(ps.p, ps.end)) ||
			try_uni_neg (new_state(ps.p, ps.end)) ||
			try_rval    (new_state(ps.p, ps.end)) ||
			(
				match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
				try_expr(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
				match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_term(parser_state ps);

static bool try_opt_term(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		if (!manage_state(ps, try_term(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_ARITHMETIC_ADD:
			if (!write_word(ps.p->out, XWORD{XIS::ADD})) { return false; }
			break;
		case xbtoken::OPERATOR_ARITHMETIC_SUB:
			if (!write_word(ps.p->out, XWORD{XIS::SUB})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_term(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_factor    (new_state(ps.p, ps.end)) &&
			try_opt_factor(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_bitshift(parser_state ps);

static bool try_opt_bitshift(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_LSHIFT, &t) || match(ps.p, xbtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
		if (!manage_state(ps, try_bitshift(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_BITWISE_LSHIFT:
			if (!write_word(ps.p->out, XWORD{XIS::LSH})) { return false; }
			break;
		case xbtoken::OPERATOR_BITWISE_RSHIFT:
			if (!write_word(ps.p->out, XWORD{XIS::RSH})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_bitshift(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_term    (new_state(ps.p, ps.end)) &&
			try_opt_term(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_less_greater(parser_state ps);

static bool try_opt_less_greater(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_LESS, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATER, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
		if (!manage_state(ps, try_less_greater(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_LOGICAL_LESS:
			if (!write_word(ps.p->out, XWORD{XIS::LT})) { return false; }
			break;
		case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:
			if (!write_word(ps.p->out, XWORD{XIS::LE})) { return false; }
			break;
		case xbtoken::OPERATOR_LOGICAL_GREATER:
			if (!write_word(ps.p->out, XWORD{XIS::GT})) { return false; }
			break;
		case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL:
			if (!write_word(ps.p->out, XWORD{XIS::GE})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_less_greater(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_bitshift    (new_state(ps.p, ps.end)) &&
			try_opt_bitshift(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_equality(parser_state ps);

static bool try_opt_equality(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL, &t) || match(ps.p, xbtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
		if (!manage_state(ps, try_equality(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_LOGICAL_EQUAL:
			if (!write_word(ps.p->out, XWORD{XIS::EQ})) { return false; }
			break;
		case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:
			if (!write_word(ps.p->out, XWORD{XIS::NE})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_equality(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_less_greater    (new_state(ps.p, ps.end)) &&
			try_opt_less_greater(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_and(parser_state ps);

static bool try_opt_and(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_AND, &t)) {
		if (!manage_state(ps, try_and(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_BITWISE_AND:
			if (!write_word(ps.p->out, XWORD{XIS::AND})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_and(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_equality    (new_state(ps.p, ps.end)) &&
			try_opt_equality(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_xor(parser_state ps);

static bool try_opt_xor(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_XOR, &t)) {
		if (!manage_state(ps, try_xor(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_BITWISE_XOR:
			if (!write_word(ps.p->out, XWORD{XIS::XOR})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_xor(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_and    (new_state(ps.p, ps.end)) &&
			try_opt_and(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_or(parser_state ps);

static bool try_opt_or(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_BITWISE_OR, &t)) {
		if (!manage_state(ps, try_or(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_BITWISE_OR:
			if (!write_word(ps.p->out, XWORD{XIS::OR})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_or(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_xor    (new_state(ps.p, ps.end)) &&
			try_opt_xor(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_logical_and(parser_state ps);

static bool try_opt_logical_and(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_AND, &t)) {
		if (!manage_state(ps, try_logical_and(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_LOGICAL_AND:
			// TODO Short-circuit
			if (!write_word(ps.p->out, XWORD{XIS::AND})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_logical_and(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_or    (new_state(ps.p, ps.end)) &&
			try_opt_or(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_logical_or(parser_state ps);

static bool try_opt_logical_or(parser_state ps)
{
	token t;
	while (match(ps.p, xbtoken::OPERATOR_LOGICAL_OR, &t)) {
		if (!manage_state(ps, try_logical_or(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case xbtoken::OPERATOR_LOGICAL_OR:
			// TODO Short-circuit
			if (!write_word(ps.p->out, XWORD{XIS::OR})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_logical_or(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_logical_and    (new_state(ps.p, ps.end)) &&
			try_opt_logical_and(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_logical_or    (new_state(ps.p, ps.end)) &&
			try_opt_logical_or(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_expr_list(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_expr(new_state(ps.p, ps.end))
		)
	) {
		return match(ps.p, xbtoken::OPERATOR_COMMA) ? try_expr_list(new_state(ps.p, ps.end)) : true;
	}
	return false;
}

static bool try_decl_expr(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_expr(new_state(ps.p, xbtoken::OPERATOR_SEMICOLON)) ||
			(
				match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)            &&
				try_expr_list(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
				match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_var(parser_state ps)
{
	token t;
	symbol *sym;
	if (
		manage_state(
			ps,
			match        (ps.p, xbtoken::KEYWORD_TYPE_AUTO)             &&
			match        (ps.p, token::ALIAS, &t)                       &&
			(sym = add_var(t.text, ps.p)) != NULL                       &&
			((ps.p->out.size -= 2) || ps.p->out.size == 0)              && // NOTE: Hacky. We want to undo instructions that were emitted by add_var. Maybe do not emit instructions in add_symbol?
			match        (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)       &&
			try_decl_expr(new_state(ps.p, xbtoken::OPERATOR_SEMICOLON)) &&
			match        (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool try_fn_param(parser_state ps, symbol *param)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, token::ALIAS, &t)
		)
	) {
		param->param = add_param(t.text, ps.p);
		if (param->param == NULL) {
			// TODO FATAL ERROR
			return false;
		}
		return true;
	}
	return false;
}

static bool try_fn_params(parser_state ps, symbol *param)
{
	if (
		manage_state(
			ps,
			try_fn_param(new_state(ps.p, ps.end), param) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_fn_params(new_state(ps.p, ps.end), param->param)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static void set_param_addr(symbol *fn)
{
	symbol *p = fn->param;
	U16 i = 0;
	while (p != NULL) {
		p->data.u = i - fn->param_count - 1;
		p = p->param;
		++i;
	}
}

static bool try_opt_fn_params(parser_state ps, bool verify_params)
{
	if (ps.p->fn == NULL) {
		// TODO FATAL ERROR
		// ERROR IN COMPILER
		return false;
	}
	const symbol fn = *ps.p->fn;
	if (
		manage_state(
			ps,
			peek(ps.p).user_type == ps.end ||
			try_fn_params(new_state(ps.p, ps.end), ps.p->fn)
		)
	) {
		ps.p->fn->param_count = 0;
		const symbol *p = ps.p->fn->param;
		while (p != NULL) {
			++ps.p->fn->param_count;
			p = p->param;
		}
		if (verify_params && ps.p->fn->param_count != fn.param_count) {
			// TODO FATAL ERROR
			// PARAMETER COUNT MISMATCH
			return false;
		}
		set_param_addr(ps.p->fn);
		return true;
	}
	*ps.p->fn = fn;
	return false;
}

static bool try_statements(parser_state ps);
static bool try_statement(parser_state ps);

static bool try_else(parser_state ps)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			ps,
			match        (ps.p, xbtoken::KEYWORD_CONTROL_ELSE) &&
			write_word   (ps.p->out, XWORD{XIS::PUT})          &&
			(jmp_addr_idx = ps.p->out.size)                    &&
			write_word   (ps.p->out, XWORD{0})                 &&
			write_word   (ps.p->out, XWORD{XIS::JMP})          &&
			push_scope   (ps.p->scopes)                        &&
			try_statement(new_state(ps.p, ps.end))
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return emit_pop_scope(ps.p);
	}
	return false;
}

static bool try_if(parser_state ps)
{
	// TODO: Using CSKIP for conditionals makes binaries more resilient once I start using IP as an offset from the A pointer.

	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			ps,
			match        (ps.p, xbtoken::KEYWORD_CONTROL_IF)                        &&
			push_scope   (ps.p->scopes)                                             &&
			match        (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_expr     (new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match        (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)            &&
			write_word   (ps.p->out, XWORD{XIS::PUT})                               &&
			(jmp_addr_idx = ps.p->out.size)                                         &&
			write_word   (ps.p->out, XWORD{0})                                      &&
			write_word   (ps.p->out, XWORD{XIS::CNJMP})                             &&
			try_statement(new_state(ps.p, ps.end))
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return manage_state(
			ps,
			emit_pop_scope(ps.p) &&
			(
				(
					try_else(new_state(ps.p, ps.end)) &&
					(ps.p->out.buffer[jmp_addr_idx].u += 3) // NOTE: A successful try_else emits an unconditional jump (PUT ADDR JMP) that we want to skip over to get into the ELSE body.
				) ||
				true
			)
		);
	}
	return false;
}

static bool try_statements(parser_state ps);

static bool try_scope(parser_state ps)
{
	if (
		manage_state(
			ps,
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)            &&
			push_scope    (ps.p->scopes)                                       &&
			try_statements(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		return emit_pop_scope(ps.p);
	}
	return false;
}

static bool try_expr_stmt(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_expr  (new_state(ps.p, xbtoken::OPERATOR_SEMICOLON)) &&
			match     (ps.p, xbtoken::OPERATOR_SEMICOLON)            &&
			write_word(ps.p->out, XWORD{XIS::TOSS})
		)
	) {
		return true;
	}
	return false;
}

static bool try_redir_lval(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			try_rval(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_lval(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_put_index_addr(new_state(ps.p, ps.end)) ||
			try_put_var_addr  (new_state(ps.p, ps.end)) ||
			try_redir_lval    (new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_lexpr(parser_state ps)
{
	token t;
	symbol *sym;
	if (
		manage_state(
			ps,
			try_lval(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_reass_var_stmt(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_lexpr (new_state(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)) &&
			match     (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)            &&
			try_expr  (new_state(ps.p, xbtoken::OPERATOR_SEMICOLON))      &&
			match     (ps.p, xbtoken::OPERATOR_SEMICOLON)                 &&
			write_word(ps.p->out, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_return_stmt(parser_state ps)
{
	if (
		manage_state(
			ps,
			match     (ps.p, xbtoken::KEYWORD_CONTROL_RETURN)        &&
			write_word(ps.p->out, XWORD{XIS::PUT})                   &&
			write_word(ps.p->out, XWORD{0})                          &&
			write_word(ps.p->out, XWORD{XIS::RLC})                   &&
			write_word(ps.p->out, XWORD{XIS::AT})                    &&
			write_word(ps.p->out, XWORD{XIS::PUT})                   &&
			write_word(ps.p->out, XWORD{1})                          &&
			write_word(ps.p->out, XWORD{XIS::SUB})                   &&
			try_expr  (new_state(ps.p, xbtoken::OPERATOR_SEMICOLON)) &&
			match     (ps.p, xbtoken::OPERATOR_SEMICOLON)            &&
			write_word(ps.p->out, XWORD{XIS::MOVD})                  && // NOTE: Address of return value is top value. Move expression result to external return memory.
			write_word(ps.p->out, XWORD{XIS::LDC})                   &&
			write_word(ps.p->out, XWORD{XIS::JMP})                     // NOTE: Return address is top value. Jump back to call site
		)
	) {
		return true;
	}
	return false;
}

static bool try_asm_instr(parser_state ps)
{
	return false;
}

static bool try_asm_block(parser_state ps)
{
	if (
		manage_state(
			ps,
			match    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)                           &&
			until_end(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R), try_asm_instr) &&
			match    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)
		)
	) {
		return true;
	}
	return false;
}

static bool try_asm_stmt(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xbtoken::KEYWORD_INTRINSIC_ASM) &&
			(
				try_asm_instr(new_state(ps.p, ps.end)) ||
				try_asm_block(new_state(ps.p, ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_statement(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_new_var       (new_state(ps.p, ps.end)) ||
			try_if            (new_state(ps.p, ps.end)) ||
			try_return_stmt   (new_state(ps.p, ps.end)) ||
			try_scope         (new_state(ps.p, ps.end)) ||
			try_reass_var_stmt(new_state(ps.p, ps.end)) ||
			try_expr_stmt     (new_state(ps.p, ps.end)) ||
			try_asm_stmt      (new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_statements(parser_state ps)
{
	if (
		manage_state(
			ps,
			until_end(new_state(ps.p, ps.end), try_statement)
		)
	) {
		return true;
	}
	return false;
}

static bool adjust_fn_rel_ptr(parser_state ps)
{
	const symbol *p = ps.p->fn->param;
	U16 params = 0;
	while (p != NULL) {
		++params;
		p = p->param;
	}
	if (params == 0) {
		return true;
	}
	return
		write_word(ps.p->out, XWORD{XIS::PUT}) &&
		write_word(ps.p->out, XWORD{params})   &&
		write_word(ps.p->out, XWORD{XIS::SUB});
}

static bool try_fn_def(parser_state ps)
{
	U16   guard_jmp_idx = 0;
	token t;
	bool verify_params = false;
	if (
		manage_state(
			ps,
			match               (ps.p,  token::ALIAS, &t)                                                 &&
			(
				(verify_params = ((ps.p->fn = find_fn(t.text, ps.p)) != NULL)) ||
				(ps.p->fn = add_fn(t.text, ps.p)) != NULL
			)                                                                                             &&
			write_rel           (ps.p, ps.p->fn)                                                          &&
			write_word          (ps.p->out, XWORD{XIS::PUTI})                                             &&
			write_word          (ps.p->out, XWORD{XIS::PUT})                                              &&
			write_word          (ps.p->out, XWORD{9})                                                     && // NOTE: 9 is the offset to get to SVC (the first instruction of the function body).
			write_word          (ps.p->out, XWORD{XIS::ADD})                                              &&
			write_word          (ps.p->out, XWORD{XIS::RLA})                                              &&
			write_word          (ps.p->out, XWORD{XIS::MOVD})                                             &&
			write_word          (ps.p->out, XWORD{XIS::PUT})                                              &&
			(guard_jmp_idx = ps.p->out.size)                                                              &&
			write_word          (ps.p->out, XWORD{0})                                                     &&
			write_word          (ps.p->out, XWORD{XIS::JMP})                                              &&
			write_word          (ps.p->out, XWORD{XIS::SVC})                                              &&
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                           &&
			push_scope          (ps.p->scopes)                                                            &&
			try_opt_fn_params   (new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), verify_params) &&
			adjust_fn_rel_ptr   (new_state(ps.p, ps.end))                                                 &&
			// TODO push another scope here (don't forget to pop)
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                           &&
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                                 &&
			try_statements      (new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R))                      &&
			match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)                                 &&
			emit_pop_scope      (ps.p)                                                                    &&
			write_word          (ps.p->out, XWORD{XIS::LDC})                                              && // NOTE: We do not explicitly set a return value since it is always 0 anyway.
			write_word          (ps.p->out, XWORD{XIS::JMP})                                                 // NOTE: Jump back to call site
		)
	) {
		ps.p->out.buffer[guard_jmp_idx].u = ps.p->out.size;
		ps.p->fn = NULL;
		return true;
	}
	return false;
}

static bool try_count_fn_param(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, token::ALIAS, &t)
		)
	) {
		++ps.p->fn->param_count;
		return true;
	}
	return false;
}

static bool try_count_fn_params(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_count_fn_param(new_state(ps.p, ps.end)) &&
			(
				peek(ps.p).user_type == ps.end ||
				(
					match(ps.p, xbtoken::OPERATOR_COMMA) &&
					try_count_fn_params(new_state(ps.p, ps.end))
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_count_opt_fn_params(parser_state ps)
{
	ps.p->fn->param_count = 0;
	if (
		manage_state(
			ps,
			peek(ps.p).user_type == ps.end ||
			try_count_fn_params(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	ps.p->fn->param_count = 0;
	return false;
}

static bool try_fn_decl(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match                  (ps.p, token::ALIAS, &t)                                   &&
			(ps.p->fn = add_fn(t.text, ps.p)) != NULL                                         &&
			match                  (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_count_opt_fn_params(new_state(ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match                  (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)            &&
			match                  (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		ps.p->fn = NULL;
		return true;
	}
	return false;
}

static bool try_global_statement(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_fn_def (new_state(ps.p, ps.end)) ||
			try_fn_decl(new_state(ps.p, ps.end)) ||
			try_new_var(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool add_main(parser *p)
{
	symbol *sym = add_fn(to_chars("main",4), p);
	if (sym == NULL) {
		return false;
	}
	sym->param_count = 2; // argc, argv
	return true;
}

static bool try_global_statements(parser_state ps)
{
	if (manage_state(ps, until_end(new_state(ps.p, ps.end), try_global_statement))) {
		return true;
	}
	return false;
}

static bool emit_call_main(parser *op)
{
	symbol *sym = find_fn(to_chars("main",4), op);
	if (sym == NULL || sym->param == NULL) { // NOTE: param will be null if there is no formal entry point defined.
		return true;
	}

	parser p = *op;
	// p.in.l = init_lexer(chars::view{"*0x00=main(*0x01,*0x02);", 24}); // 0x00 is the return value address, 0x01 is 'argc', and 0x02 is 'argv' (array of pointers).
	p.in.l = init_lexer(chars::view{ "main(0,0);", 10 });
	parser_state ps = new_state(&p, token::STOP_EOF);
	if (
		manage_state(
			ps,
			try_statement(new_state(ps.p, ps.end))
		)
	) {
		op->out = p.out;
		return true;
	}
	return false;
}

static bool try_program(parser_state ps)
{
	if (
		manage_state(
			ps,
			write_word           (ps.p->out, XWORD{XIS::SVB}) &&
			add_main             (ps.p)                       &&
			try_global_statements(new_state(ps.p, ps.end))    &&
			emit_call_main       (ps.p)
		)
	) {
		// NOTE: pop_scope not possible since we are at index 0 here.
		const U16 lsp = top_scope_stack_size(ps.p);
		return
			(
				(
					lsp == 0 ||
					(
						write_word(ps.p->out, XWORD{XIS::PUT}) &&
						write_word(ps.p->out, XWORD{lsp})      &&
						write_word(ps.p->out, XWORD{XIS::POP})
					)
				) &&
				write_word(ps.p->out, XWORD{XIS::LDB}) &&
				write_word(ps.p->out, XWORD{XIS::HALT})
			);
	}
	return false;
}

parser init_parser(lexer l, xcc_binary bin_mem, symbol *sym_mem, U16 sym_capacity)
{
	parser p = {
		input_tokens{ l, NULL, 0, 0 },
		bin_mem,
		l.last,
		symbol_stack{ sym_mem, sym_capacity, 0, 0, 0 },
		NULL,
		xcc_error{ new_eof(), xcc_error::NONE }
	};
	return p;
}

xcc_out xb(lexer l, xcc_binary mem, const U16 sym_capacity)
{
	symbol       sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	parser       p  = init_parser(l, mem, sym_mem, sym_capacity);
	parser_state ps = new_state(&p, token::STOP_EOF);

	if (
		manage_state(
			ps,
			try_program(new_state(ps.p, ps.end))
		)
	) {
		return xcc_out{ p.in.l, p.out, p.max, 0 };
	}
	return xcc_out{ p.in.l, p.out, p.max, 1 };
}

