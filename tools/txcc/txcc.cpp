#include <cstdlib>
#include "txcc.h"
#include "../../xis.h"
#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

// A much simplifed version of C with no typing (everything is an unsigned integer).
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
// term ::= factor | factor (*|/) factor
// factor ::= *val | &val | "(" expr ")"
// val ::= *val | name "(" exprs ")" | val "[" expr "]" | name | num

#include <iostream>
#include <cstdlib>
#include "txcc.h"
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

const signed XC_TOKEN_COUNT = 23;
const token XC_TOKENS[XC_TOKEN_COUNT] = {
	new_keyword ("return",                  6, ctoken::KEYWORD_CONTROL_RETURN),
	new_keyword ("if",                      2, ctoken::KEYWORD_CONTROL_IF),
	new_keyword ("else",                    4, ctoken::KEYWORD_CONTROL_ELSE),
	new_keyword ("while",                   5, ctoken::KEYWORD_CONTROL_WHILE),
	new_keyword ("asm",                     3, ctoken::KEYWORD_INTRINSIC_ASM),
	new_operator("(",                       1, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator("{",                       1, ctoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, ctoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("[",                       1, ctoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, ctoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("+",                       1, ctoken::OPERATOR_ARITHMETIC_ADD),
	new_operator("-",                       1, ctoken::OPERATOR_ARITHMETIC_SUB),
	new_operator("*",                       1, ctoken::OPERATOR_ARITHMETIC_MUL),
	new_operator("/",                       1, ctoken::OPERATOR_ARITHMETIC_DIV),
	new_operator("%",                       1, ctoken::OPERATOR_ARITHMETIC_MOD),
	new_operator("=",                       1, ctoken::OPERATOR_ASSIGNMENT_SET),
	new_operator(",",                       1, ctoken::OPERATOR_COMMA),
	new_operator(";",                       1, ctoken::OPERATOR_SEMICOLON),
	new_operator(":",                       1, ctoken::OPERATOR_COLON),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, ctoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, ctoken::LITERAL_INT, hex2u)
};

token xclex(lexer *l)
{
	return lex(l, XC_TOKENS, XC_TOKEN_COUNT);
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

static bool write_word(xbinary &buf, XWORD data)
{
	if (buf.size >= buf.capacity) {
		// TODO FATAL ERROR
		// OUT OF MEMORY
		std::cout << "out of memory" << std::endl;
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
	xbinary       out;    // The parser output.
	token         max;    // The maximally reached token.
	symbol_stack  scopes; // The symbols ordered into scopes.
	symbol       *fn;     // The current function being parsed.
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
		std::cout << "out of memory" << std::endl;
		return NULL;
	}
	const unsigned name_char_count = chcount(name.str);
	for (U16 i = p->scopes.top_index; i < p->scopes.count; ++i) {
		if (strcmp(p->scopes.symbols[i].name.str, chcount(p->scopes.symbols[i].name.str), name.str, name_char_count)) {
			// TODO FATAL ERROR
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
			sym.data.u = top_scope_stack_size(p->scopes);
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
		t = xclex(&l);
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
		xclex(&l);
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
	if (match(ps.p, ctoken::LITERAL_INT, &t)) {
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

static bool try_put_var(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, token::ALIAS, &t)
		)
	) {
		symbol *sym = find_var(t.text, ps.p);
		if (sym == NULL) { return false; }
		return
			write_rel (ps.p, sym)                 &&
			write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_expr(parser_state ps);

static bool try_put_fn_params(parser_state ps)
{
	if (
		manage_state(
			ps,
			peek(ps.p).user_type == ps.end ||
			(
				try_expr(new_state(ps.p, ps.end)) &&
				(
					peek(ps.p).user_type == ps.end ||
					(
						match(ps.p, ctoken::OPERATOR_COMMA) &&
						try_expr(new_state(ps.p, ps.end))
					)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_call_fn(parser_state ps)
{
	// TODO If the symbol is a FN, then verify the number of parameters we input.
	symbol *sym;
	U16 off_index = 0;
	token t;
	if (
		manage_state(
			ps,
			match            (ps.p, token::ALIAS, &t)                                  && // TODO Replace this with any value (can be alias, literal, indirection etc.)
			(sym = find_symbol(t.text, ps.p)) != NULL                                  && // NOTE: find_symbol instead of find_fn. That way we can call anything as if it is a function!
			match            (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			write_word       (ps.p->out, XWORD{XIS::PUT})                              &&
			write_word       (ps.p->out, XWORD{0})                                     && // NOTE: Put return value memory on stack.
			write_word       (ps.p->out, XWORD{XIS::PUTI})                             && // NOTE: Put return address on stack.
			write_word       (ps.p->out, XWORD{XIS::PUT})                              &&
			(off_index = ps.p->out.size)                                               &&
			write_word       (ps.p->out, XWORD{0})                                     && // NOTE: Unable to determine return address offset here, so just emit 0.
			write_word       (ps.p->out, XWORD{XIS::ADD})                              && // NOTE: Adjust return address to move ahead of call site.
			write_word       (ps.p->out, XWORD{XIS::PUTS})                             && // NOTE: Put the address of the return value on the stack.
			write_word       (ps.p->out, XWORD{XIS::PUT})                              &&
			write_word       (ps.p->out, XWORD{2})                                     &&
			write_word       (ps.p->out, XWORD{XIS::SUB})                              && // NOTE: Adjustment of the return value on stack since stack size and return address are top. Now points to return value address.
			try_put_fn_params(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match            (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)            &&
			(ps.p->out.buffer[off_index].u = (ps.p->out.size - off_index) + 7)         && // NOTE: Return address offset can be determined. Adjust the previously emitted 0.
			write_rel        (ps.p, sym)                                               &&
			write_word       (ps.p->out, XWORD{XIS::AT})                               &&
			write_word       (ps.p->out, XWORD{XIS::JMP})
		)
	) {
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
	while (match(ps.p, ctoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		if (!manage_state(ps, try_lit_term(new_state(ps.p, ps.end), l))) {
			return false;
		}
		switch (t.user_type) {
		case ctoken::OPERATOR_ARITHMETIC_ADD:
			result += l;
			break;
		case ctoken::OPERATOR_ARITHMETIC_SUB:
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
	while (match(ps.p, ctoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		if (!manage_state(ps, try_lit_factor(new_state(ps.p, ps.end), l))) {
			return false;
		}
		switch (t.user_type) {
		case ctoken::OPERATOR_ARITHMETIC_MUL:
			result *= l;
			break;
		case ctoken::OPERATOR_ARITHMETIC_DIV:
			result /= l;
			break;
		case ctoken::OPERATOR_ARITHMETIC_MOD:
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
	if (match(ps.p, ctoken::LITERAL_INT, &t)) {
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
				match       (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                    &&
				try_lit_expr(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R), result) &&
				match       (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
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

static bool try_term(parser_state ps);

static bool try_opt_term(parser_state ps)
{
	token t;
	while (match(ps.p, ctoken::OPERATOR_ARITHMETIC_ADD, &t) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		if (!manage_state(ps, try_term(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case ctoken::OPERATOR_ARITHMETIC_ADD:
			if (!write_word(ps.p->out, XWORD{XIS::ADD})) { return false; }
			break;
		case ctoken::OPERATOR_ARITHMETIC_SUB:
			if (!write_word(ps.p->out, XWORD{XIS::SUB})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_factor(parser_state ps);

static bool try_opt_factor(parser_state ps)
{
	token t;
	while (match(ps.p, ctoken::OPERATOR_ARITHMETIC_MUL, &t) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_DIV, &t) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		if (!manage_state(ps, try_factor(new_state(ps.p, ps.end)))) {
			return false;
		}
		switch (t.user_type) {
		case ctoken::OPERATOR_ARITHMETIC_MUL:
			if (!write_word(ps.p->out, XWORD{XIS::MUL})) { return false; }
			break;
		case ctoken::OPERATOR_ARITHMETIC_DIV:
			if (!write_word(ps.p->out, XWORD{XIS::DIV})) { return false; }
			break;
		case ctoken::OPERATOR_ARITHMETIC_MOD:
			if (!write_word(ps.p->out, XWORD{XIS::MOD})) { return false; }
			break;
		default:
			return false;
		}
	}
	return true;
}

static bool try_factor(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_call_fn(new_state(ps.p, ps.end)) ||
			try_put_lit(new_state(ps.p, ps.end)) ||
			try_put_var(new_state(ps.p, ps.end)) ||
			(
				match   (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
				try_expr(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
				match   (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_term(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_factor(new_state(ps.p, ps.end)) && try_opt_factor(new_state(ps.p, ps.end))
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
			try_term    (new_state(ps.p, ps.end)) &&
			try_opt_term(new_state(ps.p, ps.end))
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
		return match(ps.p, ctoken::OPERATOR_COMMA) ? try_expr_list(new_state(ps.p, ps.end)) : true;
	}
	return false;
}

static bool try_decl_expr(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_expr(new_state(ps.p, ctoken::OPERATOR_SEMICOLON)) ||
			(
				match(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)                    &&
				try_expr_list(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)) &&
				match(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)
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
			match        (ps.p, token::ALIAS, &t)                      &&
			(sym = add_var(t.text, ps.p)) != NULL                      &&
			((ps.p->out.size -= 2) || ps.p->out.size == 0)             && // NOTE: A bit hacky. We want to undo instructions that were emitted by add_var.
			match        (ps.p, ctoken::OPERATOR_COLON)                &&
			match        (ps.p, ctoken::OPERATOR_ASSIGNMENT_SET)       &&
			try_decl_expr(new_state(ps.p, ctoken::OPERATOR_SEMICOLON)) &&
			match        (ps.p, ctoken::OPERATOR_SEMICOLON)
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
			peek(ps.p).user_type == ps.end ||
			(
				try_fn_param(new_state(ps.p, ps.end), param) &&
				(
					peek(ps.p).user_type == ps.end ||
					(
						match(ps.p, ctoken::OPERATOR_COMMA) &&
						try_fn_params(new_state(ps.p, ps.end), param->param)
					)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static void adjust_param_addr(symbol *fn)
{
	symbol *p = fn->param;
	while (p != NULL) {
		p->data.u -= fn->param_count;
		p = p->param;
	}
}

static bool try_fn_params(parser_state ps, bool verify_params)
{
	if (ps.p->fn == NULL) {
		// TODO FATAL ERROR
		// ERROR IN COMPILER
		std::cout << "unexpected compiler error" << std::endl;
		return false;
	}
	const symbol fn = *ps.p->fn;
	if (
		manage_state(
			ps,
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
			std::cout << "parameter count mismatch" << std::endl;
			return false;
		}
		adjust_param_addr(ps.p->fn);
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
			match        (ps.p, ctoken::KEYWORD_CONTROL_ELSE) &&
			write_word   (ps.p->out, XWORD{XIS::PUT})         &&
			(jmp_addr_idx = ps.p->out.size)                   &&
			write_word   (ps.p->out, XWORD{0})                &&
			write_word   (ps.p->out, XWORD{XIS::JMP})         &&
			push_scope   (ps.p->scopes)                       &&
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
	// TODO: Using CSKIP for conditionals makes binaries more resilient once I start using IP as an offset from the PIP pointer.

	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			ps,
			match        (ps.p, ctoken::KEYWORD_CONTROL_IF)                        &&
			push_scope   (ps.p->scopes)                                            &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_expr     (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)            &&
			write_word   (ps.p->out, XWORD{XIS::PUT})                              &&
			(jmp_addr_idx = ps.p->out.size)                                        &&
			write_word   (ps.p->out, XWORD{0})                                     &&
			write_word   (ps.p->out, XWORD{XIS::CNJMP})                            &&
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
			match         (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)            &&
			push_scope    (ps.p->scopes)                                      &&
			try_statements(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match         (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)
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
			try_expr  (new_state(ps.p, ctoken::OPERATOR_SEMICOLON)) &&
			match     (ps.p, ctoken::OPERATOR_SEMICOLON)            &&
			write_word(ps.p->out, XWORD{XIS::TOSS})
		)
	) {
		return true;
	}
	return false;
}

static bool try_reass_var_stmt(parser_state ps)
{
	token t;
	symbol *sym;
	if (
		manage_state(
			ps,
			match     (ps.p, token::ALIAS, &t)                      && // TODO support *val=expr; val[i]=expr; etc.
			((sym = find_var(t.text, ps.p)) != NULL)                &&
			write_rel (ps.p, sym)                                   &&
			match     (ps.p, ctoken::OPERATOR_ASSIGNMENT_SET)       &&
			try_expr  (new_state(ps.p, ctoken::OPERATOR_SEMICOLON)) &&
			match     (ps.p, ctoken::OPERATOR_SEMICOLON)            &&
			write_word(ps.p->out, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_return_stmt(parser_state ps)
{
	// BUG: Fix this...
	if (
		manage_state(
			ps,
			match               (ps.p, ctoken::KEYWORD_CONTROL_RETURN)        &&
			match               (ps.p, ctoken::OPERATOR_SEMICOLON)            &&
			write_word          (ps.p->out, XWORD{XIS::LDC})                  &&
			try_expr            (new_state(ps.p, ctoken::OPERATOR_SEMICOLON)) && // BUG: Return value needs to be computed after LDC, but calling LDC messes up local variable addressing...
			write_word          (ps.p->out, XWORD{XIS::MOVD})                 && // NOTE: Jump back to call site
			write_word          (ps.p->out, XWORD{XIS::JMP})
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
			try_expr_stmt     (new_state(ps.p, ps.end))
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
			match               (ps.p,  token::ALIAS, &t)                                                &&
			(
				(verify_params = ((ps.p->fn = find_fn(t.text, ps.p)) != NULL)) ||
				(ps.p->fn = add_fn(t.text, ps.p)) != NULL
			)                                                                                            &&
			write_rel           (ps.p, ps.p->fn)                                                         &&
			write_word          (ps.p->out, XWORD{XIS::PUTI})                                            &&
			write_word          (ps.p->out, XWORD{XIS::PUT})                                             &&
			write_word          (ps.p->out, XWORD{9})                                                    && // NOTE: 9 is the offset to get to SVB (the first instruction of the function body).
			write_word          (ps.p->out, XWORD{XIS::ADD})                                             &&
			write_word          (ps.p->out, XWORD{XIS::RLA})                                             &&
			write_word          (ps.p->out, XWORD{XIS::MOVD})                                            &&
			write_word          (ps.p->out, XWORD{XIS::PUT})                                             &&
			(guard_jmp_idx = ps.p->out.size)                                                             &&
			write_word          (ps.p->out, XWORD{0})                                                    &&
			write_word          (ps.p->out, XWORD{XIS::JMP})                                             &&
			write_word          (ps.p->out, XWORD{XIS::SVC})                                             &&
			match               (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                           &&
			push_scope          (ps.p->scopes)                                                           &&
			try_fn_params       (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R), verify_params) &&
			adjust_fn_rel_ptr   (new_state(ps.p, ps.end))                                                &&
			match               (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                           &&
			match               (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)                                 &&
			try_statements      (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R))                      &&
			match               (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)                                 &&
			emit_pop_scope      (ps.p)                                                                   &&
			write_word          (ps.p->out, XWORD{XIS::LDC})                                             &&
			write_word          (ps.p->out, XWORD{XIS::PUT})                                             && // NOTE: Set return value to 0 if there is no explicit return.
			write_word          (ps.p->out, XWORD{0})                                                    &&
			write_word          (ps.p->out, XWORD{XIS::MOVD})                                            &&
			write_word          (ps.p->out, XWORD{XIS::JMP})                                                // NOTE: Jump back to call site
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
			peek(ps.p).user_type == ps.end ||
			(
				try_count_fn_param(new_state(ps.p, ps.end)) &&
				(
					peek(ps.p).user_type == ps.end ||
					(
						match(ps.p, ctoken::OPERATOR_COMMA) &&
						try_count_fn_params(new_state(ps.p, ps.end))
					)
				)
			)
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
			match              (ps.p, token::ALIAS, &t)                                  &&
			(ps.p->fn = add_fn(t.text, ps.p)) != NULL                                    &&
			match              (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_count_fn_params(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match              (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)            &&
			match              (ps.p, ctoken::OPERATOR_SEMICOLON)
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
			try_new_var(new_state(ps.p, ps.end)) ||
			try_fn_def (new_state(ps.p, ps.end)) ||
			try_fn_decl(new_state(ps.p, ps.end))
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
			add_main(ps.p)                                 &&
			try_global_statements(new_state(ps.p, ps.end)) &&
			emit_call_main(ps.p)
		)
	) {
		// NOTE: pop_scope not possible since we are at index 0 here.
		const U16 lsp = top_scope_stack_size(ps.p);
		return
			(
				lsp == 0 ||
				(
					write_word(ps.p->out, XWORD{XIS::PUT}) &&
					write_word(ps.p->out, XWORD{lsp})      &&
					write_word(ps.p->out, XWORD{XIS::POP})
				)
			);
	}
	return false;
}

parser init_parser(lexer l, xbinary bin_mem, symbol *sym_mem, U16 sym_capacity)
{
	parser p = { { l, NULL, 0, 0 }, bin_mem, l.last };
	p.scopes = symbol_stack{ sym_mem, sym_capacity, 0, 0, 0 };
	p.fn     = NULL;
	return p;
}

xc_out txcc(lexer l, xbinary mem, const U16 sym_capacity)
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
		return xc_out{ p.in.l, p.out, p.max, 0 };
	}
	return xc_out{ p.in.l, p.out, p.max, 1 };
}

