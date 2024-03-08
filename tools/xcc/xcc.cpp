#include <cstdlib>
#include "xcc.h"
#include "../../xis.h"
#include "../../xarch.h"
#include "../../lib/parsec/lex.h"

/// @brief The input stream of tokens. If the user provides a pre-lexed array of tokens then the match function will consume from that array, while if the token array is null then the match function will instead lex code as attached to the lexer.
/// @sa match
struct input_tokens
{
	lexer        l;
	const token *tokens;
	U16          capacity;
	U16          index;
};

static bool write_word(xbinary::buffer &buf, XWORD data)
{
	if (buf.index >= buf.capacity) {
		// TODO FATAL ERROR
		return false;
	}
	buf.buffer[buf.index++] = data;
	return true;
}

/// @brief The data structure containing meta data about a declared symbol in code, e.g. variables, constants, functions, structures, etc.
struct symbol
{
	enum {
		VAR, // A modifiable value.
		LIT, // An immediate constant.
		LBL, // An immediate constant for use as a target for jump instructions.
		FN,  // An immediate constant for use as a target for jump instructions.
		USR  // A user-defined data type.
	};
	chars   name;        // The name of the symbol.
	XWORD   data;        // The address of a variable/function, or the value of a literal/label.
	U16     category;    // VAR, LIT, LBL, FN, USR
	U16     type_exact;  // A hash representing the exact type, i.e. int, unsigned int, char**, UserType. Computed as the hash of the type name or function signature.
	U16     size;        // The number of bytes the base type occupies.
	U16     eval_size;   // The size of the type that is emitted on the stack when this symbol in invoked. Literals also generate > 0. Size of the return value for functions.
	U16     scope_index; // The scope index this symbol is defined in.
	U16     deref;       // The number of times it should be automatically dereferenced to access an underlying value. For literals 0, variables 1.
	U16     dimensions;  // The number of elements if this is an array.
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

static U16 top_scope_size(const symbol_stack &s)
{
	U16 size = 0;
	for (U16 i = s.top_index; i < s.count; ++i) {
		size += s.symbols[i].size;
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

static symbol *find_symbol(const char *name, unsigned name_char_count, symbol_stack &ss)
{
	for (signed i = ss.count - 1; i >= 0; --i) {
		if (strcmp(name, name_char_count, ss.symbols[i].name.str, chcount(ss.symbols[i].name.str))) {
			return ss.symbols + i;
		}
	}
	return NULL;
}

static symbol *find_var(const char *name, unsigned name_char_count, symbol_stack &ss)
{
	symbol *sym = find_symbol(name, name_char_count, ss);
	if (sym != NULL && sym->category != symbol::VAR) {
		return NULL;
	}
	return sym;
}

static symbol *find_fn(const char *name, unsigned name_char_count, symbol_stack &ss)
{
	symbol *sym = find_symbol(name, name_char_count, ss);
	if (sym != NULL && sym->category != symbol::FN) {
		return NULL;
	}
	return sym;
}

static symbol *add_symbol(const char *name, unsigned name_char_count, unsigned category, symbol_stack &s)
{
	if (s.count >= s.capacity) {
		// TODO FATAL ERROR
		return NULL;
	}

	for (U16 i = s.top_index; i < s.count; ++i) {
		if (strcmp(s.symbols[i].name.str, chcount(s.symbols[i].name.str), name, name_char_count)) {
			return NULL;
		}
	}

	symbol &sym = s.symbols[s.count];

	const unsigned count = name_char_count < sizeof(sym.name) - 1 ? name_char_count : sizeof(sym.name) - 1;
	unsigned i;
	for (i = 0; i < count; ++i)       { sym.name.str[i] = name[i]; }
	for (; i < sizeof(sym.name); ++i) { sym.name.str[i] = 0; }
	sym.category      = category;
	sym.size          = 1; // TODO This needs a better way to be determined
	sym.eval_size     = 1; // TODO This needs a better way to be determined
	sym.scope_index   = s.scope;
	sym.deref         = 0;
	sym.dimensions    = 1;
	sym.type_exact    = 0; // TODO Base this on the type signature of the variable/function.
	if (category == symbol::VAR) {
		sym.data.u = top_scope_size(s);
	}
	++s.count;
	return &sym;
}

static symbol *add_var(const char *name, unsigned name_char_count, symbol_stack &ss)
{
	return add_symbol(name, name_char_count, symbol::VAR, ss);
}

static symbol *add_lit(const char *name, unsigned name_char_count, U16 value, symbol_stack &ss)
{
	symbol *sym = add_symbol(name, name_char_count, symbol::LIT, ss);
	if (sym != NULL) {
		sym->data.u = value;
	}
	return sym;
}

static symbol *add_fn(const char *name, unsigned name_char_count, U16 addr, U16 size, symbol_stack &ss)
{
	symbol *sym = add_symbol(name, name_char_count, symbol::FN, ss);
	if (sym != NULL) {
		sym->data.u = addr;
		sym->size = size;
	}
	return sym;
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

static U16 top_scope_size(const parser *p)
{
	return top_scope_size(p->scopes);
}

static bool emit_pop_scope(parser *p)
{
	const U16 lsp = top_scope_size(p);
	return
		(
			lsp == 0 ||
			(
				write_word(p->out.body, XWORD{XIS::PUT}) &&
				write_word(p->out.body, XWORD{lsp}) &&
				write_word(p->out.body, XWORD{XIS::POP})
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
		t = clex(&l);
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
		clex(&l);
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
	symbol   *fn;
};

static parser_state new_state(parser *p, unsigned end)
{
	parser_state ps = { p, *p, end, p->fn };
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
			write_word(ps.p->out.body, XWORD{XIS::PUT})    &&
			write_word(ps.p->out.body, XWORD{U16(t.hash)});
	}
	return false;
}

static bool write_rel(parser *p, const symbol *sym)
{
	return
		sym->scope_index > 0 ?
		write_word(p->out.body, XWORD{XIS::RLC}) :
		write_word(p->out.body, XWORD{XIS::RLB});
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
		symbol *sym = find_var(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) { return false; }
		return
			write_word(ps.p->out.body, XWORD{XIS::PUT})    &&
			write_word(ps.p->out.body, XWORD{sym->data.u}) &&
			write_rel (ps.p, sym)                          &&
			write_word(ps.p->out.body, XWORD{XIS::AT});
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
	U16 off_index = 0;
	token t;
	if (
		manage_state(
			ps,
			match            (ps.p, token::ALIAS, &t)                                  &&
			match            (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			write_word       (ps.p->out.body, XWORD{XIS::PUTI})                        &&
			write_word       (ps.p->out.body, XWORD{XIS::PUT})                         &&
			(off_index = ps.p->out.body.index)                                         &&
			write_word       (ps.p->out.body, XWORD{off_index})                        &&
			write_word       (ps.p->out.body, XWORD{XIS::ADD})                         &&
			try_put_fn_params(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match            (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		// NOTE:
		// 4 is the number of instructions we emit to jump to the function intruction address.
			// If the number of instructions changes we also need to update the constant.
		// (ps.p->out.body.index - off_index) is the number of instructions emitted as a result of calculating input parameter values.
			// Currently, this should be at least 2 since we are emitting 2 instructions to offset the instruction pointer.
		ps.p->out.body.buffer[off_index].u = (ps.p->out.body.index - off_index) + 5;

		symbol *sym = find_fn(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) {
			// TODO FATAL ERROR
			return false;
		}
		return
			write_word(ps.p->out.body, XWORD{XIS::PUT}) &&
			write_word(ps.p->out.body, sym->data) &&
			write_word(ps.p->out.body, XWORD{XIS::JMP});
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
			if (!write_word(ps.p->out.body, XWORD{XIS::ADD})) { return false; }
			break;
		case ctoken::OPERATOR_ARITHMETIC_SUB:
			if (!write_word(ps.p->out.body, XWORD{XIS::SUB})) { return false; }
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
			if (!write_word(ps.p->out.body, XWORD{XIS::MUL})) { return false; }
			break;
		case ctoken::OPERATOR_ARITHMETIC_DIV:
			if (!write_word(ps.p->out.body, XWORD{XIS::DIV})) { return false; }
			break;
		case ctoken::OPERATOR_ARITHMETIC_MOD:
			if (!write_word(ps.p->out.body, XWORD{XIS::MOD})) { return false; }
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

static bool try_ass_expr(parser_state ps, const symbol *sym)
{
	for (U16 offset = 0; offset < sym->size; ++offset) {
		if (
			!manage_state(
				ps,
				write_word(ps.p->out.body, XWORD{XIS::PUT})                                                          &&
				write_word(ps.p->out.body, XWORD{U16(sym->data.u + offset)})                                         &&
				write_rel (ps.p, sym)                                                                                &&
				try_expr  (new_state(ps.p, (offset < sym->size - 1 ? match(ps.p, ctoken::OPERATOR_COMMA) : ps.end))) &&
				write_word(ps.p->out.body, XWORD{XIS::MOVD})                                                         &&
				(offset == sym->size - 1 || match(ps.p, ctoken::OPERATOR_COMMA))
			)
		) {
			return false;
		}
	}
	return peek(ps.p).user_type == ps.end;
}

static bool try_ass_var(parser_state ps, const symbol *sym)
{
	if (
		manage_state(
			ps,
			sym->category == symbol::VAR                                                                                       &&
			match(ps.p, ctoken::OPERATOR_ASSIGNMENT_SET)                                                                       &&
			(sym->size == 1 || match(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L))                                                  &&
			try_ass_expr(new_state(ps.p, sym->size == 1 ? ctoken::OPERATOR_SEMICOLON : ctoken::OPERATOR_ENCLOSE_BRACE_R), sym) &&
			(sym->size == 1 || match(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R))
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_arr(parser_state ps, symbol *sym)
{
	unsigned result = 0;
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::OPERATOR_ENCLOSE_BRACKET_L)         &&
			try_lit_expr<unsigned>(new_state(ps.p, ps.end), result) &&
			match(ps.p, ctoken::OPERATOR_ENCLOSE_BRACKET_R)
		)
	) {
		sym->size *= result;
		sym->dimensions *= result;
		ps.p->out.body.buffer[ps.p->out.body.index - 2].u = sym->size;
		return manage_state(
			ps,
			peek(ps.p).user_type == ps.end            ||
			try_ass_var(new_state(ps.p, ps.end), sym) //||
//			try_new_arr(new_state(ps.p, ps.end), sym) // NOTE: Disabling this for now since we need to keep track of the individual sizes for each dimension and we currently can not do that in a nice stack-based way.
		);
	}
	return false;
}

static bool try_new_var(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_TYPE_UNSIGNED) &&
			match(ps.p, token::ALIAS, &t)
		)
	) {
		symbol *sym = add_var(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) { return false; }
		if (!write_word(ps.p->out.body, XWORD{XIS::PUT}) || !write_word(ps.p->out.body, XWORD{sym->size}) || !write_word(ps.p->out.body, XWORD{XIS::PUSH})) {
			return false;
		}
		return manage_state(
			ps,
			match(ps.p, ctoken::OPERATOR_SEMICOLON) ||
			(
				try_ass_var(new_state(ps.p, ctoken::OPERATOR_SEMICOLON), sym) ||
				try_new_arr(new_state(ps.p, ctoken::OPERATOR_SEMICOLON), sym)
			) &&
			match(ps.p, ctoken::OPERATOR_SEMICOLON)
		);
	}
	return false;
}

static bool try_fn_param(parser_state ps, symbol *param)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_TYPE_UNSIGNED) &&
			match(ps.p, token::ALIAS, &t)
		)
	) {
		symbol *sym = param->param = add_var(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) { return false; }
		return
			write_word(ps.p->out.body, XWORD{XIS::PUT})  &&
			write_word(ps.p->out.body, XWORD{sym->size}) &&
			write_word(ps.p->out.body, XWORD{XIS::SUB});
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

static bool try_void_param(parser_state ps, symbol *fn)
{
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_TYPE_VOID)
		)
	) {
		fn->param = NULL;
		return true;
	}
	return false;
}

static bool try_first_fn_params(parser_state ps, symbol *fn)
{
	if (
		manage_state(
			ps,
			try_void_param(new_state(ps.p, ps.end), fn) ||
			try_fn_params(new_state(ps.p, ps.end), fn)
		)
	) {
		return true;
	}
	return false;
}

static bool try_main_param_signature(parser_state ps)
{
	if (
		manage_state(
			ps,
			peek(ps.p).user_type == ps.end ||
			match(ps.p, ctoken::KEYWORD_TYPE_VOID)
		)
	) {
		return true;
	}
	return false;
}

static bool try_main_rettype(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_TYPE_VOID)
		)
	) {
		return true;
	}
	return false;
}

static bool try_main_signature(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			try_main_rettype        (new_state(ps.p, ps.end))                                 &&
			match                   (ps.p,  token::ALIAS, &t)                                 &&
			strcmp                  (t.text.str, chcount(t.text.str), "main", 4)              &&
			match                   (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_main_param_signature(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match                   (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		symbol *sym = find_fn(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) {
			// TODO FATAL ERROR
			return false;
		}
		sym->data.u = ps.p->out.body.index;
		return true;
	}
	return false;
}

static bool try_fn_rettype(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_TYPE_VOID)
		)
	) {
		return true;
	}
	return false;
}

static symbol *add_or_find_fn(parser_state &ps, const char *name, unsigned name_char_count)
{
	symbol *sym = find_fn(name, name_char_count, ps.p->scopes);
	if (sym == NULL) {
		sym = add_fn(name, name_char_count, 0, 0, ps.p->scopes);
		if (sym == NULL) {
			// TODO FATAL ERROR
			return NULL;
		}
	}
	ps.p->fn = ps.fn = sym;
	return sym;
}

static bool try_statements(parser_state ps);

static bool try_fn_def_sig_and_body(parser_state ps)
{
	symbol *sym = NULL;
	token t;
	if (
		manage_state(
			ps,
			try_fn_rettype     (new_state(ps.p, ps.end))                                      &&
			match              (ps.p,  token::ALIAS, &t)                                      &&
			(sym = add_or_find_fn(ps, t.text.str, chcount(t.text.str))) != NULL               &&
			(sym->data.u = ps.p->out.body.index - 1)                                          && // NOTE: This points the stored function address to the SVC instruction (i.e. first instruction of any function).
			match              (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                 &&
			push_scope         (ps.p->scopes)                                                 &&
			try_first_fn_params(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R), sym) &&
			match              (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                 &&
			match              (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)                       &&
			try_statements     (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R))            &&
			match              (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)                       &&
			emit_pop_scope     (ps.p)
		)
	) {
		return true;
	}
	return false;
}

static bool try_statement(parser_state ps);

static bool try_else(parser_state ps)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			ps,
			match        (ps.p, ctoken::KEYWORD_CONTROL_ELSE) &&
			write_word   (ps.p->out.body, XWORD{XIS::PUT})    &&
			(jmp_addr_idx = ps.p->out.body.index)             &&
			write_word   (ps.p->out.body, XWORD{0})           &&
			write_word   (ps.p->out.body, XWORD{XIS::JMP})    &&
			push_scope   (ps.p->scopes)                       &&
			try_statement(new_state(ps.p, ps.end))
		)
	) {
		ps.p->out.body.buffer[jmp_addr_idx].u = ps.p->out.head.index + ps.p->out.body.index;
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
			write_word   (ps.p->out.body, XWORD{XIS::PUT})                         &&
			(jmp_addr_idx = ps.p->out.body.index)                                  &&
			write_word   (ps.p->out.body, XWORD{0})                                &&
			write_word   (ps.p->out.body, XWORD{XIS::CNJMP})                       &&
			try_statement(new_state(ps.p, ps.end))
		)
	) {
		ps.p->out.body.buffer[jmp_addr_idx].u = ps.p->out.head.index + ps.p->out.body.index;
		return manage_state(
			ps,
			emit_pop_scope(ps.p) &&
			(
				(
					try_else(new_state(ps.p, ps.end)) &&
					(ps.p->out.body.buffer[jmp_addr_idx].u += 3) // NOTE: A successful try_else emits an unconditional jump (PUT ADDR JMP) that we want to skip over to get into the ELSE body.
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
			// [ ] Remove TOSS
			// [ ] Replace with PUT EXPR_SIZE POP
			// [ ] Fix tests.
			write_word(ps.p->out.body, XWORD{XIS::TOSS})
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
			match(ps.p, token::ALIAS, &t)                                             &&
			((sym = find_var(t.text.str, chcount(t.text.str), ps.p->scopes)) != NULL) &&
			try_ass_var(new_state(ps.p, ctoken::OPERATOR_SEMICOLON), sym)             &&
			match(ps.p, ctoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}
#include <iostream>
static bool try_return_stmt(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_CONTROL_RETURN)           &&
			((ps.fn != NULL && ps.fn->eval_size == 0) || try_expr(new_state(ps.p, ctoken::OPERATOR_SEMICOLON))) &&
			match(ps.p, ctoken::OPERATOR_SEMICOLON)
		)
	) {
		// TODO There is now a return value on top of the stack.
		// TODO We need to assign that value back to the reserved return memory.
		// TODO Should be: C - sizeof(params...) - 2
		// TODO After removing SVC from function body, return value is at C - 2
		if (ps.fn != NULL) {
			std::cout << ps.fn->name.str << "," << ps.fn->eval_size << std::endl;
		} else {
			std::cout << "NO TARGET" << std::endl;
		}
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

static bool try_main_def(parser_state ps)
{
	U16 jmp_idx = 0;
	token t;
	if (
		manage_state(
			ps,
			write_word        (ps.p->out.body, XWORD{XIS::SVC})                   &&
			push_scope        (ps.p->scopes)                                      &&
			try_main_signature(new_state(ps.p, ps.end))                           &&
			match             (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)            &&
			try_statements    (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match             (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)            &&
			emit_pop_scope    (ps.p)                                              &&
			write_word        (ps.p->out.body, XWORD{XIS::LDC})
		)
	) {
		return true;
	}
	return false;
}

static bool try_fn_def(parser_state ps)
{
	U16   guard_jmp_idx = 0;
	token t;
	if (
		manage_state(
			ps,
			write_word          (ps.p->out.body, XWORD{XIS::PUT}) &&
			(guard_jmp_idx = ps.p->out.body.index)                &&
			write_word          (ps.p->out.body, XWORD{0})        &&
			write_word          (ps.p->out.body, XWORD{XIS::JMP}) &&
			write_word          (ps.p->out.body, XWORD{XIS::SVC}) &&
			try_fn_def_sig_and_body(new_state(ps.p, ps.end))      &&
			write_word          (ps.p->out.body, XWORD{XIS::LDC}) &&
			write_word          (ps.p->out.body, XWORD{XIS::JMP})
		)
	) {
		ps.p->out.body.buffer[guard_jmp_idx].u = ps.p->out.body.index;
		return true;
	}
	return false;
}

static bool try_global_statement(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_new_var (new_state(ps.p, ps.end)) ||
			try_main_def(new_state(ps.p, ps.end)) ||
			try_fn_def  (new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_global_statements(parser_state ps)
{
	if (manage_state(ps, until_end(new_state(ps.p, ps.end), try_global_statement))) {
		return true;
	}
	return false;
}

static bool try_program(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_global_statements(new_state(ps.p, ps.end))
		)
	) {
		// NOTE: pop_scope not possible since we are at index 0 here.
		const U16 lsp = top_scope_size(ps.p);
		return
			(
				lsp == 0 ||
				(
					write_word(ps.p->out.body, XWORD{XIS::PUT}) &&
					write_word(ps.p->out.body, XWORD{lsp})      &&
					write_word(ps.p->out.body, XWORD{XIS::POP})
				)
			);
	}
	return false;
}

xc_out xcc(lexer l, xbinary mem, const U16 sym_capacity)
{
	symbol sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	parser p         = { { l, NULL, 0, 0 }, mem, l.last };
	p.scopes         = symbol_stack{ sym_mem, sym_capacity, 0, 0, 0 };
	parser_state ps  = new_state(&p, token::STOP_EOF);

	add_fn("main", 4, 0, 0, p.scopes);

	if (!manage_state(ps, try_program(new_state(ps.p, ps.end)))) {
		return xc_out{ p.in.l, p.out, 0, p.max, 1 };
	}
	return xc_out{ p.in.l, p.out, U16(p.out.head.index + p.out.body.index + p.out.tail.index), p.max, 0 };
}
