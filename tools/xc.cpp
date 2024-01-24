#include "xc.h"
#include "../xis.h"
#include "../xarch.h"
#include "../lib/parsec/lex.h"
#include "../lib/MiniLib/MTL/mtlList.h"

struct input_tokens
{
	lexer        l;
	const token *tokens;
	U16          capacity;
	U16          index;
};

static bool write_word(xbinary::buffer &buf, XWORD data)
{
	if (buf.index == 0xffff) { return false; }
	buf.buffer[buf.index++] = data;
	return true;
}

struct symbol
{
	enum type_t { VAR, LIT, LBL, FN };
	char      name[32];
	XWORD     data;
	unsigned  type;
	U16       size;
	U16       scope_index;
	U16       dim;         // The dimension if this is an array (if multi-dimensional, the next dimension will be stored in the 'next' pointer).
	U16       deref;       // The number of times it should be dereferenced to access an underlying value. For literals 0, variables 1, single pointers 2, multi-pointers >2.
	symbol   *next;        // The next dimension in a multi-dimensional array. The next name will be empty so that there are no accidental naming collisions.
};

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
		if (strcmp(name, name_char_count, ss.symbols[i].name, chcount(ss.symbols[i].name))) {
			return ss.symbols + i;
		}
	}
	return NULL;
}

static symbol *find_var(const char *name, unsigned name_char_count, symbol_stack &ss)
{
	symbol *sym = find_symbol(name, name_char_count, ss);
	if (sym != NULL && sym->type != symbol::VAR) {
		return NULL;
	}
	return sym;
}

static symbol *find_fn(const char *name, unsigned name_char_count, symbol_stack &ss)
{
	symbol *sym = find_symbol(name, name_char_count, ss);
	if (sym != NULL && sym->type != symbol::FN) {
		return NULL;
	}
	return sym;
}

static symbol *add_symbol(const char *name, unsigned name_char_count, unsigned type, symbol_stack &s)
{
	if (s.count >= s.capacity) {
		return NULL;
	}

	for (U16 i = s.top_index; i < s.count; ++i) {
		if (strcmp(s.symbols[i].name, chcount(s.symbols[i].name), name, name_char_count)) {
			return NULL;
		}
	}

	symbol &sym = s.symbols[s.count];

	const unsigned count = name_char_count < sizeof(sym.name) - 1 ? name_char_count : sizeof(sym.name) - 1;
	unsigned i;
	for (i = 0; i < count; ++i)       { sym.name[i] = name[i]; }
	for (; i < sizeof(sym.name); ++i) { sym.name[i] = 0; }
	sym.type        = type;
	sym.size        = 1;
	sym.scope_index = s.scope;
	sym.dim         = 0;
	sym.deref       = 0;
	sym.next        = NULL;
	if (type == symbol::VAR) {
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

struct parser
{
	input_tokens in;
	xbinary      out;
	symbol_stack scopes;
	U16          max_token_index;
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
	token t;
	U16 lex_index = 0;
	if (p->in.tokens != NULL) {
		t = p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
	} else {
		lexer l = p->in.l;
		t = clex(&l);
		lex_index = l.head;
	}
	if (out != NULL) {
		*out = t;
	}

	if (t.type == token::STOP) {
		return type == token::STOP_EOF;
	}
	if (type == t.user_type) {
		p->in.l.head = lex_index;
		++p->in.index;
		if (p->in.index > p->max_token_index) {
			p->max_token_index = p->in.index;
		}
		return true;
	}
	return false;
}

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
		U16 max_index = ps.p->max_token_index;
		*ps.p = ps.restore_point;
		if (max_index > ps.p->max_token_index) {
			ps.p->max_token_index = max_index;
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

static bool try_expr(parser_state ps);

static bool try_factor(parser_state ps)
{
	if (
		manage_state(
			ps,
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

static bool try_fn_param(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, ctoken::KEYWORD_TYPE_UNSIGNED) &&
			match(ps.p, token::ALIAS, &t)
		)
	) {
		symbol *sym = add_var(t.text.str, chcount(t.text.str), ps.p->scopes); // TODO: Add a 'add_param' version that does not modify LSP
		if (sym == NULL) { return false; }
		return write_word(ps.p->out.body, XWORD{XIS::PUT}) && write_word(ps.p->out.body, XWORD{U16(1)}) && write_word(ps.p->out.body, XWORD{XIS::PUSH}); // TODO: This should really not affect LSP, but since we do we emit 1 (ideally we emit the actual word size of the symbol, but currently only size=1 is supported)
	}
	return false;
}

static bool try_fn_params(parser_state ps)
{
	if (
		manage_state(
			ps,
			peek(ps.p).user_type == ps.end ||
			(
				try_fn_param(new_state(ps.p, ps.end)) &&
				(
					peek(ps.p).user_type == ps.end ||
					(
						match(ps.p, ctoken::OPERATOR_COMMA) &&
						try_fn_params(new_state(ps.p, ps.end))
					)
				)
			)
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
			match        (ps.p, ctoken::KEYWORD_TYPE_VOID)              &&
			match        (ps.p,  token::ALIAS, &t)                      &&
			strcmp(t.text.str, chcount(t.text.str), "main", 4)          &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L) &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		symbol *sym = find_fn(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) { return false; }
		sym->data.u = ps.p->out.body.index;
		return true;
	}
	return false;
}

static bool try_fn_signature(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match        (ps.p, ctoken::KEYWORD_TYPE_VOID)                         &&
			match        (ps.p,  token::ALIAS, &t)                                 &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_fn_params(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		symbol *sym = add_fn(t.text.str, chcount(t.text.str), ps.p->out.body.index, 0, ps.p->scopes); // TODO: The size of the function should correspond to the size out the return value.
		if (sym == NULL) { return false; }
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

static bool try_statement(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_new_var(new_state(ps.p, ps.end)) ||
			try_if     (new_state(ps.p, ps.end)) ||
			try_scope  (new_state(ps.p, ps.end))
			// TODO: add expression here
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
			write_word        (ps.p->out.body, XWORD{XIS::SVB})                   &&
			push_scope        (ps.p->scopes)                                      &&
			try_main_signature(new_state(ps.p, ps.end))                           &&
			match             (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)            &&
			try_statements    (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match             (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)            &&
			emit_pop_scope    (ps.p)                                              &&
			write_word        (ps.p->out.body, XWORD{XIS::LDA}) // NOTE: This returns from the entire program.
		)
	) {
		return true;
	}
	return false;
}

static bool try_fn_def(parser_state ps)
{
	U16 jmp_idx = 0;
	token t;
	if (
		manage_state(
			ps,
			write_word      (ps.p->out.body, XWORD{XIS::PUT})                   &&
			(jmp_idx = ps.p->out.body.index)                                    &&
			write_word      (ps.p->out.body, XWORD{0})                          &&
			write_word      (ps.p->out.body, XWORD{XIS::JMP})                   &&
			push_scope      (ps.p->scopes)                                      &&
			try_fn_signature(new_state(ps.p, ps.end))                           &&
			match           (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)            &&
			try_statements  (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match           (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)            &&
			emit_pop_scope  (ps.p)                                              &&
			write_word      (ps.p->out.body, XWORD{XIS::JMP}) // TODO: This assumes that the return address is the top stack value here.
		)
	) {
		ps.p->out.body.buffer[jmp_idx].u = ps.p->out.body.index;
		return true;
	}
	return false;
}

static bool try_global_statement(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_new_var(new_state(ps.p, ps.end))  ||
			try_main_def(new_state(ps.p, ps.end)) ||
			try_fn_def (new_state(ps.p, ps.end))
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
			lsp == 0 ||
			(
				// TODO: This should be an LDA and HALT
				write_word(ps.p->out.body, XWORD{XIS::PUT}) &&
				write_word(ps.p->out.body, XWORD{lsp})      &&
				write_word(ps.p->out.body, XWORD{XIS::POP})
			);
	}
	return false;
}

xc_out xcc(lexer l, xbinary mem, const U16 sym_capacity)
{
	symbol sym_mem[sym_capacity];
	parser p          = { { l, NULL, 0, 0 }, mem };
	p.max_token_index = 0;
	p.scopes          = symbol_stack{ sym_mem, sym_capacity, 0, 0, 0 };
	parser_state ps   = new_state(&p, token::STOP_EOF);

	add_fn("main", 4, 0, 0, p.scopes);

	if (!manage_state(ps, try_program(new_state(ps.p, ps.end)))) {
		return { p.in.l, p.out, 0, p.max_token_index, 1 };
	}
	return { p.in.l, p.out, U16(p.out.head.index + p.out.body.index + p.out.tail.index), p.max_token_index, 0 };
}
