#include "xc.h"
#include "../xis.h"
#include "../xarch.h"
#include "../lib/parsec/lex.h"
#include "../lib/MiniLib/MTL/mtlList.h"

// TODO: Using CSKIP for conditionals makes binaries more resilient once I start using IP as an offset from the PIP pointer.

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

struct scope
{
	struct symbol
	{
		enum type_t { VAR, LIT, LBL, FN };
		char     name[32];
		XWORD    data;
		unsigned type;
		U16      size;
	};
	struct fwd_label
	{
		char   name[32];
		XWORD *loc;
	};
	mtlList<symbol>    symbols;
	mtlList<fwd_label> fwd_labels; // labels that the user wants to jump to before they have been declared.
	U16                lsp;        // local stack pointer.
};

struct scope_stack
{
	scope  scopes[128];
	signed index;
};

static bool push_scope(scope_stack &ss)
{
	++ss.index;
	ss.scopes[ss.index].symbols.RemoveAll();
	ss.scopes[ss.index].fwd_labels.RemoveAll();
	ss.scopes[ss.index].lsp = 0;
	return true;
}

static bool pop_scope(scope_stack &ss)
{
	ss.scopes[ss.index].symbols.RemoveAll();
	ss.scopes[ss.index].fwd_labels.RemoveAll();
	ss.scopes[ss.index].lsp = 0;
	--ss.index;
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

static scope::symbol *find_symbol(const char *name, unsigned name_char_count, scope_stack &ss, U16 *back_sp_offset = NULL)
{
	if (back_sp_offset != NULL) {
		*back_sp_offset = 0;
	}
	for (signed i = ss.index; i >= 0; --i) {
		for (mtlItem<scope::symbol> *n = ss.scopes[ss.index].symbols.GetFirst(); n != NULL; n = n->GetNext()) {
			if (strcmp(name, name_char_count, n->GetItem().name, chcount(n->GetItem().name))) {
				return &n->GetItem();
			}
		}
		if (back_sp_offset != NULL) {
			*back_sp_offset += ss.scopes[ss.index - 1].lsp;
		}
	}
	return NULL;
}

static scope::symbol *find_var(const char *name, unsigned name_char_count, scope_stack &ss, U16 *back_sp_offset = NULL)
{
	scope::symbol *sym = find_symbol(name, name_char_count, ss);
	if (sym != NULL && sym->type != scope::symbol::VAR) {
		return NULL;
	}
	return sym;
}

static scope::symbol *find_lbl(const char *name, unsigned name_char_count, scope &ss)
{
	for (mtlItem<scope::symbol> *n = ss.symbols.GetFirst(); n != NULL; n = n->GetNext()) {
		if (n->GetItem().type == scope::symbol::LBL && strcmp(name, name_char_count, n->GetItem().name, chcount(n->GetItem().name))) {
			return &n->GetItem();
		}
	}
	return NULL;
}

static scope::symbol *find_fn(const char *name, unsigned name_char_count, scope_stack &ss)
{
	scope::symbol *sym = find_symbol(name, name_char_count, ss);
	if (sym != NULL && sym->type != scope::symbol::FN) {
		return NULL;
	}
	return sym;
}

static scope::symbol *add_symbol(const char *name, unsigned name_char_count, unsigned type, scope &s)
{
	for (mtlItem<scope::symbol> *i = s.symbols.GetFirst(); i != NULL; i = i->GetNext()) {
		if (strcmp(i->GetItem().name, chcount(i->GetItem().name), name, name_char_count)) {
			return NULL;
		}
	}
	scope::symbol &sym = s.symbols.AddLast();
	const unsigned count = name_char_count < sizeof(sym.name) - 1 ? name_char_count : sizeof(sym.name) - 1;
	unsigned i;
	for (i = 0; i < count; ++i)       { sym.name[i] = name[i]; }
	for (; i < sizeof(sym.name); ++i) { sym.name[i] = 0; }
	sym.type = type;
	sym.size = 1;
	if (type == scope::symbol::VAR) {
		sym.data.u = s.lsp;
		++s.lsp;
	}
	return &sym;
}

static scope::symbol *add_symbol(const char *name, unsigned name_char_count, unsigned lit, scope_stack &ss)
{
	return add_symbol(name, name_char_count, lit, ss.scopes[ss.index]);
}

static scope::symbol *add_var(const char *name, unsigned name_char_count, scope_stack &ss)
{
	return add_symbol(name, name_char_count, scope::symbol::VAR, ss);
}

static scope::symbol *add_lit(const char *name, unsigned name_char_count, U16 value, scope_stack &ss)
{
	scope::symbol *sym = add_symbol(name, name_char_count, scope::symbol::LIT, ss);
	if (sym != NULL) {
		sym->data.u = value;
	}
	return sym;
}

static scope::symbol *add_lbl(const char *name, unsigned name_char_count, U16 addr, scope_stack &ss)
{
	scope::symbol *sym = add_symbol(name, name_char_count, scope::symbol::LBL, ss);
	if (sym != NULL) {
		sym->type = scope::symbol::LBL;
		sym->data.u = addr;
		for (mtlItem<scope::fwd_label> *n = ss.scopes[ss.index].fwd_labels.GetFirst(); n != NULL;) {
			if (strcmp(name, name_char_count, n->GetItem().name, chcount(n->GetItem().name))) {
				n->GetItem().loc->u = sym->data.u;
				n = n->Remove();
			} else {
				n = n->GetNext();
			}
		}
	}
	return sym;
}

static scope::symbol *add_fn(const char *name, unsigned name_char_count, U16 addr, U16 size, scope_stack &ss)
{
	scope::symbol *sym = add_symbol(name, name_char_count, scope::symbol::FN, ss);
	if (sym != NULL) {
		sym->data.u = addr;
		sym->size = size;
	}
	return sym;
}

struct parser
{
	input_tokens  in;
	xbinary       out;
	scope_stack   scopes;
	U16           max_token_index;
};

static scope &top_scope(parser *p)
{
	return p->scopes.scopes[p->scopes.index];
}

static bool emit_pop_scope(parser *p)
{
	return
		(
			top_scope(p).lsp == 0 ||
			(
				write_word(p->out.body, XWORD{XIS::PUT}) &&
				write_word(p->out.body, XWORD{U16(top_scope(p).lsp)}) &&
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

static bool try_put_var(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match(ps.p, token::ALIAS, &t)
		)
	) {
		U16 offset;
		scope::symbol *sym = find_var(t.text.str, chcount(t.text.str), ps.p->scopes, &offset);
		if (sym == NULL) { return false; }
		return
			write_word(ps.p->out.body, XWORD{XIS::PUT})    &&
			write_word(ps.p->out.body, XWORD{sym->data.u}) &&
			write_word(ps.p->out.body, XWORD{XIS::CREL})   &&
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

static bool try_ass_var(parser_state ps, scope::symbol *sym)
{
	if (
		manage_state(
			ps,
			match     (ps.p, ctoken::OPERATOR_ASSIGNMENT_SET)       &&
			write_word(ps.p->out.body, XWORD{XIS::PUT})             &&
			write_word(ps.p->out.body, XWORD{sym->data.u})          &&
			write_word(ps.p->out.body, XWORD{XIS::CREL})            &&
			try_expr  (new_state(ps.p, ctoken::OPERATOR_SEMICOLON)) &&
			write_word(ps.p->out.body, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool try_new_arr(parser_state ps, scope::symbol *sym)
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
		return
			write_word(ps.p->out.body, XWORD{XIS::PUT})    &&
			write_word(ps.p->out.body, XWORD{U16(result)}) &&
			write_word(ps.p->out.body, XWORD{XIS::PUSH});
		// TODO: Check if assignment happens...
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
		scope::symbol *sym = add_var(t.text.str, chcount(t.text.str), ps.p->scopes);
		if (sym == NULL) { return false; }
		if (!write_word(ps.p->out.body, XWORD{XIS::PUT}) || !write_word(ps.p->out.body, XWORD{sym->size}) || !write_word(ps.p->out.body, XWORD{XIS::PUSH})) {
			return false;
		}
		return manage_state(
				ps,
				match(ps.p, ctoken::OPERATOR_SEMICOLON) ||
				(
					try_new_arr(new_state(ps.p, ctoken::OPERATOR_SEMICOLON), sym) ||
					try_ass_var(new_state(ps.p, ctoken::OPERATOR_SEMICOLON), sym)
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
		scope::symbol *sym = add_var(t.text.str, chcount(t.text.str), ps.p->scopes); // TODO: Add a 'add_param' version that does not modify LSP
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

static bool try_fn_signature(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			match        (ps.p, ctoken::KEYWORD_TYPE_VOID)                         &&
			match        (ps.p, token::ALIAS, &t)                                  &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L)            &&
			try_fn_params(new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match        (ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		scope::symbol *sym = add_fn(t.text.str, chcount(t.text.str), ps.p->out.body.index, 0, ps.p->scopes); // TODO: The size of the function should correspond to the size out the return value.
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
			// TODO: add if statement here
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

static bool try_fn_def(parser_state ps)
{
	token t;
	if (
		manage_state(
			ps,
			try_fn_signature(new_state(ps.p, ps.end))                           &&
			match           (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_L)            &&
			push_scope      (ps.p->scopes)                                      &&
			try_statements  (new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			match           (ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R)            &&
			pop_scope       (ps.p->scopes)                                      &&
			write_word      (ps.p->out.body, XWORD{XIS::JMP}) // TODO: This assumes that the return address is the top stack value here.
		)
	) {
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
		return
			top_scope(ps.p).lsp == 0 ||
			(
				write_word(ps.p->out.body, XWORD{XIS::PUT})            &&
				write_word(ps.p->out.body, XWORD{top_scope(ps.p).lsp}) &&
				write_word(ps.p->out.body, XWORD{XIS::POP})
			);
	}
	return false;
}

xc_out xcc(lexer l, xbinary mem)
{
	parser p          = { { l, NULL, 0, 0 }, mem };
	p.max_token_index = 0;
	p.scopes.index    = 0;
	parser_state ps   = new_state(&p, token::STOP_EOF);
	if (!manage_state(ps, try_program(new_state(ps.p, ps.end)))) {
		return { p.in.l, p.out, 0, p.max_token_index, 1 };
	}
	return { p.in.l, p.out, U16(p.out.head.index + p.out.body.index + p.out.tail.index), p.max_token_index, 0 };
}
