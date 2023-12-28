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

bool write_word(xbinary::buffer &buf, XWORD data)
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

scope::symbol *find_symbol(const char *name, unsigned name_char_count, scope_stack &ss, U16 &back_sp_offset)
{
	back_sp_offset = 0;
	for (signed i = ss.index; i >= 0; --i) {
		for (mtlItem<scope::symbol> *n = ss.scopes[ss.index].symbols.GetFirst(); n != NULL; n = n->GetNext()) {
			if (strcmp(name, name_char_count, n->GetItem().name, chcount(n->GetItem().name))) {
				return &n->GetItem();
			}
		}
		back_sp_offset += ss.scopes[ss.index - 1].lsp;
	}
	return NULL;
}

scope::symbol *find_lbl(const char *name, unsigned name_char_count, scope &ss)
{
	for (mtlItem<scope::symbol> *n = ss.symbols.GetFirst(); n != NULL; n = n->GetNext()) {
		if (n->GetItem().type == scope::symbol::LBL && strcmp(name, name_char_count, n->GetItem().name, chcount(n->GetItem().name))) {
			return &n->GetItem();
		}
	}
	return NULL;
}

scope::symbol *find_fn(const char *name, unsigned name_char_count, scope_stack &ss)
{
	U16 temp;
	scope::symbol *sym = find_symbol(name, name_char_count, ss, temp);
	if (sym != NULL && sym->type != scope::symbol::FN) {
		return NULL;
	}
	return sym;
}

scope::symbol *add_symbol(const char *name, unsigned name_char_count, unsigned type, scope &s)
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

scope::symbol *add_symbol(const char *name, unsigned name_char_count, unsigned lit, scope_stack &ss)
{
	return add_symbol(name, name_char_count, lit, ss.scopes[ss.index]);
}

scope::symbol *add_var(const char *name, unsigned name_char_count, scope_stack &ss)
{
	return add_symbol(name, name_char_count, scope::symbol::VAR, ss);
}

scope::symbol *add_lit(const char *name, unsigned name_char_count, U16 value, scope_stack &ss)
{
	scope::symbol *sym = add_symbol(name, name_char_count, scope::symbol::LIT, ss);
	if (sym != NULL) {
		sym->data.u = value;
	}
	return sym;
}

scope::symbol *add_lbl(const char *name, unsigned name_char_count, U16 addr, scope_stack &ss)
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

scope::symbol *add_fn(const char *name, unsigned name_char_count, U16 addr, U16 size, scope_stack &ss)
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

static token peek(parser *p)
{
	token t;
	if (p->in.tokens != NULL) {
		t = p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
	} else {
		lexer l = p->in.l;
		t = xlex(&l);
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
		t = xlex(&l);
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

xc_output xcompile_c(lexer l, xbinary mem)
{
	xc_output out = { l, mem, 0, 0 };
	return out;
}
