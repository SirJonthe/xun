#include <fstream>
#include <cstddef>
#include "xcc.h"

xcc_text::xcc_text( void ) : txt(NULL), len(0), sum(cc0::sum::md5())
{}

xcc_text::xcc_text(const xcc_text &txt) : xcc_text()
{
	xcc_new_text(*this, txt.len);
	for (uint32_t i = 0; i < len; ++i) {
		this->txt[i] = txt.txt[i];
	}
	this->txt[len] = 0;
}

xcc_text &xcc_text::operator=(const xcc_text &txt)
{
	if (this != &txt) {
		xcc_new_text(*this, txt.len);
		for (uint32_t i = 0; i < len; ++i) {
			this->txt[i] = txt.txt[i];
		}
		this->txt[len] = 0;
	}
	return *this;
}

xcc_text::~xcc_text( void )
{
	xcc_clear_text(*this);
}

void xcc_new_text(xcc_text &txt, uint32_t len)
{
	xcc_clear_text(txt);
	txt.txt = new char[len + 1];
	txt.len = len;
	txt.txt[len] = 0;
}

void xcc_clear_text(xcc_text &txt)
{
	delete [] txt.txt;
	txt.txt = NULL;
	txt.len = 0;
	txt.sum = cc0::sum::md5();
}

bool xcc_load_text(const chars::view &filename, xcc_text &out)
{
	xcc_clear_text(out);
	std::ifstream fin(std::string(filename.str, filename.len));
	if (fin.is_open()) {
		fin.seekg(0, std::ios::end);
		xcc_new_text(out, fin.tellg());
		fin.seekg(0);
		fin.read(out.txt, out.len);
		out.sum = cc0::sum::md5(out.txt, out.len);
	} else {
		return false;
	}
	fin.close();
	return true;
}

bool xcc_strcmp(const char *a, unsigned a_count, const char *b, unsigned b_count)
{
	if (a_count != b_count) { return false; }
	for (unsigned i = 0; i < a_count; ++i) {
		if (a[i] != b[i]) {
			return false;
		}
	}
	return true;
}

unsigned xcc_chcount(const char *s)
{
	unsigned n = 0;
	while (s[n] != 0) { ++n; }
	return n;
}

bool xcc_write_word(xcc_binary &buf, XWORD data)
{
	if (buf.size >= buf.capacity) {
		return false;
	}
	buf.buffer[buf.size++] = data;
	return true;
}

U16 xcc_top_scope_stack_size(const xcc_symbol_stack &s)
{
	U16 size = 0;
	for (signed i = s.count - 1; i >= 0 && s.symbols[i].scope_index == s.scope; --i) {
		size += s.symbols[i].size;
	}
	return size;
}

U16 xcc_loop_stack_size(const xcc_symbol_stack &s, U16 scope_index)
{
	U16 size = 0;
	for (signed i = s.count - 1; i >= 0 && s.symbols[i].scope_index > scope_index; --i) {
		size += s.symbols[i].size;
	}
	return size;
}

bool xcc_push_scope(xcc_symbol_stack &ss)
{
	++ss.scope;
	return true;
}

bool xcc_pop_scope(xcc_symbol_stack &ss, token *undef)
{
	bool retval = true;
	if (ss.scope >= 1) {
		while (ss.count > 0 && ss.symbols[ss.count - 1].scope_index == ss.scope) {
			--ss.count;
			if (ss.symbols[ss.count].link > 0) {
				if (undef != NULL) {
					*undef = ss.symbols[ss.count].tok;
				}
				retval = false;
			}
		}
	} else {
		ss.count = 0;
	}
	--ss.scope;
	return retval;
}

chars empty_chars( void )
{
	chars c;
	for (uint32_t i = 0; i < sizeof(c.str); ++i) {
		c.str[i] = 0;
	}
	return c;
}

xcc_parser xcc_init_parser(lexer l, xcc_binary bin_mem, xcc_symbol *sym_mem, U16 sym_capacity)
{
	xcc_parser p = {
		l,
		bin_mem,
		l.last,
		xcc_symbol_stack{ sym_mem, sym_capacity, 0, 0 },
		NULL,
		xcc_error{ new_eof(), xcc_error::NONE, empty_chars(), 0 },
		empty_chars(),
		cc0::sum::md5(l.code.str, l.code.len)
	};
	return p;
}

void xcc_set_error(xcc_parser *p, U16 code, const chars &file, unsigned line)
{
	if (p->error.code == xcc_error::NONE) {
		p->error = xcc_error{ p->in.last, code, file, line };
	}
}

bool xcc_push_scope(xcc_parser *p)
{
	return xcc_push_scope(p->scopes);
}

bool xcc_pop_scope(xcc_parser *p)
{
	if (!xcc_pop_scope(p->scopes, &p->in.last)) {
		xcc_set_error(p, xcc_error::UNDEF, p->file, __LINE__);
		return false;
	}
	return true;
}

bool xcc_write_word(xcc_parser *p, XWORD data)
{
	if (!xcc_write_word(p->out, data)) {
		xcc_set_error(p, xcc_error::MEMORY, p->file, __LINE__);
		return false;
	}
	return true;
}

bool xcc_write_rel(xcc_parser *p, const xcc_symbol *sym, U16 offset)
{
	if (sym->storage == xcc_symbol::STORAGE_LIT) {
		return
			xcc_write_word(p, XWORD{XIS::PUT})                  &&
			xcc_write_word(p, XWORD{U16(sym->data.u + offset)});
	}
	return
		xcc_write_word(p, XWORD{XIS::PUT})                  &&
		xcc_write_word(p, XWORD{U16(sym->data.u + offset)}) &&
		sym->storage == xcc_symbol::STORAGE_STATIC ? xcc_write_word(p, XWORD{XIS::RLA}) : (
			sym->scope_index > 0 ?
				xcc_write_word(p, XWORD{XIS::RLC}) :
				xcc_write_word(p, XWORD{XIS::RLB})
		);
}

xcc_symbol *xcc_find_symbol(const chars &name, xcc_parser *p)
{
	const unsigned name_char_count = xcc_chcount(name.str);
	for (signed i = p->scopes.count - 1; i >= 0; --i) {
		if (xcc_strcmp(name.str, name_char_count, p->scopes.symbols[i].tok.text.str, xcc_chcount(p->scopes.symbols[i].tok.text.str))) {
			return p->scopes.symbols + i;
		}
	}
	return NULL;
}

xcc_symbol *xcc_find_var(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_AUTO && sym->storage != xcc_symbol::STORAGE_STATIC && sym->storage != xcc_symbol::STORAGE_PARAM) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_lit(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_LIT) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_fn(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_FN) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_lbl(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_LBL && sym->scope_index != p->scopes.scope) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_add_symbol(const token &tok, unsigned storage, xcc_parser *p, U16 value)
{
	if (p->scopes.count >= p->scopes.capacity) {
		xcc_set_error(p, xcc_error::MEMORY, p->file, __LINE__);
		return NULL;
	}
	const unsigned name_char_count = xcc_chcount(tok.text.str);
	if (name_char_count > 0) {
		for (signed i = p->scopes.count - 1; i >= 0 && p->scopes.symbols[i].scope_index == p->scopes.scope; --i) {
			if (xcc_strcmp(p->scopes.symbols[i].tok.text.str, xcc_chcount(p->scopes.symbols[i].tok.text.str), tok.text.str, name_char_count)) {
				xcc_set_error(p, xcc_error::REDEF, p->file, __LINE__);
				return NULL;
			}
		}
	}

	xcc_symbol &sym = p->scopes.symbols[p->scopes.count];
	sym.tok         = tok;
	sym.storage     = storage;
	sym.type        = xcc_symbol::TYPE_UNSIGNED;
	sym.scope_index = p->scopes.scope;
	sym.param_count = 0;
	sym.size        = 1;
	sym.param       = NULL;
	sym.data.u      = value;
	sym.link        = 0;
	switch (storage) {
	case xcc_symbol::STORAGE_PARAM:
	case xcc_symbol::STORAGE_LIT:
	case xcc_symbol::STORAGE_STATIC:
		sym.size = 0;
		break;
	case xcc_symbol::STORAGE_AUTO:
	case xcc_symbol::STORAGE_FN:
		sym.size = 1;
		break;
	}
	++p->scopes.count;
	return &sym;
}

bool xcc_add_memory(xcc_parser *p, U16 size)
{
	token empty = new_token("", 0, token::NONE, token::NONE);
	xcc_symbol *sym = xcc_add_symbol(empty, xcc_symbol::STORAGE_AUTO, p, xcc_top_scope_stack_size(p->scopes) + 1);
	sym->size = size;
	return sym != NULL;
}

xcc_symbol *xcc_add_var(const token &tok, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_AUTO, p, xcc_top_scope_stack_size(p->scopes) + 1);
}

xcc_symbol *xcc_add_svar(const token &tok, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_STATIC, p, p->out.size + 1);
}

xcc_symbol *xcc_add_param(const token &tok, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_PARAM, p, 0);
}

xcc_symbol *xcc_add_lit(const token &tok, U16 value, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_LIT, p, value);
}

xcc_symbol *xcc_add_fn(const token &tok, xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_symbol(tok, xcc_symbol::STORAGE_FN, p, xcc_top_scope_stack_size(p->scopes) + 1);
	if (sym != NULL) {
		sym->link = p->out.size;
	}
	return sym;
}

xcc_symbol *xcc_add_lbl(const token &tok, xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_symbol(tok, xcc_symbol::STORAGE_LBL, p, xcc_top_scope_stack_size(p->scopes) + 1);
	if (sym != NULL) {
		sym->link = p->out.size;
	}
	return sym;
}

U16 xcc_top_scope_stack_size(const xcc_parser *p)
{
	return xcc_top_scope_stack_size(p->scopes);
}

U16 xcc_loop_stack_size(const xcc_parser *p, U16 loop_scope)
{
	return xcc_loop_stack_size(p->scopes, loop_scope);
}

token xcc_peek(xcc_parser *p, token (*lexfn)(lexer*))
{
	lexer l = p->in;
	return lexfn(&l);
}

bool xcc_match(xcc_parser *p, unsigned type, token *out, token (*lexfn)(lexer*))
{
	// Read from lexer. This is done in a separate copy to avoid committing to reads of unexpected types.
	lexer l = p->in;
	lexfn(&l);

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
		p->in = l;
		++p->in.index;
		return true;
	}

	// No match.
	return false;
}

xcc_parser_state xcc_new_state(const xcc_parser_state &ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope)
{
	xcc_parser_state nps = { ps.p, *ps.p, &ps, ps.p->filesum, ps.cwd, ps.swd, end, break_ip, continue_ip, loop_scope };
	return nps;
}

xcc_parser_state xcc_new_state(xcc_parser *p, const xcc_parser_state *ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope)
{
	xcc_parser_state nps = { p, *p, ps, p->filesum, ps != NULL ? ps->cwd : chars::view{NULL,0,0}, ps != NULL ? ps->swd : chars::view{NULL,0,0}, end, break_ip, continue_ip, loop_scope };
	return nps;
}

bool xcc_manage_state(xcc_parser_state &ps, bool success)
{
	if (!success) {
		ps.restore_point.error = ps.p->error;
		ps.restore_point.max = ps.p->max.index >= ps.restore_point.max.index ? ps.p->max : ps.restore_point.max;
		*ps.p = ps.restore_point;
	}
	return success;
}
