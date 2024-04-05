#include <cstddef>
#include "xcc.h"

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

bool xcc_pop_scope(xcc_symbol_stack &ss)
{
	if (ss.scope >= 1) {
		while (ss.count > 0 && ss.symbols[ss.count - 1].scope_index == ss.scope) {
			--ss.count;
		}
	} else {
		ss.count = 0;
	}
	--ss.scope;
	return true;
}

xcc_parser xcc_init_parser(lexer l, xcc_binary bin_mem, xcc_symbol *sym_mem, U16 sym_capacity)
{
	xcc_parser p = {
		xcc_input_tokens{ l, NULL, 0, 0 },
		bin_mem,
		l.last,
		xcc_symbol_stack{ sym_mem, sym_capacity, 0, 0 },
		NULL,
		xcc_error{ new_eof(), xcc_error::NONE, 0 }
	};
	return p;
}

void xcc_set_error(xcc_parser *p, U16 code, unsigned line)
{
	if (p->error.code == xcc_error::NONE) {
		p->error = xcc_error{ p->in.l.last, code, line };
	}
}

bool xcc_write_word(xcc_parser *p, XWORD data)
{
	if (!xcc_write_word(p->out, data)) {
		xcc_set_error(p, xcc_error::MEMORY, __LINE__);
		return false;
	}
	return true;
}

bool xcc_write_rel(xcc_parser *p, const xcc_symbol *sym, U16 offset)
{
	if (sym->category == xcc_symbol::LIT) {
		return
			xcc_write_word(p, XWORD{XIS::PUT})                  &&
			xcc_write_word(p, XWORD{U16(sym->data.u + offset)});
	}
	return
		xcc_write_word(p, XWORD{XIS::PUT})                  &&
		xcc_write_word(p, XWORD{U16(sym->data.u + offset)}) &&
		sym->category == xcc_symbol::SVAR ? xcc_write_word(p, XWORD{XIS::RLA}) : (
			sym->scope_index > 0 ?
				xcc_write_word(p, XWORD{XIS::RLC}) :
				xcc_write_word(p, XWORD{XIS::RLB})
		);
}

xcc_symbol *xcc_find_symbol(const chars &name, xcc_parser *p)
{
	const unsigned name_char_count = xcc_chcount(name.str);
	for (signed i = p->scopes.count - 1; i >= 0; --i) {
		if (xcc_strcmp(name.str, name_char_count, p->scopes.symbols[i].name.str, xcc_chcount(p->scopes.symbols[i].name.str))) {
			return p->scopes.symbols + i;
		}
	}
	return NULL;
}

xcc_symbol *xcc_find_var(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->category != xcc_symbol::VAR && sym->category != xcc_symbol::SVAR && sym->category != xcc_symbol::PARAM) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_lit(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->category != xcc_symbol::LIT) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_fn(const chars &name, xcc_parser *p)
{
	xcc_symbol *sym = xcc_find_symbol(name, p);
	if (sym != NULL && sym->category != xcc_symbol::FN) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_add_symbol(const chars &name, unsigned category, xcc_parser *p, U16 value, const token *reserved, signed num_tokens)
{
	if (p->scopes.count >= p->scopes.capacity) {
		xcc_set_error(p, xcc_error::MEMORY, __LINE__);
		return NULL;
	}
	const unsigned name_char_count = xcc_chcount(name.str);
	if (name_char_count > 0) {
		for (signed i = p->scopes.count - 1; i >= 0 && p->scopes.symbols[i].scope_index == p->scopes.scope; --i) {
			if (xcc_strcmp(p->scopes.symbols[i].name.str, xcc_chcount(p->scopes.symbols[i].name.str), name.str, name_char_count)) {
				xcc_set_error(p, xcc_error::REDEF, __LINE__);
				return NULL;
			}
		}
		if (reserved != NULL) {
			for (signed i = 0; i < num_tokens; ++i) {
				if (reserved[i].type == token::KEYWORD && xcc_strcmp(reserved[i].text.str, xcc_chcount(reserved[i].text.str), name.str, name_char_count)) {
					// This is a non-fatal error, since we may match against another pattern.
					return NULL;
				}
			}
		}
	}

	xcc_symbol &sym = p->scopes.symbols[p->scopes.count];
	sym.name        = name;
	sym.category    = category;
	sym.scope_index = p->scopes.scope;
	sym.param_count = 0;
	sym.size        = 1;
	sym.param       = NULL;
	sym.data.u      = value;
	switch (category) {
	case xcc_symbol::PARAM:
	case xcc_symbol::LIT:
	case xcc_symbol::SVAR:
		sym.size = 0;
		break;
	case xcc_symbol::VAR:
	case xcc_symbol::FN:
		sym.size = 1;
		break;
	}
	++p->scopes.count;
	return &sym;
}

bool xcc_add_memory(xcc_parser *p, U16 size)
{
	chars empty_name;
	for (unsigned i = 0; i < sizeof(empty_name.str); ++i) {
		empty_name.str[i] = 0;
	}
	xcc_symbol *sym = xcc_add_symbol(empty_name, xcc_symbol::VAR, p, xcc_top_scope_stack_size(p->scopes) + 1);
	sym->size = size;
	return sym != NULL;
}

xcc_symbol *xcc_add_var(const chars &name, xcc_parser *p, const token *reserved, signed num_tokens)
{
	return xcc_add_symbol(name, xcc_symbol::VAR, p, xcc_top_scope_stack_size(p->scopes) + 1, reserved, num_tokens);
}

xcc_symbol *xcc_add_svar(const chars &name, xcc_parser *p, const token *reserved, signed num_tokens)
{
	return xcc_add_symbol(name, xcc_symbol::SVAR, p, p->out.size + 1, reserved, num_tokens);
}

xcc_symbol *xcc_add_param(const chars &name, xcc_parser *p, const token *reserved, signed num_tokens)
{
	return xcc_add_symbol(name, xcc_symbol::PARAM, p, 0, reserved, num_tokens);
}

xcc_symbol *xcc_add_lit(const chars &name, U16 value, xcc_parser *p, const token *reserved, signed num_tokens)
{
	return xcc_add_symbol(name, xcc_symbol::LIT, p, value, reserved, num_tokens);
}

xcc_symbol *xcc_add_fn(const chars &name, xcc_parser *p, const token *reserved, signed num_tokens)
{
	return xcc_add_symbol(name, xcc_symbol::FN, p, xcc_top_scope_stack_size(p->scopes) + 1, reserved, num_tokens);
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
	token t;
	if (p->in.tokens != NULL) {
		t = p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
	} else {
		lexer l = p->in.l;
		t = lexfn(&l);
	}
	return t;
}

bool xcc_match(xcc_parser *p, unsigned type, token *out, token (*lexfn)(lexer*))
{
	lexer l = p->in.l;

	// Read from token stream if it is available.
	if (p->in.tokens != NULL) {
		l.last = p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
		l.last.index = p->in.index;
	}
	// Otherwise read from lexer. This is done in a separate copy to avoid committing to reads of unexpected types.
	else {
		lexfn(&l);
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

xcc_parser_state xcc_new_state(xcc_parser *p, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope)
{
	xcc_parser_state ps = { p, *p, end, break_ip, continue_ip, loop_scope };
	return ps;
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
