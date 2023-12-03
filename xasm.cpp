#include "xasm.h"
#include "lib/MiniLib/MTL/mtlList.h"

const signed X_TOKEN_COUNT = 28;
const token X_TOKENS[X_TOKEN_COUNT] = {
	new_keyword ("nop",                     3, xtoken::KEYWORD_INSTRUCTION_NOP),
	new_keyword ("put",                     3, xtoken::KEYWORD_INSTRUCTION_PUT),
	new_keyword ("add",                     3, xtoken::KEYWORD_INSTRUCTION_ADD),
	new_keyword ("sub",                     3, xtoken::KEYWORD_INSTRUCTION_SUB),
	new_keyword ("mul",                     3, xtoken::KEYWORD_INSTRUCTION_MUL),
	new_keyword ("div",                     3, xtoken::KEYWORD_INSTRUCTION_DIV),
	new_keyword ("mod",                     3, xtoken::KEYWORD_INSTRUCTION_MOD),
	new_keyword ("iadd",                    4, xtoken::KEYWORD_INSTRUCTION_IADD),
	new_keyword ("isub",                    4, xtoken::KEYWORD_INSTRUCTION_ISUB),
	new_keyword ("imul",                    4, xtoken::KEYWORD_INSTRUCTION_IMUL),
	new_keyword ("idiv",                    4, xtoken::KEYWORD_INSTRUCTION_IDIV),
	new_keyword ("imod",                    4, xtoken::KEYWORD_INSTRUCTION_IMOD),
	//new_keyword ("movu",                    4, xtoken::KEYWORD_INSTRUCTION_MOVU),
	//new_keyword ("movd",                    4, xtoken::KEYWORD_INSTRUCTION_MOVD),
	new_operator("@",                       1, xtoken::OPERATOR_DIRECTIVE_AT),
	new_operator("&",                       1, xtoken::OPERATOR_DIRECTIVE_ADDR),
	new_operator("$",                       1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
	new_keyword ("eval",                    4, xtoken::KEYWORD_DIRECTIVE_EVAL),
	new_keyword ("scope",                   5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
	new_keyword ("here",                    4, xtoken::KEYWORD_DIRECTIVE_HERE),
	//new_keyword ("top",                     3, 0),
	//new_keyword ("frame",                   5, 0),
	//new_keyword ("entry",                   5, 0),
	new_keyword ("lit",                     3, xtoken::KEYWORD_DIRECTIVE_LIT),
	new_operator(":",                       1, xtoken::OPERATOR_COLON),
	new_operator("[",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("{",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator(",",                       1, xtoken::OPERATOR_COMMA),
	new_operator(".",                       1, xtoken::OPERATOR_STOP),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22, token::ALIAS),
	new_literal ("[0-9]+",                  6, xtoken::LITERAL_INT)
};

token xlex(lexer *l)
{
	return lex(l, X_TOKENS, X_TOKEN_COUNT);
}

struct input_tokens
{
	const token *tokens;
	U16          capacity;
	U16          index;
};

struct binary_buffer
{
	XWORD *buffer;
	U16    capacity;
	U16    index;
};

bool write_word(binary_buffer &buf, XWORD data)
{
	if (buf.index == 0xffff) { return false; }
	buf.buffer[buf.index++] = data;
	return true;
}

struct output_binary
{
	binary_buffer head;
	binary_buffer body;
	binary_buffer tail;
};

struct scope
{
	struct symbol
	{
		char     name[32];
		XWORD    data;
		unsigned lit;
	};
	mtlList<symbol> symbols;
	U16             lsp; // local stack pointer
};

struct scope_stack
{
	scope  scopes[128];
	signed index;
};

static void push_scope(scope_stack &ss)
{
	++ss.index;
	ss.scopes[ss.index].symbols.RemoveAll();
	ss.scopes[ss.index].lsp = 0;
}

static void pop_scope(scope_stack &ss)
{
	ss.scopes[ss.index].symbols.RemoveAll();
	ss.scopes[ss.index].lsp = 0;
	--ss.index;
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

scope::symbol &add_symbol(const char *name, unsigned name_char_count, unsigned lit, scope &s)
{
	scope::symbol &sym = s.symbols.AddLast();
	unsigned count = name_char_count < sizeof(sym.name) - 1 ? name_char_count : sizeof(sym.name) - 1;
	unsigned i;
	for (i = 0; i < count; ++i) {
		sym.name[i] = name[i];
	}
	for (; i < sizeof(sym.name); ++i) {
		sym.name[i] = 0;
	}
	sym.lit = lit;
	if (!lit) {
		sym.data.u = s.lsp;
		++s.lsp;
	}
	return sym;
}

scope::symbol &add_symbol(const char *name, unsigned name_char_count, unsigned lit, scope_stack &ss)
{
	return add_symbol(name, name_char_count, lit, ss.scopes[ss.index]);
}

bool add_var(const char *name, unsigned name_char_count, scope_stack &ss)
{
	add_symbol(name, name_char_count, 0, ss);
	return true;
}

bool add_lit(const char *name, unsigned name_char_count, U16 value, scope_stack &ss)
{
	scope::symbol &sym = add_symbol(name, name_char_count, 1, ss);
	sym.data.u = value;
	return true;
}

struct parser
{
	input_tokens  in;
	output_binary out;
	scope_stack   scopes;
};

static bool match(parser *p, unsigned type)
{
	if (p->in.index >= p->in.capacity || p->in.tokens[p->in.index].type == token::STOP) {
		return type == token::STOP_EOF;
	}
	if (type == p->in.tokens[p->in.index].user_type) {
		++p->in.index;
		return true;
	}
	return false;
}

static token peek(parser *p)
{
	return p->in.index < p->in.capacity ? p->in.tokens[p->in.index] : new_eof();
}

struct parser_state
{
	parser   *p;
	parser    restore_point;
	unsigned  end;
};

static parser_state new_state(parser *p, unsigned end)
{
	return parser_state{ p, *p, end };
}

static bool manage_state(parser_state &ps, bool success)
{
	if (!success) {
		*ps.p = ps.restore_point;
	}
	return success;
}

static bool parse_instruction_nop  (parser_state ps);
static bool parse_decl_list        (parser_state ps);
static bool parse_directive_scope  (parser_state ps);
static bool parse_directive        (parser_state ps);
static bool parse_var              (parser_state ps);
static bool parse_lit              (parser_state ps);
static bool parse_param            (parser_state ps);
static bool parse_param_list       (parser_state ps);
static bool parse_directive_at     (parser_state ps);
static bool parse_directive_addr   (parser_state ps);
static bool parse_instruction_put  (parser_state ps);
static bool parse_instructions     (parser_state ps);
static bool parse_emit_lit_list    (parser_state ps);
static bool parse_statements       (parser_state ps);
static bool parse_scoped_statements(parser_state ps, U16 stack_offset);
static bool parse_program          (parser_state ps);

static bool parse_instruction_nop(parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_NOP)) {
		return write_word(ps.p->out.body, XWORD{XIS::NOP});
	}
	return false;
}

static bool parse_decl_var(parser_state ps)
{
	const token t = peek(ps.p);
	if (manage_state(ps, match(ps.p, token::ALIAS))) {
		if (!add_var(t.chars, chcount(t.chars), ps.p->scopes)) { return false; }
		return true;
	}
	return false;
}

static bool parse_decl_mem(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L) && parse_lit(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)) && match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R))) {
		ps.p->scopes.scopes[ps.p->scopes.index].lsp += ps.p->out.body.buffer[--ps.p->out.body.index].u;
		return true;
	}
	return false;
}

static bool parse_decl_list(parser_state ps)
{
	const token t = peek(ps.p);
	if (t.user_type == ps.end) { return true; }
	if (
		manage_state(
			ps,
			parse_decl_var(new_state(ps.p, ps.end)) ||
			parse_decl_mem(new_state(ps.p, ps.end))
		)
	) {
		return !match(ps.p, xtoken::OPERATOR_COMMA) || parse_decl_list(new_state(ps.p, ps.end));
	}
	return false;
}

static bool parse_directive_scope(parser_state ps)
{
	push_scope(ps.p->scopes);
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::KEYWORD_DIRECTIVE_SCOPE) &&
			match(ps.p, xtoken::OPERATOR_COLON) &&
			parse_decl_list(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L)) &&
			match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L) &&
			parse_scoped_statements(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R), ps.p->scopes.scopes[ps.p->scopes.index].lsp) &&
			match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		pop_scope(ps.p->scopes);
		return true;
	}
	return false;
}

static bool parse_directive(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) && parse_directive_scope(new_state(ps.p, ps.end)))) {
		return true;
	}
	return false;
}

static bool parse_var(parser_state ps)
{
	token t = peek(ps.p);
	if (match(ps.p, token::ALIAS)) {
		U16 sp_offset = 0;
		scope::symbol *sym = find_symbol(t.chars, chcount(t.chars), ps.p->scopes, sp_offset);
		if (sym == NULL || sym->lit) { return false; }
		return
			write_word(ps.p->out.body, XWORD{(U16)(sym->data.u - sp_offset)}) &&
			write_word(ps.p->out.body, XWORD{XIS::CREL}) &&
			write_word(ps.p->out.body, XWORD{XIS::AT});
	}
	return false;
}

static bool parse_lit(parser_state ps)
{
	token t = peek(ps.p);
	if (match(ps.p, xtoken::LITERAL_INT)) {
		return write_word(ps.p->out.body, XWORD{(U16)t.hash});
	} else if (match(ps.p, token::ALIAS)) {
		U16 sp_offset = 0; // Not really needed;
		scope::symbol *sym = find_symbol(t.chars, chcount(t.chars), ps.p->scopes, sp_offset);
		if (sym == NULL || !sym->lit) { return false; }
		return write_word(ps.p->out.body, XWORD{(U16)(sym->data.u)});
	} else if (match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) && match(ps.p, xtoken::KEYWORD_DIRECTIVE_EVAL)) {
		// TODO: This is where we add support for $eval(expr)
		// TODO: We want the compiler to precompute the expression inside and just emit the answer as a constant to the output binary.
		return false; 
	}
	return false;
}

static bool parse_param(parser_state ps)
{
	return (manage_state(ps, parse_directive_at(new_state(ps.p, ps.end)) || parse_directive_addr(new_state(ps.p, ps.end)) || parse_var(new_state(ps.p, ps.end)) || parse_lit(new_state(ps.p, ps.end))));
}

static bool parse_put_param(parser_state ps)
{
	if (
		manage_state(
			ps,
			write_word(ps.p->out.body, XWORD{XIS::PUT}) &&
			parse_param(new_state(ps.p, ps.end)) &&
			(!match(ps.p, xtoken::OPERATOR_COMMA) || parse_put_param(new_state(ps.p, ps.end)))
		)
	) {
		return true;
	}
	return false;
}

static bool parse_param_list(parser_state ps)
{
	if (peek(ps.p).user_type == ps.end) { return true; }
	if (manage_state(ps, parse_param(new_state(ps.p, ps.end)))) {
		return match(ps.p, xtoken::OPERATOR_COMMA) ? parse_param_list(new_state(ps.p, ps.end)) : true;
	}
	return false;
}

static bool parse_directive_at(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_AT) && parse_param(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out.body, XWORD{XIS::AT});
	}
	return false;
}

static bool parse_directive_addr(parser_state ps)
{
	// TODO: This is not correctly implemented.
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_ADDR) && parse_param(new_state(ps.p, ps.end)))) {
		return true;
	}
	return false;
}

static bool parse_instruction_put(parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_PUT)) {
		return manage_state(ps, parse_put_param(new_state(ps.p, xtoken::OPERATOR_STOP)));
	}
	return false;
}

static bool parse_repeat_param(parser_state ps, U16 repeat_instruction)
{
	if (
		manage_state(
			ps,
			write_word(ps.p->out.body, XWORD{XIS::PUT}) &&
			parse_param(new_state(ps.p, ps.end)) &&
			write_word(ps.p->out.body, XWORD{repeat_instruction}) &&
			(!match(ps.p, xtoken::OPERATOR_COMMA) || parse_repeat_param(new_state(ps.p, ps.end), repeat_instruction))
		)
	) {
		return true;
	}
	return false;
}

static bool parse_instruction_with_put(parser_state ps, unsigned token_type, U16 i)
{
	if (match(ps.p, token_type)) {
		return manage_state(ps, parse_repeat_param(new_state(ps.p, xtoken::OPERATOR_STOP), i));
	}
	return false;
}

static bool parse_instructions(parser_state ps)
{
	return manage_state(
		ps,
		(
			parse_instruction_nop(new_state(ps.p, ps.end)) ||
			parse_instruction_put(new_state(ps.p, ps.end)) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_ADD , XIS::ADD) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_SUB , XIS::SUB) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_MUL , XIS::MUL) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_DIV , XIS::DIV) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_MOD , XIS::MOD) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IADD, XIS::IADD) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_ISUB, XIS::ISUB) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IMUL, XIS::IMUL) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IDIV, XIS::IDIV) ||
			parse_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IMOD, XIS::IMOD)
		) &&
		match(ps.p, xtoken::OPERATOR_STOP)
	);
}

static bool parse_emit_lit_list(parser_state ps)
{
	if (manage_state(ps, parse_lit(new_state(ps.p, ps.end)))) {
		return match(ps.p, xtoken::OPERATOR_STOP) || (match(ps.p, xtoken::OPERATOR_COMMA) && parse_emit_lit_list(new_state(ps.p, ps.end)));
	}
	return false;
}

static bool parse_statements(parser_state ps)
{
	if (peek(ps.p).user_type == ps.end) { return true; }
	if (
		manage_state(
			ps,
			parse_emit_lit_list(new_state(ps.p, ps.end)) ||
			parse_directive(new_state(ps.p, ps.end)) ||
			parse_instructions(new_state(ps.p, ps.end))
		)
	) {
		return peek(ps.p).user_type == ps.end || parse_statements(new_state(ps.p, ps.end));
	}
	return false;
}

static bool parse_scoped_statements(parser_state ps, U16 stack_offset)
{
	if (stack_offset > 0 && (!write_word(ps.p->out.body, XWORD{XIS::PUT}) || !write_word(ps.p->out.body, XWORD{stack_offset}) || !write_word(ps.p->out.body, XWORD{XIS::PUSH}))) {
		return false;
	}
	if (manage_state(ps, parse_statements(new_state(ps.p, ps.end)))) {
		return
			stack_offset > 0 ?
			write_word(ps.p->out.body, XWORD{XIS::PUT}) &&
			write_word(ps.p->out.body, XWORD{stack_offset}) &&
			write_word(ps.p->out.body, XWORD{XIS::POP}) :
			true;
	}
	return false;
}

static bool parse_program(parser_state ps)
{
	if (manage_state(ps, parse_statements(new_state(ps.p, ps.end)))) {
		return true;
	}
	return false;
}

U16 assemble_xasm(U16 max_tokens, const token *tokens, U16 max_binary_body, XWORD *body)
{
	parser       p  = { { tokens, max_tokens, 0 }, { { NULL, 0, 0 }, { body, max_binary_body, 0 }, { NULL, 0, 0 } } };
	p.scopes.index = 0;
	parser_state ps = new_state(&p, token::STOP_EOF);
	if (!manage_state(ps, parse_program(new_state(ps.p, ps.end)))) {
		return 0;
	}
	return p.out.head.index + p.out.body.index + p.out.tail.index;
}
