#include "xasm.h"
#include "../../lib/MiniLib/MTL/mtlList.h"

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

const signed X_TOKEN_COUNT = 61;
const token X_TOKENS[X_TOKEN_COUNT] = {
	new_keyword ("nop",                     3, xtoken::KEYWORD_INSTRUCTION_NOP),
	new_keyword ("at",                      2, xtoken::KEYWORD_INSTRUCTION_AT),
	new_keyword ("set",                     3, xtoken::KEYWORD_INSTRUCTION_SET),
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
	new_keyword ("lsh",                     3, xtoken::KEYWORD_INSTRUCTION_LSH),
	new_keyword ("rsh",                     3, xtoken::KEYWORD_INSTRUCTION_RSH),
	new_keyword ("and",                     3, xtoken::KEYWORD_INSTRUCTION_AND),
	new_keyword ("or",                      2, xtoken::KEYWORD_INSTRUCTION_OR),
	new_keyword ("xor",                     3, xtoken::KEYWORD_INSTRUCTION_XOR),
	new_keyword ("mov",                     3, xtoken::KEYWORD_INSTRUCTION_MOVU),
//	new_keyword ("movd",                    4, xtoken::KEYWORD_INSTRUCTION_MOVD), // no args
	new_keyword ("toss",                    4, xtoken::KEYWORD_INSTRUCTION_TOSS),
	new_keyword ("eq",                      2, xtoken::KEYWORD_INSTRUCTION_EQ),
	new_keyword ("ne",                      2, xtoken::KEYWORD_INSTRUCTION_NE),
	new_keyword ("gt",                      2, xtoken::KEYWORD_INSTRUCTION_GT),
	new_keyword ("lt",                      2, xtoken::KEYWORD_INSTRUCTION_LT),
	new_keyword ("ge",                      2, xtoken::KEYWORD_INSTRUCTION_GE),
	new_keyword ("le",                      2, xtoken::KEYWORD_INSTRUCTION_LE),
	new_keyword ("igt",                     3, xtoken::KEYWORD_INSTRUCTION_IGT),
	new_keyword ("ilt",                     3, xtoken::KEYWORD_INSTRUCTION_ILT),
	new_keyword ("ige",                     3, xtoken::KEYWORD_INSTRUCTION_IGE),
	new_keyword ("ile",                     3, xtoken::KEYWORD_INSTRUCTION_ILE),
	new_keyword ("jmp",                     3, xtoken::KEYWORD_INSTRUCTION_JMP),
	new_keyword ("cjmp",                    4, xtoken::KEYWORD_INSTRUCTION_CJMP),
	new_keyword ("skip",                    4, xtoken::KEYWORD_INSTRUCTION_SKIP),
	new_keyword ("cskip",                   5, xtoken::KEYWORD_INSTRUCTION_CSKIP),
	new_operator("@",                       1, xtoken::OPERATOR_DIRECTIVE_AT),
	new_operator("&",                       1, xtoken::OPERATOR_DIRECTIVE_ADDR),
	new_operator("%",                       1, xtoken::OPERATOR_DIRECTIVE_LABEL),
	new_operator("$",                       1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
	new_keyword ("eval",                    4, xtoken::KEYWORD_DIRECTIVE_EVAL),
	new_keyword ("size",                    4, xtoken::KEYWORD_DIRECTIVE_SIZE),
	new_keyword ("bin",                     3, xtoken::KEYWORD_DIRECTIVE_BIN),
	new_keyword ("scope",                   5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
	new_keyword ("syntax",                  6, xtoken::KEYWORD_DIRECTIVE_SYNTAX),
	new_keyword ("here",                    4, xtoken::KEYWORD_DIRECTIVE_HERE),
	new_keyword ("top",                     3, xtoken::KEYWORD_DIRECTIVE_TOP),
	new_keyword ("frame",                   5, xtoken::KEYWORD_DIRECTIVE_FRAME),
	new_keyword ("base",                    4, xtoken::KEYWORD_DIRECTIVE_BASE),
	new_keyword ("lit",                     3, xtoken::KEYWORD_DIRECTIVE_LIT),
	new_operator(":",                       1, xtoken::OPERATOR_COLON),
	new_operator("[",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("{",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("(",                       1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator(",",                       1, xtoken::OPERATOR_COMMA),
	new_operator(".",                       1, xtoken::OPERATOR_STOP),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, xtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xtoken::LITERAL_INT, hex2u)
};

token xasm_lex(lexer *l)
{
	return lex(l, X_TOKENS, X_TOKEN_COUNT);
}

struct input_tokens
{
	lexer        l;        // The input lexer that creates tokens from text. Is used if 'tokens' is null.
	const token *tokens;   // Input token array
	U16          capacity; // The max capacity of the input token array (not used for the lexer). Parser may quit earlier if a stop token is encountered.
	U16          index;    // The index in the input token array to currently read (not used for the lexer).
};

static bool write_word(xcbe_binary &buf, XWORD data)
{
	if (buf.size >= buf.capacity) { return false; }
	buf.buffer[buf.size++] = data;
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

static scope::symbol *find_symbol(const char *name, unsigned name_char_count, scope_stack &ss, U16 &back_sp_offset)
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
	U16 temp;
	scope::symbol *sym = find_symbol(name, name_char_count, ss, temp);
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

/// @brief The main data structure for the parser.
struct parser
{
	input_tokens  in;     // The input tokens to parse.
	xcbe_binary   out;    // The resulting binary.
	scope_stack   scopes; // The scope stack, containing all defined symbols.
	token         max;    // The token furthest in the sequence that was lexed.
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
		t = xasm_lex(&l);
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
		xasm_lex(&l);
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
	parser   *p;             // The address of the parser.
	parser    restore_point; // A hard copy of the parser at the point where the parser state was created. Will be used as the restore point if the parser fails to identify a given pattern.
	unsigned  end;           // The character that signifies the end of a code segment, such as a closing brace, quote, or end-of-file.
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
			//std::cout << "max {" << max.text.str << "," << max.index << "," << max.head << "," << max.row << "," << max.col << "}" << std::endl;
			ps.p->max = max;
		} else {
			//std::cout << "not max {" << max.text.str << "," << max.index << "," << max.head << "," << max.row << "," << max.col << "}" << std::endl;
		}
	}
	return success;
}

static bool try_instruction_nop  (parser_state ps);
static bool try_decl_var         (parser_state ps);
static bool try_decl_mem         (parser_state ps);
static bool try_decl_arr         (parser_state ps);
static bool try_decl_list        (parser_state ps);
static bool try_directive_scope  (parser_state ps);
static bool try_directive        (parser_state ps);
static bool try_var_addr         (parser_state ps);
static bool try_var              (parser_state ps);
static bool try_lit              (parser_state ps);
static bool try_param            (parser_state ps);
static bool try_putparam         (parser_state ps);
static bool try_put_param        (parser_state ps);
static bool try_directive_at     (parser_state ps);
static bool try_instruction_put  (parser_state ps);
static bool try_instructions     (parser_state ps);
static bool try_emit_lit_list    (parser_state ps);
static bool try_statements       (parser_state ps);
static bool try_scoped_statements(parser_state ps, U16 stack_offset);
static bool try_program          (parser_state ps);

static bool try_instruction_nop(parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_NOP)) {
		return write_word(ps.p->out, XWORD{XIS::NOP});
	}
	return false;
}

static bool try_instruction_at(parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_AT)) {
		return write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_decl_var(parser_state ps)
{
	const token t = peek(ps.p);
	if (manage_state(ps, match(ps.p, token::ALIAS))) {
		if (add_var(t.text.str, chcount(t.text.str), ps.p->scopes) == NULL) { return false; }
		return true;
	}
	return false;
}

static bool try_decl_mem(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L) && try_lit(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)) && match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R))) {
		top_scope(ps.p).lsp += ps.p->out.buffer[--ps.p->out.size].u;
		return true;
	}
	return false;
}

static bool try_decl_arr(parser_state ps)
{
	U16 lsp = top_scope(ps.p).lsp;
	if (manage_state(ps, try_decl_var(new_state(ps.p, ps.end)) && try_decl_mem(new_state(ps.p, ps.end)))) {
		U16 size = top_scope(ps.p).lsp - lsp;
		if (size == 0) { return false; }
		top_scope(ps.p).lsp -= 1; // TODO: Maybe LSP should not be modified at all. Verify.
		top_scope(ps.p).symbols.GetLast()->GetItem().size = size - 1;
		return true;
	}
	return false;
}

static bool try_decl_list(parser_state ps)
{
	const token t = peek(ps.p);
	if (t.user_type == ps.end) { return true; }
	if (
		manage_state(
			ps,
			try_decl_arr(new_state(ps.p, ps.end)) ||
			try_decl_var(new_state(ps.p, ps.end)) ||
			try_decl_mem(new_state(ps.p, ps.end))
		)
	) {
		return !match(ps.p, xtoken::OPERATOR_COMMA) || try_decl_list(new_state(ps.p, ps.end));
	}
	return false;
}

static bool try_directive_scope(parser_state ps)
{
	push_scope(ps.p->scopes);
	if (
		manage_state(
			ps,
			match                (ps.p, xtoken::KEYWORD_DIRECTIVE_SCOPE)                                  &&
			match                (ps.p, xtoken::OPERATOR_COLON)                                           &&
			try_decl_list        (new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L))                      &&
			match                (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L)                                 &&
			try_scoped_statements(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R), top_scope(ps.p).lsp) &&
			match                (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		pop_scope(ps.p->scopes);
		return true;
	}
	return false;
}

static bool try_directive_bin(parser_state ps)
{
	if (!(write_word(ps.p->out, XWORD{XIS::PUT}) && write_word(ps.p->out, XWORD{0}) && write_word(ps.p->out, XWORD{XIS::SKIP}))) {
		return false;
	}
	U16 ip = ps.p->out.size;
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::KEYWORD_DIRECTIVE_BIN) &&
			try_emit_lit_list(new_state(ps.p, xtoken::OPERATOR_STOP)) &&
			match(ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		ps.p->out.buffer[ip - 2].u = ps.p->out.size - ip;
		return true;
	}
	return false;
}

static bool try_decl_lit(parser_state ps)
{
	token t = peek(ps.p);
	if (manage_state(ps, match(ps.p, token::ALIAS) && match(ps.p, xtoken::OPERATOR_COMMA) && try_lit(new_state(ps.p, ps.end)))) {
		if (add_lit(t.text.str, chcount(t.text.str), ps.p->out.buffer[--ps.p->out.size].u, ps.p->scopes) == NULL) { return false; }
		return true;
	}
	return false;
}

static bool try_directive_lit(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::KEYWORD_DIRECTIVE_LIT) &&
			try_decl_lit(new_state(ps.p, xtoken::OPERATOR_STOP)) &&
			match(ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool try_label(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_LABEL))) {
		token t = peek(ps.p);
		if (manage_state(ps, match(ps.p, token::ALIAS) && match(ps.p, xtoken::OPERATOR_COLON))) {
			return add_lbl(t.text.str, chcount(t.text.str), ps.p->out.size, ps.p->scopes) != NULL;
		}
	}
	return false;
}

static bool try_directive(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) &&
			(
				try_directive_scope(new_state(ps.p, ps.end)) ||
				try_directive_bin  (new_state(ps.p, ps.end)) ||
				try_directive_lit  (new_state(ps.p, ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_lit_index(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L) && try_lit(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)) && match(ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R))) {
		// HACK: The top binary value is now the offset. We need to do something with this in the parent scope.
		return true;
	}
	return false;
}

static U16 parse_lit_index(parser_state &ps)
{
	U16 index = 0;
	if (try_lit_index(new_state(ps.p, ps.end))) {
		index = ps.p->out.buffer[--ps.p->out.size].u;
	}
	return index;
}

static bool try_reg(parser_state ps)
{
	if (match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR)) {
		if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_HERE)) {
			U16 index = parse_lit_index(ps);
			if (ps.p->out.size > 0 && ps.p->out.buffer[ps.p->out.size - 1].u == XIS::PUT) {
				--ps.p->out.size; // We have already written PUT to the output, but we need to change that to a PUTI.
				if (!write_word(ps.p->out, XWORD{XIS::PUTI})) {
					return false;
				}
			}
			return index == 0 ?
				true :
				(write_word(ps.p->out, XWORD{XIS::PUT}) && write_word(ps.p->out, XWORD{index}) && write_word(ps.p->out, XWORD{XIS::ADD}));
		} else if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_TOP)) {
			U16 index = parse_lit_index(ps);
			if (ps.p->out.size > 0 && ps.p->out.buffer[ps.p->out.size - 1].u == XIS::PUT) {
				--ps.p->out.size; // We have already written PUT to the output, but we need to change that to a PUTS.
				if (!write_word(ps.p->out, XWORD{XIS::PUTS})) {
					return false;
				}
			}
			return index == 0 ?
				true :
				(write_word(ps.p->out, XWORD{XIS::PUT}) && write_word(ps.p->out, XWORD{index}) && write_word(ps.p->out, XWORD{XIS::SUB}));
		} else if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_FRAME)) {
			U16 index = parse_lit_index(ps);
			return index == 0 ?
				(write_word(ps.p->out, XWORD{U16(0)}) && write_word(ps.p->out, XWORD{XIS::RLC})) :
				(write_word(ps.p->out, XWORD{U16(0)}) && write_word(ps.p->out, XWORD{XIS::RLC}) && write_word(ps.p->out, XWORD{XIS::PUT}) && write_word(ps.p->out, XWORD{index}) && write_word(ps.p->out, XWORD{XIS::ADD}));
		} else if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_BASE)) {
			U16 index = parse_lit_index(ps);
			return index == 0 ?
				(write_word(ps.p->out, XWORD{U16(0)}) && write_word(ps.p->out, XWORD{XIS::RLB})) :
				(write_word(ps.p->out, XWORD{U16(0)}) && write_word(ps.p->out, XWORD{XIS::RLB}) && write_word(ps.p->out, XWORD{XIS::PUT}) && write_word(ps.p->out, XWORD{index}) && write_word(ps.p->out, XWORD{XIS::ADD}));
		}
	}
	return false;
}

static bool try_var_addr(parser_state ps)
{
	token t = peek(ps.p);
	if (match(ps.p, token::ALIAS)) {
		U16 index = parse_lit_index(ps);
		U16 sp_offset = 0;
		scope::symbol *sym = find_symbol(t.text.str, chcount(t.text.str), ps.p->scopes, sp_offset);
		if (sym == NULL || sym->type != scope::symbol::VAR) { return false; }
		return
			write_word(ps.p->out, XWORD{(U16)(sym->data.u - sp_offset + index)}) &&
			write_word(ps.p->out, XWORD{XIS::RLC});
	}
	return false;
}

static bool try_var(parser_state ps)
{
	if (manage_state(ps, (match(ps.p, xtoken::OPERATOR_DIRECTIVE_ADDR) && try_var_addr(new_state(ps.p, ps.end))))) {
		return true;
	}
	if (manage_state(ps, try_var_addr(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_lit_expr(parser_state ps, U16 &x)
{
	return false;
}

static bool try_directive_eval(parser_state ps)
{
	XWORD x;
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::KEYWORD_DIRECTIVE_EVAL) &&
			match(ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L) &&
			try_lit_expr(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), x.u) &&
			match(ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		return write_word(ps.p->out, x);
	}
	return false;
}

static bool try_var_size(parser_state ps)
{
	token t = peek(ps.p);
	if (match(ps.p, token::ALIAS)) {
		U16 sp_offset; // unused
		scope::symbol *sym = find_symbol(t.text.str, chcount(t.text.str), ps.p->scopes, sp_offset);
		if (sym == NULL) { return false; }
		return write_word(ps.p->out, XWORD{sym->size});
	}
	return false;
}

static bool try_directive_size(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::KEYWORD_DIRECTIVE_SIZE) &&
			match(ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L) &&
			try_var_size(new_state(ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match(ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		return true;
	}
	return false;
}

static bool try_lit_directive(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_directive_eval(new_state(ps.p, ps.end)) ||
			try_directive_size(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_lit(parser_state ps)
{
	// TODO: Add support for index (especially useful for aliased literals and evals).
	token t;
	if (match(ps.p, xtoken::LITERAL_INT, &t)) {
		U16 index = parse_lit_index(ps);
		return write_word(ps.p->out, XWORD{(U16)(t.hash + index)});
	} else if (match(ps.p, token::ALIAS, &t)) {
		U16 sp_offset = 0; // Not really needed;
		scope::symbol *sym = find_symbol(t.text.str, chcount(t.text.str), ps.p->scopes, sp_offset);
		if (sym == NULL || sym->type != scope::symbol::LIT) { return false; }
		U16 index = parse_lit_index(ps);
		return write_word(ps.p->out, XWORD{(U16)(sym->data.u + index)});
	} else if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_LABEL) && match(ps.p, token::ALIAS, &t))) {
		scope::symbol *lbl = find_lbl(t.text.str, chcount(t.text.str), top_scope(ps.p));
		if (lbl != NULL) {
			if (!write_word(ps.p->out, XWORD{lbl->data.u})) { return false; }
		} else {
			scope::fwd_label fwd;
			const unsigned count = chcount(t.text.str);
			unsigned i;
			for (i = 0; i < count; ++i)       { fwd.name[i] = t.text.str[i]; }
			for (; i < sizeof(fwd.name); ++i) { fwd.name[i] = 0; }
			fwd.loc = ps.p->out.buffer + ps.p->out.size;
			top_scope(ps.p).fwd_labels.AddLast(fwd);
			if (!write_word(ps.p->out, XWORD{0})) { return false; }
		}
		// TODO: If we use relative IP pointers, we also need to emit a modifying instruction here
		return true;
	} else if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) && try_lit_directive(new_state(ps.p, ps.end)))) {
		return true;
	}
	return false;
}

static bool try_param(parser_state ps)
{
	// NOTE: The order of these can cause trouble as some functions consume tokens despite failing.
	if (
		manage_state(
			ps,
			try_directive_at(new_state(ps.p, ps.end)) ||
			try_var(new_state(ps.p, ps.end)) ||
			try_lit(new_state(ps.p, ps.end)) ||
			try_reg(new_state(ps.p, ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_putparam(parser_state ps)
{
	if (write_word(ps.p->out, XWORD{XIS::PUT}) && try_param(new_state(ps.p, ps.end))) {
		return true;
	}
	return false;
}

static bool try_put_param(parser_state ps)
{
	if (
		manage_state(
			ps,
			try_putparam(new_state(ps.p, ps.end)) &&
			(!match(ps.p, xtoken::OPERATOR_COMMA) || try_put_param(new_state(ps.p, ps.end)))
		)
	) {
		return true;
	}
	return false;
}

static bool try_directive_at(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::OPERATOR_DIRECTIVE_AT) && try_param(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_instruction_put(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::KEYWORD_INSTRUCTION_PUT) && try_put_param(new_state(ps.p, xtoken::OPERATOR_STOP)))) {
		return true;
	}
	return false;
}

static bool try_repeat_param(parser_state ps, U16 repeat_instruction)
{
	if (
		manage_state(
			ps,
			try_putparam(new_state(ps.p, ps.end)) &&
			write_word(ps.p->out, XWORD{repeat_instruction}) &&
			(!match(ps.p, xtoken::OPERATOR_COMMA) || try_repeat_param(new_state(ps.p, ps.end), repeat_instruction))
		)
	) {
		return true;
	}
	return false;
}

static bool try_instruction_with_put(parser_state ps, unsigned token_type, U16 i)
{
	if (match(ps.p, token_type)) {
		if (peek(ps.p).user_type == ps.end) {
			return write_word(ps.p->out, XWORD{i});
		}
		return manage_state(ps, try_repeat_param(new_state(ps.p, xtoken::OPERATOR_STOP), i));
	}
	return false;
}

static bool try_instruction_jmp(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::KEYWORD_INSTRUCTION_JMP) && write_word(ps.p->out, XWORD{XIS::PUT}) && try_param(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out, XWORD{XIS::JMP});
	}
	return false;
}

static bool try_instruction_cjmp(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::KEYWORD_INSTRUCTION_CJMP) && write_word(ps.p->out, XWORD{XIS::PUT}) && try_param(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out, XWORD{XIS::CJMP});
	}
	return false;
}

static bool try_instruction_skip(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::KEYWORD_INSTRUCTION_SKIP) && write_word(ps.p->out, XWORD{XIS::PUT}) && try_param(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out, XWORD{XIS::SKIP});
	}
	return false;
}

static bool try_instruction_cskip(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::KEYWORD_INSTRUCTION_CSKIP) && write_word(ps.p->out, XWORD{XIS::PUT}) && try_param(new_state(ps.p, ps.end)))) {
		return write_word(ps.p->out, XWORD{XIS::CSKIP});
	}
	return false;
}

static bool try_instruction_set(parser_state ps)
{
	if (
		manage_state(
			ps,
			match(ps.p, xtoken::KEYWORD_INSTRUCTION_SET) &&
			try_putparam(new_state(ps.p, ps.end))        &&
			match(ps.p, xtoken::OPERATOR_COMMA)          &&
			try_putparam(new_state(ps.p, ps.end))
		)
	) {
		return write_word(ps.p->out, XWORD{XIS::MOVD});
	}
	return false;
}

static bool try_instruction_toss(parser_state ps)
{
	if (manage_state(ps, match(ps.p, xtoken::KEYWORD_INSTRUCTION_TOSS))) {
		return write_word(ps.p->out, XWORD{XIS::TOSS});
	}
	return false;
}

static bool try_instructions(parser_state ps)
{
	if (
		manage_state(
			ps,
			(
				try_instruction_nop     (new_state(ps.p, ps.end))                                              ||
				try_instruction_at      (new_state(ps.p, ps.end))                                              ||
				try_instruction_put     (new_state(ps.p, ps.end))                                              ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_ADD,  XIS::ADD)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_SUB,  XIS::SUB)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_MUL,  XIS::MUL)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_DIV,  XIS::DIV)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_MOD,  XIS::MOD)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IADD, XIS::IADD) ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_ISUB, XIS::ISUB) ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IMUL, XIS::IMUL) ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IDIV, XIS::IDIV) ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IMOD, XIS::IMOD) ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_LSH,  XIS::LSH)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_RSH,  XIS::RSH)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_AND,  XIS::AND)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_OR,   XIS::OR)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_XOR,  XIS::XOR)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_MOVU, XIS::MOVU) ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_EQ,   XIS::EQ)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_NE,   XIS::NE)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_LE,   XIS::LE)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_GE,   XIS::GE)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_LT,   XIS::LT)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_GT,   XIS::GT)   ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_ILE,  XIS::ILE)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IGE,  XIS::IGE)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_ILT,  XIS::ILT)  ||
				try_instruction_with_put(new_state(ps.p, ps.end), xtoken::KEYWORD_INSTRUCTION_IGT,  XIS::IGT)  ||
				try_instruction_jmp     (new_state(ps.p, ps.end))                                              ||
				try_instruction_cjmp    (new_state(ps.p, ps.end))                                              ||
				try_instruction_skip    (new_state(ps.p, ps.end))                                              ||
				try_instruction_cskip   (new_state(ps.p, ps.end))                                              ||
				try_instruction_set     (new_state(ps.p, ps.end))                                              ||
				try_instruction_toss    (new_state(ps.p, ps.end))
			) &&
			match(ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool try_emit_lit_list(parser_state ps)
{
	while (manage_state(ps, try_lit(new_state(ps.p, ps.end)))) {
		if (peek(ps.p).user_type == ps.end) {
			return true;
		} else if (!match(ps.p, xtoken::OPERATOR_COMMA)) {
			break;
		}
	}
	return false;
}

static bool try_statements(parser_state ps)
{
	while (peek(ps.p).user_type != ps.end) {
		if (
			!manage_state(
				ps,
				try_label(new_state(ps.p, ps.end))        ||
				try_directive(new_state(ps.p, ps.end))    ||
				try_instructions(new_state(ps.p, ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_scoped_statements(parser_state ps, U16 stack_offset)
{
	if (stack_offset > 0 && (!write_word(ps.p->out, XWORD{XIS::PUT}) || !write_word(ps.p->out, XWORD{stack_offset}) || !write_word(ps.p->out, XWORD{XIS::PUSH}))) {
		return false;
	}
	if (manage_state(ps, try_statements(new_state(ps.p, ps.end)))) {
		return
			stack_offset > 0 ?
			write_word(ps.p->out, XWORD{XIS::PUT}) &&
			write_word(ps.p->out, XWORD{stack_offset}) &&
			write_word(ps.p->out, XWORD{XIS::POP}) :
			true;
	}
	return false;
}

static bool try_program(parser_state ps)
{
	if (
		manage_state(
			ps,
			write_word    (ps.p->out, XWORD{XIS::SVB})  &&
			try_statements(new_state(ps.p, ps.end))     &&
			write_word    (ps.p->out, XWORD{XIS::LDB})  &&
			write_word    (ps.p->out, XWORD{XIS::HALT})
		)
	) {
		return true;
	}
	return false;
}

xcbe_out xasm(U16 max_tokens, const token *tokens, U16 max_binary_body, XWORD *body)
{
	parser p = parser {
		input_tokens {
			init_lexer({NULL,0}),
			tokens,
			max_tokens,
			0
		},
		xcbe_binary {
			body, max_binary_body, 0
		}
	};
	p.scopes.index = 0;
	p.max = p.in.l.last;
	parser_state ps = new_state(&p, token::STOP_EOF);
	if (!manage_state(ps, try_program(new_state(ps.p, ps.end)))) {
		return { lexer{{NULL,0},0}, xcbe_binary{NULL,0,0}, p.max, 1 };
	}
	return { p.in.l, p.out, p.max, 0 };
}

xcbe_out xasm(lexer l, xcbe_binary memory)
{
	parser p = parser {
		input_tokens {
			l,
			NULL,
			0,
			0
		},
		memory
	};
	p.scopes.index = 0;
	p.max = p.in.l.last;
	parser_state ps = new_state(&p, token::STOP_EOF);
	if (!manage_state(ps, try_program(new_state(ps.p, ps.end)))) {
		return { p.in.l, p.out, p.max, 1 };
	}
	return { p.in.l, p.out, p.max, 0 };
}
