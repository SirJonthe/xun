#include "xasm.h"
#include "../../../lib/MiniLib/MTL/mtlList.h"

/// @brief Constructs a new xcc_parser state from an end token.
/// @param end A token user type representing the end of the token stream.
/// @return A new xcc_parser state.
#define new_state(end) xcc_new_state(ps.p, end)

/// @brief Manages the xcc_parser state so it properly rewinds if the parsing fails.
/// @param success The success of the parsing.
/// @return The success of the parsing.
#define manage_state(success) xcc_manage_state(ps, ps.p->error.code == xcc_error::NONE && (success))

/// @brief Sets an error in the xcc_parser.
/// @param p The xcc_parser.
/// @param code The error code.
#define set_error(p, code) xcc_set_error(p, code, __LINE__)

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

const signed X_TOKEN_COUNT = 63;
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
	new_keyword ("clock",                   5, xtoken::KEYWORD_INSTRUCTION_CLOCK),
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
	new_comment ("//",                      2),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, xtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xtoken::LITERAL_INT, hex2u)
};

token xasm_lex(lexer *l)
{
	return lex(l, X_TOKENS, X_TOKEN_COUNT);
}

static token peek(xcc_parser *p)
{
	return xcc_peek(p, xasm_lex);
}

static bool match(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xasm_lex);
}

static bool try_instruction_nop  (xcc_parser_state ps);
static bool try_instruction_clock(xcc_parser_state ps);
static bool try_decl_var         (xcc_parser_state ps);
static bool try_decl_mem         (xcc_parser_state ps);
static bool try_decl_list        (xcc_parser_state ps);
static bool try_directive_scope  (xcc_parser_state ps);
static bool try_directive        (xcc_parser_state ps);
static bool try_var_addr         (xcc_parser_state ps);
static bool try_var              (xcc_parser_state ps);
static bool try_lit              (xcc_parser_state ps);
static bool try_param            (xcc_parser_state ps);
static bool try_putparam         (xcc_parser_state ps);
static bool try_put_param        (xcc_parser_state ps);
static bool try_directive_at     (xcc_parser_state ps);
static bool try_instruction_put  (xcc_parser_state ps);
static bool try_instructions     (xcc_parser_state ps);
static bool try_emit_lit_list    (xcc_parser_state ps);
static bool try_statements       (xcc_parser_state ps);
static bool try_scoped_statements(xcc_parser_state ps, U16 stack_offset);
static bool try_program          (xcc_parser_state ps);

static bool try_instruction_nop(xcc_parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_NOP)) {
		return xcc_write_word(ps.p->out, XWORD{XIS::NOP});
	}
	return false;
}

static bool try_instruction_clock(xcc_parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_CLOCK)) {
		return xcc_write_word(ps.p->out, XWORD{XIS::CLOCK});
	}
	return false;
}

static bool try_instruction_at(xcc_parser_state ps)
{
	if (match(ps.p, xtoken::KEYWORD_INSTRUCTION_AT)) {
		return xcc_write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_decl_var(xcc_parser_state ps)
{
	const token t = peek(ps.p);
	if (
		manage_state(
			match(ps.p, token::ALIAS)
		)
	) {
		if (xcc_add_var(t.text, ps.p) == NULL) { return false; }
		return true;
	}
	return false;
}

static bool try_decl_mem(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			try_lit       (new_state(xtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)      &&
			xcc_add_memory(ps.p, ps.p->out.buffer[--ps.p->out.size].u)
		)
	) {
		return true;
	}
	return false;
}

static bool try_decl_list(xcc_parser_state ps)
{
	const token t = peek(ps.p);
	if (t.user_type == ps.end) { return true; }
	if (
		manage_state(
			try_decl_var(new_state(ps.end)) ||
			try_decl_mem(new_state(ps.end))
		)
	) {
		return !match(ps.p, xtoken::OPERATOR_COMMA) || try_decl_list(new_state(ps.end));
	}
	return false;
}

static bool try_directive_scope(xcc_parser_state ps)
{
	xcc_push_scope(ps.p->scopes);
	if (
		manage_state(
			match                (ps.p, xtoken::KEYWORD_DIRECTIVE_SCOPE)                                       &&
			match                (ps.p, xtoken::OPERATOR_COLON)                                                &&
			try_decl_list        (new_state(xtoken::OPERATOR_ENCLOSE_BRACE_L))                                 &&
			match                (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L)                                      &&
			try_scoped_statements(new_state(xtoken::OPERATOR_ENCLOSE_BRACE_R), xcc_top_scope_stack_size(ps.p)) &&
			match                (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		xcc_pop_scope(ps.p->scopes);
		return true;
	}
	return false;
}

static bool try_directive_bin(xcc_parser_state ps)
{
	if (!(xcc_write_word(ps.p->out, XWORD{XIS::PUT}) && xcc_write_word(ps.p->out, XWORD{0}) && xcc_write_word(ps.p->out, XWORD{XIS::SKIP}))) {
		return false;
	}
	U16 ip = ps.p->out.size;
	if (
		manage_state(
			match            (ps.p, xtoken::KEYWORD_DIRECTIVE_BIN) &&
			try_emit_lit_list(new_state(xtoken::OPERATOR_STOP))    &&
			match            (ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		ps.p->out.buffer[ip - 2].u = ps.p->out.size - ip;
		return true;
	}
	return false;
}

static bool try_decl_lit(xcc_parser_state ps)
{
	token t = peek(ps.p);
	if (
		manage_state(
			match(ps.p, token::ALIAS) &&
			match(ps.p, xtoken::OPERATOR_COMMA) &&
			try_lit(new_state(ps.end))
		)
	) {
		if (xcc_add_lit(t.text, ps.p->out.buffer[--ps.p->out.size].u, ps.p) == NULL) { return false; }
		return true;
	}
	return false;
}

static bool try_directive_lit(xcc_parser_state ps)
{
	if (
		manage_state(
			match       (ps.p, xtoken::KEYWORD_DIRECTIVE_LIT) &&
			try_decl_lit(new_state(xtoken::OPERATOR_STOP))    &&
			match       (ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool try_directive(xcc_parser_state ps)
{
	if (
		manage_state(
			match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) &&
			(
				try_directive_scope(new_state(ps.end)) ||
				try_directive_bin  (new_state(ps.end)) ||
				try_directive_lit  (new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_lit_index(xcc_parser_state ps)
{
	if (
		manage_state(
			match  (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			try_lit(new_state(xtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			match  (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)
		)
	) {
		// HACK: The top binary value is now the offset. We need to do something with this in the parent scope.
		return true;
	}
	return false;
}

static U16 parse_lit_index(xcc_parser_state &ps)
{
	U16 index = 0;
	if (try_lit_index(new_state(ps.end))) {
		index = ps.p->out.buffer[--ps.p->out.size].u;
	}
	return index;
}

static bool try_reg(xcc_parser_state ps)
{
	if (match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR)) {
		if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_HERE)) {
			U16 index = parse_lit_index(ps);
			if (ps.p->out.size > 0 && ps.p->out.buffer[ps.p->out.size - 1].u == XIS::PUT) {
				--ps.p->out.size; // We have already written PUT to the output, but we need to change that to a PUTI.
				if (!xcc_write_word(ps.p->out, XWORD{XIS::PUTI})) {
					return false;
				}
			}
			return index == 0 ?
				true :
				(xcc_write_word(ps.p->out, XWORD{XIS::PUT}) && xcc_write_word(ps.p->out, XWORD{index}) && xcc_write_word(ps.p->out, XWORD{XIS::ADD}));
		} else if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_TOP)) {
			U16 index = parse_lit_index(ps);
			if (ps.p->out.size > 0 && ps.p->out.buffer[ps.p->out.size - 1].u == XIS::PUT) {
				--ps.p->out.size; // We have already written PUT to the output, but we need to change that to a PUTS.
				if (!xcc_write_word(ps.p->out, XWORD{XIS::PUTS})) {
					return false;
				}
			}
			return index == 0 ?
				true :
				(xcc_write_word(ps.p->out, XWORD{XIS::PUT}) && xcc_write_word(ps.p->out, XWORD{index}) && xcc_write_word(ps.p->out, XWORD{XIS::SUB}));
		} else if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_FRAME)) {
			U16 index = parse_lit_index(ps);
			return index == 0 ?
				(xcc_write_word(ps.p->out, XWORD{U16(0)}) && xcc_write_word(ps.p->out, XWORD{XIS::RLC})) :
				(xcc_write_word(ps.p->out, XWORD{U16(0)}) && xcc_write_word(ps.p->out, XWORD{XIS::RLC}) && xcc_write_word(ps.p->out, XWORD{XIS::PUT}) && xcc_write_word(ps.p->out, XWORD{index}) && xcc_write_word(ps.p->out, XWORD{XIS::ADD}));
		} else if (match(ps.p, xtoken::KEYWORD_DIRECTIVE_BASE)) {
			U16 index = parse_lit_index(ps);
			return index == 0 ?
				(xcc_write_word(ps.p->out, XWORD{U16(0)}) && xcc_write_word(ps.p->out, XWORD{XIS::RLB})) :
				(xcc_write_word(ps.p->out, XWORD{U16(0)}) && xcc_write_word(ps.p->out, XWORD{XIS::RLB}) && xcc_write_word(ps.p->out, XWORD{XIS::PUT}) && xcc_write_word(ps.p->out, XWORD{index}) && xcc_write_word(ps.p->out, XWORD{XIS::ADD}));
		}
	}
	return false;
}

static bool try_var_addr(xcc_parser_state ps)
{
	// TODO Unify how this works with XB (xcc_write_rel) so that XASM can address variables declared in XB.
	token t = peek(ps.p);
	if (match(ps.p, token::ALIAS)) {
		U16 index = parse_lit_index(ps);
		xcc_symbol *sym = xcc_find_symbol(t.text, ps.p);
//		if (sym == NULL) { return false; }
//		return xcc_write_rel(ps.p, sym, index);
		if (sym == NULL || sym->category != xcc_symbol::VAR) { return false; }
		return
			xcc_write_word(ps.p->out, XWORD{(U16)(sym->data.u + index)}) &&
			xcc_write_word(ps.p->out, XWORD{XIS::RLC});
	}
	return false;
}

static bool try_var(xcc_parser_state ps)
{
	if (
		manage_state(
			match       (ps.p, xtoken::OPERATOR_DIRECTIVE_ADDR) &&
			try_var_addr(new_state(ps.end))
		)
	) {
		return true;
	}
	if (
		manage_state(
			try_var_addr(new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_lit_expr(xcc_parser_state ps, U16 &x)
{
	return false;
}

static bool try_directive_eval(xcc_parser_state ps)
{
	XWORD x;
	if (
		manage_state(
			match       (ps.p, xtoken::KEYWORD_DIRECTIVE_EVAL)                   &&
			match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)           &&
			try_lit_expr(new_state(xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), x.u) &&
			match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		return xcc_write_word(ps.p->out, x);
	}
	return false;
}

static bool try_var_size(xcc_parser_state ps)
{
	token t = peek(ps.p);
	if (match(ps.p, token::ALIAS)) {
		xcc_symbol *sym = xcc_find_symbol(t.text, ps.p);
		if (sym == NULL) { return false; }
		return xcc_write_word(ps.p->out, XWORD{sym->size});
	}
	return false;
}

static bool try_directive_size(xcc_parser_state ps)
{
	if (
		manage_state(
			match       (ps.p, xtoken::KEYWORD_DIRECTIVE_SIZE)              &&
			match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			try_var_size(new_state(xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
		)
	) {
		return true;
	}
	return false;
}

static bool try_lit_directive(xcc_parser_state ps)
{
	if (
		manage_state(
			try_directive_eval(new_state(ps.end)) ||
			try_directive_size(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_lit(xcc_parser_state ps)
{
	// TODO: Add support for index (especially useful for aliased literals and evals).
	token t;
	if (match(ps.p, xtoken::LITERAL_INT, &t)) {
		U16 index = parse_lit_index(ps);
		return xcc_write_word(ps.p->out, XWORD{(U16)(t.hash + index)});
	} else if (match(ps.p, token::ALIAS, &t)) {
		xcc_symbol *sym = xcc_find_symbol(t.text, ps.p);
		if (sym == NULL || sym->category != xcc_symbol::LIT) { return false; }
		U16 index = parse_lit_index(ps);
		return xcc_write_word(ps.p->out, XWORD{(U16)(sym->data.u + index)});
	} else if (manage_state(match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) && try_lit_directive(new_state(ps.end)))) {
		return true;
	}
	return false;
}

static bool try_param(xcc_parser_state ps)
{
	// NOTE: The order of these can cause trouble as some functions consume tokens despite failing.
	if (
		manage_state(
			try_directive_at(new_state(ps.end)) ||
			try_var         (new_state(ps.end)) ||
			try_lit         (new_state(ps.end)) ||
			try_reg         (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_putparam(xcc_parser_state ps)
{
	if (xcc_write_word(ps.p->out, XWORD{XIS::PUT}) && try_param(new_state(ps.end))) {
		return true;
	}
	return false;
}

static bool try_put_param(xcc_parser_state ps)
{
	if (
		manage_state(
			try_putparam(new_state(ps.end)) &&
			(
				!match       (ps.p, xtoken::OPERATOR_COMMA) ||
				try_put_param(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_directive_at(xcc_parser_state ps)
{
	if (
		manage_state(
			match    (ps.p, xtoken::OPERATOR_DIRECTIVE_AT) &&
			try_param(new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::AT});
	}
	return false;
}

static bool try_instruction_put(xcc_parser_state ps)
{
	if (
		manage_state(
			match        (ps.p, xtoken::KEYWORD_INSTRUCTION_PUT) &&
			try_put_param(new_state(xtoken::OPERATOR_STOP))
		)
	) {
		return true;
	}
	return false;
}

static bool try_repeat_param(xcc_parser_state ps, U16 repeat_instruction)
{
	if (
		manage_state(
			try_putparam  (new_state(ps.end))                    &&
			xcc_write_word(ps.p->out, XWORD{repeat_instruction}) &&
			(
				!match          (ps.p, xtoken::OPERATOR_COMMA) ||
				try_repeat_param(new_state(ps.end), repeat_instruction)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool try_instruction_with_put(xcc_parser_state ps, unsigned token_type, U16 i)
{
	if (match(ps.p, token_type)) {
		if (peek(ps.p).user_type == ps.end) {
			return xcc_write_word(ps.p->out, XWORD{i});
		}
		return manage_state(
			try_repeat_param(new_state(xtoken::OPERATOR_STOP), i)
		);
	}
	return false;
}

static bool try_instruction_jmp(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::KEYWORD_INSTRUCTION_JMP) &&
			xcc_write_word(ps.p->out, XWORD{XIS::PUT})            &&
			try_param     (new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::JMP});
	}
	return false;
}

static bool try_instruction_cjmp(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::KEYWORD_INSTRUCTION_CJMP) &&
			xcc_write_word(ps.p->out, XWORD{XIS::PUT})             &&
			try_param     (new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::CJMP});
	}
	return false;
}

static bool try_instruction_skip(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::KEYWORD_INSTRUCTION_SKIP) &&
			xcc_write_word(ps.p->out, XWORD{XIS::PUT})             &&
			try_param     (new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::SKIP});
	}
	return false;
}

static bool try_instruction_cskip(xcc_parser_state ps)
{
	if (
		manage_state(
			match         (ps.p, xtoken::KEYWORD_INSTRUCTION_CSKIP) &&
			xcc_write_word(ps.p->out, XWORD{XIS::PUT})              &&
			try_param     (new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::CSKIP});
	}
	return false;
}

static bool try_instruction_set(xcc_parser_state ps)
{
	if (
		manage_state(
			match       (ps.p, xtoken::KEYWORD_INSTRUCTION_SET) &&
			try_putparam(new_state(ps.end))                     &&
			match       (ps.p, xtoken::OPERATOR_COMMA)          &&
			try_putparam(new_state(ps.end))
		)
	) {
		return xcc_write_word(ps.p->out, XWORD{XIS::MOVD});
	}
	return false;
}

static bool try_instruction_toss(xcc_parser_state ps)
{
	if (manage_state(match(ps.p, xtoken::KEYWORD_INSTRUCTION_TOSS))) {
		return xcc_write_word(ps.p->out, XWORD{XIS::TOSS});
	}
	return false;
}

static bool try_instructions(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				try_instruction_nop     (new_state(ps.end))                                              ||
				try_instruction_clock   (new_state(ps.end))                                              ||
				try_instruction_at      (new_state(ps.end))                                              ||
				try_instruction_put     (new_state(ps.end))                                              ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_ADD,  XIS::ADD)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_SUB,  XIS::SUB)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_MUL,  XIS::MUL)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_DIV,  XIS::DIV)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_MOD,  XIS::MOD)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_IADD, XIS::IADD) ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_ISUB, XIS::ISUB) ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_IMUL, XIS::IMUL) ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_IDIV, XIS::IDIV) ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_IMOD, XIS::IMOD) ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_LSH,  XIS::LSH)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_RSH,  XIS::RSH)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_AND,  XIS::AND)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_OR,   XIS::OR)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_XOR,  XIS::XOR)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_MOVU, XIS::MOVU) ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_EQ,   XIS::EQ)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_NE,   XIS::NE)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_LE,   XIS::LE)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_GE,   XIS::GE)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_LT,   XIS::LT)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_GT,   XIS::GT)   ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_ILE,  XIS::ILE)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_IGE,  XIS::IGE)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_ILT,  XIS::ILT)  ||
				try_instruction_with_put(new_state(ps.end), xtoken::KEYWORD_INSTRUCTION_IGT,  XIS::IGT)  ||
				try_instruction_jmp     (new_state(ps.end))                                              ||
				try_instruction_cjmp    (new_state(ps.end))                                              ||
				try_instruction_skip    (new_state(ps.end))                                              ||
				try_instruction_cskip   (new_state(ps.end))                                              ||
				try_instruction_set     (new_state(ps.end))                                              ||
				try_instruction_toss    (new_state(ps.end))
			) &&
			match(ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool try_emit_lit_list(xcc_parser_state ps)
{
	while (manage_state(try_lit(new_state(ps.end)))) {
		if (peek(ps.p).user_type == ps.end) {
			return true;
		} else if (!match(ps.p, xtoken::OPERATOR_COMMA)) {
			break;
		}
	}
	return false;
}

static bool try_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			try_directive   (new_state(ps.end)) ||
			try_instructions(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool try_statements(xcc_parser_state ps)
{
	while (peek(ps.p).user_type != ps.end) {
		if (
			!manage_state(
				try_statement(new_state(ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

static bool try_scoped_statements(xcc_parser_state ps, U16 stack_offset)
{
	if (stack_offset > 0 && (!xcc_write_word(ps.p->out, XWORD{XIS::PUT}) || !xcc_write_word(ps.p->out, XWORD{stack_offset}) || !xcc_write_word(ps.p->out, XWORD{XIS::PUSH}))) {
		return false;
	}
	if (manage_state(try_statements(new_state(ps.end)))) {
		return
			stack_offset > 0 ?
			xcc_write_word(ps.p->out, XWORD{XIS::PUT})     &&
			xcc_write_word(ps.p->out, XWORD{stack_offset}) &&
			xcc_write_word(ps.p->out, XWORD{XIS::POP}) :
			true;
	}
	return false;
}

static bool try_program(xcc_parser_state ps)
{
	if (
		manage_state(
			xcc_write_word(ps.p->out, XWORD{XIS::SVB})  &&
			try_statements(new_state(ps.end))           &&
			xcc_write_word(ps.p->out, XWORD{XIS::LDB})  &&
			xcc_write_word(ps.p->out, XWORD{XIS::HALT})
		)
	) {
		return true;
	}
	return false;
}

xcc_out xasm(lexer l, xcc_binary memory, const U16 sym_capacity)
{
	xcc_symbol       sym_mem[sym_capacity]; // NOTE: There is a risk that many compilers will not allow declaring an array of a size not known at compile-time.
	xcc_parser       p  = xcc_init_parser(l, memory, sym_mem, sym_capacity);
	xcc_parser_state ps = xcc_new_state(&p, token::STOP_EOF);
	if (!manage_state(try_program(new_state(ps.end)))) {
		return { p.in.l, p.out, p.max, 1 };
	}
	return { p.in.l, p.out, p.max, 0 };
}

bool xasm_inline(xcc_parser_state ps)
{
	if (
		manage_state(
			try_statement(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

#undef new_state
#undef manage_state
#undef set_error
