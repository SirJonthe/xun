#include <iostream>

#include "xasm.h"
#include "xis.h"
#include "xun.h"

#include "MiniLib/MTL/mtlParser.h"

struct Def
{
	mtlString   name;
	xerx::uword addr;
	xerx::uword size;
};
struct Txt
{
	mtlString name;
	mtlString subst;
};
struct FwdLblRef
{
	mtlString             name;
	mtlItem<xerx::uword> *i;
};
struct Scope
{
	mtlList<Def>       var;
	mtlList<Def>       lbl;
	mtlList<Txt>       txt;
	mtlList<FwdLblRef> fwd_ref;
	xerx::uword        start;
	xerx::uword        end;
};
struct Assembly
{
	mtlList<xerx::uword> instr;
	mtlList<Scope>       scopes;
};

void PrintMsg(const mtlChars &msg)
{
	for (int i = 0; i < msg.GetSize(); ++i) {
		std::cout << msg[i];
	}
	std::cout << std::endl;
}
void PrintMsg(int line, const mtlChars &msg)
{
	std::cout << "(" << line << ") ";
	PrintMsg(msg);
}

void PrintMsg(const mtlChars &msg1, const mtlChars &msg2)
{
	for (int i = 0; i < msg1.GetSize(); ++i) {
		std::cout << msg1[i];
	}
	std::cout << ": ";
	for (int i = 0; i < msg2.GetSize(); ++i) {
		std::cout << msg2[i];
	}
	std::cout << std::endl;
}
void PrintMsg(int line, const mtlChars &msg1, const mtlChars &msg2)
{
	std::cout << "(" << line << ") ";
	PrintMsg(msg1, msg2);
}

enum ParamType
{
	LIT1 = 1,
	VAR1 = LIT1 << 1,
	LIT2 = LIT1 << 2,
	VAR2 = LIT1 << 3,
	LIT3 = LIT1 << 4,
	VAR3 = LIT1 << 5
};

struct ParamInfo
{
	mtlChars    name;
	int         dref;
	xerx::uword bits;
	enum {
		Undef = 0,
		Lit = 1,
		Var = 2
	} btype;
};

struct InstrInfo
{
	mtlChars                   str;
	xerx::InstructionSet::Type instr;
	xerx::udword               cnt;
	xerx::udword               param;
};

static const InstrInfo i_info[xerx::InstructionSet::LAST + 1] = {
	{ "nop",  xerx::InstructionSet::NOP,  0, 0 },
	{ "halt",  xerx::InstructionSet::HALT,  0, 0 },

//	{ "inc",  xerx::InstructionSet::INC,  1, VAR1 },
//	{ "dec",  xerx::InstructionSet::DEC,  1, VAR1 },

//	{ "jmp",  xerx::InstructionSet::JMP,  1, VAR1|LIT1 },

//	{ "sav",  xerx::InstructionSet::SAV,  1, VAR1 },
//	{ "use",  xerx::InstructionSet::USE,  1, VAR1 },

	{ "push", xerx::InstructionSet::PUSH, 1, LIT1 },
	{ "pop",  xerx::InstructionSet::POP,  1, LIT1 },

	{ "not",  xerx::InstructionSet::NOT,  1, VAR1 },
	{ "neg",  xerx::InstructionSet::NEG,  1, VAR1 },

	{ "dref", xerx::InstructionSet::DREF, 2, VAR1|VAR2 },

	{ "set",  xerx::InstructionSet::SET,  2, VAR1|LIT2 },
	{ "mov",  xerx::InstructionSet::MOV,  2, VAR1|VAR2|LIT2 },
//	{ "dset", xerx::InstructionSet::DSET, 2, VAR1|LIT2 },
	{ "dmov", xerx::InstructionSet::DMOV, 2, VAR1|VAR2|LIT2 },

	{ "and",  xerx::InstructionSet::AND,  2, VAR1|VAR2|LIT2 },
	{ "or",   xerx::InstructionSet::OR,   2, VAR1|VAR2|LIT2 },
	{ "xor",  xerx::InstructionSet::XOR,  2, VAR1|VAR2|LIT2 },

	{ "lsu",  xerx::InstructionSet::LSU,  2, VAR1|VAR2|LIT2 },
	{ "rsu",  xerx::InstructionSet::RSU,  2, VAR1|VAR2|LIT2 },
	{ "lss",  xerx::InstructionSet::LSS,  2, VAR1|VAR2|LIT2 },
	{ "rss",  xerx::InstructionSet::RSS,  2, VAR1|VAR2|LIT2 },

	{ "add",  xerx::InstructionSet::ADD,  2, VAR1|VAR2|LIT2 },
	{ "sub",  xerx::InstructionSet::SUB,  2, VAR1|VAR2|LIT2 },
	{ "mul",  xerx::InstructionSet::MUL,  2, VAR1|VAR2|LIT2 },
	{ "div",  xerx::InstructionSet::DIV,  2, VAR1|VAR2|LIT2 },
	{ "mod",  xerx::InstructionSet::MOD,  2, VAR1|VAR2|LIT2 },

	{ "int",  xerx::InstructionSet::INT,  2, VAR1|LIT1|VAR2|LIT2 },

	{ "je",   xerx::InstructionSet::JE,   3, VAR1|LIT1|VAR2|LIT2|VAR3|LIT3 },
	{ "jne",  xerx::InstructionSet::JNE,  3, VAR1|LIT1|VAR2|LIT2|VAR3|LIT3 },
	{ "jl",   xerx::InstructionSet::JL,   3, VAR1|LIT1|VAR2|LIT2|VAR3|LIT3 },
	{ "jle",  xerx::InstructionSet::JLE,  3, VAR1|LIT1|VAR2|LIT2|VAR3|LIT3 },
	{ "jg",   xerx::InstructionSet::JG,   3, VAR1|LIT1|VAR2|LIT2|VAR3|LIT3 },
	{ "jge",  xerx::InstructionSet::JGE,  3, VAR1|LIT1|VAR2|LIT2|VAR3|LIT3 },

	{ "str",  xerx::InstructionSet::SET,  2,  VAR1|LIT2 } // special case (no corresponding instruction on XUN)
};

const mtlChars registers = "SP, IP, X1, X2, X3, X4, A, B, C";

const InstrInfo *FindInstruction(const mtlChars &op)
{
	for (int i = 0; i < (int)xerx::InstructionSet::LAST + 1; ++i) {
		if (i_info[i].str.Compare(op)) { return &i_info[i]; }
	}
	return NULL;
}

bool ToNumber(const mtlChars &str, const mtlList<Scope> &scopes, xerx::uword &num);

const Def *FindVariable(const mtlChars &var, xerx::uword &off, const mtlList<Scope> &scopes)
{
	mtlSyntaxParser p;
	p.SetBuffer(var);
	mtlArray<mtlChars> m;

	mtlChars name = var;
	if (!p.IsEnd() && p.Match("%w", m) == 0) {
		name = m[0];
	} else {
		return NULL;
	}

	xerx::uword num = 0;
	while (!p.IsEnd()) {
		switch (p.Match("[%S]", m)) {
		case 0:
			if (ToNumber(m[0], scopes, num)) {
				off += num;
			} else {
				return NULL;
			}
			break;
		default:
			return NULL;
		}
	}

	const mtlItem<Scope> *s = scopes.GetLast();
	while (s != NULL) {
		const mtlItem<Def> *v = s->GetItem().var.GetFirst();
		while (v != NULL) {
			if (v->GetItem().name.Compare(name)) { return &v->GetItem(); }
			v = v->GetNext();
		}
		s = s->GetPrev();
	}
	return NULL;
}

const Def *FindLabel(const mtlChars &lbl, const mtlList<Scope> &scopes)
{
	const mtlItem<Scope> *s = scopes.GetLast();
	const mtlItem<Def> *l = s->GetItem().lbl.GetFirst();
	while (l != NULL) {
		if (l->GetItem().name.Compare(lbl)) { return &l->GetItem(); }
		l = l->GetNext();
	}
	return NULL;
}

bool ToNumber(const mtlChars &str, const mtlList<Scope> &scopes, xerx::uword &num)
{
	const mtlChars digits = "0123456789abcdef";
	mtlSyntaxParser p;
	p.SetBuffer(str);
	mtlArray<mtlChars> m;
	const Def *def = NULL;
	int base = 0;
	int sign = 1;
	xerx::uword off  = 0;
	switch (p.Match("- %| +")) {
	case 0:
		sign = -1;
		break;
	case 1:
		sign = 1;
		break;
	}
	switch (p.Match("%?(d:)%i%0 %| h:%w%0 %| b:%i%0 %| &%w %| %s", m)) {
	case 0:
		base = 10;
		m[0] = m[1];
		break;
	case 1:
		base = 16;
		break;
	case 2:
		base = 2;
		return false; // Parser fails to recognize 16 digits as an int (mtlString::ToInt fails)
		break;
	case 3:
		def = FindVariable(m[0], off, scopes);
		if (def == NULL) { return false; }
		num = def->addr + off;
		return true;
	case 4:
		def = FindLabel(m[0], scopes);
		if (def == NULL) { return false; }
		num = def->addr;
		return true;
	default:
		break;
	}

	int x = 0;
	for (int i = 0; i < m[0].GetSize(); ++i) {
		int n;
		for (n = 0; n < digits.GetSize(); ++n) {
			if (mtlChars::ToLower(m[0][i]) == digits[n]) { break; }
		}
		if (n >= base) {
			PrintMsg("Wrong base", str);
			return false;
		}
		x = x * base + n;
	}

	x = x * sign;

	if (x < xerx::SMIN || x > xerx::UMAX) {
		PrintMsg("Overflow/underflow", str);
		return false;
	}
	num = (xerx::uword)x;

	return true;
}

bool IsValidName(const mtlChars &name)
{
	if (name.GetSize() < 1) {
		PrintMsg("No name");
		return false;
	}
	if (!mtlChars::IsAlpha(name[0]) && mtlChars::ToLower(name[0]) != '_') {
		PrintMsg("Invalid char in name", name);
		return false;
	}
	for (int i = 1; i < name.GetSize(); ++i) {
		if (!mtlChars::IsAlphanumeric(name[i]) && mtlChars::ToLower(name[i]) != '_') {
			PrintMsg("Invalid char in name", name);
			return false;
		}
	}
	if (FindInstruction(name) != NULL) {
		PrintMsg("Reserved name", name);
		return false;
	} else {
		mtlSyntaxParser p;
		p.SetBuffer(registers);
		mtlArray<mtlChars> m;
		while (!p.IsEnd()) {
			switch (p.Match("%s, %| %s", m)) {
			case 0:
			case 1:
				if (name.Compare(m[0])) {
					PrintMsg("Reserved name", name);
					return false;
				}
				break;
			}
		}
	}

	return true;
}

ParamInfo ClassifyParameter(const mtlChars &param, const mtlList<Scope> &scopes)
{
	ParamInfo pi;
	pi.dref = 0;
	pi.btype = ParamInfo::Undef;
	mtlSyntaxParser p;
	p.SetBuffer(param);
	mtlArray<mtlChars> m;
	while (p.Match("@") == 0) {
		++pi.dref;
	}
	mtlChars name = p.GetBufferRemaining().GetTrimmed();
	if (ToNumber(name, scopes, pi.bits)) {
		if (p.Match("&") == 0) {
			--pi.dref;
		}
		pi.name = name;
		xerx::uword off = 0;
		if (FindVariable(p.GetBufferRemaining().GetTrimmed(), off, scopes) != NULL) {
			++pi.dref;
			pi.bits += off;
		}
		pi.btype = pi.dref < 0 ? ParamInfo::Undef : (pi.dref == 0 ? ParamInfo::Lit : ParamInfo::Var);
	} else {
		++pi.dref;
		xerx::uword off = 0;
		const Def *v = FindVariable(name, off, scopes);
		pi.name = name;
		if (v != NULL) {
			pi.name = v->name;
			pi.btype = ParamInfo::Var;
			pi.bits = v->addr + off;
		}
	}
	return pi;
}

bool AssembleInstruction(const mtlChars &op, const mtlChars &params, mtlList<xerx::uword> &instr, mtlList<Scope> &scopes)
{
	const InstrInfo *i = FindInstruction(op);
	if (i == NULL) {
		PrintMsg("Unknown instr", op);
		return false;
	}
	xerx::udword param_bits = i->param;
	mtlSyntaxParser p;
	p.SetBuffer(params);
	mtlArray<mtlChars> m;
	int c = 0;

	if (i->instr == xerx::InstructionSet::HALT) {
		mtlString sp_param;
		sp_param.FromInt(scopes.GetLast()->GetItem().end - scopes.GetFirst()->GetItem().start);
		AssembleInstruction("POP", sp_param, instr, scopes);
		//sp_param.Append("SP,").AppendInt(scopes.GetLast()->GetItem().end - scopes.GetFirst()->GetItem().start);
		//AssembleInstruction("SUB", sp_param, instr, scopes);
	} else if (i->str.Compare("STR")) {
		if (p.Match("%S,\"%s\"%0", m) == 0) {
			mtlString null_str;
			null_str.Copy(m[1]);
			for (int i = 0; i < (null_str.GetSize() / 2) + 1; ++i) {
				xerx::uword ch2i = (((xerx::uword)null_str[i*2]+1) << 8) | ((xerx::uword)null_str[(i*2)]);
				mtlString str_arg;
				str_arg.Append(m[0]).Append("[").AppendInt(i).Append("],").AppendInt((int)ch2i);
				if (!AssembleInstruction("SET", str_arg, instr, scopes)) { return false; }
			}
		} else {
			PrintMsg("Unexpected param", params);
			return false;
		}
		return true;
	}

	mtlList<xerx::uword> t;
	t.AddLast(i->instr);
	mtlList<ParamInfo> pi;
	ParamInfo param;

	mtlString dmov;

	while (!p.IsEnd()) {
		switch (p.Match("%S, %| %S", m)) {
		case 0:
		case 1:
			param = ClassifyParameter(m[0], scopes);
			if ((param.btype & param_bits) == 0 && param.btype != ParamInfo::Undef) {
				PrintMsg("Unexpected param", m[0]);
				return false;
			}
			if (param.dref > 1) {
				mtlString x_reg;
				x_reg.Append("X").AppendInt(c + 1);
				mtlString dref1;
				dref1.Append(x_reg).Append(",").Append(param.name);
				mtlString dref2;
				dref2.Append(x_reg).Append(",").Append(x_reg);
				AssembleInstruction("DREF", dref1, instr, scopes);
				for (int i = 2; i < param.dref; ++i) {
					AssembleInstruction("DREF", dref2, instr, scopes);
				}
				mtlChars dst = param.name; // save original name before reclassifying, or else we will do a DMOV from X to X, i.e. no work
				param = ClassifyParameter(x_reg, scopes); // reclassify
				if (param.btype == ParamInfo::Undef) { return false; }
				// if this is the DST param, emit a DMOV instruction (except for when instruction is a conditional)
				if (c == 0 && (i->instr < xerx::InstructionSet::JE || i->instr > xerx::InstructionSet::JGE)) {
					dmov.Append(dst).Append(",").Append(x_reg);
				}

			} else if ((param.btype == ParamInfo::Lit || param.btype == ParamInfo::Undef) && (param_bits & (ParamInfo::Lit|ParamInfo::Var)) == (ParamInfo::Lit|ParamInfo::Var)) {
				mtlString x_reg;
				x_reg.Append("X").AppendInt(c + 1);
				mtlString dref1;
				dref1.Append(x_reg).Append(",").Append(param.name);
				AssembleInstruction("SET", dref1, instr, scopes);
				param = ClassifyParameter(x_reg, scopes); // reclassify
				if (param.btype == ParamInfo::Undef) { return false; }
			}
			t.AddLast(param.bits);
			pi.AddLast(param);
			param_bits = param_bits >> 2;
			++c;
			break;

		default:
			break;
		}
	}

	if (c != (int)i->cnt) {
		PrintMsg("Param count mismatch", params);
		return false;
	}

	mtlItem<xerx::uword> *it = t.GetFirst();
	instr.AddLast(it->GetItem());
	it = it->GetNext();
	mtlItem<ParamInfo>   *pit = pi.GetFirst();
	while (it != NULL && pit != NULL) {
		instr.AddLast(it->GetItem());
		if (pit->GetItem().btype == ParamInfo::Undef) {
			scopes.GetLast()->GetItem().fwd_ref.AddLast();
			scopes.GetLast()->GetItem().fwd_ref.GetLast()->GetItem().name.Copy(pit->GetItem().name);
			scopes.GetLast()->GetItem().fwd_ref.GetLast()->GetItem().i = instr.GetLast();
		}
		it = it->GetNext();
		pit = pit->GetNext();
	}

	// Emit an instruction to write back to memory if dereferencing operator was used
	if (dmov.GetSize() > 0) {
		AssembleInstruction("DMOV", dmov, instr, scopes);
	}

	bool ret_val = it == NULL && pit == NULL;
	if (!ret_val) {
		PrintMsg("Logic error (my bad)");
	}
	return ret_val;
}

bool AssembleText(const mtlChars &name, const mtlChars &subst, mtlList<Scope> &scopes)
{
	if (!IsValidName(name)) { return false; }
	scopes.GetLast()->GetItem().txt.AddLast();
	scopes.GetLast()->GetItem().txt.GetLast()->GetItem().name.Copy(name);
	scopes.GetLast()->GetItem().txt.GetLast()->GetItem().subst.Copy(subst);
	return true;
}

bool AddLabel(const mtlChars &name, bool validate_name, mtlList<xerx::uword> &instr, mtlList<Scope> &scopes)
{
	if (validate_name && !IsValidName(name)) {
		PrintMsg("Naming", name);
		return false;
	}
	mtlItem<Def> *i = scopes.GetLast()->GetItem().lbl.GetFirst();
	while (i != NULL) {
		if (i->GetItem().name.Compare(name)) {
			PrintMsg("Label redef", name);
			return false;
		}
		i = i->GetNext();
	}
	i = scopes.GetLast()->GetItem().var.GetFirst();
	while (i != NULL) {
		if (i->GetItem().name.Compare(name)) {
			PrintMsg("Label redef", name);
			return false;
		}
		i = i->GetNext();
	}
	scopes.GetLast()->GetItem().lbl.AddLast();
	scopes.GetLast()->GetItem().lbl.GetLast()->GetItem().name.Copy(name);
	scopes.GetLast()->GetItem().lbl.GetLast()->GetItem().addr = xerx::NanoController::IP_START - instr.GetSize();
	scopes.GetLast()->GetItem().lbl.GetLast()->GetItem().size = 0;
	return true;
}

bool AddVar(const mtlChars &label, const mtlChars &name, const mtlChars &size_str, mtlList<Scope> &scopes)
{
	if (scopes.GetSize() > 1 && !IsValidName(name)) {
		PrintMsg("Naming", name);
		return false;
	}
	int size = 1;
	if (size_str.GetSize() > 0 && (!size_str.ToInt(size) || size < 1 || size >= xerx::UMAX)) {
		PrintMsg("Invalid size", size_str);
		return false;
	}
	mtlItem<Def> *i = scopes.GetLast()->GetItem().var.GetFirst();
	while (i != NULL) {
		if (i->GetItem().name.Compare(name)) {
			PrintMsg("Var redef", name);
			return false;
		}
		i = i->GetNext();
	}
	if (label.Compare(name)) {
		PrintMsg("Label redef", name);
		return false;
	}
	scopes.GetLast()->GetItem().var.AddLast();
	scopes.GetLast()->GetItem().var.GetLast()->GetItem().name.Copy(name);
	scopes.GetLast()->GetItem().var.GetLast()->GetItem().addr = scopes.GetLast()->GetItem().end;
	scopes.GetLast()->GetItem().var.GetLast()->GetItem().size = size;
	scopes.GetLast()->GetItem().end += size;

	return true;
}

bool ResolveForwardLabels(mtlList<Scope> &scopes)
{
	mtlItem<FwdLblRef> *f = scopes.GetLast()->GetItem().fwd_ref.GetFirst();
	while (f != NULL) {
		const Def *lbl = FindLabel(f->GetItem().name, scopes);
		if (lbl != NULL) {
			f->GetItem().i->GetItem() = lbl->addr;
		} else {
			PrintMsg("Undef label", f->GetItem().name);
			return false;
		}
		f = f->GetNext();
	}
	return true;
}

bool AssembleScope(const mtlChars &contents, mtlList<xerx::uword> &instr, mtlList<Scope> &scopes);

bool AssembleLabel(const mtlChars &label, const mtlChars &defs, const mtlChars &code, mtlList<xerx::uword> &instr, mtlList<Scope> &scopes)
{
	if (scopes.GetSize() > 0) {
		if (!AddLabel(label, true, instr, scopes)) { return false; }
	}

	mtlString end_label;
	end_label.Append("/").Append(label);

	scopes.AddLast();
	scopes.GetLast()->GetItem().start = (scopes.GetSize() > 1) ? scopes.GetLast()->GetPrev()->GetItem().end : xerx::NanoController::SP;
	scopes.GetLast()->GetItem().end = scopes.GetLast()->GetItem().start;
	if (!AddLabel(label, true, instr, scopes)) { return false; }

	mtlArray<mtlChars> m;
	mtlSyntaxParser p;
	p.SetBuffer(defs);
	while (!p.IsEnd()) {
		switch (p.Match("%w[%S], %| %w, %| %w[%S] %| %w", m)) {
		case 0:
		case 2:
			if (!AddVar(label, m[0], m[1], scopes)) { return false; }
			break;

		case 1:
		case 3:
			if (!AddVar(label, m[0], "", scopes)) { return false; }
			break;

		default:
			PrintMsg("Syntax error", defs);
			return false;
		}
	}

	xerx::uword stack_change = scopes.GetLast()->GetItem().end - scopes.GetLast()->GetItem().start;
	mtlString sp_param;
	sp_param.FromInt(stack_change);

	xerx::uword outer_i_start = instr.GetSize();
	if (stack_change > 0) {
		AssembleInstruction("PUSH", sp_param, instr, scopes);
	}
	xerx::uword inner_i_start = instr.GetSize();
	if (!AssembleScope(code, instr, scopes)) { return false; }
	xerx::uword inner_i_end = instr.GetSize();

	scopes.GetLast()->GetItem().lbl.GetLast()->GetItem().size = inner_i_end - inner_i_start;
	AddLabel(end_label, false, instr, scopes);

	if (!ResolveForwardLabels(scopes)) { return false; }

	if (stack_change > 0) {
		AssembleInstruction("POP", sp_param, instr, scopes);
	}
	xerx::uword outer_i_end = instr.GetSize();

	scopes.RemoveLast();
	if (scopes.GetSize() > 0) {
		scopes.GetLast()->GetItem().lbl.GetLast()->GetItem().size = outer_i_end - outer_i_start;
		AddLabel(end_label, false, instr, scopes);
	}

	return true;
}

bool LoadFile(const mtlChars &file, mtlString &code)
{
	if (!mtlBufferFile(file, code)) {
		PrintMsg("File not found", file);
		return false;
	}
	return true;
}

bool AssembleScope(const mtlChars &contents, mtlList<xerx::uword> &instr, mtlList<Scope> &scopes)
{
	mtlSyntaxParser p;
	mtlArray<mtlChars> m;
	p.SetBuffer(contents);
	while (!p.IsEnd()) {
		switch (p.Match("#%s# %| text %w,\"%S\". %| ins \"%S\". %| %w:{%s} %| %w: %| %w;%S:{%s} %| %w %s.", m)) {
		case 0:
			// comment
			break;

		case 1:
			// same as a define, need to store it internally somewhere
			if (!AssembleText(m[0], m[1], scopes)) { return false; }
			break;

		case 2:
		{
			//mtlString code;
			//if (!LoadFile(m[0], code) || !AssembleScope(code, instr, scopes)) { return false; }
			PrintMsg("Unimplemented feature", "ins");
			return false;
			break;
		}

		case 3:
			if (!AssembleLabel(m[0], "", m[1], instr, scopes)) { return false; }
			break;

		case 4:
			if (!AssembleLabel(m[0], "", "", instr, scopes)) { return false; }
			break;

		case 5:
			if (!AssembleLabel(m[0], m[1], m[2], instr, scopes)) { return false; }
			break;

		case 6:
			if (!AssembleInstruction(m[0], m[1], instr, scopes)) { return false; }
			break;

		default:
			PrintMsg("Unknown syntax", p.GetBufferRemaining());
			return false;
		}
	}
	return true;
}

bool ToBinary(const mtlList<xerx::uword> &instr, xerx::Binary &out)
{
	if (!out.Allocate(instr.GetSize() + 1)) {
		PrintMsg("Too big bin");
		return false;
	}
	out[out.GetSize() - 1].u = xerx::InstructionSet::HALT;
	xerx::uword n = 0;
	const mtlItem<xerx::uword> *i = instr.GetFirst();
	while (i != NULL) {
		out[n].u = i->GetItem();
		++n;
		i = i->GetNext();
	}
	return true;
}

bool xerx::Assemble(const mtlChars &file, xerx::Binary &out)
{
	out.Delete();
	mtlList<Scope> scopes;
	mtlList<xerx::uword> instr;
	mtlString code;
	return
		LoadFile(file, code) &&
		AssembleLabel("MAIN", registers, code, instr, scopes) &&
		ToBinary(instr, out);
}
