#include "xasm2.h"
#include "MiniLib/MTL/mtlParser.h"

const mtlChars registers[] = {
	"$s",                       // stack pointer
	"$i",                       // instruction pointer
	"$x1", "$x2", "$x3", "$x4", // internal registers, x4 is used to store errors
	"$m",                       // mark pointer (entry point for function)
	"$p"                        // program pointer (entry point for program, all absolute addresses are offset by this)
	"$a", "$b", "$c",           // general purpose registers
};

void xerx::Assembler::ClearState( void )
{
	m_error.type = 0;
	m_error.loc = 0;
	m_error.file.Free();
	m_error.msg.Free();
	m_file_system = nullptr;
}

bool xerx::Assembler::BufferFile(const mtlChars &file, mtlString &out)
{
	if (m_file_system == nullptr) { // load from disk
		if (mtlBufferFile(file, out) == false) {
			return false;
		}
	} else { // load from file_system
		xerx::uword hnd = m_file_system->GetHandle(file);
		xerx::udword size = 0;
		xerx::Binary bin;
		if (hnd != 0 && m_file_system->GetSize(hnd, size) == true && m_file_system->BeginReadBin(hnd, 0, size, bin) == true) {
			out.SetSize(int(bin.GetSize()));
			if (xerx::udword(out.GetSize()) != bin.GetSize()) {
				return false;
			}
			for (int i = 0; i < out.GetSize(); ++i) {
				out[i] = bin[xerx::udword(i)].u; // BUG: Binary stores two chars for every element. Fix this.
			}

			m_file_system->EndReadBin(hnd);
		} else {
			return false;
		}
	}

	return true;
}

bool xerx::Assembler::Assemble(const mtlChars &text)
{
	mtlSyntaxParser p;
	p.SetBuffer(text);
	mtlArray<mtlChars> m;
	while (p.IsEnd() == false) {
		switch (p.Match("#%s# %| %w %s.", m)) {
		case 0: // comment
			break;
		case 1:
			if (AssembleInstruction(m[0], m[1]) == false) {
				return false;
			}
			break;
		default:
			return false;
		}
	}
	return true;
}

xerx::uword xerx::Assembler::FindDef(const mtlChars &def)
{
	// search top
	// search bottom (global)

	mtlItem<Def> *d = nullptr;

	StackFrame &top = m_frames.GetLast()->GetItem();
	d = top.defs.GetFirst();
	while (d != nullptr) {
		if (d->GetItem().alias.Compare(def) == true) {
			return d->GetItem().val;
		}
		d = d->GetNext();
	}

	StackFrame &bot = m_frames.GetFirst()->GetItem();
	d = bot.defs.GetFirst();
	while (d != nullptr) {
		if (d->GetItem().alias.Compare(def) == true) {
			return d->GetItem().val;
		}
		d = d->GetNext();
	}

	return 0;
}

bool xerx::Assembler::AssembleInstruction(const mtlChars &instr, const mtlChars &params)
{
	xerx::uword i = FindInstruction(instr);
	if (i == xerx::InstructionSet::LAST) {
		return false;
	}
	PushBinary(i);

	mtlSyntaxParser p;
	p.SetBuffer(params);
	mtlArray<mtlChars> m;

	while (p.IsEnd() == false) {
		xerx::uword p = 0;
		switch (p.Match("%s, %| %s", m)) {
		case 0:
			// there are more params to follow
			p = FindDef(m[0]);
			if (p == 0) {
				return false;
			}
			PushBinary(p);
			break;
		case 1:
			// this is the last param
			p = FindDef(m[0]);
			if (p == 0) {
				return false;
			}
			PushBinary(p);
			return p.IsEnd() == true;
		default:
			return false;
		}
	}

	return false;
}

bool xerx::Assembler::PrintToFile(xerx::Binary &out) const
{
	if (out.Allocate(xerx::udword(m_bin.GetSize())) == false) {
		return false;
	}
	xerx::uword i = 0;
	const mtlItem<xerx::word_t> *n = m_bin.GetFirst();
	while (n != nullptr) {

		out[i] = n->GetItem();

		++i;
		n = n->GetNext();
	}
}

bool xerx::Assembler::Assemble(const mtlChars &entry_file, xerx::Binary &out, xerx::FileSystem *file_system)
{
	// buffer file
	// emit boiler plate code
	// $global():
		// first instructions are global initializations
		// call main().
	// end $global.

	ClearState();

	m_file_system = file_system;
	out.Delete();

	mtlString file_bin;
	if (BufferFile(entry_file, file_bin) == false) {
		return false;
	}
	if (Assemble(file_bin) == false) {
		return false;
	}
	if (PrintToFile(out) == false) {
		return false;
	}

	ClearState();

	return true;
}
