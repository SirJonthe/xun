#include <iostream>
#include "xfs.h"

XFSUtility::Folder &XFSUtility::Folder::InsertFolder(const std::string &path)
{
	const size_t n = path.find_first_of('/');
	if (n != std::string::npos) {
		Folder &f = folders[path.substr(0, n)];
		return f.InsertFolder(path.substr(n + 1));
	}
	Folder &f = folders[path];
	f.parent = this;
	return f;
}

XFSUtility::File &XFSUtility::Folder::InsertFile(const std::string &path)
{
	const size_t n = path.find_first_of('/');
	if (n != std::string::npos) {
		Folder &f = folders[path.substr(0, n)];
		return f.InsertFile(path.substr(n + 1));
	}
	File &f = files[path];
	f.parent = this;
	return f;
}

void XFSUtility::Folder::Info(uint32_t level) const
{
	for (auto it = folders.begin(); it != folders.end(); it++) {
		for (uint32_t i = 0; i < level; ++i) {
			std::cout << "  ";
		}
		std::cout << it->first << ": " << it->second.folders.size() + it->second.files.size() << " entries" << std::endl;
		it->second.Info(level + 1);
	}
	for (auto it = files.begin(); it != files.end(); it++) {
		for (uint32_t i = 0; i < level; ++i) {
			std::cout << "  ";
		}
		std::cout << it->first << ": " << it->second.contents.size() << " bytes" << std::endl;
	}
}

uint32_t XFSUtility::ParseOctal(const char *str, uint32_t byte_count) const
{
	uint32_t result = 0;
	while (byte_count-- > 0 && *str) {
		result = (result << 3) | (*str++ - '0');
	}
	return result;
}

uint32_t XFSUtility::StrLen(const char *s, uint32_t max) const
{
	uint32_t c = 0;
	for (c = 0; c < max; ++c) {
		if (s[c] == 0) {
			break;
		}
	}
	return c;
}

std::string XFSUtility::GetPath(const char *prefix, const char *name) const
{
	std::string out;
	const uint32_t plen = StrLen(prefix, 155);
	const uint32_t nlen = StrLen(name, 100);
	out.reserve(plen + nlen);
	for (uint32_t i = 0; i < plen; ++i) { out.push_back(prefix[i]); }
	for (uint32_t i = 0; i < nlen; ++i) { out.push_back(name[i]); }
	return out;
}

bool XFSUtility::NewBlock( void )
{
	if (m_block_i + 1 >= m_num_blocks) {
		return false;
	}
	++m_block_i;
	XFSBlock &block = m_blocks[m_block_i];
	block.header.next  = { 0, 0 };
	block.header.prev  = { 0, 0 };
	block.header.entry = { 0, 0 };
	block.header.size  = 0;
	return true;
}

bool XFSUtility::Write(uint8_t x, Entry *p_entry)
{
	if (m_block_i >= m_num_blocks) {
		return false;
	}
	XFSBlock &block = m_blocks[m_block_i];
	block.data.bin[block.header.size++] = x;
	if (p_entry != nullptr) {
		++p_entry->size;
	}
	if (block.header.size <= 1) {
		block.header.next = Addr32{ 0, 0 };
		block.header.prev = m_block_i > 0 ? ToAddr32((m_block_i - 1) * sizeof(XFSBlock)) : Addr32{ 0, 0 };
		block.header.entry = m_block_i > 0 ? m_blocks[m_block_i - 1].header.entry : Addr32{ 0, 0 };
	} else if (m_blocks[m_block_i].header.size >= sizeof(XFSBlock::data) && !NewBlock()) {
		return false;
	}
	return true;
}

bool XFSUtility::WriteFile(const File &file, Entry *p_entry)
{
	if (!NewBlock()) {
		return false;
	}
	m_blocks[m_block_i].header.entry = ToAddr32(RelPtr(p_entry));
	p_entry->loc = ToAddr32(RelPtr(m_blocks + m_block_i));
	for (uint32_t i = 0; i < file.contents.size(); ++i) {
		if (!Write(file.contents[i], p_entry)) {
			return false;
		}
	}
	return true;
}

XFSUtility::Entry XFSUtility::NewEntry(const std::string &name, uint8_t type, Addr32 parent_fold_loc) const
{
	struct Lim
	{
		static uint32_t min(uint32_t a, uint32_t b) {
			return a < b ? a : b;
		}
	};

	Entry entry;
	entry.parent = parent_fold_loc;
	for (uint32_t j = 0; j < Lim::min(name.size(), 64); ++j) {
		entry.name[j] = name[j];
	}
	for (uint32_t j = Lim::min(name.size(), 64); j < 64; ++j) {
		entry.name[j] = 0;
	}
	entry.loc = Addr32{ 0, 0 };
	entry.size = 0;
	entry.type = type;
	entry.unused[0] = 0;
	entry.unused[1] = 0;
	entry.unused[2] = 0;
	return entry;
}

bool XFSUtility::WriteRoot(const Folder &root)
{
	if (!NewBlock()) {
		return false;
	}
	if (!Write(NewEntry("", ENTRYTYPE_FOLDER, Addr32{0, 0}), nullptr)) {
		return false;
	}
	return WriteFolder(root, m_blocks->data.rec);
}

bool XFSUtility::WriteFolder(const Folder &folder, Entry *p_entry)
{
	if (!NewBlock()) {
		return false;
	}
	const Addr32 entryi = p_entry != nullptr ? ToAddr32(RelPtr(p_entry)) : Addr32{ 0, 0 };
	m_blocks[m_block_i].header.entry = entryi;

	XFSBlock *block = m_blocks + m_block_i;
	p_entry->loc = ToAddr32(RelPtr(block));

	for (auto i = folder.folders.begin(); i != folder.folders.end(); i++) {

		// If we do not fit the entire header inside the remaining data in the block, we allocate a new block.
		if (sizeof(XFSBlock::data) - m_blocks[m_block_i].header.size < sizeof(Entry)) {
			if (!NewBlock()) {
				return false;
			}
			m_blocks[m_block_i].header.entry = entryi;
		}

		if (!Write(NewEntry(i->first, ENTRYTYPE_FOLDER, p_entry->loc), p_entry)) {
			return false;
		}
		std::cout << "recorded folder " << i->first << " to " << p_entry->loc.Flat() << std::endl;
	}
	for (auto i = folder.files.begin(); i != folder.files.end(); i++) {

		// If we do not fit the entire header inside the remaining data in the block, we allocate a new block.
		if (sizeof(XFSBlock::data) - m_blocks[m_block_i].header.size < sizeof(Entry)) {
			if (!NewBlock()) {
				return false;
			}
			m_blocks[m_block_i].header.entry = entryi;
		}

		if (!Write(NewEntry(i->first, ENTRYTYPE_FILE, p_entry->loc), p_entry)) {
			return false;
		}
	}
	
	Entry *entry = block->data.rec;
	for (auto i = folder.folders.begin(); i != folder.folders.end(); i++) {
		if (entry + 1 >= block->data.rec + sizeof(block->data.rec)) {
			block = GetPtr<XFSBlock>(block->header.next.Flat());
			entry = block->data.rec;
		}

		if (!WriteFolder(i->second, entry)) {
			return false;
		}
		++entry;
	}
	for (auto i = folder.files.begin(); i != folder.files.end(); i++) {
		if (entry + 1 >= block->data.rec + sizeof(block->data.rec)) {
			block = GetPtr<XFSBlock>(block->header.next.Flat());
			entry = block->data.rec;
		}

		if (!WriteFile(i->second, entry)) {
			return false;
		}
		++entry;
	}
	return true;
}

void XFSUtility::List(const XFSUtility::XFSBlock &b, uint8_t type, uint32_t level) const
{
	const Entry *entry;
	int32_t size;
	switch (type) {
	case ENTRYTYPE_FOLDER:
		size = b.header.size;
		entry = b.data.rec;
		while (size >= sizeof(Entry)) {
			for (uint32_t i = 0; i < level; ++i) {
				std::cout << "  ";
			}
			std::cout << (entry->type == ENTRYTYPE_FOLDER ? "+ " : "  ") << entry->name << " " << entry->size << " @" << entry->loc.Flat() << std::endl;
			if (entry->type == ENTRYTYPE_FOLDER && entry->loc.Flat() != 0) {
				List(*GetPtr<XFSBlock>(entry->loc.Flat()), entry->type, level + 1);
			}
			++entry;
			size -= sizeof(Entry);
		}

		break;
	case ENTRYTYPE_FILE:
	case ENTRYTYPE_NONE:
	default:
		break;
	}
}

uint32_t XFSUtility::RelPtr(const void *ptr) const
{
	return uintptr_t(ptr) - uintptr_t(m_blocks);
}

Addr32 XFSUtility::ToAddr32(uint32_t addr) const
{
	return Addr32{ U16(addr >> 16), U16(addr) };
}

bool XFSUtility::HealthCheckFolder(const XFSBlock *folder, const Entry *p_entry, std::vector<Addr32> &addr) const
{
	if (folder->header.size / sizeof(Entry) * sizeof(Entry) != folder->header.size) {
		std::cout << "[ERR] XFSUtility::HealthCheck(): entry for \"" << p_entry->name << "\" size not evenly divisible by " << sizeof(Entry) << std::endl;
		return false;
	}
	const Entry *e = folder->data.rec;
	int32_t size = folder->header.size;
	while (size > 0) {
		if (e->type != ENTRYTYPE_NONE && e->loc.Flat() == 0) {
			std::cout << "[ERR] XFSUtility::HealthCheck(): entry for \"" << e->name << "\" pointing to location 0" << std::endl;
			return false;
		}
		switch (e->type) {
		case ENTRYTYPE_FILE:
			if (!HealthCheckFile(GetPtr<XFSBlock>(e->loc.Flat()), e, addr)) {
				return false;
			}
			break;
		case ENTRYTYPE_FOLDER:
			if (!HealthCheckFolder(GetPtr<XFSBlock>(e->loc.Flat()), e, addr)) {
				return false;
			}
			break;
		case ENTRYTYPE_NONE:
			break;
		default:
			std::cout << "[ERR] XFSUtility::HealthCheck(): entry for \"" << e->name << "\" has unsupported type: " << uint32_t(e->type) << std::endl;
			return false;
		}
		size -= sizeof(Entry);
	}
	addr.pop_back();
	return true;
}

bool XFSUtility::HealthCheckFile(const XFSBlock *file, const Entry *p_entry, std::vector<Addr32> &addr) const
{
	// check file size and block link integrity
	return true;
}

XFSUtility::XFSUtility(Disk &disk) : m_disk(&disk), m_blocks(GetPtr<XFSBlock>(0)), m_num_blocks((disk.GetCapacity() - 4) / sizeof(XFSBlock)), m_block_i(0)
{}

void XFSUtility::Wipe( void )
{
	if (m_disk == nullptr) { return; }

	for (uint32_t i = 0; i < m_disk->GetCapacity(); ++i) {
		m_disk->GetData()[i] = 0;
	}
	m_block_i = UINT32_MAX;
}

bool XFSUtility::WriteTAR(const uint8_t *tar_binary, uint32_t byte_count)
{
	struct TARHeader
	{
		char name[100];     /*   0 */
		char mode[8];       /* 100 */
		char uid[8];        /* 108 */
		char gid[8];        /* 116 */
		char size[12];      /* 124 */
		char mtime[12];     /* 136 */
		char chksum[8];     /* 148 */
		char typeflag;      /* 156 */
		char linkname[100]; /* 157 */
		char magic[6];      /* 257 */
		char version[2];    /* 263 */
		char uname[32];     /* 265 */
		char gname[32];     /* 297 */
		char devmajor[8];   /* 329 */
		char devminor[8];   /* 337 */
		char prefix[155];   /* 345 */
		                    /* 500 */
		char padding[12];   /* 500 */
	};
	static constexpr uint32_t TAR_BLOCK = 512;

	if (m_disk == nullptr) { return false; }

	Wipe();

	Folder root;
	root.parent = nullptr;

	// Step 1: Convert TAR to internal format.
	TARHeader header;
	uint32_t i = 0;
	while (i < byte_count - sizeof(TARHeader)) {
		uint8_t *data = reinterpret_cast<uint8_t*>(&header);
		for (uint32_t j = 0; j < sizeof(TARHeader); ++j) {
			data[j] = tar_binary[i++];
		}

		if (header.name[0] == '\0') {
			break;
		}

		const unsigned size = ParseOctal(header.size, sizeof(header.size));
		std::string    path = GetPath(header.prefix, header.name);
		if (path[path.size() - 1] == '/') {
			path = path.substr(0, path.size() - 1);
		}

		switch (header.typeflag) {
		case 0:
		case '0': // File
			{
				File &f = root.InsertFile(path);
				f.contents.reserve(size);
				for (uint32_t n = 0; n < size; ++n) {
					f.contents.push_back(tar_binary[i + n]);
				}
			}
			break;
		
		case '5': // Folder
			root.InsertFolder(path);
			break;
		}

		const unsigned blocks = (size + TAR_BLOCK - 1) / TAR_BLOCK; // This is only for skipping ahead to the next header.
		i += blocks * TAR_BLOCK;
	}

	root.Info(0);

	// Step 2: Convert internal format to XFS
	return WriteRoot(root);
}

void XFSUtility::List( void ) const
{
	if (!HealthCheck()) { return; }
	List(*m_blocks, m_blocks->header.size != 0 ? ENTRYTYPE_FOLDER : ENTRYTYPE_NONE, 0);
}

bool XFSUtility::HealthCheck( void ) const
{
	if (m_disk == nullptr) {
		std::cout << "[ERR] XFSUtility::HealthCheck(): no disk attached" << std::endl;
		return false;
	}
	std::vector<Addr32> a;
	a.push_back(ToAddr32(0));
	XFSBlock *root = m_blocks;
	if (root->header.size != 0 && (root->header.entry.Flat() != 0 || root->header.prev.Flat() != 0)) {
		std::cout << "[ERR] XFSUtility::HealthCheck(): malformed root header" << std::endl;
		return false;
	}
	// 2) The sub entries must have a parent pointer to the last address on the vector
	// 3) A next pointer may not link back to a pointer existing on the vector
	// 4) sum of file sizes match entry size
	// 5) The linked list of blocks is unbroken in either direction.
	// 6) The linked list of blocks is acyclical
	return HealthCheckFolder(root, nullptr, a);
}