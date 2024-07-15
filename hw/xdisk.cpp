#include <map>
#include "xhwids.h"
#include "xdisk.h"

Disk::Disk(uint32_t capacity) : m_data(new uint8_t[capacity]), m_size(capacity)
{}

Disk::~Disk( void )
{
	delete [] m_data;
	m_size = 0;
}

uint8_t Disk::Read(Addr32 i)
{
	return m_data[i.Flat() % m_size];
}

void Disk::Write(Addr32 i, uint8_t data)
{
	m_data[i.Flat() % m_size] = data;
}

uint32_t Disk::GetCapacity( void ) const
{
	return m_size;
}

uint8_t *Disk::GetData( void )
{
	return m_data;
}

const uint8_t *Disk::GetData( void ) const
{
	return m_data;
}

bool DiskReader::HandlePacket(const Device::Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        return true;
		case Packet::TYPE_CONNECT:    return true;
		case Packet::TYPE_DISCONNECT: return true;
		case Packet::TYPE_PING:       return true;
		case Packet::TYPE_PONG:       return true;
		case Packet::TYPE_DATA:       return true;
		case MSG_TYPE_READ:
			// 2 words define the read location
			// word 1 is the high word
			// word 2 is the low word
			SetExternalState(1);
			if (msg.header[Packet::HEADER_SIZE] == 2) {
				Packet r = NewPacket(MSG_TYPE_READ);
				const Addr32 loc = { msg.payload[0], msg.payload[1] };
//				if (loc.Flat() < GetCapacity()) {
//					uint32_t n = 2;
//					const uint32_t next = loc + Packet::PAYLOAD_WORD_SIZE - 2 < GetCapacity() ? loc + Packet::PAYLOAD_WORD_SIZE - 2 : GetCapacity();
//					for (; n < next; ++n) {
//						r.payload[n] = Read(loc + n - 2).u;
//					}
//					for (; n < Packet::PAYLOAD_WORD_SIZE; ++n) {
//						r.payload[n] = 0;
//					}
//					r.payload[0] = (next & 0xffff0000) >> 16;
//					r.payload[1] = (next & 0xffff);
//					r.header[Packet::HEADER_SIZE] = next - loc;
//					Output(r);
//				} else {
//					Error("Read out of bounds");
//				}
			} else {
				Error("Payload size not 2");
			}
			return true;
		case MSG_TYPE_WRITE:
			// 2 words define the read location
			// word 1 is the high word
			// word 2 is the low word
			SetExternalState(1);
			if (msg.header[Packet::HEADER_SIZE] >= 2) {
//				const Addr32 loc = { msg.payload[0], msg.payload[1] };
//				if (loc < GetCapacity()) {
//					uint32_t n = 2;
//					const uint32_t next = loc + Packet::PAYLOAD_WORD_SIZE - 2 < GetCapacity() ? loc + Packet::PAYLOAD_WORD_SIZE - 2 : GetCapacity();
//					for (; n < next; ++n) {
//						Write(loc + n - 2, XWORD{msg.payload[n]});
//					}
//					Packet r = NewPacket(MSG_TYPE_WRITE);
//					r.payload[0] = (next & 0xffff0000) >> 16;
//					r.payload[1] = (next & 0xffff);
//					r.header[Packet::HEADER_SIZE] = 2;
//					Output(r);
//				} else {
//					Error("Write out of bounds");
//				}
			} else {
				Error("Payload size not at least 2");
			}
			return true;
		case MSG_TYPE_INFO:
			{
				SetExternalState(1);
				Packet p = NewPacket(MSG_TYPE_INFO);
				p.payload[0] = HasAttachment();
				p.payload[1] = (GetCapacity() & 0xff00) >> 16;
				p.payload[2] = GetCapacity() & 0x00ff;
				p.header[Packet::HEADER_SIZE] = 3;
				Output(p);
			}
			return true;
	}
	return false;
}

DiskReader::DiskReader( void ) : Device("XERXES(tm) Data Disk Reader", XHWID_DISK), m_attachment(nullptr)
{
	SetCyclesPerSecond(0);
}

uint8_t DiskReader::Read(Addr32 i)
{
	return HasAttachment() ? m_attachment->Read(i) : 0;
}

void DiskReader::Write(Addr32 i, uint8_t data)
{
	if (HasAttachment()) {
		m_attachment->Write(i, data);
	}
}

bool DiskReader::HasAttachment( void ) const
{
	return m_attachment != nullptr;
}

bool DiskReader::Attach(Disk *disk)
{
	if (HasAttachment()) {
		return false;
	}
	m_attachment = disk;
	return true;
}

void DiskReader::Eject( void )
{
	m_attachment = nullptr;
}

uint32_t DiskReader::GetCapacity( void ) const
{
	return HasAttachment() ? m_attachment->GetCapacity() : 0;
}
