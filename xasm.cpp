#include <sstream>
#include <map>
#include <iostream>
#include "lib/MiniLib/MTL/mtlString.h"
#include "xasm.h"

std::map<std::string, XIS::Enum> InitKeywords( void )
{
	std::map<std::string, XIS::Enum> keywords;
	for (int i = 0; i < XIS::COUNT; ++i) {
		keywords[ISTR[i]] = XIS::Enum(i);
	}
	return keywords;
}

std::string ToUpper(const std::string &str)
{
	std::string out;
	out.resize(str.size());
	for (size_t i = 0; i < out.size(); ++i) {
		out[i] = std::toupper(str[i]);
	}
	return out;
}

void ToUpper(const mtlChars &str, mtlString &out)
{
	out.SetSize(str.GetSize());
	for (int i = 0; i < out.GetSize(); ++i) {
		out[i] = std::toupper(str[i]);
	}
}

enum BaseFormat
{
	BASE_2,
	BASE_10,
	BASE_16
};

BaseFormat Base(const mtlChars &str)
{
	size_t i = 0;
	if (str.GetSize() > 0 && str[0] == '-') { ++i; }
	if (str.GetSize() > 2) {
		if (str[i] == '0' && str[i + 1] == 'b') { return BASE_2; }
		if (str[i] == '0' && str[i + 1] == 'x') { return BASE_16; }
	}
	return BASE_10;
}

BaseFormat Base(const std::string &str)
{
	return Base(mtlChars::FromDynamic(str.c_str(), str.size()));
}

bool IsNum(const mtlChars &str)
{
	if (str.GetSize() == 0) { return false; }

	BaseFormat base = Base(str);
	int i = 0;
	if (str[i] == '-') { ++i; }
	if (base != BASE_10) { i += 2; }
	for (; i < str.GetSize(); ++i) {
		if ((str[i] < '0' || str[i] > '9') && (base != BASE_16 || str[i] < 'A' || str[i] > 'F')) {
			return false;
		}
	}
	return true;
}

bool IsNum(const std::string &str)
{
	return IsNum(mtlChars::FromDynamic(str.c_str(), str.size()));
}

XWORD ToNum(const std::string &str)
{
	std::string num;
	num.reserve(str.size());

	BaseFormat base = Base(str);

	if (base != BASE_10) {
		size_t i = 2;
		if (str.size() > 0 && str[0] == '-') {
			num.push_back('-');
			++i;
		}
		for (; i < str.size(); ++i) {
			num.push_back(str[i]);
		}
	} else {
		num = str;
	}

	std::istringstream sin(num);
	int64_t out;
	sin >> out;

	XWORD x;
	if (out < 0) { x.i = out; }
	else { x.u = out; }

	return x;
}

XWORD ToNum(const mtlChars &str)
{
	std::string s(str.GetChars(), str.GetSize());
	return ToNum(s);
}

Binary Transform(std::list<XWORD> in)
{
	Binary out;
	out.Create(in.size());
	size_t n = 0;
	for (auto i = in.begin(); i != in.end(); ++i) {
		out[n++] = *i;
	}
	return out;
}

AssemblyResult AssembleXASM(const Source &file)
{
	AssemblyResult   out;
	std::list<XWORD> bin;

	std::istringstream sin(file.GetSource());
	std::string        token;

	std::map<std::string, XIS::Enum> keywords = InitKeywords();

	while (sin >> token) {
		token = ToUpper(token);
		auto keyw = keywords.find(token);
		if (keyw != keywords.end()) {
			bin.push_back({ U16(keyw->second) });
			if (keyw->second == XIS::PUT && sin >> token) {
				if (IsNum(token)) {
					bin.push_back(ToNum(token));
				} else {
					out.errors.push_back({ "Unknown symbol: " + token, 0 });
					break;
				}
			}
		} else {
			out.errors.push_back({ "No instruction: " + token, 0 });
			break;
		}
	}

	if (out.errors.size() == 0) {
		out.binary = Transform(bin);
	}

	return out;
}
