
#pragma once

#include <string>

std::string quote(const std::string& s, const std::string &q="\"");
std::string quote(char c);

void msg(const std::string& s);
void msg(const char *s);

