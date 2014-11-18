
#include <string>
#include <iostream>

#include "helpers.h"

std::string quote(const std::string& s, const std::string &q) {
  return q + s + q;
}

std::string quote(char c) {
  return quote(std::string(1,c), "'");
}

void msg(const std::string& s) { std::cout << s << std::endl; }
void msg(const char *s) { std::cout << s << std::endl; }
