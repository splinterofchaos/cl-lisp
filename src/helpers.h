
#pragma once

#include <llvm/ADT/StringRef.h>
#include <string>

std::string quote(const std::string& s, const std::string &q="\"");
std::string quote(char c);

void msg(const std::string& s);
void msg(const char *s);

template<typename Value>
using VarList = std::vector<std::pair<std::string, Value>>;

/// Returns an iterator to the variable called `name` in `vars`.
template<typename Value>
auto findVar(VarList<Value> &vars, llvm::StringRef name) {
  auto it = std::find_if(vars.rbegin(), vars.rend(),
                         [&](const auto &var) { return var.first == name; });
  // Convert to a forward iterator before returning.
  return it != vars.rend() ? it.base() - 1 : vars.end();
}

/// Gets `name` from `vars`. If it cannot be found, returns Value(0).
template<typename Value>
Value getVar(VarList<Value> &vars, llvm::StringRef name) {
  auto it = findVar(vars, name);
  if (it == vars.end())
    return Value(0);
  return it->second;
}

template<typename Value>
void popVar(VarList<Value> &vars) {
  vars.erase(vars.end() - 1);
}
