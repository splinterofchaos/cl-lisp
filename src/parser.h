
#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "ast.h"

struct Reader
{
  std::string filename;
  std::istream& source;
  unsigned lnum;

  std::vector<std::unique_ptr<SExpr>> commands;

  Reader(std::string name, std::istream& is)
    : source(is)
  {
    filename = name;
    lnum = 0;
  }

  std::string curline;
  size_t linepos = 0;
  char previous = 0;

  unsigned depth = 0;

  char getc() {
    char c;
    if (!source.get(c))
      return EOF;
    linepos++;
    if (c == '\n') {
      lnum++;
      linepos = 0;
    }
    return c;
  }

  template<typename T>
  bool get(T &x) {
    source >> x;
    linepos += source.gcount();
    return source.good();
  }

  void unget() {
    linepos--;
    source.unget();
  }

  char peek() {
    return source.peek();
  }

  void error(std::string msg) {
    std::cerr << "Error in " << filename
              << ':' << lnum << ':' << linepos << std::endl;
    std::cerr << msg << std::endl;
    exit(1);
  }
};

SExpr *sexpr(Reader &r);
