
#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "ast.h"
#include "helpers.h"

struct SourcePos {
  std::istream::pos_type fpos;
  size_t linepos = 0;
  size_t lnum = 1;

  SourcePos() { }
  SourcePos(std::istream::pos_type f, size_t linepos, size_t lnum)
    : fpos(f), linepos(linepos), lnum(lnum) {
  }
};

struct SourceRange {
  SourcePos begin;
  SourcePos end;

  SourceRange(SourcePos b, SourcePos e) : begin(b), end(e) { }
  SourceRange(SourcePos b) : begin(b), end(b) { }
};

struct Reader
{
  std::string filename;
  std::istream& source;
  size_t linepos = 0;
  size_t lnum = 1;

  Reader(std::string name, std::istream& is)
    : source(is)
  {
    filename = name;
  }

  SourcePos pos() const {
    return SourcePos(source.tellg(), linepos, lnum);
  }

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

  void error(SourcePos from, std::string msg) {
    std::cerr << "Error in " << filename << " from "
              << from.lnum << ':' << from.linepos << " to "
              << lnum << ':' << linepos << std::endl;
    std::cerr << msg << std::endl;
    exit(1);
  }

  void error(std::string msg) {
    std::cerr << "Error in " << filename
              << ':' << lnum << ':' << linepos << std::endl;
    std::cerr << msg << std::endl;
    exit(1);
  }
};

SExpr *sexpr(Reader &r);
