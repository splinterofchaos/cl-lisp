
#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <experimental/optional>

#include "ast.h"
#include "helpers.h"

struct SourcePos {
  std::istream::pos_type fpos;
  size_t linepos = 0;
  size_t lnum = 1;

  SourcePos() = default;
  SourcePos(std::istream::pos_type f, size_t linepos, size_t lnum)
    : fpos(f), linepos(linepos), lnum(lnum) {
  }
};

struct SourceRange {
  SourcePos begin;
  SourcePos end;

  SourceRange() = default;
  SourceRange(SourcePos b, SourcePos e) : begin(b), end(e) { }
  SourceRange(SourcePos b) : begin(b), end(b) { }
};

struct Lexed {
  enum Token {
    STRING,
    IDENTIFIER,
    INT,
    DOUBLE,
    OPEN, CLOSE,  // '(' and ')'
    END,          // EOF (EOF may be a macro, so don't use that name.)
    NOTHING,
    N_TOKENS
  };

  SourceRange range;
  std::string lexeme;
  Token tok;

  const SourcePos &start() const {
    return range.begin;
  }

  Lexed() = default;
  Lexed(SourcePos b, SourcePos e, std::string l, Token t)
    : range(b, e), lexeme(std::move(l)), tok(t) { }
  Lexed(SourcePos p, char c, Token t)
    : range(p), lexeme(1, c), tok(t) { }
};

struct Reader
{
  std::string filename;
  std::istream& source;
  size_t linepos = 0;
  size_t lnum = 1;

  std::experimental::optional<Lexed> unlexed;

  Reader(std::string name, std::istream& is)
    : source(is), unlexed()
  {
    filename = name;
  }

  SourcePos pos() const {
    return SourcePos(source.tellg(), linepos, lnum);
  }

  unsigned depth = 0;

  Lexed lex();
  void unlex(Lexed l) {
    unlexed = std::move(l);
  }

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
