
#include "parser.h"

// A simple helper.
static void skipwhite(Reader &r) {
  while (std::isspace(r.peek())) {
    r.getc();
  }
}

Lexed Reader::lex()
{
  if (unlexed) {
    Lexed l = std::move(*unlexed);
    unlexed = {};
    return l;
  }

  skipwhite(*this);
  SourcePos start = pos();

  std::string contents;
  Lexed::Token t = Lexed::NOTHING;
  switch (char c = peek() ) {
    case '(':
    case ')': getc();
              return Lexed(pos(), c, c == '(' ? Lexed::OPEN : Lexed::CLOSE);

    case ';': do { getc(); }
              while (peek() != EOF && peek() != '\n');
              return lex();

    case '"': {
      getc();  // Eat the starting quote.

      bool esc;  // True if the last character equalled '\'.
      char c;    // The current char being processed.
      while (c = getc(), c != '"') {
        if (c == EOF) error("unterminated string");
        if (esc) {
          esc = false;
          if (c == 'n') c = '\n';
        } else if (c == '\\') {
          esc = true;
          continue;
        }
        contents.push_back(c);
      }
      t = Lexed::STRING;
      break;
    }

    case EOF: return Lexed(pos(), 0, Lexed::END);

    default: {
      // A number or identifier.
      auto pred = [](char c) -> bool {
        return !oneOf({'(', ')', '"'}, c) && std::isgraph(c);
      };
      if (!pred(peek())) error(std::string("Can't lex ") + peek());

      SourcePos b = pos();
      while (pred(peek())) contents.push_back(getc());

      t = Lexed::IDENTIFIER;

      // Disambiguate the overloaded std::isdigit().
      auto isdigit = [](char c) { return std::isdigit(c); };

      // Check if this is a number.
      auto dot = contents.find('.');  // Float, first.
      if (dot != std::string::npos &&
          std::all_of(contents.begin(), contents.begin() + dot, isdigit) &&
          std::all_of(contents.begin() + dot + 1, contents.end(), isdigit))
        t = Lexed::DOUBLE;
      else if (std::all_of(contents.begin(), contents.end(), isdigit))
        t = Lexed::INT;
    }
  }

  return Lexed(start, pos(), std::move(contents), t);
}

static Defun *defun(Reader &r);
static Progn *progn(Reader &r);

static SExpr *list(Reader &r, Lexed lexed);
static SExpr *list_item(Reader &r);

/// sexpr : a top level expression of the form "(expr)" or "expr"
SExpr *sexpr(Reader &r) {
  Lexed lexed = r.lex();
  SExpr *e = nullptr;
  if (lexed.tok == Lexed::OPEN) {
    r.depth++;

    e = sexpr(r);
    if (e && r.lex().tok != Lexed::CLOSE)
      r.error(lexed.start(), "unterminated expression");

    r.depth--;
    return e;
  } else if (lexed.tok == Lexed::CLOSE) {
    if (r.depth == 0) r.error("unmatched ')'");
    r.unlex(std::move(lexed));
    return nullptr;
  } else {
    e = list(r, std::move(lexed));
  }

  return e;
}

static SExpr *list_item(Reader &r)
{
  SExpr *e = nullptr;
  Lexed l = r.lex();
  switch (l.tok) {
    case Lexed::INT:    e = new Int(std::stoi(l.lexeme)); break;
    case Lexed::DOUBLE: e = new Double(std::stod(l.lexeme)); break;
    case Lexed::STRING: e = new String(std::move(l.lexeme)); break;
    case Lexed::IDENTIFIER: e = new Symbol(std::move(l.lexeme)); break;
    default: r.unlex(std::move(l));
             if (l.tok == Lexed::OPEN) e = sexpr(r);
  }
  return e;
}

static SExpr *list(Reader &r, Lexed lexed)
{
  if (lexed.tok == Lexed::END)
    return nullptr;
  if (lexed.tok != Lexed::IDENTIFIER)
    r.error(lexed.start(), "not a function identifier: " + lexed.lexeme);

  const std::string &id = lexed.lexeme;

  if (id == "if") {
    SExpr *cond = list_item(r);
    SExpr *t = list_item(r);
    SExpr *f = list_item(r);
    return new If(cond, t, f);
  }
  if (id == "setq") {
    Lexed name = r.lex();
    if (name.tok != Lexed::IDENTIFIER)
      r.error(name.start(), "invalid or no variable name");
    SExprPtr e(list_item(r));
    if (!e) r.error("no assignment value");
    return new Setq(std::move(name.lexeme), std::move(e));
  }
  if (id == "defun")
    return defun(r);
  if (id == "progn")
    return progn(r);

  List::Items items;
  items.emplace_back(new Symbol(std::move(id)));

  while (SExpr *e = list_item(r))
    items.emplace_back(e);

  if (items.size() == 1)
    return items.front().release();

  return new List(std::move(items));
}

static Progn *progn(Reader &r)
{
  std::vector<SExprPtr> body;
  while(SExpr *e = sexpr(r))
    body.emplace_back(e);
  return new Progn("progn", std::move(body));
}

static Defun *defun(Reader &r)
{
  Lexed fname = r.lex();
  if (fname.tok != Lexed::IDENTIFIER)
    r.error(fname.start(), "defun: bad or no function name");

  SourcePos argListBegin = r.pos();
  if (r.lex().tok != Lexed::OPEN)
    r.error(argListBegin, "no argument list");

  std::vector<std::string> args;
  Lexed arg;
  while (arg = r.lex(), arg.tok != Lexed::CLOSE) {
    if (arg.tok == Lexed::END)
      r.error(argListBegin, "unterminated argument list");
    if (arg.tok != Lexed::IDENTIFIER)
      r.error(arg.start(), arg.lexeme + " is not an identifier");
    args.emplace_back(std::move(arg.lexeme));
  }

  return new Defun(std::move(fname.lexeme), std::move(args), progn(r));
}

