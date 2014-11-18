
#include "parser.h"

static void skipwhite(Reader &r);
static String *string(Reader &r);
static Symbol *symbol(Reader &r);
static Int *num(Reader &r);
static SExpr *atom(Reader &r);
static SExpr *list(Reader &r);

// A simple helper.
static void skipwhite(Reader &r) {
  while (std::isspace(r.peek())) {
    r.getc();
  }
}

/// sexpr : a top level expression of the form "(expr)" or "expr"
SExpr *sexpr(Reader &r) {
  skipwhite(r);

  char c = r.peek();
  if (c == EOF)
    return nullptr;
  if (c == '(') {
    r.getc();
    r.depth++;
    SExpr *e = sexpr(r);
    skipwhite(r);
    if (e && (c = r.getc()) != ')')
      r.error("unterminated expression");
    r.depth--;
    return e;
  } else if (c == ')') {
    if (r.depth == 0)
      r.error("unmatched ')'");
    else
      return nullptr;
  }

  if (r.depth == 0)
    r.error("Error: expression cannot be at top level");

  return list(r);
}

static SExpr *list(Reader &r)
{
  skipwhite(r);
  List::Items items;
  while (skipwhite(r), r.peek() != EOF && r.peek() != ')') {
    SExpr *e = nullptr;
    if (r.peek() == '(')
      e = sexpr(r);
    else
      e = atom(r);
    if (!e)
      return nullptr;
    items.emplace_back(e);
  }

  if (items.size() == 1)
    return items.front().release();
  return new List(std::move(items));
}

static SExpr *atom(Reader &r)
{
  skipwhite(r);
  SExpr *ret = num(r);
  if (!ret)
    ret = string(r);
  if (!ret) {
    ret = symbol(r);
  }
  return ret;
}

String *string(Reader &r) {
  if (r.peek() != '"')
    return nullptr;
  r.getc();
  std::string s;
  char c;
  bool esc = false;
  while (c = r.getc(), c != '"') {
    if (c == '\n' || c == EOF)
      r.error("unterminated string");
    if (esc) {
      if (c == 'n')
        c = '\n';
      else
        r.error("unhandled escape seq");
    } else if (c == '\\') {
      esc = true;
      continue;
    }
    s.push_back(c);
    esc = false;
  }
  return new String{std::move(s)};
}

static Symbol *symbol(Reader &r)
{
  auto pred = [](char c) { return c != '(' && c != ')' && std::isgraph(c); };
  if (!pred(r.peek()) || std::isdigit(r.peek()))
    return nullptr;

  std::string name;
  while (pred(r.peek()))
    name.push_back(r.getc());
  return name.size() ? new Symbol(name) : nullptr;
}

/// num : Only integers accepted so far.
static Int *num(Reader &r) {
  if (!std::isdigit(r.peek()))
    return nullptr;

  int x;
  if (!r.get(x))
    r.error("couldn't read num");
  return new Int(x);
}
