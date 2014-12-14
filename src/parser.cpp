
#include "parser.h"

static void skipwhite(Reader &r);
static String *string(Reader &r);
static Symbol *symbol(Reader &r);
static SExpr *num(Reader &r);
static SExpr *atom(Reader &r);

static Defun *defun(Reader &r);
static Progn *progn(Reader &r);

static SExpr *list(Reader &r);
static SExpr *list_item(Reader &r);

static std::string identifier(Reader &r);


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
    SourcePos p = r.pos();
    r.getc();
    r.depth++;
    SExpr *e = sexpr(r);
    skipwhite(r);
    if (e && (c = r.getc()) != ')')
      r.error(p, "unterminated expression");
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

static SExpr *list_item(Reader &r)
{
  skipwhite(r);
  SExpr *e = nullptr;
  if (r.peek() == '(')
    e = sexpr(r);
  else
    e = atom(r);
  return e;
}

static SExpr *list(Reader &r)
{
  skipwhite(r);

  std::string id = identifier(r);

  if (id == "if") {
    SExpr *cond = list_item(r);
    SExpr *t = list_item(r);
    SExpr *f = list_item(r);
    return new If(cond, t, f);
  }
  if (id == "setq") {
    id = identifier(r);
    if (id == "") r.error("invalid or no variable name");
    SExprPtr e(list_item(r));
    if (!e) r.error("no assignment value");
    return new Setq(std::move(id), std::move(e));
  }
  if (id == "defun")
    return defun(r);
  if (id == "progn")
    return progn(r);

  List::Items items;
  if (id != "") items.emplace_back(new Symbol(std::move(id)));

  while (skipwhite(r), r.peek() != EOF && r.peek() != ')') {
    SExpr *e = list_item(r);
    if (!e) return nullptr;
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

static Progn *progn(Reader &r)
{
  std::vector<SExprPtr> body;
  while(SExpr *e = sexpr(r))
    body.emplace_back(e);
  return new Progn("progn", std::move(body));
}

static Defun *defun(Reader &r)
{
  std::string name = identifier(r);
  if (name == "") r.error("defun: bad or no function name");

  SExpr *earglist = list_item(r);
  std::vector<std::string> args;
  if (earglist && earglist->is_list) {
    for (auto& i : static_cast<List *>(earglist)->items) {
      if (!i->is_sym) r.error("arguments must be symbols");
      args.emplace_back(std::move(static_cast<Symbol *>(i.get())->ident));
    }
  } else if (earglist && earglist->is_sym) {
    args.emplace_back(std::move(static_cast<Symbol *>(earglist)->ident));
  } else if (earglist) {
    r.error("defun: bad argument list");
  }
  delete earglist;

  return new Defun(std::move(name), std::move(args), progn(r));
}

std::string identifier(Reader &r)
{
  skipwhite(r);
  auto pred = [](char c) {
    return c != '(' && c != ')' && c != '"' && std::isgraph(c);
  };
  if (!pred(r.peek()) || std::isdigit(r.peek()))
    return "";

  std::string name;
  while (pred(r.peek()))
    name.push_back(r.getc());
  return name;
}

static Symbol *symbol(Reader &r)
{
  std::string name = identifier(r);
  return name.size() ? new Symbol(name) : nullptr;
}

static SExpr *num(Reader &r) {
  if (!std::isdigit(r.peek()))
    return nullptr;

  int x;
  if (!r.get(x))
    r.error("couldn't read integer or float");
  if (r.peek() != '.')
    return new Int(x);

  double fp = 0;  // Floating part.
  if (!r.get(fp))
    r.error("couldn't read float");
  return new Double(x + fp);
}
