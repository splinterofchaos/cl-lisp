
#pragma once

#include <initializer_list>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "helpers.h"

#include <llvm/IR/Value.h>
struct Llvm;  // forward declaration

enum LispType {
  NONE,  ///< Used to differentiate "not yet typed" from `void`.
  VOID,
  INT,
  STRING,
  N_LISP_TYPES
};

struct SExpr
{
  bool is_sym = false;
  bool is_list = false;

  // Each SEXpr defines how to generate its own code.
  virtual llvm::Value *codegen(Llvm&) = 0;
  virtual ~SExpr() { }

  /// The type of this AST node.
  // TODO: Use llvm::Type instead of LispType.
  virtual llvm::Type *ltype(Llvm &) = 0;
};

using SExprPtr = std::unique_ptr<SExpr>;

/// An identifier.
struct Symbol : SExpr
{
  std::string ident;

  Symbol(std::string s) : ident(std::move(s)) {
    is_sym = true;
  }

  llvm::Value *codegen(Llvm&);

  /// Type deduced by variable lookup.
  llvm::Type *ltype(Llvm &);
};

struct String : SExpr
{
  std::string contents;

  String(std::string s) : contents(std::move(s)) { }

  llvm::Value *codegen(Llvm &);
  llvm::Type  *ltype(Llvm &);
};

struct Int : SExpr
{
  int val;
  Int(int x) : val(x) { }

  llvm::Value *codegen(Llvm &);
  llvm::Type  *ltype(Llvm &);
};

/// A list of sexprs, like (a b c)
struct List : SExpr
{
  using Item = std::unique_ptr<SExpr>;
  using Items = std::vector<Item>;
  Items items;

  List(Items is) : items(std::move(is)) {
    is_list = true;
  }

  llvm::Value *codegen(Llvm&);
  llvm::Type *ltype(Llvm &);
};

// SPECIAL SYNTAX ITEMS //

struct If : SExpr
{
  SExprPtr cond, t, f;

  If(SExpr* cond, SExpr* t, SExpr* f) : cond(cond), t(t), f(f) { }
  If(SExprPtr cond, SExprPtr t, SExprPtr f)
    : cond(std::move(cond)), t(std::move(t)), f(std::move(f))
  {
  }

  llvm::Value *codegen(Llvm&);
  llvm::Type *ltype(Llvm &);
};

struct Setq : SExpr
{
  std::string ident;
  SExprPtr expr;

  Setq(std::string ident, SExprPtr e)
    : ident(std::move(ident)), expr(std::move(e))
  {
  }

  llvm::Value *codegen(Llvm&);
  llvm::Type *ltype(Llvm &);
};

struct Progn : SExpr
{
  std::string name;  ///< Optional block name.
  std::vector<SExprPtr> body;

  Progn(std::vector<SExprPtr> body) : body(std::move(body)) { }
  Progn(std::string name, std::vector<SExprPtr> body)
    : body(std::move(body)), name(std::move(name))
  {
  }

  llvm::Value *codegen(Llvm&);

  /// Type determined by the last statement.
  llvm::Type *ltype(Llvm &);
};

struct Defun : SExpr
{
  std::string name;
  std::vector<std::string> args;
  std::unique_ptr<Progn> prog;

  Defun(std::string n, std::vector<std::string> args, Progn *body)
    : name(std::move(n)), args(std::move(args)), prog(body)
  {
    prog->name = this->name + "_body";
  }

  llvm::Value *codegen(Llvm&);
  llvm::Type  *ltype(Llvm &);
};

