
#pragma once

#include <initializer_list>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "llvm.h"
#include "helpers.h"

struct SExpr
{
  bool is_sym = false;
  bool is_list = false;

  // Each SEXpr defines how to generate its own code.
  virtual llvm::Value *codegen(Llvm&) = 0;

  virtual ~SExpr() { }
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
};

struct String : SExpr
{
  std::string contents;

  String(std::string s) : contents(std::move(s)) { }

  llvm::Value *codegen(Llvm &vm) {
    return vm.builder.CreateGlobalString(contents);
  }
};

struct Int : SExpr
{
  int val;
  Int(int x) : val(x) {}

  llvm::Value *codegen(Llvm &vm) { return vm.getInt(val); }
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
};

enum LispType {
  NONE, VOID, INT, STRING
};

struct Declfun : SExpr
{
  std::string name;
  LispType returnType = NONE;
  std::vector<LispType> args;

  Declfun(std::string name) : name(std::move(name)) {}

  bool add_arg(std::string type) {
    LispType t;
    if (type == "void") t = VOID;
    else if (type == "int") t = INT;
    else if (type == "string") t = STRING;
    else return false;

    if (returnType == NONE)
      returnType = t;
    else
      args.push_back(t);
    return true;
  }

  llvm::Value *codegen(Llvm&);
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
};
