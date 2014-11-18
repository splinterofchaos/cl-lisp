
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

  // Each SEXpr defines how to generate its own code.
  virtual llvm::Value *codegen(Llvm&) = 0;
};

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

  List(Items is) : items(std::move(is)) { }

  llvm::Value *codegen(Llvm&);
};

