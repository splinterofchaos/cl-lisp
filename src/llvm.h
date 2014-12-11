
#pragma once

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/TargetSelect.h>

#include <vector>
#include <string>

#include "helpers.h"
#include "ast.h"

/// A light wrapper around llvm with just a few helpers.
/// (NOT an llvm abstraction.)
struct Llvm
{
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<> builder;

  /// The main function and its block.
  llvm::Function   *prog;
  llvm::BasicBlock *bb;

  VarList<llvm::Value *> vars;

  /// A list of every user-defined function.
  /// When a function is defined, we do not know the type of its arguments or
  /// result, so we keep the entire AST for future reference.
  VarList<Defun *> functions;

  Llvm();

  llvm::Value *storeVar(llvm::StringRef, llvm::Value *, bool force=false);

  // Store an undefined value by its type.
  llvm::Value *storeVar(llvm::StringRef, llvm::Type *, bool force=false);

  llvm::Type *intTy();
  llvm::Type *doubleTy();

  llvm::Value *getInt(int x, size_t size=sizeof(int)*8);
  llvm::Value *getDouble(double);

  llvm::Type *stringTy() {
    return llvm::Type::getInt8PtrTy(llvm::getGlobalContext());
  }

  llvm::Type *voidTy() {
    return llvm::Type::getVoidTy(llvm::getGlobalContext());
  }

  template<typename...Args>
  void registerExtern(llvm::Type *ret, const char *name, bool var, Args...args)
  {
    auto ty = llvm::FunctionType::get(ret, {args...}, var); 
    module->getOrInsertFunction(name, ty);
  }
};

