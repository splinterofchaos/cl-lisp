
#pragma once

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/TargetSelect.h>

#include <map>
#include <string>

/// A light wrapper around llvm with just a few helpers.
/// (NOT an llvm abstraction.)
struct Llvm
{
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<> builder;

  /// The main function and its block.
  llvm::Function   *prog;
  llvm::BasicBlock *bb;

  std::map<std::string, llvm::Value *> vars;

  Llvm();

  llvm::Type *intTy();
  llvm::Value *getInt(int x, size_t size=sizeof(int)*8);

  template<typename...Args>
  void registerExtern(llvm::Type *ret, const char *name, bool var, Args...args)
  {
    auto ty = llvm::FunctionType::get(ret, {args...}, var); 
    module->getOrInsertFunction(name, ty);
  }
};

