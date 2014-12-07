
#include "llvm.h"

Llvm::Llvm()
  : module(new llvm::Module("unnamed module", llvm::getGlobalContext()))
  , builder(llvm::getGlobalContext())
{
  llvm::Type *nil = llvm::Type::getVoidTy(llvm::getGlobalContext());
  prog = llvm::Function::Create(llvm::FunctionType::get(nil, false),
                                llvm::GlobalValue::InternalLinkage,
                                "main", module.get());
  bb = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", prog);
  builder.SetInsertPoint(bb);

  // Link in the interpreter.
  if (!llvm::InitializeNativeTarget()) {
    LLVMLinkInInterpreter();
  }

  storeVar("T", getInt(1, 1));
  storeVar("NIL", getInt(0, 1));
}

llvm::Value *Llvm::storeVar(llvm::StringRef name, llvm::Value *val, bool force)
{
  llvm::Value *alloc = force ? nullptr : getVar(vars, name);

  if (!alloc || force) {
    alloc = new llvm::AllocaInst (
        val->getType(), name, builder.GetInsertBlock()
    );
    vars.emplace_back(std::move(name), alloc);
  }

  builder.CreateStore(val, alloc);

  return alloc;
}

llvm::Type *Llvm::intTy() {
  return builder.getIntNTy(sizeof(int) * 8);
}

llvm::Value *Llvm::getInt(int x, size_t size) {
  return builder.getInt(llvm::APInt(size, x));
}

