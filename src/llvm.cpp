
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

  vars["T"] = getInt(1, 1);
  vars["NIL"] = getInt(0, 1);
}

llvm::Type *Llvm::intTy() {
  return builder.getIntNTy(sizeof(int) * 8);
}

llvm::Value *Llvm::getInt(int x, size_t size) {
  return builder.getInt(llvm::APInt(size, x));
}

