
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

llvm::Value *Llvm::storeVar(llvm::StringRef name, llvm::Type *ty, bool force)
{
  llvm::Value *undef = force ? nullptr : getVar(vars, name);

  if (!undef) {
    undef = llvm::UndefValue::get(ty);
    vars.emplace_back(name, undef);
  }

  return undef;
}

llvm::Type *Llvm::intTy() {
  return builder.getIntNTy(sizeof(int) * 8);
}

llvm::Type *Llvm::doubleTy() {
  return builder.getDoubleTy();
}

llvm::Value *Llvm::getInt(int x, size_t size) {
  return llvm::ConstantInt::get(module->getContext(), llvm::APInt(size, x));
}

llvm::Value *Llvm::getDouble(double x) {
  return llvm::ConstantFP::get(module->getContext(), llvm::APFloat(x));
}
