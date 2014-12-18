
#include "llvm.h"

Llvm::Llvm()
  : module(new llvm::Module("unnamed module", llvm::getGlobalContext()))
  , builder(llvm::getGlobalContext())
{
  llvm::Type *nil = llvm::Type::getVoidTy(llvm::getGlobalContext());
  prog = llvm::Function::Create(llvm::FunctionType::get(nil, false),
                                llvm::GlobalValue::ExternalLinkage,
                                "main", module.get());
  bb = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", prog);
  builder.SetInsertPoint(bb);

  // Link in the interpreter.
  if (!llvm::InitializeNativeTarget()) {
    LLVMLinkInInterpreter();
  }

  storeVar("T", getInt(1, 1), INT);
  storeVar("NIL", getInt(0, 1), INT);
}

llvm::Value *Llvm::storeVar(llvm::StringRef name,
                            llvm::Value *val, LispType ty,
                            bool force)
{
  LValue *lval = force ? nullptr : getVar(name);

  if (!lval) {
    llvm::Value *alloc = !val ? nullptr : new llvm::AllocaInst (
        val->getType(),
        llvm::Twine(name, ".alloc"),
        builder.GetInsertBlock()
    );
    vars.emplace_back(name, LValue{alloc, ty});
    lval = &vars.back().second;
  }

  llvm::Value *ptr = lval->val;
  if (val) builder.CreateStore(val, ptr);

  return ptr;
}

LValue *Llvm::getVar(llvm::StringRef name) {
  auto it = findVar(vars, name);
  return it != std::end(vars) ? &it->second : nullptr;
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
