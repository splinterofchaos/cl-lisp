
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
  llvm::Value *alloc = force ? nullptr : getVar(name);

  if (!alloc || force) {
    alloc = new llvm::AllocaInst (
        val->getType(), name, builder.GetInsertBlock()
    );
    vars.emplace_back(std::move(name), alloc);
  }

  builder.CreateStore(val, alloc);

  return alloc;
}

auto findVar(Llvm &vm, llvm::StringRef name)
{
  return std::find_if(vm.vars.rbegin(), vm.vars.rend(),
                      [&](const Llvm::Var &v) { return v.first == name; });
}

llvm::Value *Llvm::getVar(llvm::StringRef name)
{
  auto it = findVar(*this, name);
  return it != std::rend(vars) ? it->second : nullptr;
}

void Llvm::delVar(llvm::StringRef name)
{
  auto it = findVar(*this, name);
  if (it != std::rend(vars))
    // Note: reverse_iterator points to the /next/ item.
    vars.erase(it.base() - 1);
}

void Llvm::popVar()
{
  vars.erase(vars.end() - 1);
}

llvm::Type *Llvm::intTy() {
  return builder.getIntNTy(sizeof(int) * 8);
}

llvm::Value *Llvm::getInt(int x, size_t size) {
  return builder.getInt(llvm::APInt(size, x));
}

