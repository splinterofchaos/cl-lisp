
#include <map>

#include "ast.h"

/// Pushes a block to the parent function and move the insert pointer to it.
static void push_block(Llvm &vm, llvm::BasicBlock *b) {
  vm.builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(b);
  vm.builder.SetInsertPoint(b);
}

/// Handles the code for an if branch.
llvm::Value *if_branch(Llvm &vm, SExpr* e,
                       llvm::BasicBlock **b, llvm::BasicBlock *merge)
{
  push_block(vm, *b);               // Create the branch.
  llvm::Value *v = e->codegen(vm);  // Generate the code.
  vm.builder.CreateBr(merge);       // Leave the branch.
  *b = vm.builder.GetInsertBlock(); // Update the block's position.

  return v;
}

llvm::Value *if_statement(Llvm &vm, SExpr *pred, SExpr *t, SExpr *f)
{
  auto cond = pred->codegen(vm);
  if (!cond) return nullptr;

  llvm::BasicBlock *ifso  = llvm::BasicBlock::Create(llvm::getGlobalContext(), "true");
  llvm::BasicBlock *ifnot = llvm::BasicBlock::Create(llvm::getGlobalContext(), "false");
  llvm::BasicBlock *merge = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge");
  vm.builder.CreateCondBr(cond, ifso, ifnot);

  auto tcode = if_branch(vm, t, &ifso, merge);
  auto fcode = if_branch(vm, f, &ifnot, merge);
  if (!tcode || !fcode) return nullptr;

  push_block(vm, merge);

  if (tcode->getType() != fcode->getType()) {
    std::cerr << "Type mismatch in then/else branch: " << std::endl;
    std::cerr << "left  hand: ";
    tcode->getType()->dump();
    std::cerr << "right hand: ";
    fcode->getType()->dump();
    std::cerr << std::endl;
    exit(1);
  }

  llvm::PHINode *phi = vm.builder.CreatePHI(tcode->getType(), 2, "iftmp");
  phi->addIncoming(tcode, ifso);
  phi->addIncoming(fcode, ifnot);
  return phi;
}

template<typename Args>
llvm::Value *binop(Llvm &vm, std::string fname,
                   const Args& args)
{
  static std::map<std::string, llvm::Instruction::BinaryOps> binops = {
    {"+", llvm::Instruction::Add},
    {"-", llvm::Instruction::Sub},
    {"*", llvm::Instruction::Mul},
    {"/", llvm::Instruction::SDiv},
  };
  auto it = binops.find(fname);
  if (it == std::end(binops))
    return nullptr;
  auto op = it->second;

  if (args.size() == 0)
    return vm.getInt(0);

  // LISP operators are chainable. 
  // Ex: (+ 1) = 1; (+ 1 1 1) = 3
  llvm::Value *acc = args[0]->codegen(vm);
  for (int i=1; i < args.size(); i++) {
    llvm::Value *last = args[i]->codegen(vm);
    if (!acc || !last) {
      std::cerr << "binop on NIL" << std::endl;
      exit(1);
    }
    if (!acc->getType()->isIntegerTy() ||
        !last->getType()->isIntegerTy()) {
      std::cerr << "Illegal operation on non-int." << std::endl;
      exit(1);
    }
    acc = vm.builder.CreateBinOp(op, acc, last);
  }
  return acc;
}

template<typename Args>
llvm::Value *call(Llvm &vm, const std::string &fname, const Args &args)
{
  // Check if this is a binary operation.
  if (llvm::Value *v = binop(vm, fname, args))
    return v;

  // Regular function.
  auto f = vm.module->getFunction(fname);
  if (!f) return nullptr;

  std::vector<llvm::Value *> fargs;
  for (auto &sexp : args) {
    llvm::Value *val = sexp->codegen(vm);
    fargs.push_back(val);
  }
  return vm.builder.CreateCall(f, fargs);
}

llvm::Value *Symbol::codegen(Llvm &vm)
{
  auto it = vm.vars.find(ident);
  if (it != std::end(vm.vars)) {
    if (it->second->getType()->isPtrOrPtrVectorTy())
      return vm.builder.CreateLoad(it->second, ident);
    else
      return it->second;
  }

  if (llvm::Value *v = call(vm, ident, std::vector<SExpr*>{}))
    return v;

  std::cerr << "Unknown function or variable: " << ident << std::endl;
  exit(1);
}

llvm::Value *alloc(Llvm &vm, const std::string &ident, SExpr *e)
{
  llvm::Value *val = e->codegen(vm);
  llvm::AllocaInst *alloc = new llvm::AllocaInst(
      val->getType(), ident, vm.builder.GetInsertBlock()
  );
  vm.builder.CreateStore(val, alloc);
  vm.vars[ident] = alloc;
  return alloc;
}

llvm::Value *If::codegen(Llvm &vm)
{
  return if_statement(vm, cond.get(), t.get(), f.get());
}

llvm::Value *Setq::codegen(Llvm &vm)
{
  return alloc(vm, ident, expr.get());
}

llvm::Value *List::codegen(Llvm &vm)
{
  if (items.front()->is_sym) {
    Symbol *sym = static_cast<Symbol *>(items.front().get());

    // Check if it's a defined variable.
    auto it = vm.vars.find(sym->ident);
    if (it != std::end(vm.vars))
      return it->second;

    // Maybe a function.
    std::vector<SExpr *> args;
    for (size_t i = 1; i < items.size(); i++)
      args.push_back(items[i].get());
    if (llvm::Value *v = call(vm, sym->ident, args))
      return v;

    std::cerr << "Unknown function or variable: " << sym->ident << std::endl;
    exit(1);
  }
  return nullptr;
}
