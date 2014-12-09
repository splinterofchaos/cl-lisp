
#include <map>

#include "ast.h"
#include "llvm.h"

// -- HELPERS -- //
static void assert_is_int(llvm::Value *v, const std::string &msg)
{
  llvm::Type *ty = v->getType();
  if (ty->isPointerTy()) ty = ty->getContainedType(0);
  if (!ty->isIntegerTy()) {
    std::cerr << msg << std::endl;
    exit(1);
  }
}

template<typename Str>
llvm::Twine twine(Str &&s)
{
  return std::forward<Str>(s);
}

template<typename S1, typename S2, typename...Str>
llvm::Twine twine(S1 &&a, S2 &&b, Str &&...str)
{
  llvm::Twine t(std::forward<S1>(a));
  return twine(std::move(t) + std::forward<S2>(b), std::forward<Str>(str)...);
}

llvm::Type *lisp_to_vm(Llvm &vm, LispType lt)
{
  switch(lt) {
    case INT:    return vm.intTy();
    case STRING: return vm.stringTy();
    case VOID:   return llvm::Type::getVoidTy(llvm::getGlobalContext());

    default: std::cerr << "unhandled type" << std::endl;
             exit(1);
  }
}

std::string to_string(llvm::Type *ty) {
  switch(ty->getTypeID()) {
    case llvm::Type::VoidTyID:   return "void";
    case llvm::Type::IntegerTyID:  return "int";

    case llvm::Type::PointerTyID: {
      llvm::Type *cnt = ty->getContainedType(0);
      if (cnt->isIntegerTy(8))
        return "string";
      return to_string(cnt) + "*";
    }

    default: std::cerr << "unhandled type" << std::endl;
             exit(1);
  }
}

/// Pushes a block to the parent function and move the insert pointer to it.
static void push_block(Llvm &vm, llvm::BasicBlock *b) {
  vm.builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(b);
  vm.builder.SetInsertPoint(b);
}

// -- L-TYPE -- //

static llvm::Type *functionOrVar(Llvm &vm, const std::string &name) {
  if (name == "T" || name == "NIL")
    return vm.intTy();

  llvm::Type *ty = nullptr;

  Defun *f = getVar(vm.functions, name);
  if (f) {
    ty = f->ltype(vm);
  } else {
    auto *val = getVar(vm.vars, name);
    if (val) ty = val->getType();
  }
  return ty;
}

llvm::Type *Symbol::ltype(Llvm &vm) {
  return functionOrVar(vm, ident);
}

llvm::Type *String::ltype(Llvm &vm) {
  return vm.stringTy();
}

llvm::Type *Int::ltype(Llvm &vm) {
  return vm.intTy();
}

llvm::Type *If::ltype(Llvm &vm) {
  auto ty = t->ltype(vm);
  ty = (f->ltype(vm) == ty) ? ty : vm.voidTy();
  return ty;
}

llvm::Type *Setq::ltype(Llvm &vm) {
  auto ty = expr->ltype(vm);
  vm.storeVar(ident, ty);
  return ty;
}

llvm::Type *Progn::ltype(Llvm &vm) {
  llvm::Type *ty = nullptr;
  for (auto &expr : body)
    ty = expr->ltype(vm);
  return ty;
}

llvm::Type *Defun::ltype(Llvm &vm) {
  return prog->ltype(vm);
}

llvm::Type *List::ltype(Llvm &vm) {
  if (items.size() == 1) return items.front()->ltype(vm);

  // A function invocation:
  Symbol *fsym = (Symbol *) items.front().get();
  if (!fsym->is_sym) return nullptr;

  const std::string &name = fsym->ident;
  if (oneOf({"<", ">", "=", "<=", ">=" ,"+", "-", "*", "/"}, name))
    return vm.intTy();

  if (name == "printf")
    return vm.intTy();

  Defun *f = getVar(vm.functions, fsym->ident);
  if (!f) return vm.voidTy();

  llvm::Type *ty = nullptr;
  varBlock(vm.vars, [&] {
    for (size_t i=0; i < items.size() - 1 && i < f->args.size(); i++)
      vm.storeVar(f->args[i], items[i+1]->ltype(vm));
    ty = f->ltype(vm);
  });

  return ty;
}

// -- AST CODE -- //

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

llvm::Value *progn_code(Llvm &vm, Progn &prog)
{
  llvm::Value *last = nullptr;
  for (auto &e : prog.body)
    last = e->codegen(vm);
  return last;
}

template<typename Args>
llvm::Value *boolop(Llvm &vm, const std::string &fname, const Args& args)
{
  using Maker = llvm::Value *(llvm::IRBuilder<>::*)(llvm::Value *, llvm::Value *,
                                                    const llvm::Twine &);
  using OpMap = std::map<std::string, Maker>;
  static OpMap boolops {
    {"<",  &llvm::IRBuilder<>::CreateICmpSLT},
    {"<=", &llvm::IRBuilder<>::CreateICmpSLE},
    {">",  &llvm::IRBuilder<>::CreateICmpSGT},
    {">=", &llvm::IRBuilder<>::CreateICmpSGE},
    {"=",  &llvm::IRBuilder<>::CreateICmpEQ},
  };

  auto it = boolops.find(fname);
  if (it == std::end(boolops))
    return nullptr;
  auto op = it->second;

  // Applying only one value always returns
  // Ex: (= 1) = T; (< 1) = T.
  if (args.size() == 1)
    return vm.getInt(1, 1);

  llvm::Value *acc = nullptr;

  // Each comparison is made against the previous one.
  // Ex: (< 1 2 3) = T   => {1 < 2 ^ 2 < 3}
  //     (< 1 5 4) = NIL => {1 < 5 ^ !(5 < 4)}
  llvm::Value *last = args.front()->codegen(vm);
  assert_is_int(last, "boolop on " + to_string(last->getType()));

  for (size_t i=1; i < args.size(); i++) {
    llvm::Value *next = args[1]->codegen(vm);
    assert_is_int(next, "boolop on " + to_string(next->getType()));

    auto name = twine(last->getName(), it->first, next->getName());
    acc = (vm.builder.*op)(last, next, name);

    last = next;
  }

  return acc;
}

template<typename Args>
llvm::Value *binop(Llvm &vm, std::string fname,
                   const Args& args)
{
  if (auto ret = boolop(vm, fname, args))
    return ret;

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
  assert_is_int(acc, "binop on " + to_string(acc->getType()));

  for (int i=1; i < args.size(); i++) {
    llvm::Value *last = args[i]->codegen(vm);
    if (!acc || !last) {
      std::cerr << "binop on NIL" << std::endl;
      exit(1);
    }
    assert_is_int(last, "binop on " + to_string(last->getType()));

    auto name = twine(acc->getName(), fname, last->getName());
    acc = vm.builder.CreateBinOp(op, acc, last, name);
  }
  return acc;
}

llvm::Value *call(Llvm &vm,
                  std::string fname,
                  const std::vector<SExpr *> &args,
                  llvm::Type *lty=nullptr)
{
  // Check if this is a binary operation.
  if (llvm::Value *v = binop(vm, fname, args))
    return v;

  // Regular function.
  auto f = vm.module->getFunction(fname);
  if (!f) {
    Defun *def = getVar(vm.functions, fname);
    if (!def) return nullptr;

    // Mangle the function name for overloading.
    fname.push_back(':');
    for (auto *a : args)
      fname.push_back(a->ltype(vm)->getTypeID());

    f = vm.module->getFunction(fname);
    if (!f) {
      if (!lty) {
        std::cerr << "Unknown return type for " << fname << std::endl;
        exit(1);
      }

      varBlock(vm.vars, [&] {
        // Create the function.
        std::vector<llvm::Type *> argTys(args.size(), nullptr);
        for (size_t i=0; i < args.size() && i < def->args.size(); i++) {
        llvm::Type *ty = args[i]->ltype(vm);
          vm.storeVar(def->args[i], ty, true);
          argTys[i] = ty;
        }

        auto fty = llvm::FunctionType::get(def->ltype(vm), argTys, false);
        f = llvm::Function::Create(fty, llvm::GlobalValue::InternalLinkage,
                                   fname, vm.module.get());
      });

      if (!f) {
        std::cerr << "Could not create function: " << fname << std::endl;
        exit(1);
      }

      // Define the function.
      auto ip = vm.builder.GetInsertBlock();
      vm.builder.SetInsertPoint(
          llvm::BasicBlock::Create(llvm::getGlobalContext(), fname, f)
      );

      varBlock(vm.vars, [&] {
        auto ai = f->arg_begin();         // Function argument iterator.
        auto ii = std::begin(def->args);  // Identifier iterator.
        for (; ai != f->arg_end() && ii != std::end(def->args); ai++, ii++)
          vm.storeVar(*ii, ai, true);

        vm.builder.CreateRet(progn_code(vm, *def->prog));
      });

      // Put back the previous insert point.
      vm.builder.SetInsertPoint(ip);

    }
  }
  
  f = vm.module->getFunction(fname);
  if (!f) {
    std::cerr << "Undefined function: " << fname << std::endl;
    exit(1);
  }

  // Build the name of the result for easier IR reading.
  std::string resName;

  std::vector<llvm::Value *> fargs;
  for (auto &sexp : args) {
    llvm::Value *val = sexp->codegen(vm);
    fargs.push_back(val);

    if (!resName.empty())
      resName.push_back(',');
    resName += val->getName();
  }

  return vm.builder.CreateCall(f, fargs, twine(fname, "(", resName, ")"));
}

llvm::Value *Symbol::codegen(Llvm &vm)
{
  if (llvm::Value *v = getVar(vm.vars, ident)) {
    if (v->getType()->isPtrOrPtrVectorTy())
      return vm.builder.CreateLoad(v, ident);
    else
      return v;
  }

  if (llvm::Value *v = call(vm, ident, {}))
    return v;

  std::cerr << "Unknown function or variable: " << ident << std::endl;
  exit(1);
}

llvm::Value *String::codegen(Llvm &vm)
{
  return vm.builder.CreateGlobalString(contents);
}

llvm::Value *Int::codegen(Llvm &vm)
{
  return vm.getInt(val);
}

llvm::Value *If::codegen(Llvm &vm)
{
  return if_statement(vm, cond.get(), t.get(), f.get());
}

llvm::Value *Setq::codegen(Llvm &vm)
{
  auto v = expr.get()->codegen(vm);
  vm.storeVar(ident, v);
  vm.storeVar(ident, expr.get()->ltype(vm));
  return v;
}

llvm::Value *Progn::codegen(Llvm &vm)
{
  return progn_code(vm, *this);
}

llvm::Value *Defun::codegen(Llvm &vm)
{
  vm.functions.emplace_back(name, this);
  return nullptr;
}

llvm::Value *List::codegen(Llvm &vm)
{
  if (items.size() == 1)
    return items.front()->codegen(vm);

  if (items.front()->is_sym) {
    Symbol *sym = static_cast<Symbol *>(items.front().get());

    // Maybe a function.
    std::vector<SExpr *> args;
    for (size_t i = 1; i < items.size(); i++)
      args.push_back(items[i].get());
    if (llvm::Value *v = call(vm, sym->ident, args, ltype(vm)))
      return v;

    std::cerr << "Unknown function or variable: " << sym->ident << std::endl;
    exit(1);
  }
  return nullptr;
}
