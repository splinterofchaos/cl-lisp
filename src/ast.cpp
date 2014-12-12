
#include <map>
#include <set>

#include "ast.h"
#include "llvm.h"

// -- HELPERS -- //
static llvm::Type *unPtr(llvm::Type *ty) {
  if (!ty) return nullptr;
  return ty->isPointerTy() ? ty->getContainedType(0) : ty;
}

static void assert_is_int(llvm::Value *v, const std::string &msg)
{
  if (!unPtr(v->getType())->isIntegerTy()) {
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

static std::string to_string(llvm::Type *ty) {
  if (!ty) return "nothing";

  switch(ty->getTypeID()) {
    case llvm::Type::VoidTyID:     return "void";
    case llvm::Type::IntegerTyID:  return ty->isIntegerTy(8) ? "char" : "int";
    case llvm::Type::DoubleTyID:   return "double";

    case llvm::Type::ArrayTyID:    return to_string(ty->getArrayElementType());

    case llvm::Type::PointerTyID: return to_string(ty->getContainedType(0)) + "*";

    default: std::cerr << "unhandled type" << std::endl;
             exit(1);
  }
}

/// Mangles a function name for use in overloading.
/// Ex: (f 1)   => call f:int
///     (f 1.0) => call f:double
static std::string fname_mangle(const std::string &fname,
                                const std::vector<llvm::Type *> &args) {
  std::string mangled;
  for (auto a : args) {
    if (mangled.size()) mangled.push_back(',');
    mangled += to_string(a);
  }
  return fname + ":" + mangled;
}

/// Returns the common type for use in if statement and binary operator codegen.
static llvm::Type *commonType(Llvm &vm, llvm::Type *a, llvm::Type *b) {
  a = unPtr(a);
  b = unPtr(b);
  if (!a)
    return b;
  if (!b)
    return a;
  if (a->isDoubleTy())
    return b->isDoubleTy() || b->isIntegerTy() ? a : vm.voidTy();
  if (a->isIntegerTy())
    return b->isDoubleTy() ? b : a;
  return a->getTypeID() == b->getTypeID() ? a : vm.voidTy();
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
  llvm::Type *ty = functionOrVar(vm, ident);
  if (ty) ty = unPtr(ty);  // We want the contained type.
  return ty;
}

llvm::Type *String::ltype(Llvm &vm) {
  return vm.stringTy();
}

llvm::Type *Int::ltype(Llvm &vm) {
  return vm.intTy();
}

llvm::Type *Double::ltype(Llvm &vm) {
  return vm.doubleTy();
}

llvm::Type *If::ltype(Llvm &vm) {
  return commonType(vm, t->ltype(vm), f->ltype(vm));
}

llvm::Type *Setq::ltype(Llvm &vm) {
  auto ty = expr->ltype(vm);
  vm.storeVar(ident, ty);
  return ty;
}

llvm::Type *Progn::ltype(Llvm &vm) {
  llvm::Type *ty = nullptr;

  // Statements in progn may declare variables that will be replaced when we
  // call codegen().
  varBlock(vm.vars, [&] {
    for (auto &expr : body)
      ty = expr->ltype(vm);
  });
  return ty;
}

llvm::Type *Defun::ltype(Llvm &vm) {
  return prog->ltype(vm);
}

llvm::Type *List::ltype(Llvm &vm) {
  if (items.size() == 1) return items.front()->ltype(vm);

  // A function invocation:
  Symbol *fsym = (Symbol *) items.front().get();
  if (!fsym || !fsym->is_sym) return nullptr;

  const std::string &name = fsym->ident;
  if (oneOf({"<", ">", "=", "<=", ">="}, name))
    return vm.intTy();  // TODO: Should be bool.

  if (oneOf({"+", "-", "*", "/"}, name)) {
    for (size_t i=1; i < items.size(); i++) {
      llvm::Type *ty = items[i]->ltype(vm);
      if (!ty) return nullptr;
      if (unPtr(ty)->isDoubleTy())
        return vm.doubleTy();
    }
    return vm.intTy();
  }

  if (name == "printf")
    return vm.intTy();

  Defun *def = getVar(vm.functions, fsym->ident);
  if (!def) return nullptr;

  std::vector<llvm::Type *> argTys;
  for (size_t i=1; i < items.size(); i++) {
    llvm::Type *ty = items[i]->ltype(vm);
    if (!ty) return nullptr;
    argTys.push_back(ty);
  }
  std::string fname = fname_mangle(fsym->ident, argTys);

  // May already be defined.
  if (auto f = vm.module->getFunction(fname))
    return f->getReturnType();

  // To avoid recursion, keep track of all the functions we have tried to
  // deduce the type of.
  static std::set<std::string> history;
  if (history.find(fname) == history.end())
    history.insert(fname);
  else
    return nullptr;

  llvm::Type *ty = nullptr;
  varBlock(vm.vars, [&] {
    for (size_t i=0; i < argTys.size() && i < def->args.size(); i++)
      vm.storeVar(def->args[i], argTys[i]);
    ty = def->ltype(vm);
  });

  return ty;
}

/// Does basic conversion on the value, `v` to the target type, `ty`.
static llvm::Value *convert(Llvm &vm, llvm::Type *ty, llvm::Value *x) {
  if (ty->isDoubleTy() && x->getType()->isIntegerTy())
    return vm.builder.CreateSIToFP(x, ty, twine(x->getName(), ".double"));
  if (ty->isIntegerTy() && x->getType()->isDoubleTy())
    return vm.builder.CreateFPToSI(x, ty, twine(x->getName(), ".int"));
  return x;
}

// -- AST CODE -- //

/// Handles the code for an if branch.
llvm::Value *if_branch(Llvm &vm, SExpr* e, llvm::Type *asType,
                       llvm::BasicBlock **b, llvm::BasicBlock *merge)
{
  push_block(vm, *b);               // Create the branch.
  llvm::Value *v = e->codegen(vm);  // Generate the code.
  vm.builder.CreateBr(merge);       // Leave the branch.
  *b = vm.builder.GetInsertBlock(); // Update the block's position.
  return convert(vm, asType, v);
}

llvm::Value *if_statement(Llvm &vm, SExpr *pred, SExpr *t, SExpr *f)
{
  auto cond = pred->codegen(vm);

  llvm::BasicBlock *ifso  = llvm::BasicBlock::Create(llvm::getGlobalContext(), "true");
  llvm::BasicBlock *ifnot = llvm::BasicBlock::Create(llvm::getGlobalContext(), "false");
  llvm::BasicBlock *merge = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge");
  vm.builder.CreateCondBr(cond, ifso, ifnot);

  auto ty = commonType(vm, t->ltype(vm), f->ltype(vm));
  auto tcode = if_branch(vm, t, ty, &ifso, merge);
  auto fcode = if_branch(vm, f, ty, &ifnot, merge);

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

  llvm::PHINode *phi = vm.builder.CreatePHI(ty, 2, "iftmp");
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

static bool shouldUseDouble(Llvm &vm, const std::vector<SExpr *> &xs) {
  return std::any_of(xs.begin(), xs.end(),
                     [&](auto e) { auto ty = e->ltype(vm);
                                   return ty && unPtr(ty)->isDoubleTy(); });
}

template<typename Args>
llvm::Value *boolop(Llvm &vm, const std::string &fname, const Args& args)
{
  using Maker = llvm::Value *(llvm::IRBuilder<>::*)(llvm::Value *, llvm::Value *,
                                                    const llvm::Twine &);
  using OpMap = std::map<std::string, std::pair<Maker,Maker>>;
  static OpMap boolops {
    {"<",  {&llvm::IRBuilder<>::CreateICmpSLT, &llvm::IRBuilder<>::CreateFCmpOLT}},
    {"<=", {&llvm::IRBuilder<>::CreateICmpSLE, &llvm::IRBuilder<>::CreateFCmpOLE}},
    {">",  {&llvm::IRBuilder<>::CreateICmpSGT, &llvm::IRBuilder<>::CreateFCmpOGT}},
    {">=", {&llvm::IRBuilder<>::CreateICmpSGE, &llvm::IRBuilder<>::CreateFCmpOGE}},
    {"=",  {&llvm::IRBuilder<>::CreateICmpEQ , &llvm::IRBuilder<>::CreateFCmpOEQ}},
  };

  auto it = boolops.find(fname);
  if (it == std::end(boolops))
    return nullptr;

  // Applying only one value always returns
  // Ex: (= 1) = T; (< 1) = T.
  if (args.size() == 1)
    return vm.getInt(1, 1);

  bool useDouble = shouldUseDouble(vm, args);
  auto op = useDouble ? it->second.second : it->second.first;

  llvm::Value *acc = nullptr;

  auto codegen = [&](SExpr *e) {
    llvm::Value *v = e->codegen(vm);

    llvm::Type *ty = v->getType();
    if (!ty->isDoubleTy() && !ty->isIntegerTy()) {
      std::cerr << "Error: " << it->first << " on " << to_string(ty) << std::endl;
      exit(1);
    }

    return convert(vm, useDouble ? vm.doubleTy() : vm.intTy(), v);
  };


  // Each comparison is made against the previous one.
  // Ex: (< 1 2 3) = T   => {1 < 2 ^ 2 < 3}
  //     (< 1 5 4) = NIL => {1 < 5 ^ !(5 < 4)}
  llvm::Value *last = codegen(args.front());

  for (size_t i=1; i < args.size(); i++) {
    llvm::Value *next = codegen(args[1]);
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

  using Op = llvm::Instruction::BinaryOps;
  static std::map<std::string, std::pair<Op,Op>> binops = {
    {"+", {llvm::Instruction::Add,  llvm::Instruction::FAdd}},
    {"-", {llvm::Instruction::Sub,  llvm::Instruction::FSub}},
    {"*", {llvm::Instruction::Mul,  llvm::Instruction::FMul}},
    {"/", {llvm::Instruction::SDiv, llvm::Instruction::FDiv}},
  };
  auto it = binops.find(fname);
  if (it == std::end(binops))
    return nullptr;

  if (args.size() == 0)
    return vm.getInt(0);

  // Check if the arguments are integers or double.
  bool useDouble = shouldUseDouble(vm, args);

  // Select the function from the {int, double} pair.
  Op op = useDouble ? it->second.second : it->second.first;

  auto codegen = [&](SExpr *e) {
    llvm::Value *v = e->codegen(vm);

    llvm::Type *ty = v->getType();
    if (!ty || (!ty->isDoubleTy() && !ty->isIntegerTy())) {
      std::cerr << "Error: " << it->first << " on " << to_string(ty) << std::endl;
      exit(1);
    }

    return convert(vm, useDouble ? vm.doubleTy() : vm.intTy(), v);
  };

  // LISP operators are chainable. 
  // Ex: (+ 1) = 1; (+ 1 1 1) = 3
  llvm::Value *acc = codegen(args[0]);
  for (size_t i=1; i < args.size(); i++) {
    llvm::Value *last = codegen(args[i]);
    acc = vm.builder.CreateBinOp(op, acc, last,
                                 twine(acc->getName(), fname, last->getName()));
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

    std::vector<llvm::Type *> argTys;
    for (auto a : args) {
      llvm::Type *ty = a->ltype(vm);

      if (!ty) {
        std::cerr << "Cannot define " << fname
                  << ": argument has no type" << std::endl;
        exit(1);
      }
      argTys.push_back(ty);
    }
    fname = fname_mangle(fname, argTys);

    f = vm.module->getFunction(fname);
    if (!f) {
      if (!lty) {
        std::cerr << "Unknown return type for " << fname << std::endl;
        exit(1);
      }

      varBlock(vm.vars, [&] {
        // Create the function.
        for (size_t i=0; i < args.size() && i < def->args.size(); i++)
          vm.storeVar(def->args[i], argTys[i], true);

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
        for (; ai != f->arg_end() && ii != std::end(def->args); ai++, ii++) {
          ai->setName(*ii);
          vm.storeVar(*ii, ai, true);
        }

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
    if (!val) {
      std::cerr << "Can't generate code for argument of " << fname << std::endl;
      exit(1);
    }
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
    if (v->getType()->isPointerTy())
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

llvm::Value *Double::codegen(Llvm &vm)
{
  return vm.getDouble(val);
}

llvm::Value *If::codegen(Llvm &vm)
{
  return if_statement(vm, cond.get(), t.get(), f.get());
}

llvm::Value *Setq::codegen(Llvm &vm)
{
  auto v = expr.get()->codegen(vm);
  vm.storeVar(ident, v);
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
