
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

llvm::Type *lisp_to_vm(LispType lt)
{
  auto &ctx = llvm::getGlobalContext();
  switch(lt) {
    case VOID:   return llvm::Type::getVoidTy(ctx);
    case INT:    return llvm::Type::getIntNTy(ctx, sizeof(int) * 8);
    case DOUBLE: return llvm::Type::getDoubleTy(ctx);
    case STRING_LIT: return llvm::Type::getIntNPtrTy(ctx, 8);

    default: std::cerr << "unhandled type: " << std::endl;
             exit(1);
  }
}

LispType vm_to_lisp(llvm::Type *ty) {
  if (!ty) return NONE;

  switch(ty->getTypeID()) {
    case llvm::Type::VoidTyID:     return VOID;
    case llvm::Type::IntegerTyID:  return INT;
    case llvm::Type::DoubleTyID:   return DOUBLE;

    case llvm::Type::ArrayTyID:    return vm_to_lisp(ty->getArrayElementType());

    case llvm::Type::PointerTyID:
      ty = ty->getContainedType(0);
      return ty->isIntegerTy(8) ? STRING_LIT : vm_to_lisp(ty);

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

static std::string to_string(LispType ty) {
  switch(ty) {
    case NONE:   return "???";
    case VOID:   return "void";
    case NUMBER: return "number";
    case INT:    return "int";
    case DOUBLE: return "double";
    case STRING_LIT: return "string";

    default: std::cerr << "unhandled type" << std::endl;
             exit(1);
  }
}

/// Checks if a type can represent a real value.
static bool isConcrete(LispType ty) {
  return ty > NONE && ty != NUMBER && ty != NUMBER_END && ty < N_LISP_TYPES;
}

/// Mangles a function name for use in overloading.
/// Ex: (f 1)   => call f:int
///     (f 1.0) => call f:double
template<typename Args>
static std::string fname_mangle(const std::string &fname, const Args &args) {
  std::string mangled;
  for (auto a : args) {
    if (mangled.size()) mangled.push_back(',');
    mangled += to_string(a);
  }
  return fname + ":" + mangled;
}

static bool isNumber(LispType ty) {
  return ty >= NUMBER && ty < NUMBER_END;
}

static bool isNumber(llvm::Type *ty) {
  return ty->isDoubleTy() || ty->isIntegerTy();
}

static bool isNumber(llvm::Value *ty) {
  return isNumber(ty->getType());
}

/// Returns the common type for use in if statement and binary operator codegen.
static LispType commonType(LispType a, LispType b) {
  // A type of 0 may mean a failure to type due to recursion, in which case,
  // the other type represents the non-recursive branch.
  if (!a) return b;
  if (!b) return a;
  if (isNumber(a))
    return isNumber(b) ? std::max(a, b) : VOID;
  return a == b ? a : VOID;
}

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

llvm::Value *progn_code(Llvm &vm, Progn &prog)
{
  llvm::Value *last = nullptr;
  for (auto &e : prog.body)
    last = e->codegen(vm);
  return last;
}

llvm::Function *defineFunction(Llvm &vm, Defun *def, const std::string &fname,
                               const std::vector<LispType> &argTys)
{
  // To avoid recursion, keep track of all the functions we have tried to
  // deduce the type of.
  static std::set<std::string> history;
  if (history.find(fname) == history.end())
    history.insert(fname);
  else
    return nullptr;

  auto f = vm.module->getFunction(fname);
  if (f) return f;

  // Deduce the return type.
  LispType lty = NONE;
  varBlock(vm.vars, [&] {
    for (size_t i=0; i < argTys.size() && i < def->args.size(); i++)
      vm.storeVar(def->args[i], argTys[i], true);
    lty = def->ltype(vm);
  });
  std::vector<llvm::Type *> tys;
  std::transform(argTys.begin(), argTys.end(), std::back_inserter(tys),
                 lisp_to_vm);

  auto fty = llvm::FunctionType::get(lisp_to_vm(lty), tys, false);
  f = llvm::Function::Create(fty, llvm::GlobalValue::InternalLinkage,
                             fname, vm.module.get());

  if (!f) {
    std::cerr << "Could not create function: " << fname << std::endl;
    exit(1);
  }

  auto ip = vm.builder.GetInsertBlock();
  vm.builder.SetInsertPoint(
      llvm::BasicBlock::Create(llvm::getGlobalContext(), fname, f)
  );

  varBlock(vm.vars, [&] {
    auto ii = std::begin(def->args);  // Identifier iterator.
    auto ai = f->arg_begin();         // Function argument iterator.
    auto ti = std::begin(argTys);     // Argument type iterator
    for (; ai != f->arg_end() && ii != std::end(def->args);
         ai++, ii++, ti++) {
    ai->setName(*ii);
    vm.storeVar(*ii, ai, *ti, true);
    }

    vm.builder.CreateRet(progn_code(vm, *def->prog));
  });

  // Put back the previous insert point.
  vm.builder.SetInsertPoint(ip);

  return f;
}



// -- L-TYPE -- //

static LispType functionOrVarTy(Llvm &vm, const std::string &name) {
  if (name == "T" || name == "NIL")
    return INT;

  LispType ty = NONE;

  if (Defun *f = getVar(vm.functions, name))
    ty = f->ltype(vm);
  else if(LValue *var = vm.getVar(name))
    ty = var->ltype;

  return ty;
}

LispType Symbol::ltype(Llvm &vm) {
  return functionOrVarTy(vm, ident);
}

LispType String::ltype(Llvm &) {
  return STRING_LIT;
}

LispType Int::ltype(Llvm &) {
  return INT;
}

LispType Double::ltype(Llvm &vm) {
  return DOUBLE;
}

LispType If::ltype(Llvm &vm) {
  return f ? commonType(t->ltype(vm), f->ltype(vm)) : VOID;
}

LispType Setq::ltype(Llvm &vm) {
  auto ty = expr->ltype(vm);
  vm.storeVar(ident, ty);
  return ty;
}

LispType Progn::ltype(Llvm &vm) {
  LispType ty = NONE;

  // Statements in progn may declare variables that will be replaced when we
  // call codegen().
  varBlock(vm.vars, [&] {
    for (auto &expr : body)
      ty = expr->ltype(vm);
  });
  return ty;
}

LispType Defun::ltype(Llvm &vm) {
  return prog->ltype(vm);
}

LispType List::ltype(Llvm &vm) {
  if (items.size() == 1) return items.front()->ltype(vm);

  // A function invocation:
  Symbol *fsym = (Symbol *) items.front().get();
  if (!fsym || !fsym->is_sym) return NONE;

  const std::string &name = fsym->ident;
  if (oneOf({"<", ">", "=", "<=", ">="}, name))
    return INT;  // TODO: Should be bool.

  if (oneOf({"+", "-", "*", "/"}, name)) {
    LispType retTy = NUMBER;
    for (size_t i=1; i < items.size(); i++) {
      auto ty = items[i]->ltype(vm);

      if (!ty)     // Recursively defined functions can't be typed immediately,
        continue;  // but we can give a guess and error out later if it's wrong.

      retTy = commonType(retTy, ty);
    }
    return retTy;
  }

  // May already an external function like "printf". (Imported in main().)
  if (auto f = vm.module->getFunction(name))
    return vm_to_lisp(f->getReturnType());

  Defun *def = getVar(vm.functions, fsym->ident);
  if (!def) return NONE;

  // TODO: Partial deduction.
  std::vector<LispType> argTys;
  for (size_t i=1; i < items.size(); i++) {
    LispType ty = items[i]->ltype(vm);
    if (!isConcrete(ty)) return NONE;
    argTys.push_back(ty);
  }
  std::string fname = fname_mangle(fsym->ident, argTys);

  // May have already defined this.
  auto f = vm.module->getFunction(fname);
  if (!f) f = defineFunction(vm, def, fname, argTys);
  return f ?  vm_to_lisp(f->getReturnType()) : NONE;
}

/// Does basic conversion on the value, `src` to the target type, `ty`.
llvm::Value *convert(Llvm &vm, llvm::Type *ty, llvm::Value *src) {
  if (!ty || src->getType() == ty)
    return src;

  auto srcTy = src->getType();

  using Cast = llvm::CastInst;
  auto op = Cast::getCastOpcode(src, true, ty, true);

  if (!Cast::castIsValid(op, src, ty)) {
    return src;
    std::cerr << "Error: Invalid cast\n";
    std::cerr << "from: " << std::flush;
    src->dump();
    src->getType()->dump();
    std::cerr << "to: " << std::flush;
    ty->dump();
    exit(1);
  }

  return vm.builder.CreateCast(op, src, ty,
                               twine(src->getName(), ".", to_string(ty)));
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
  return asType ? convert(vm, asType, v) : v;
}

llvm::Value *if_statement(Llvm &vm, SExpr *pred, SExpr *t, SExpr *f)
{
  auto cond = pred->codegen(vm);
  auto cname = cond->getName();

  auto &ctx = llvm::getGlobalContext();
  auto *ifso  = llvm::BasicBlock::Create(ctx, twine("if ", cname));
  auto *merge = llvm::BasicBlock::Create(ctx, twine("endif ", cname));

  if (!f) {  // No false branch?
    vm.builder.CreateCondBr(cond, ifso, merge);
    if_branch(vm, t, nullptr, &ifso, merge);
    push_block(vm, merge);
    return nullptr;
  }

  auto *ifnot = llvm::BasicBlock::Create(ctx, twine("if-not ", cname));

  vm.builder.CreateCondBr(cond, ifso, ifnot);

  auto ty = lisp_to_vm(commonType(t->ltype(vm), f->ltype(vm)));
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

  llvm::PHINode *phi = vm.builder.CreatePHI(ty, 2, twine(tcode->getName(), " or ",
                                                         fcode->getName()));
  phi->addIncoming(tcode, ifso);
  phi->addIncoming(fcode, ifnot);
  return phi;
}

static bool shouldUseDouble(Llvm &vm, const std::vector<SExpr *> &xs) {
  return std::any_of(xs.begin(), xs.end(),
                     [&](auto e) { return e->ltype(vm) == DOUBLE; });
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

    if (!isNumber(v)) {
      std::cerr << "Error: " << it->first << " on " << std::flush;
      v->getType()->dump();
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
    if (!isNumber(v)) {
      std::cerr << "Error: " << it->first << " on "
                << to_string(v->getType()) << std::endl;
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
                  const std::vector<SExpr *> &args)
{
  // Check if this is a binary operation.
  if (llvm::Value *v = binop(vm, fname, args))
    return v;

  // Regular function.
  auto f = vm.module->getFunction(fname);
  if (!f) {
    Defun *def = getVar(vm.functions, fname);
    if (!def) return nullptr;

    std::vector<LispType> argTys;
    for (auto a : args) {
      LispType ty = a->ltype(vm);

      if (!isConcrete(ty)) {
        std::cerr << "Cannot define " << fname
                  << ": argument is a " << to_string(ty) << std::endl;
        exit(1);
      }
      argTys.push_back(ty);
    }

    fname = fname_mangle(fname, argTys);
    f = vm.module->getFunction(fname);
    
    if (!f)
      defineFunction(vm, def, fname, argTys);
  }
  
  f = vm.module->getFunction(fname);
  if (!f) {
    std::cerr << "Undefined function: " << fname << std::endl;
    exit(1);
  }

  // Build the name of the result for easier IR reading.
  std::string resName;

  std::vector<llvm::Value *> fargs;
  auto argTyIt = f->getArgumentList().begin();
  for (auto &sexp : args) {
    llvm::Value *val = sexp->codegen(vm);
    if (!val) {
      std::cerr << "Can't generate code for argument of " << fname << std::endl;
      exit(1);
    }
    fargs.push_back(convert(vm, (argTyIt++)->getType(), val));

    if (!resName.empty())
      resName.push_back(',');
    resName += val->getName();
  }

  return vm.builder.CreateCall(f, fargs, twine(fname, "(" + resName + ")"));
}

llvm::Value *Symbol::codegen(Llvm &vm)
{
  if (LValue *var = vm.getVar(ident)) {
    if (var->val->getType()->isPointerTy())
      return vm.builder.CreateLoad(var->val, ident);
    else
      return var->val;
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
  auto v = expr->codegen(vm);
  vm.storeVar(ident, v, expr->ltype(vm));
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
    if (llvm::Value *v = call(vm, sym->ident, args))
      return v;

    std::cerr << "Unknown function or variable: " << sym->ident << std::endl;
    exit(1);
  }
  return nullptr;
}
