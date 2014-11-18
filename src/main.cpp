
#include <initializer_list>
#include <iostream>
#include <fstream>
#include <array>
#include <type_traits>
#include <algorithm>
#include <memory>
#include <stdio.h>
#include <utility>

#include <vector>
#include <map>

#include "ast.h"
#include "helpers.h"
#include "llvm.h"
#include "parser.h"

int main(int argc, char **argv)
{
  Llvm vm;
  vm.registerExtern(vm.intTy(), "printf", true);

  std::unique_ptr<Reader> r;
  std::ifstream src;
  if (argc == 2) {
    src.open(argv[1]);
    r.reset(new Reader(argv[1], src));
  } else {
    r.reset(new Reader("-", std::cin));
  }

  msg("Parse:");
  std::vector<std::unique_ptr<SExpr>> toplevel;
  while (SExpr *e = sexpr(*r))
    toplevel.emplace_back(e);

  msg("Compile:");
  llvm::Value *last;
  for (auto &e : toplevel)
    last = e->codegen(vm);
  vm.builder.CreateRet(last);
  vm.module->dump();

  std::string err;
  std::unique_ptr<llvm::ExecutionEngine> ee(
    llvm::EngineBuilder(std::move(vm.module)).setErrorStr(&err).create());
  if (!ee) {
    std::cerr << "Could not create exe engine: " << err << std::endl;
    return 1;
  }
  ee->runFunction(vm.prog, {});
}
