LLVM_CXXFLAGS := `llvm-config --cxxflags`
LLVM_LDFLAGS := `llvm-config --ldflags --libs --system-libs`
CXX := clang++
CXXFLAGS := -fno-rtti -O0 -g -std=c++14

OBJ = build/main.o build/helpers.o build/llvm.o build/ast.o build/parser.o

.PHONY: builddir all

all : builddir lisp

lisp : $(OBJ)
	$(CXX) $(LLVM_CXXFLAGS) $^ $(LLVM_LDFLAGS) -o lisp

build/main.o : src/main.cpp src/helpers.h src/llvm.h src/parser.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< -c -o $@

build/helpers.o : src/helpers.cpp src/helpers.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< -c -o $@

build/llvm.o : src/llvm.cpp src/ast.h src/helpers.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< -c -o $@

build/ast.o : src/ast.cpp src/llvm.h src/helpers.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< -c -o $@

build/parser.o : src/parser.cpp src/parser.h src/ast.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< -c -o $@

builddir : build
build : 
	mkdir -p build

