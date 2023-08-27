//
// Created by lucas on 8/16/23.
//

#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"
#include "scanner.h"
#include "object.h"

void initParser(Parser* parser);
void initCompiler(VM* vm, Compiler* current, Compiler* compiler, Parser* parser, FunctionType type);
ObjFunction* compile(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, const char* source);
void markCompilerRoots(VM* vm, Compiler* compiler);

#endif //CLOX_COMPILER_H
