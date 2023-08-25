//
// Created by lucas on 8/16/23.
//

#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"
#include "scanner.h"
#include "object.h"

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef struct {
    Token name;
    int depth;
} Local;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
} FunctionType;

typedef struct {
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

ObjFunction* compile(VM* vm, const char* source);

#endif //CLOX_COMPILER_H
