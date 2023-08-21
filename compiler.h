//
// Created by lucas on 8/16/23.
//

#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"
#include "scanner.h"

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

bool compile(VM* vm, const char* source, Chunk* chunk);

#endif //CLOX_COMPILER_H
