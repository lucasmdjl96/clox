//
// Created by lucas on 8/15/23.
//

#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "value.h"
#include "table.h"
#include "object.h"

void initVM(VM* vm);
void freeVM(VM* vm, Compiler* compiler);
InterpretResult interpret(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, const char* source);
void push(VM* vm, Value value);
Value pop(VM* vm);
void defineClockNative(VM* vm, Compiler* compiler);


#endif //CLOX_VM_H
