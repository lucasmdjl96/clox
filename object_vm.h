//
// Created by lmdjo on 24/08/2023.
//

#ifndef CLOX_OBJECT_VM_H
#define CLOX_OBJECT_VM_H

#include "vm.h"

ObjClosure* newClosure(VM* vm, ObjFunction* function);
ObjFunction* newFunction(VM* vm);
ObjNative* newNative(VM* vm, NativeFn function, int arity);
ObjString* takeString(VM* vm, char* chars, int length);
ObjString* copyString(VM* vm, const char* chars, int length);
ObjUpvalue* newUpvalue(VM*, Value* slot);

#endif //CLOX_OBJECT_VM_H
