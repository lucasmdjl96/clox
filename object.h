//
// Created by lmdjo on 20/08/2023.
//

#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "common.h"
#include "value.h"
#include "chunk.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value) ((ObjNative*)AS_OBJ(value))
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)


void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

ObjClass* newClass(VM* vm, Compiler* compiler, ObjString* name);
ObjClosure* newClosure(VM* vm, Compiler* compiler, ObjFunction* function);
ObjFunction* newFunction(VM* vm, Compiler* compiler);
ObjInstance* newInstance(VM* vm, Compiler* compiler, ObjClass* klass);
ObjNative* newNative(VM* vm, Compiler* compiler, NativeFn function, int arity);
ObjString* takeString(VM* vm, Compiler* compiler, char* chars, int length);
ObjString* copyString(VM* vm, Compiler* compiler, const char* chars, int length);
ObjUpvalue* newUpvalue(VM*, Compiler* compiler, Value* slot);

#endif //CLOX_OBJECT_H
