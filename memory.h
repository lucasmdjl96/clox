//
// Created by lucas on 8/13/23.
//

#ifndef CLOX_MEMORY_H
#define CLOX_MEMORY_H

#include "common.h"
#include "object.h"
#include "vm.h"

#define ALLOCATE(type, count) \
    (type*)reallocate(vm, compiler, NULL, 0, sizeof(type) * (count))

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define FREE(type, pointer) reallocate(vm, compiler, pointer, sizeof(type), 0)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(vm, compiler, pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(vm, compiler, pointer, sizeof(type) * (oldCount), 0)

void* reallocate(VM* vm, Compiler* compiler, void* pointer, size_t oldSize, size_t newSize);
void markObject(VM* vm, Obj* object);
void markValue(VM* vm, Value value);
void collectGarbage(VM* vm, Compiler* compiler);
void* freeObjects(VM* vm, Compiler* compiler);
void markTable(VM* vm, Table* table);

#endif //CLOX_MEMORY_H
