//
// Created by lmdjo on 21/08/2023.
//

#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "value.h"
#include "vm.h"

void initTable(Table* table);
void freeTable(VM* vm, Compiler* compiler, Table* table);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableSet(VM* vm, Compiler* compiler, Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, ObjString* key);
void tableAddAll(VM* vm, Compiler* compiler, Table* from, Table* to);
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);
void tableRemoveWhite(Table* table);

#endif //CLOX_TABLE_H
