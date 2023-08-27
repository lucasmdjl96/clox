//
// Created by lucas on 8/13/23.
//

#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"

void initChunk(Chunk* chunk);
void freeChunk(VM* vm, Compiler* compiler, Chunk* chunk);
void writeChunk(VM* vm, Compiler* compiler, Chunk* chunk, uint8_t byte, int line);
int addConstant(VM* vm, Compiler* compiler, Chunk* chunk, Value value);

#endif //CLOX_CHUNK_H
