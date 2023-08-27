//
// Created by lucas on 8/13/23.
//

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

void freeChunk(VM* vm, Compiler* compiler, Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(vm, compiler, &chunk->constants);
    initChunk(chunk);
}

void writeChunk(VM* vm, Compiler* compiler, Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

int addConstant(VM* vm, Compiler* compiler, Chunk* chunk, Value value) {
    push(vm, value);
    writeValueArray(vm, compiler, &chunk->constants, value);
    pop(vm);
    return chunk->constants.count - 1; // index
}
