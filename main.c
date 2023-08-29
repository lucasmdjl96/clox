
#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "scanner.h"
#include "compiler.h"

static void repl() {
    Parser parser;
    initParser(&parser);
    VM vm;
    initVM(&vm);
    Compiler compiler;
    initCompiler(&vm, NULL, &compiler, &parser, TYPE_SCRIPT);
    vm.initString = NULL;
    vm.initString = copyString(&vm, &compiler, "init", 4);
    defineClockNative(&vm, &compiler);
    char line[1024];
    setvbuf(stdout, NULL, _IONBF, 0);
    for (;;) {
        printf(">");

        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        Scanner scanner;
        initScanner(&scanner, line);

        interpret(&vm, &compiler, &parser, &scanner, line);
    }
    freeVM(&vm, &compiler);
}

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*)malloc(fileSize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char* path) {
    char* source = readFile(path);
    Scanner scanner;
    initScanner(&scanner, source);
    Parser parser;
    initParser(&parser);
    VM vm;
    initVM(&vm);
    Compiler compiler;
    initCompiler(&vm, NULL, &compiler, &parser, TYPE_SCRIPT);
    vm.initString = NULL;
    vm.initString = copyString(&vm, &compiler, "init", 4);
    defineClockNative(&vm, &compiler);
    InterpretResult result = interpret(&vm, &compiler, &parser, &scanner, source);
    free(source);

    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);
    freeVM(&vm, &compiler);
}

int main(int argc, const char* argv[]) {

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: clox [path]\n");
        exit(64);
    }

    return 0;
}
