//
// Created by lucas on 8/16/23.
//

#include <stdio.h>
#include <stdlib.h>
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef enum {
    PREC_NONE,
    PREC_ASSIGNEMENT,   // =
    PREC_OR,            // or
    PREC_AND,           // and
    PREC_EQUALITY,      // == !=
    PREC_COMPARISON,    // < <= > >=
    PREC_TERM,          // + -
    PREC_FACTOR,        // * /
    PREC_UNARY,         // ! -
    PREC_CALL,          // . ()
    PREC_PRIMARY,
} Precedence;

typedef void (*ParseFn)(Parser*, Scanner*, Chunk*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void errorAt(Parser* parser, Token* token, const char* message) {
    if (parser->panicMode) return;
    parser->panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser->hadError = true;
}

static void error(Parser* parser, const char* message) {
    errorAt(parser, &parser->previous, message);
}

static void errorArCurrent(Parser* parser, const char* message) {
    errorAt(parser, &parser->current, message);
}

static void advance(Parser* parser, Scanner* scanner) {
    parser->previous = parser->current;

    for (;;) {
        parser->current = scanToken(scanner);
        if (parser->current.type != TOKEN_ERROR) break;

        errorArCurrent(parser, parser->current.start);
    }
}

static void consume(Parser* parser, Scanner* scanner, TokenType type, const char* message) {
    if (parser->current.type == type) {
        advance(parser, scanner);
        return;
    }

    errorArCurrent(parser, message);
}

static void emitByte(Parser* parser, Chunk* currentChunk, uint8_t byte) {
    writeChunk(currentChunk, byte, parser->previous.line);
}

static void emitReturn(Parser* parser, Chunk* currentChunk) {
    emitByte(parser, currentChunk, OP_RETURN);
}

static void emitBytes(Parser* parser, Chunk* currentChunk, uint8_t byte1, uint8_t byte2) {
    emitByte(parser, currentChunk, byte1);
    emitByte(parser, currentChunk, byte2);
}

static uint8_t makeConstant(Parser* parser, Chunk* currentChunk, Value value) {
    int constant = addConstant(currentChunk, value);
    if (constant > UINT8_MAX) {
        error(parser, "Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Parser* parser, Chunk* currentChunk, Value value) {
    emitBytes(parser, currentChunk, OP_CONSTANT, makeConstant(parser, currentChunk, value));
}

static void endCompiler(Parser* parser, Chunk* currentChunk) {
    emitReturn(parser, currentChunk);
#ifdef DEBUG_PRINT_CODE
    if (!parser->hadError) {
        disassembleChunk(currentChunk, "code");
    }
#endif
}

static void parsePrecedence(Parser* parser, Scanner* scanner, Chunk* currentChunk, Precedence precedence);
static ParseRule* getRule(TokenType type);
static void expression(Parser* parser, Scanner* scanner, Chunk* currentChunk);

static void binary(Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    TokenType operatorType = parser->previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(parser, scanner, currentChunk, (Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_PLUS:
            emitByte(parser, currentChunk, OP_ADD);
            break;
        case TOKEN_MINUS:
            emitByte(parser, currentChunk, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emitByte(parser, currentChunk, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emitByte(parser, currentChunk, OP_DIVIDE);
            break;
        default:
            return; // Unreachable.
    }
}

static void grouping(Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    expression(parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    double value = strtod(parser->previous.start, NULL);
    emitConstant(parser, currentChunk, value);
}

static void unary(Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    TokenType  operatorType = parser->previous.type;

    // Compile the operand.
    parsePrecedence(parser, scanner, currentChunk, PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_MINUS:
            emitByte(parser, currentChunk, OP_NEGATE);
            break;
        default:
            return; // Unreachable
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {NULL, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_GREATER] = {NULL, NULL, PREC_NONE},
    [TOKEN_GREATER_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_LESS] = {NULL, NULL, PREC_NONE},
    [TOKEN_LESS_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_IDENTIFIER] = {NULL, NULL, PREC_NONE},
    [TOKEN_STRING] = {NULL, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {NULL, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {NULL, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Parser* parser, Scanner* scanner, Chunk* currentChunk, Precedence precedence) {
    advance(parser, scanner);
    ParseFn prefixRule = getRule(parser->previous.type)->prefix;
    if (prefixRule == NULL) {
        error(parser, "Expect expression.");
        return;
    }

    prefixRule(parser, scanner, currentChunk);

    while (precedence <= getRule(parser->current.type)->precedence) {
        advance(parser, scanner);
        ParseFn infixRule = getRule(parser->previous.type)->infix;
        infixRule(parser, scanner, currentChunk);
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void initParser(Parser* parser) {
    parser->hadError = false;
    parser->panicMode = false;
}

static void expression(Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    parsePrecedence(parser, scanner, currentChunk, PREC_ASSIGNEMENT);
}

bool compile(const char* source, Chunk* chunk) {
    Scanner scanner;
    initScanner(&scanner, source);
    Chunk* currentChunk = chunk;

    Parser parser;
    initParser(&parser);
    advance(&parser, &scanner);
    expression(&parser, &scanner, currentChunk);
    consume(&parser, &scanner, TOKEN_EOF, "Expect end of expression.");
    endCompiler(&parser, currentChunk);
    return !parser.hadError;
}