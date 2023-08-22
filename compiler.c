//
// Created by lucas on 8/16/23.
//

#include <stdio.h>
#include <stdlib.h>
#include "compiler.h"
#include "scanner.h"
#include "object.h"

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

typedef void (*ParseFn)(VM*, Parser*, Scanner*, Chunk*, bool);

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

static bool check(Parser* parser, TokenType type) {
    return parser->current.type == type;
}

static bool match(Parser* parser, Scanner* scanner, TokenType type) {
    if (!check(parser, type)) return false;
    advance(parser, scanner);
    return true;
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

static void parsePrecedence(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, Precedence precedence);
static ParseRule* getRule(TokenType type);
static void expression(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk);
static void statement(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk);
static void declaration(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk);

static void binary(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    TokenType operatorType = parser->previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(vm, parser, scanner, currentChunk, (Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:
            emitBytes(parser, currentChunk, OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUAL_EQUAL:
            emitByte(parser, currentChunk, OP_EQUAL);
            break;
        case TOKEN_GREATER:
            emitByte(parser, currentChunk, OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytes(parser, currentChunk, OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            emitByte(parser, currentChunk, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            emitBytes(parser, currentChunk, OP_GREATER, OP_NOT);
            break;
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

static void literal(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    switch (parser->previous.type) {
        case TOKEN_FALSE:
            emitByte(parser, currentChunk, OP_FALSE);
            break;
        case TOKEN_TRUE:
            emitByte(parser, currentChunk, OP_TRUE);
            break;
        case TOKEN_NIL:
            emitByte(parser, currentChunk, OP_NIL);
            break;
        default:
            return; // Unreachable.
    }
}

static void grouping(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    expression(vm, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    double value = strtod(parser->previous.start, NULL);
    emitConstant(parser, currentChunk, NUMBER_VAL(value));
}

static void string(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    emitConstant(parser, currentChunk, OBJ_VAL(copyString(vm, parser->previous.start + 1,
                                                          parser->previous.length - 2)));
}

static uint8_t identifierConstant(VM* vm, Parser* parser, Chunk* currentChunk, Token* name) {
    return makeConstant(parser, currentChunk, OBJ_VAL(copyString(vm, name->start, name->length)));
}

static void namedVariable(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, Token name, bool canAssign) {
    uint8_t arg = identifierConstant(vm, parser, currentChunk, &name);

    if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        expression(vm, parser, scanner, currentChunk);
        emitBytes(parser, currentChunk, OP_SET_GLOBAL, arg);
    } else {
        emitBytes(parser, currentChunk, OP_GET_GLOBAL, arg);
    }
}

static void variable(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    namedVariable(vm, parser, scanner, currentChunk, parser->previous, canAssign);
}

static void unary(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    TokenType  operatorType = parser->previous.type;

    // Compile the operand.
    parsePrecedence(vm, parser, scanner, currentChunk, PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG:
            emitByte(parser, currentChunk, OP_NOT);
            break;
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
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, Precedence precedence) {
    advance(parser, scanner);
    ParseFn prefixRule = getRule(parser->previous.type)->prefix;
    if (prefixRule == NULL) {
        error(parser, "Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNEMENT;
    prefixRule(vm, parser, scanner, currentChunk, canAssign);

    while (precedence <= getRule(parser->current.type)->precedence) {
        advance(parser, scanner);
        ParseFn infixRule = getRule(parser->previous.type)->infix;
        infixRule(vm, parser, scanner, currentChunk, canAssign);
    }

    if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        error(parser, "Invalid assignment target.");
    }
}


static uint8_t parseVariable(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk, const char* errorMessage) {
    consume(parser, scanner, TOKEN_IDENTIFIER, errorMessage);
    return identifierConstant(vm, parser, currentChunk, &parser->previous);
}

static void defineVariable(Parser* parser, Chunk* currentChunk, uint8_t global) {
    emitBytes(parser, currentChunk, OP_DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void initParser(Parser* parser) {
    parser->hadError = false;
    parser->panicMode = false;
}

static void expression(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    parsePrecedence(vm, parser, scanner, currentChunk, PREC_ASSIGNEMENT);
}

static void varDeclaration(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    uint8_t global = parseVariable(vm, parser, scanner, currentChunk, "Expect variable name.");

    if (match(parser, scanner, TOKEN_EQUAL)) {
        expression(vm, parser, scanner, currentChunk);
    } else {
        emitByte(parser, currentChunk, OP_NIL);
    }
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(parser, currentChunk, global);
}

static void expressionStatement(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    expression(vm, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(parser, currentChunk, OP_POP);
}

static void printStatement(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    expression(vm, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(parser, currentChunk, OP_PRINT);
}

static void synchronize(Parser* parser, Scanner* scanner) {
    parser->panicMode = false;

    while (parser->current.type != TOKEN_EOF) {
        if (parser->previous.type == TOKEN_SEMICOLON) return;
        switch (parser->current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:
                ; // Do nothing.
        }

        advance(parser, scanner);
    }
}

static void declaration(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    if (match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(vm, parser, scanner, currentChunk);
    } else {
        statement(vm, parser, scanner, currentChunk);
    }

    if (parser->panicMode) synchronize(parser, scanner);
}

static void statement(VM* vm, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    if (match(parser, scanner, TOKEN_PRINT)) {
        printStatement(vm, parser, scanner, currentChunk);
    } else {
        expressionStatement(vm, parser, scanner, currentChunk);
    }
}

bool compile(VM* vm, const char* source, Chunk* chunk) {
    Scanner scanner;
    initScanner(&scanner, source);
    Chunk* currentChunk = chunk;

    Parser parser;
    initParser(&parser);
    advance(&parser, &scanner);

    while (!match(&parser, &scanner, TOKEN_EOF)) {
        declaration(vm, &parser, &scanner, currentChunk);
    }

    endCompiler(&parser, currentChunk);
    return !parser.hadError;
}