//
// Created by lucas on 8/16/23.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

typedef void (*ParseFn)(VM*, Compiler*, Parser*, Scanner*, Chunk*, bool);

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

static int emitLoop(Parser* parser, Chunk* currentChunk, int loopStart) {
    emitByte(parser, currentChunk, OP_LOOP);

    int offset = currentChunk->count - loopStart + 2;
    if (offset > UINT16_MAX) error(parser, "Loop body too large.");

    emitByte(parser, currentChunk, (offset >> 8) & 0xff);
    emitByte(parser, currentChunk, offset & 0xff);
}

static int emitJump(Parser* parser, Chunk* currentChunk, uint8_t instruction) {
    emitByte(parser, currentChunk, instruction);
    emitByte(parser, currentChunk, 0xff);
    emitByte(parser, currentChunk, 0xff);
    return currentChunk->count - 2;
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

static void patchJump(Parser* parser, Chunk* currentChunk, int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk->count - offset - 2;

    if (jump > UINT16_MAX) {
        error(parser, "Too much code to jump over.");
    }

    currentChunk->code[offset] = (jump >> 8) & 0xff;
    currentChunk->code[offset + 1] = jump & 0xff;

}

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
}

static void endCompiler(Parser* parser, Chunk* currentChunk) {
    emitReturn(parser, currentChunk);
#ifdef DEBUG_PRINT_CODE
    if (!parser->hadError) {
        disassembleChunk(currentChunk, "code");
    }
#endif
}

static void beginScope(Compiler* compiler) {
    compiler->scopeDepth++;
}

static void endScope(Compiler* compiler, Parser* parser, Chunk* currentChunk) {
    compiler->scopeDepth--;

    while (compiler->localCount > 0 &&
        compiler->locals[compiler->localCount - 1].depth > compiler->scopeDepth) {
        emitByte(parser, currentChunk, OP_POP);
        compiler->localCount--;
    }
}

static void parsePrecedence(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, Precedence precedence);
static ParseRule* getRule(TokenType type);
static void expression(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk);
static void statement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk);
static void declaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk);

static void binary(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    TokenType operatorType = parser->previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(vm, compiler, parser, scanner, currentChunk, (Precedence)(rule->precedence + 1));

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

static void literal(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
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

static void grouping(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    expression(vm, compiler, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    double value = strtod(parser->previous.start, NULL);
    emitConstant(parser, currentChunk, NUMBER_VAL(value));
}

static void or_(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    int elseJump = emitJump(parser, currentChunk, OP_JUMP_IF_FALSE);
    int endJump = emitJump(parser, currentChunk, OP_JUMP);

    patchJump(parser, currentChunk,elseJump);
    emitByte(parser, currentChunk, OP_POP);

    parsePrecedence(vm, compiler, parser, scanner, currentChunk, PREC_OR);
    patchJump(parser, currentChunk, endJump);
}

static void and_(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    int endJump = emitJump(parser, currentChunk, OP_JUMP_IF_FALSE);

    emitByte(parser, currentChunk, OP_POP);
    parsePrecedence(vm, compiler, parser, scanner, currentChunk, PREC_AND);

    patchJump(parser, currentChunk, endJump);
}

static void string(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    emitConstant(parser, currentChunk, OBJ_VAL(copyString(vm, parser->previous.start + 1,
                                                          parser->previous.length - 2)));
}

static uint8_t identifierConstant(VM* vm, Parser* parser, Chunk* currentChunk, Token* name) {
    return makeConstant(parser, currentChunk, OBJ_VAL(copyString(vm, name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Parser* parser, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error(parser, "Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static void addLocal(Compiler* compiler, Parser* parser, Token name) {
    if (compiler->localCount == UINT8_COUNT) {
        error(parser, "Too many local variables in function.");
        return;
    }

    Local* local = &compiler->locals[compiler->localCount++];
    local->name = name;
    local->depth = -1;
}

static void declareVariable(Compiler* compiler, Parser* parser) {
    if (compiler->scopeDepth == 0) return;

    Token* name = &parser->previous;
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (local->depth != -1 && local->depth < compiler->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error(parser, "Already a variable with this name in this scope.");
        }
    }

    addLocal(compiler, parser, *name);
}

static void namedVariable(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(compiler, parser, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(vm, parser, currentChunk, &name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        expression(vm, compiler, parser, scanner, currentChunk);
        emitBytes(parser, currentChunk, setOp, (uint8_t)arg);
    } else {
        emitBytes(parser, currentChunk, getOp, (uint8_t)arg);
    }
}

static void variable(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    namedVariable(vm, compiler, parser, scanner, currentChunk, parser->previous, canAssign);
}

static void unary(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, bool canAssign) {
    TokenType  operatorType = parser->previous.type;

    // Compile the operand.
    parsePrecedence(vm, compiler, parser, scanner, currentChunk, PREC_UNARY);

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
    [TOKEN_AND] = {NULL, and_, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_NONE},
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

static void parsePrecedence(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk, Precedence precedence) {
    advance(parser, scanner);
    ParseFn prefixRule = getRule(parser->previous.type)->prefix;
    if (prefixRule == NULL) {
        error(parser, "Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNEMENT;
    prefixRule(vm, compiler, parser, scanner, currentChunk, canAssign);

    while (precedence <= getRule(parser->current.type)->precedence) {
        advance(parser, scanner);
        ParseFn infixRule = getRule(parser->previous.type)->infix;
        infixRule(vm, compiler, parser, scanner, currentChunk, canAssign);
    }

    if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        error(parser, "Invalid assignment target.");
    }
}


static uint8_t parseVariable(VM* vm, Compiler* compiler,  Parser* parser, Scanner* scanner, Chunk* currentChunk, const char* errorMessage) {
    consume(parser, scanner, TOKEN_IDENTIFIER, errorMessage);

    declareVariable(compiler, parser);
    if (compiler->scopeDepth > 0) return 0;

    return identifierConstant(vm, parser, currentChunk, &parser->previous);
}

static void markInitialized(Compiler* compiler) {
    compiler->locals[compiler->localCount - 1].depth = compiler->scopeDepth;
}

static void defineVariable(Compiler* compiler, Parser* parser, Chunk* currentChunk, uint8_t global) {
    if (compiler->scopeDepth > 0) {
        markInitialized(compiler);
        return;
    }
    emitBytes(parser, currentChunk, OP_DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void initParser(Parser* parser) {
    parser->hadError = false;
    parser->panicMode = false;
}

static void expression(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    parsePrecedence(vm, compiler, parser, scanner, currentChunk, PREC_ASSIGNEMENT);
}

static void block(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
        declaration(vm, compiler, parser, scanner, currentChunk);
    }

    consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void varDeclaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    uint8_t global = parseVariable(vm, compiler, parser, scanner, currentChunk, "Expect variable name.");

    if (match(parser, scanner, TOKEN_EQUAL)) {
        expression(vm, compiler, parser, scanner, currentChunk);
    } else {
        emitByte(parser, currentChunk, OP_NIL);
    }
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(compiler, parser, currentChunk, global);
}

static void expressionStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    expression(vm, compiler, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(parser, currentChunk, OP_POP);
}

static void forStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    beginScope(compiler);
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(parser, scanner, TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(vm, compiler, parser, scanner, currentChunk);
    } else {
        expressionStatement(vm, compiler, parser, scanner, currentChunk);
    }

    int loopStart = currentChunk->count;

    int exitJump = -1;
    if (!match(parser, scanner, TOKEN_SEMICOLON)) {
        expression(vm, compiler, parser, scanner, currentChunk);
        consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(parser, currentChunk, OP_JUMP_IF_FALSE);
        emitByte(parser, currentChunk, OP_POP);
    }

    if (!match(parser, scanner, TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(parser, currentChunk, OP_JUMP);
        int incrementStart = currentChunk->count;
        expression(vm, compiler, parser, scanner, currentChunk);
        emitByte(parser, currentChunk, OP_POP);
        consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(parser, currentChunk, loopStart);
        loopStart = incrementStart;
        patchJump(parser, currentChunk, bodyJump);
    }

    statement(vm, compiler, parser, scanner, currentChunk);
    emitLoop(parser, currentChunk, loopStart);

    if (exitJump != -1) {
        patchJump(parser, currentChunk, exitJump);
        emitByte(parser, currentChunk, OP_POP);
    }

    endScope(compiler, parser, currentChunk);
}

static void ifStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(vm, compiler, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(parser, currentChunk, OP_JUMP_IF_FALSE);
    emitByte(parser, currentChunk, OP_POP);
    statement(vm, compiler, parser, scanner, currentChunk);

    int elseJump = emitJump(parser, currentChunk, OP_JUMP);

    patchJump(parser, currentChunk, thenJump);

    emitByte(parser, currentChunk, OP_POP);
    if (match(parser, scanner, TOKEN_ELSE)) statement(vm , compiler, parser, scanner, currentChunk);
    patchJump(parser, currentChunk, elseJump);
}

static void printStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    expression(vm, compiler, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(parser, currentChunk, OP_PRINT);
}

static void whileStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    int loopStart = currentChunk->count;
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(vm, compiler, parser, scanner, currentChunk);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(parser, currentChunk, OP_JUMP_IF_FALSE);
    emitByte(parser, currentChunk, OP_POP);
    statement(vm, compiler, parser, scanner, currentChunk);
    emitLoop(parser, currentChunk, loopStart);

    patchJump(parser, currentChunk, exitJump);
    emitByte(parser, currentChunk, OP_POP);
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

static void declaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    if (match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(vm, compiler, parser, scanner, currentChunk);
    } else {
        statement(vm, compiler, parser, scanner, currentChunk);
    }

    if (parser->panicMode) synchronize(parser, scanner);
}

static void statement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Chunk* currentChunk) {
    if (match(parser, scanner, TOKEN_PRINT)) {
        printStatement(vm, compiler, parser, scanner, currentChunk);
    } else if (match(parser, scanner, TOKEN_FOR)) {
        forStatement(vm, compiler, parser, scanner, currentChunk);
    } else if (match(parser, scanner, TOKEN_IF)) {
        ifStatement(vm, compiler, parser, scanner, currentChunk);
    } else if (match(parser, scanner, TOKEN_WHILE)) {
        whileStatement(vm, compiler, parser, scanner, currentChunk);
    } else if (match(parser, scanner, TOKEN_LEFT_BRACE)) {
        beginScope(compiler);
        block(vm, compiler, parser, scanner, currentChunk);
        endScope(compiler, parser, currentChunk);
    } else {
        expressionStatement(vm, compiler, parser, scanner, currentChunk);
    }
}

bool compile(VM* vm, const char* source, Chunk* chunk) {
    Scanner scanner;
    initScanner(&scanner, source);
    Chunk* currentChunk = chunk;

    Parser parser;
    initParser(&parser);
    advance(&parser, &scanner);

    Compiler compiler;
    initCompiler(&compiler);

    while (!match(&parser, &scanner, TOKEN_EOF)) {
        declaration(vm, &compiler, &parser, &scanner, currentChunk);
    }

    endCompiler(&parser, currentChunk);
    return !parser.hadError;
}