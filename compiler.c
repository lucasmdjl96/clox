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
#include "memory.h"

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

typedef void (*ParseFn)(VM*, Compiler*, Parser*, Scanner*, bool);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static Chunk* currentChunk(Compiler* compiler) {
    return &compiler->function->chunk;
}

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

static void errorAtCurrent(Parser* parser, const char* message) {
    errorAt(parser, &parser->current, message);
}

static void advance(Parser* parser, Scanner* scanner) {
    parser->previous = parser->current;

    for (;;) {
        parser->current = scanToken(scanner);
        if (parser->current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser, parser->current.start);
    }
}

static void consume(Parser* parser, Scanner* scanner, TokenType type, const char* message) {
    if (parser->current.type == type) {
        advance(parser, scanner);
        return;
    }

    errorAtCurrent(parser, message);
}

static bool check(Parser* parser, TokenType type) {
    return parser->current.type == type;
}

static bool match(Parser* parser, Scanner* scanner, TokenType type) {
    if (!check(parser, type)) return false;
    advance(parser, scanner);
    return true;
}

static void emitByte(VM* vm, Compiler* compiler, Parser* parser, uint8_t byte) {
    writeChunk(vm, compiler, currentChunk(compiler), byte, parser->previous.line);
}

static void emitReturn(VM* vm, Compiler* compiler, Parser* parser) {
    emitByte(vm, compiler, parser, OP_NIL);
    emitByte(vm, compiler, parser, OP_RETURN);
}

static void emitBytes(VM* vm, Compiler* compiler, Parser* parser, uint8_t byte1, uint8_t byte2) {
    emitByte(vm, compiler, parser, byte1);
    emitByte(vm, compiler, parser, byte2);
}

static void emitLoop(VM* vm, Compiler* compiler, Parser* parser, int loopStart) {
    emitByte(vm, compiler, parser, OP_LOOP);

    int offset = currentChunk(compiler)->count - loopStart + 2;
    if (offset > UINT16_MAX) error(parser, "Loop body too large.");

    emitByte(vm, compiler, parser, (offset >> 8) & 0xff);
    emitByte(vm, compiler, parser, offset & 0xff);
}

static int emitJump(VM* vm, Compiler* compiler, Parser* parser, uint8_t instruction) {
    emitByte(vm, compiler, parser, instruction);
    emitByte(vm, compiler, parser, 0xff);
    emitByte(vm, compiler, parser, 0xff);
    return currentChunk(compiler)->count - 2;
}

static uint8_t makeConstant(VM* vm, Compiler* compiler, Parser* parser, Value value) {
    int constant = addConstant(vm, compiler, currentChunk(compiler), value);
    if (constant > UINT8_MAX) {
        error(parser, "Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(VM* vm, Compiler* compiler, Parser* parser, Value value) {
    emitBytes(vm, compiler, parser, OP_CONSTANT, makeConstant(vm, compiler, parser, value));
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

void initCompiler(VM* vm, Compiler* current, Compiler* compiler, Parser* parser, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction(vm, compiler);
    if (type != TYPE_SCRIPT) {
        compiler->function->name = copyString(vm, compiler, parser->previous.start, parser->previous.length);
    }

    Local* local = &compiler->locals[compiler->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->name.start = "";
    local->name.length = 0;
}

static ObjFunction* endCompiler(VM* vm, Compiler* compiler, Parser* parser) {
    emitReturn(vm ,compiler, parser);
    ObjFunction* function = compiler->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser->hadError) {
        disassembleChunk(currentChunk(compiler), function->name != NULL
            ? function->name->chars : "<script>");
    }
#endif

    return function;
}

static void beginScope(Compiler* compiler) {
    compiler->scopeDepth++;
}

static void endScope(VM* vm, Compiler* compiler, Parser* parser) {
    compiler->scopeDepth--;

    while (compiler->localCount > 0 &&
        compiler->locals[compiler->localCount - 1].depth > compiler->scopeDepth) {
        if (compiler->locals[compiler->localCount - 1].isCaptured) {
            emitByte(vm, compiler, parser, OP_CLOSE_UPVALUE);
        } else {
            emitByte(vm, compiler, parser, OP_POP);
        }
        compiler->localCount--;
    }
}

static void parsePrecedence(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Precedence precedence);
static ParseRule* getRule(TokenType type);
static void expression(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner);
static void statement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner);
static void declaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner);
static uint8_t argumentList(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner);

static void binary(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    TokenType operatorType = parser->previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(vm, compiler, parser, scanner, (Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:
            emitBytes(vm, compiler, parser, OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUAL_EQUAL:
            emitByte(vm, compiler, parser, OP_EQUAL);
            break;
        case TOKEN_GREATER:
            emitByte(vm, compiler, parser, OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytes(vm, compiler, parser, OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            emitByte(vm, compiler, parser, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            emitBytes(vm, compiler, parser, OP_GREATER, OP_NOT);
            break;
        case TOKEN_PLUS:
            emitByte(vm, compiler, parser, OP_ADD);
            break;
        case TOKEN_MINUS:
            emitByte(vm, compiler, parser, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emitByte(vm, compiler, parser, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emitByte(vm, compiler, parser, OP_DIVIDE);
            break;
        default:
            return; // Unreachable.
    }
}

static void call(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    uint8_t argCount = argumentList(vm, compiler, parser, scanner);
    emitBytes(vm, compiler, parser, OP_CALL, argCount);
}

static void literal(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    switch (parser->previous.type) {
        case TOKEN_FALSE:
            emitByte(vm, compiler, parser, OP_FALSE);
            break;
        case TOKEN_TRUE:
            emitByte(vm, compiler, parser, OP_TRUE);
            break;
        case TOKEN_NIL:
            emitByte(vm, compiler, parser, OP_NIL);
            break;
        default:
            return; // Unreachable.
    }
}

static void grouping(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    expression(vm, compiler, parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    double value = strtod(parser->previous.start, NULL);
    emitConstant(vm, compiler, parser, NUMBER_VAL(value));
}

static void or_(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    Chunk* chunk = currentChunk(compiler);
    int elseJump = emitJump(vm, compiler, parser, OP_JUMP_IF_FALSE);
    int endJump = emitJump(vm, compiler, parser, OP_JUMP);

    patchJump(parser, chunk,elseJump);
    emitByte(vm, compiler, parser, OP_POP);

    parsePrecedence(vm, compiler, parser, scanner, PREC_OR);
    patchJump(parser, chunk, endJump);
}

static void and_(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    Chunk* chunk = currentChunk(compiler);
    int endJump = emitJump(vm, compiler, parser, OP_JUMP_IF_FALSE);

    emitByte(vm, compiler, parser, OP_POP);
    parsePrecedence(vm, compiler, parser, scanner, PREC_AND);

    patchJump(parser, chunk, endJump);
}

static void string(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    emitConstant(vm, compiler, parser, OBJ_VAL(copyString(vm, compiler, parser->previous.start + 1,
                                                          parser->previous.length - 2)));
}

static uint8_t identifierConstant(VM* vm, Compiler* compiler, Parser* parser, Chunk* currentChunk, Token* name) {
    return makeConstant(vm, compiler, parser, OBJ_VAL(copyString(vm, compiler, name->start, name->length)));
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

static int addUpvalue(Compiler* compiler, Parser* parser, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error(parser, "Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;

}

static int resolveUpvalue(Compiler* compiler, Parser* parser, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, parser, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, parser, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, parser, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, parser, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Compiler* compiler, Parser* parser, Token name) {
    if (compiler->localCount == UINT8_COUNT) {
        error(parser, "Too many local variables in closure.");
        return;
    }

    Local* local = &compiler->locals[compiler->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
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

static void namedVariable(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(compiler, parser, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(compiler, parser, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(vm, compiler, parser, currentChunk(compiler), &name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        expression(vm, compiler, parser, scanner);
        emitBytes(vm, compiler, parser, setOp, (uint8_t)arg);
    } else {
        emitBytes(vm, compiler, parser, getOp, (uint8_t)arg);
    }
}

static void variable(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    namedVariable(vm, compiler, parser, scanner, parser->previous, canAssign);
}

static void unary(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, bool canAssign) {
    TokenType  operatorType = parser->previous.type;

    // Compile the operand.
    parsePrecedence(vm, compiler, parser, scanner, PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG:
            emitByte(vm, compiler, parser, OP_NOT);
            break;
        case TOKEN_MINUS:
            emitByte(vm, compiler, parser, OP_NEGATE);
            break;
        default:
            return; // Unreachable
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
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

static void parsePrecedence(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, Precedence precedence) {
    advance(parser, scanner);
    ParseFn prefixRule = getRule(parser->previous.type)->prefix;
    if (prefixRule == NULL) {
        error(parser, "Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNEMENT;
    prefixRule(vm, compiler, parser, scanner, canAssign);

    while (precedence <= getRule(parser->current.type)->precedence) {
        advance(parser, scanner);
        ParseFn infixRule = getRule(parser->previous.type)->infix;
        infixRule(vm, compiler, parser, scanner, canAssign);
    }

    if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        error(parser, "Invalid assignment target.");
    }
}


static uint8_t parseVariable(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, const char* errorMessage) {
    consume(parser, scanner, TOKEN_IDENTIFIER, errorMessage);

    declareVariable(compiler, parser);
    if (compiler->scopeDepth > 0) return 0;

    return identifierConstant(vm, compiler, parser, currentChunk(compiler), &parser->previous);
}

static void markInitialized(Compiler* compiler) {
    if (compiler->scopeDepth == 0) return;
    compiler->locals[compiler->localCount - 1].depth = compiler->scopeDepth;
}

static void defineVariable(VM* vm, Compiler* compiler, Parser* parser, uint8_t global) {
    if (compiler->scopeDepth > 0) {
        markInitialized(compiler);
        return;
    }
    emitBytes(vm, compiler, parser, OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    uint8_t argCount = 0;
    if (!check(parser, TOKEN_RIGHT_PAREN)) {
        do {
            expression(vm, compiler, parser, scanner);
            if (argCount == 255) {
                error(parser, "Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(parser, scanner, TOKEN_COMMA));
    }
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

void initParser(Parser* parser) {
    parser->hadError = false;
    parser->panicMode = false;
}

static void expression(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    parsePrecedence(vm, compiler, parser, scanner, PREC_ASSIGNEMENT);
}

static void block(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
        declaration(vm, compiler, parser, scanner);
    }

    consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, FunctionType type) {
    Compiler newCompiler;
    initCompiler(vm, compiler, &newCompiler, parser, type);
    beginScope(&newCompiler);

    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after closure name.");
    if (!check(parser, TOKEN_RIGHT_PAREN)) {
        do {
            newCompiler.function->arity++;
            if (newCompiler.function->arity > 255) {
                errorAtCurrent(parser, "Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable(vm, &newCompiler, parser, scanner, "Expect parameter name.");
            defineVariable(vm, &newCompiler, parser, constant);
        } while (match(parser, scanner, TOKEN_COMMA));
    }
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(parser, scanner, TOKEN_LEFT_BRACE, "Expect '{' before closure body.");
    block(vm, &newCompiler, parser, scanner);

    ObjFunction* function = endCompiler(vm, &newCompiler, parser);
    emitBytes(vm, compiler, parser, OP_CLOSURE, makeConstant(vm, compiler, parser, OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(vm, compiler, parser, newCompiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(vm, compiler, parser, newCompiler.upvalues[i].index);
    }
}

static void funDeclaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    uint8_t global = parseVariable(vm, compiler, parser, scanner, "Expect closure name.");
    markInitialized(compiler);
    function(vm, compiler, parser, scanner, TYPE_FUNCTION);
    defineVariable(vm, compiler, parser, global);
}

static void varDeclaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    uint8_t global = parseVariable(vm, compiler, parser, scanner, "Expect variable name.");

    if (match(parser, scanner, TOKEN_EQUAL)) {
        expression(vm, compiler, parser, scanner);
    } else {
        emitByte(vm, compiler, parser, OP_NIL);
    }
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(vm, compiler, parser, global);
}

static void expressionStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    expression(vm, compiler, parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(vm, compiler, parser, OP_POP);
}

static void forStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    Chunk* chunk = currentChunk(compiler);
    beginScope(compiler);
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(parser, scanner, TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(vm, compiler, parser, scanner);
    } else {
        expressionStatement(vm, compiler, parser, scanner);
    }

    int loopStart = chunk->count;

    int exitJump = -1;
    if (!match(parser, scanner, TOKEN_SEMICOLON)) {
        expression(vm, compiler, parser, scanner);
        consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(vm, compiler, parser, OP_JUMP_IF_FALSE);
        emitByte(vm, compiler, parser, OP_POP);
    }

    if (!match(parser, scanner, TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(vm, compiler, parser, OP_JUMP);
        int incrementStart = chunk->count;
        expression(vm, compiler, parser, scanner);
        emitByte(vm, compiler, parser, OP_POP);
        consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(vm, compiler, parser, loopStart);
        loopStart = incrementStart;
        patchJump(parser, chunk, bodyJump);
    }

    statement(vm, compiler, parser, scanner);
    emitLoop(vm, compiler, parser, loopStart);

    if (exitJump != -1) {
        patchJump(parser, chunk, exitJump);
        emitByte(vm, compiler, parser, OP_POP);
    }

    endScope(vm, compiler, parser);
}

static void ifStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    Chunk* chunk = currentChunk(compiler);
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(vm, compiler, parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(vm, compiler, parser, OP_JUMP_IF_FALSE);
    emitByte(vm, compiler, parser, OP_POP);
    statement(vm, compiler, parser, scanner);

    int elseJump = emitJump(vm, compiler, parser, OP_JUMP);

    patchJump(parser, chunk, thenJump);

    emitByte(vm, compiler, parser, OP_POP);
    if (match(parser, scanner, TOKEN_ELSE)) statement(vm , compiler, parser, scanner);
    patchJump(parser, chunk, elseJump);
}

static void printStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    expression(vm, compiler, parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(vm, compiler, parser, OP_PRINT);
}

static void returnStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    if (compiler->type == TYPE_SCRIPT) {
        error(parser, "Can't return from top-level code.");
    }

    if (match(parser, scanner, TOKEN_SEMICOLON)) {
        emitReturn(vm, compiler, parser);
    } else {
        expression(vm ,compiler, parser, scanner);
        consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(vm, compiler, parser, OP_RETURN);
    }
}

static void whileStatement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    Chunk* chunk = currentChunk(compiler);
    int loopStart = chunk->count;
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(vm, compiler, parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(vm, compiler, parser, OP_JUMP_IF_FALSE);
    emitByte(vm, compiler, parser, OP_POP);
    statement(vm, compiler, parser, scanner);
    emitLoop(vm, compiler, parser, loopStart);

    patchJump(parser, chunk, exitJump);
    emitByte(vm, compiler, parser, OP_POP);
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

static void declaration(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    if (match(parser, scanner, TOKEN_FUN)) {
        funDeclaration(vm, compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(vm, compiler, parser, scanner);
    } else {
        statement(vm, compiler, parser, scanner);
    }

    if (parser->panicMode) synchronize(parser, scanner);
}

static void statement(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner) {
    if (match(parser, scanner, TOKEN_PRINT)) {
        printStatement(vm, compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_FOR)) {
        forStatement(vm, compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_IF)) {
        ifStatement(vm, compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_RETURN)) {
        returnStatement(vm, compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_WHILE)) {
        whileStatement(vm, compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_LEFT_BRACE)) {
        beginScope(compiler);
        block(vm, compiler, parser, scanner);
        endScope(vm, compiler, parser);
    } else {
        expressionStatement(vm, compiler, parser, scanner);
    }
}

ObjFunction* compile(VM* vm, Compiler* compiler, Parser* parser, Scanner* scanner, const char* source) {
    advance(parser, scanner);

    initCompiler(vm, NULL, compiler, parser, TYPE_SCRIPT);

    while (!match(parser, scanner, TOKEN_EOF)) {
        declaration(vm, compiler, parser, scanner);
    }

    ObjFunction* function = endCompiler(vm, compiler, parser);
    return parser->hadError ? NULL : function;
}

void markCompilerRoots(VM* vm, Compiler* compiler) {
    while (compiler != NULL) {
        markObject(vm, (Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}