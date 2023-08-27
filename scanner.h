//
// Created by lucas on 8/16/23.
//

#ifndef CLOX_SCANNER_H
#define CLOX_SCANNER_H


#include "common.h"

void initScanner(Scanner* scanner, const char* source);
Token scanToken(Scanner* scanner);

#endif //CLOX_SCANNER_H
