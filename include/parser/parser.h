#ifndef PARSER_H
#define PARSER_H

#include <tokenizer/tokenizer.h>
#include <vector>
#include <stack>
#include "ast/ast.h"
using namespace std;

typedef struct Operator {
    size_t priotyLevel;
    string operatorStr;
    size_t scope_group;
} Operator_t;

typedef struct Operand {
    string value;
} Operand_t;

struct file_parse_unit {
    string file_name;
    AstBody* program;
    vector<string>* imports;
    bool is_parsing_error;
};

file_parse_unit* parseProgram(TokenStream* tokenStream);

// utils
bool isIdentifierPoly(string polyArgToken);

#endif // PARSER_H