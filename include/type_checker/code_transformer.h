#ifndef CODE_TRANSFORMER_H
#define CODE_TRANSFORMER_H

#include "type_checker/type_checker.h"
AstIfStatement* transformCaseToIfStatement(SymbolTable * symbolTable, FunctionSymbol* functionOfBody, Symbol * matchTarget, AstCaseBlock * caseBlock, EnumSymbol * enumSymbol);
void resolve_closures(SymbolTable* symbolTable);

void tranform_function_call_to_lambda(AstFunctionCall* functionCall, int call_stack_difference);

void lift_function_call(AstFunctionCall* functionCall, int num_static_links_to_walk);

#endif // CODE_TRANSFORMER_H