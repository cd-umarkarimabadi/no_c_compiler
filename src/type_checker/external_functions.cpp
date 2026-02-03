#include "type_checker/extern_function.h"

FunctionSymbol* createExternalFunctionSymbol() {
    FunctionSymbol* symbol = new FunctionSymbol();
    symbol->params = new vector<Symbol*>();
    symbol->isExternal = true;
    return symbol;
}

FunctionSymbol* createExternalPrintStrFunction(Type* string_type) {
    FunctionSymbol* fSymbol = createExternalFunctionSymbol();
    fSymbol->name = PRINT_F_NAME;
    fSymbol->functionDepthLevel = GLOBAL_DEPTH_ZERO;
    fSymbol->returnType = &voidReturn;
    fSymbol->numberOfArgs = 2;
    fSymbol->hasVarArgs = true;
    fSymbol->params->push_back(new Symbol("msg", string_type));
    fSymbol->params->push_back(new Symbol("args", &typeAnyReturn));
    return fSymbol;
}

FunctionSymbol* createExternalSlClosureCreate() {
    auto closureCreate = createExternalFunctionSymbol();
    closureCreate->name = CLOSURE_CREATE_SL_F_NAME;
    closureCreate->returnType = &typeAnyReturn;
    return closureCreate;
}

