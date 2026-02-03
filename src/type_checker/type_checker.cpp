#include "ast/ast.h"
#include "ast/ast_fun.h"
#include "type_checker/type_checker.h"
#include "parser/parser.h"
#include <assert.h>
#include "type_checker/extern_function.h"
#include "type_checker/code_transformer.h"

#include <signal.h>
#define DEBUG_BREAK() raise(SIGTRAP)

void emit_error_info(AstNode* node, string message_err) {
    if (node != NULL) {
        cout << node->file_name << ":" << node->line_number << ":" << node->column_number << ": " << message_err << endl;
    }
    throw runtime_error(message_err);
}

Type* inferTypeOfExpression(SymbolTable * symbolTable, FunctionSymbol * functionOfBody, AstNode * node, PolyEnv * polyEnv, FrameScope* frame_scope);
Type* unifyTypes(Type * returnType, PolyEnv * polyEnv);

Type* getFutureType(SymbolTable* symbolTable) {
    Type* futureType = getTypeDefinitionByName(symbolTable, FUTURE_STRUCT_NAME);
    if (futureType == NULL) {
        emit_error_info(NULL, "future type cannot be found please import \"src/threading.noc\"");
    }
    return futureType;
}

Type* getStrType(SymbolTable* symbolTable) {
    // Add print function
    Type* str_type = getTypeDefinitionByName(symbolTable, "string");
    if (str_type == NULL) {
        emit_error_info(NULL, "string type cannot be found please import \"src/string.noc\"");
    }
    return str_type;
}

bool isPrimtive = true;
bool isNotPrimtive = false;
BaseType voidReturn("void", isPrimtive);
BaseType intReturn("int", isPrimtive);
BaseType doubleReturn("double", isPrimtive);
BaseType boolReturn("bool", isPrimtive);
BaseType nullReturnType("null", isPrimtive);
BaseType byteReturn("byte", isPrimtive);
BaseType charReturn("char", isPrimtive);
BaseType typeAnyReturn("any", isNotPrimtive);

AstTryLabel tryLabel();

primitive_type_conversion_t prim_matrix_conversion_table[9] = {
    {.left = &intReturn,    .right = &intReturn, .inferred = &intReturn, .order = same},
    {.left = &intReturn,    .right = &doubleReturn, .inferred = &doubleReturn, .order = left_i_right_d},
    {.left = &doubleReturn, .right = &intReturn, .inferred = &doubleReturn, .order = left_d_right_i},
    {.left = &doubleReturn, .right = &doubleReturn, .inferred = &doubleReturn,  .order = same},
    {.left = &charReturn,   .right = &charReturn, .inferred = &charReturn,  .order = same},
    {.left = &intReturn,    .right = &charReturn, .inferred = &intReturn,  .order = left_i_right_c},
    {.left = &charReturn,   .right = &intReturn, .inferred = &intReturn,  .order = left_c_right_i},
    {.left = &charReturn,   .right = &byteReturn, .inferred = &charReturn,  .order = same},
    {.left = &byteReturn,   .right = &charReturn, .inferred = &byteReturn,  .order = same},
};

primitive_type_conversion_t prim_array_matrix_conversion_table[2] = {
    {.left = &charReturn, .right = &byteReturn, .inferred = &charReturn,  .order = same},
    {.left = &byteReturn, .right = &charReturn, .inferred = &byteReturn,  .order = same},
};

bool areReturnTypesEqual(Type * returnType_1, Type * returnType_2) {

    if (returnType_1->type_of_type != returnType_2->type_of_type) {
        return false;
    }

    if (returnType_1->type_of_type == TypeOfType::BaseType) {

        BaseType* returnType1_as_basic = (BaseType*)returnType_1;
        BaseType* returnType2_as_basic = (BaseType*)returnType_2;

        return returnType1_as_basic->typeStr == returnType2_as_basic->typeStr;
    }

    else if (returnType_1->type_of_type == TypeOfType::ArrayType) {
        // Check base dimensions 
        ArrayType* returnType1_as_arr = (ArrayType*)returnType_1;
        ArrayType* returnType2_as_arr = (ArrayType*)returnType_2;

        if (returnType1_as_arr->num_dimensions != returnType2_as_arr->num_dimensions) {
            return false;
        }
        return areReturnTypesEqual(returnType1_as_arr->baseType, returnType2_as_arr->baseType);
    }

    else if (returnType_1->type_of_type == TypeOfType::EnumType) {

        EnumType* returnType1_as_enum = (EnumType*)returnType_1;
        EnumType* returnType2_as_enum = (EnumType*)returnType_2;
        return returnType1_as_enum->enumName == returnType2_as_enum->enumName;
    }

    else if (returnType_1->type_of_type == TypeOfType::PolyType) {

        PolyType* returnType1_as_poly = (PolyType*)returnType_1;
        PolyType* returnType2_as_poly = (PolyType*)returnType_2;
        return returnType1_as_poly->polyStr == returnType2_as_poly->polyStr;
    }

    else if (returnType_1->type_of_type == TypeOfType::LambdaType) {
        LambdaType* returnType1AsFunction = (LambdaType*)returnType_1;
        LambdaType* returnType2AsFunction = (LambdaType*)returnType_2;

        if (!areReturnTypesEqual(returnType1AsFunction->returnType, returnType2AsFunction->returnType)) {
            return false;
        }
        if (returnType1AsFunction->arguments->size() != returnType2AsFunction->arguments->size()) {
            return false;
        }
        for (size_t i = 0; i < returnType1AsFunction->arguments->size(); i++) {
            if (!areReturnTypesEqual(returnType1AsFunction->arguments->at(i), returnType2AsFunction->arguments->at(i))) {
                return false;
            }
        }
        return true;
    }

    else if (returnType_1->type_of_type == TypeOfType::StructPolyType) {

        StructPolyMorphicType* returnType1AsStructPoly = (StructPolyMorphicType*)returnType_1;
        StructPolyMorphicType* returnType2AsStructPoly = (StructPolyMorphicType*)returnType_2;

        if (!areReturnTypesEqual(returnType1AsStructPoly->baseType, returnType2AsStructPoly->baseType)) {
            return false;
        }
        if (returnType1AsStructPoly->polyTypes->size() != returnType2AsStructPoly->polyTypes->size()) {
            return false;
        }

        for (size_t i = 0; i < returnType1AsStructPoly->polyTypes->size(); i++) {
            if (!areReturnTypesEqual(returnType1AsStructPoly->polyTypes->at(i), returnType2AsStructPoly->polyTypes->at(i))) {
                return false;
            }
        }
        return true;
    }
    else {
        assert_never_reaches("areReturnTypesEqual missing type case");
    }

}

void throwIfReturnTypeNotBool(Type * returnType, AstNode * statement) {
    if (returnType == NULL) {
        emit_error_info(statement, "return type of if expression cannot be inferred");
        throw runtime_error("return type of if expression cannot be inferred");
    }

    if (!isReturnTypeBool(returnType)) {
        emit_error_info(statement, "return type of if expression is not boolean");
    }
}

Type* matchTypeConversion(primitive_type_conversion_t* table, size_t table_len, Type* left, Type* right) {
    for (size_t i = 0; i < table_len; i++) {
        primitive_type_conversion_t trans = table[i];
        if (areReturnTypesEqual(trans.left, left) &&
            areReturnTypesEqual(trans.right, right)) {
            return trans.inferred;
        }
    }
    return NULL;
}

Type* matchPrimitiveConversion(Type* left, Type* right) {
    return matchTypeConversion(prim_matrix_conversion_table,
        LEN(prim_matrix_conversion_table),
        left, right);
}

Type* matchArrayTypeConversion(Type* left, Type* right) {
    return matchTypeConversion(prim_array_matrix_conversion_table,
        LEN(prim_array_matrix_conversion_table),
        left, right);
}

LRTypeOrder getLRPrimitiveTypeTransformation(Type* left, Type* right) {
    for (size_t i = 0; i < LEN(prim_matrix_conversion_table); i++) {
        primitive_type_conversion_t trans = prim_matrix_conversion_table[i];
        if (areReturnTypesEqual(trans.left, left) && areReturnTypesEqual(trans.right, right)) {
            return trans.order;
        }
    }
    return bad_match;
}

Type* get_err_type(SymbolTable* symbolTable) {
    Type* type = getTypeDefinitionByName(symbolTable, "err");
    if (type == NULL) {
        emit_error_info(NULL, "err type cannot be found please import \"src/error.noc\"");
    }
    assert(type != NULL);
    return type;
}

bool isFunctionVoid(FunctionSymbol* functionSymbol) {
    return isReturnTypeVoid(functionSymbol->returnType);
}

bool isReturnTypeBool(Type* returnType) {
    if (returnType->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)returnType;
        return basicType->typeStr == boolReturn.typeStr;
    }
    return false;
}

bool isReturnTypeDouble(Type* returnType) {
    if (returnType->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)returnType;
        return basicType->typeStr == doubleReturn.typeStr;
    }
    return false;
}

bool isReturnTypeInt(Type* returnType) {
    if (returnType->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)returnType;
        return basicType->typeStr == intReturn.typeStr;
    }
    return false;
}

bool isReturnTypeNull(Type* returnType) {
    if (returnType->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)returnType;
        return basicType->typeStr == nullReturnType.typeStr;
    }
    return false;
}

bool isReturnTypeVoid(Type* returnType) {
    if (returnType->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)returnType;
        return basicType->typeStr == voidReturn.typeStr;
    }
    return false;
}

bool isTypeLambda(Type* type) {
    return type->type_of_type == TypeOfType::LambdaType;
}

bool isTypeEnum(Type* type) {
    return type->type_of_type == TypeOfType::EnumType;
}

bool isTypePoly(Type* type) {
    return type->type_of_type == TypeOfType::PolyType;
}

bool isTypeBaseType(Type* type) {
    return type->type_of_type == TypeOfType::BaseType;
}

bool isTypeStructPoly(Type* type) {
    return type->type_of_type == TypeOfType::StructPolyType;
}

bool isTypeAny(Type* type) {

    if (type->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)type;
        return basicType->typeStr == typeAnyReturn.typeStr;
    }
    return false; // I guess I can return true since I do not care about the type resolution
}

bool isTypeStruct(Type* type) {
    if (type->type_of_type == TypeOfType::BaseType) {
        BaseType* basicType = (BaseType*)type;
        return !basicType->isPrimitive;
    }
    return false;
}

bool isTypePrimitive(Type* type) {
    if (type->type_of_type == TypeOfType::BaseType) {
        BaseType* basicType = (BaseType*)type;
        return basicType->isPrimitive;
    }
    return false;
}

ArrayType* createArrType(Type * baseType, int numDimensions) {
    assert(numDimensions >= 1);
    ArrayType* type = new ArrayType();
    type->baseType = baseType;
    type->num_dimensions = numDimensions;
    return type;
}

LambdaType* createLambdaType(vector<Type*>*arguments, Type * returnType) {
    auto lambdaType = new LambdaType();
    lambdaType->returnType = returnType;
    lambdaType->arguments = arguments;
    return lambdaType;
}

Symbol* createLambdaSymbol(string symbolName, Type * type) {
    Symbol* symbol = new Symbol(symbolName, type);
    symbol->symbolType = SymbolType::Lambda;
    return symbol;
}

bool isTypeArray(Type* type) {
    return type->type_of_type == TypeOfType::ArrayType;
}

Type* getTypeFromArrayAccess(ArrayType * arrType, size_t num_dimensions_access) {
    if (arrType->num_dimensions == num_dimensions_access) {
        return arrType->baseType;
    }
    else {
        // Create a new type who type is array but with one less access
        return createArrType(arrType->baseType, arrType->num_dimensions - 1);
    }
}

BaseType* createStructReturnType(string typeStr) {
    bool isPrimitive = false;
    BaseType * returnType = new BaseType(typeStr, isPrimitive);
    return returnType;
}

FrameScope createFrameScope(FrameScope* parent) {
    FrameScope scope = FrameScope();
    if (parent != NULL) {
        scope.function_depth_level = parent->function_depth_level + 1;
        scope.parent = parent;
    }
    else {
        scope.function_depth_level = 0;
        scope.parent = NULL;
    }
    scope.symbols = new vector<map<string, Symbol*>*>();
    return scope;
}

int getCurrentFrameIndex(FrameScope* scope) {
    return scope->symbols->size() - 1;
}

Symbol* searchSymbolInFunctionsFrames(FrameScope* scope, string name) {
    auto frameCntxToSearch = scope;
    while (frameCntxToSearch != NULL) {
        int currentFrameIndex = getCurrentFrameIndex(frameCntxToSearch);
        while (currentFrameIndex >= 0) {
            auto scopedSymbolMap = frameCntxToSearch->symbols->at(currentFrameIndex);
            if (scopedSymbolMap->count(name) != 0) {
                return scopedSymbolMap->at(name);
            }
            currentFrameIndex -= 1;
        }
        frameCntxToSearch = frameCntxToSearch->parent;
    }
    return NULL;
}

void addGlobalSymbol(SymbolTable* symbolTable, Symbol* symbol) {
    if (symbolTable->globalSymbolTable->count(symbol->name) != 0) {
        throw runtime_error("symbol already exists");
    }
    symbolTable->globalSymbolTable->insert_or_assign(symbol->name, symbol);
}

Symbol* searchGlobalSymbol(SymbolTable * symbolTable, string name) {
    if (symbolTable->globalSymbolTable->count(name) == 0) {
        return NULL;
    }
    return symbolTable->globalSymbolTable->at(name);
}

Symbol* searchSymbol(SymbolTable * symbolTable, FrameScope * scope_ctxt, string name) {
    auto symbol = searchSymbolInFunctionsFrames(scope_ctxt, name);
    if (symbol == NULL) {
        return searchGlobalSymbol(symbolTable, name);
    }
    return symbol;
}

void addSymbolToCurrentFrame(FrameScope * scope_ctx, Symbol * symbol) {
    int currentFrameIndex = getCurrentFrameIndex(scope_ctx);
    auto scopedSymbolMap = scope_ctx->symbols->at(currentFrameIndex);
    symbol->function_depth_level = scope_ctx->function_depth_level;
    scopedSymbolMap->insert_or_assign(symbol->name, symbol);
}

void exitFrame(FrameScope * scope_ctx) {
    int frameIndex = getCurrentFrameIndex(scope_ctx);
    if (frameIndex < 0) {
        throw runtime_error("Already exited the frame");
    }
    // TODO :: Change to unique pointers 
    auto scopedSymbolMap = scope_ctx->symbols->at(frameIndex);
    scope_ctx->symbols->pop_back();
    delete scopedSymbolMap;
}

void enterNewFrame(FrameScope* scope_ctx) {
    scope_ctx->symbols->push_back(new map<string, Symbol*>());
}

// Start of function symbol 

void addParamToFunctionSymbol(FunctionSymbol * functionSymbol, Symbol * type) {
    functionSymbol->params->push_back(type);
}

NodeTypeMap* createNodeTypeMap() {
    auto nodeTypeMap = new NodeTypeMap();
    nodeTypeMap->typeMap = new map<AstNode*, Type*>();
    return nodeTypeMap;
}


void addFunctionSymbolToFrameScope(
    SymbolTable* symbolTable,
    FunctionSymbol* functionSymbol,
    FrameScope* frame_scope) {
    // Note (the function type is used so assiociate a type with the symbol)
    Symbol* symbol = new Symbol(functionSymbol->name, functionSymbol->lambdaType);
    symbol->symbolType = SymbolType::FunctionDefinition;

    if (frame_scope == NULL) {
        assert(functionSymbol->functionDepthLevel == GLOBAL_DEPTH_ZERO);
        addGlobalSymbol(symbolTable, symbol);
    }
    else {
        addSymbolToCurrentFrame(frame_scope, symbol);
    }
}

// TODO :: this function is confused me with addFunctionSymbolToFrameScope
void addFunctionSymbol(SymbolTable * symbolTable,
    FunctionSymbol * functionSymbol,
    FrameScope* frame_scope) {

    if (symbolTable->fSymbolTable->table->count(functionSymbol->name) != 0) {
        emit_error_info(functionSymbol->functionDef, "function definition already exists");
    }
    symbolTable->nodeTypeMap->insert_or_assign(functionSymbol, createNodeTypeMap());
    symbolTable->fSymbolTable->table->insert_or_assign(functionSymbol->name, functionSymbol);

    addFunctionSymbolToFrameScope(symbolTable, functionSymbol, frame_scope);
}

FunctionSymbol* getFunctionSymbol(SymbolTable * symbolTable, string functionName) {
    if (symbolTable->fSymbolTable->table->count(functionName) == 0) {
        return NULL;
    }
    return symbolTable->fSymbolTable->table->at(functionName);
}

FunctionSymbol* createFunctionSymbol() {
    FunctionSymbol* fSymbol = new FunctionSymbol();
    fSymbol->needsPolyBinding = false;
    fSymbol->params = new vector<Symbol*>();
    fSymbol->nonLocalIdentifiers = new vector<NonLocalIdentifier*>();
    return fSymbol;
}

// Start of enum 
EnumSymbol* createEnumSymbol(string enumName) {
    EnumSymbol* enumSymbol = new EnumSymbol();
    enumSymbol->name = enumName;
    enumSymbol->enums = new map<string, int>();
    return enumSymbol;
}

void addEnumValueToEnum(EnumSymbol * enumSymbol, string name, int intValue) {
    enumSymbol->enums->insert_or_assign(name, intValue);
}

EnumSymbol* getEnumSymbol(SymbolTable * symbolTable, string enumName) {
    if (symbolTable->enumSymbolTable->table->count(enumName) == 0) {
        return NULL;
    }
    return symbolTable->enumSymbolTable->table->at(enumName);
}

bool isInEnum(EnumSymbol * enumSymbol, string toSearch) {
    return enumSymbol->enums->count(toSearch) != 0;
}

int getIntValueFromEnum(EnumSymbol * enumSymbol, string toSearch) {
    if (!isInEnum(enumSymbol, toSearch)) {
        assert_never_reaches("searching for an enum value that does not exist");
    }
    return enumSymbol->enums->at(toSearch);
}

EnumType* creatEnumType(string enumName) {
    EnumType* type = new EnumType();
    type->enumName = enumName;
    type->type_of_type = TypeOfType::EnumType;
    return type;
}

void addEnumSymbol(SymbolTable * table, EnumSymbol * enumSymbol) {
    table->enumSymbolTable->table->insert_or_assign(enumSymbol->name, enumSymbol);
    Symbol* symbol = new Symbol(enumSymbol->name, enumSymbol->type);
    symbol->symbolType = SymbolType::Enum;
    table->globalSymbolTable->insert_or_assign(symbol->name, symbol);
}


Symbol* storeAssignment(
    Type * returnType,
    string symbolStr,
    AstNode * identifierNode,
    FrameScope* frame_scope) {

    if (isReturnTypeVoid(returnType)) {
        emit_error_info(identifierNode, "You canont assign a void type");
    }
    Symbol* rootSymbol = searchSymbolInFunctionsFrames(frame_scope, symbolStr);
    // In this situation the type if being overwritten and we should type check if the assignment 
    // of the right side is the same. We should detect the following type change
    // x = 1;
    // x = "something" 
    if (rootSymbol != NULL) {
        assert(rootSymbol->symbolType != SymbolType::FunctionDefinition);
        // if (isTypeArray(rootSymbol->returnType)) {
        //     emit_error_info(identifierNode, "You cannot reassign an array after it has been declared");
        // }

        if (isReturnTypeNull(returnType) && isTypePrimitive(rootSymbol->returnType)) {
            emit_error_info(identifierNode, "You cannot assign null to primitive");
        }
        else {
            // We know its not a primitive but allow null assignment 
            bool returnTypesEqual = areReturnTypesEqual(rootSymbol->returnType, returnType);
            if (!returnTypesEqual) {
                if (!isReturnTypeNull(returnType)) {
                    emit_error_info(identifierNode, "A symbol has been redeclared with a differnt type");
                }
            }
            return rootSymbol;
        }
    }
    assert(rootSymbol == NULL);
    // TODO :: @hack :: if we are storing a return type of type function then this means that the identifer 
    // Should be thought of a lambda symbol. This hack is pretty ugly and I should think about changing it
    if (isTypeLambda(returnType)) {
        auto lamdbaSymbol = createLambdaSymbol(symbolStr, returnType);
        addSymbolToCurrentFrame(frame_scope, lamdbaSymbol);
        return lamdbaSymbol;
    }
    else {
        Symbol* symbol = new Symbol(symbolStr, returnType);
        symbol->symbolType = SymbolType::Identifier;
        addSymbolToCurrentFrame(frame_scope, symbol);
        return symbol;
    }
}

// Poly expansion functions 
FunctionSymbol* expandPoly(SymbolTable * symbolTable,
    FunctionSymbol * polyFunctionSymbol,
    vector<Type*>*functionCallArgTypes,
    PolyEnv* polyEnv,
    FrameScope* frame_scope);

Type* expandPoly(SymbolTable * symbolTable,
    string structName,
    StructPolyMorphicType * polyStructType,
    StructPolyMorphicType * resolvedType);

bool doesTypeHaveEmbeddedPolyType(Type * type) {
    if (isTypeBaseType(type)) {
        return false;
    }
    if (isTypeEnum(type)) {
        return false;
    }
    if (isTypeArray(type)) {
        ArrayType* arrType = (ArrayType*)type;
        return doesTypeHaveEmbeddedPolyType(arrType->baseType);
    }
    if (isTypePoly(type)) {
        return true;
    }
    if (isTypeStructPoly(type)) {
        StructPolyMorphicType* structPolyType = (StructPolyMorphicType*)type;
        for (auto polyArg : *structPolyType->polyTypes) {
            bool isArgPoly = doesTypeHaveEmbeddedPolyType(polyArg);
            if (isArgPoly) {
                return true;
            }
        }
        return false;
    }

    if (isTypeLambda(type)) {
        LambdaType* lambdaType = (LambdaType*)type;
        if (doesTypeHaveEmbeddedPolyType(lambdaType->returnType)) {
            return true;
        }

        for (auto polyArg : *lambdaType->arguments) {
            bool isArgPoly = doesTypeHaveEmbeddedPolyType(polyArg);
            if (isArgPoly) {
                return true;
            }
        }
        return false;
    }
    assert_never_reaches("doesTypeHavePolyStruct missing type");
}

PolyEnv* createPolyEnv() {
    PolyEnv* polyEnv = new PolyEnv();
    polyEnv->env = new map<string, Type*>();
    return polyEnv;
}

Type* resolveType(PolyEnv * polyEnv, string polyType) {
    return polyEnv->env->at(polyType);
}

void incrementUnresolvedPolyTypes(PolyEnv * polyEnv) {
    polyEnv->num_unresolved_polyTypes++;
}

bool contains(PolyEnv * polyEnv, string polyType) {
    return polyEnv->env->count(polyType) != 0;
}

void addEnv(PolyEnv * env, string polyType, Type * concrete) {
    env->env->insert_or_assign(polyType, concrete);
}

bool isPolyArg(string baseTypeOfArr, AstStructNode * structDef) {
    if (isIdentifierPoly(baseTypeOfArr)) {
        for (auto polyHeader : *structDef->polyHeader) {
            if (baseTypeOfArr == polyHeader) {
                return true;
            }
        }
    }
    return false;
}

PolyType* createPolyType(string polyTypeStr) {
    auto polyType = new PolyType();
    polyType->polyStr = polyTypeStr;
    return polyType;
}

StructPolyMorphicType* createStructPolyTypeFromBase(Type * baseType) {
    auto structPolyType = new StructPolyMorphicType();
    structPolyType->baseType = baseType;
    return structPolyType;
}

void addPolyArgToStructPoly(StructPolyMorphicType * polyStruct, Type * polyType) {
    polyStruct->polyTypes->push_back(polyType);
}

BaseType* getBaseType(StructPolyMorphicType* polyStruct) {
    if (polyStruct->baseType->type_of_type == TypeOfType::BaseType) {
        return (BaseType*)polyStruct->baseType;
    }

    if (polyStruct->baseType->type_of_type == TypeOfType::StructPolyType) {
        return getBaseType((StructPolyMorphicType*)polyStruct->baseType);
    }

    // TODO :: Currrently nested type declerations are not support :: Support will need to be added for complex Data structured like maps 
    assert_never_reaches("unsupported base type resoluton");
}

StructPolyMorphicType* getBaseStructType(Type * type) {
    if (type->type_of_type == TypeOfType::ArrayType) {
        ArrayType* arrType = (ArrayType*)type;
        assert(arrType->baseType->type_of_type == TypeOfType::StructPolyType);
        return (StructPolyMorphicType*)arrType->baseType;
    }

    if (type->type_of_type == TypeOfType::StructPolyType) {
        return (StructPolyMorphicType*)type;
    }

    throw runtime_error("terminal compiler error, cannot resolve to get base struct type from type passed");
}

// Start struct symbol table 

void addStructTypeSymbolTable(SymbolTable * symbolTable, string strctName, Type * type) {
    Symbol* symbol = new Symbol(strctName, type);
    symbol->symbolType = SymbolType::Struct;
    symbolTable->globalSymbolTable->insert_or_assign(strctName, symbol);
}

void addStructAstToSymbolTable(SymbolTable * symbolTable, AstStructNode * structDef) {
    Type* structReturnType = NULL;
    if (structDef->isDefinedAsPoly) {
        auto structPolyType = new StructPolyMorphicType();
        structPolyType->baseType = createStructReturnType(structDef->name);
        for (auto polyHeader : *structDef->polyHeader) {
            auto polyType = new PolyType();
            polyType->polyStr = polyHeader;
            structPolyType->polyTypes->push_back(polyType);
        }
        structReturnType = structPolyType;
    }
    else {
        structReturnType = createStructReturnType(structDef->name);
    }

    addStructTypeSymbolTable(symbolTable, structReturnType->toString(), structReturnType);
    addStructTypeSymbolTable(symbolTable, structDef->name, structReturnType);
}

void addStructLayoutTableEntry(SymbolTable * symbolTable, string structName, StructLayout * layout) {
    layout->identifier_index = symbolTable->structSymbolTable->table->size() + 1;
    if (symbolTable->structSymbolTable->table->count(structName) == 0) {
        symbolTable->structSymbolTable->table->insert_or_assign(structName, layout);
    }
    else {
        assert_never_reaches("struct layout entry is being added again");
    }
}

StructLayout* searchStructLayout(SymbolTable * symbolTable, string symbol) {
    if (symbolTable->structSymbolTable->table->count(symbol) == 0) {
        return NULL;
    }
    return symbolTable->structSymbolTable->table->at(symbol);
}

Field* searchField(StructLayout* layout, string field_name) {

    for (auto field : *layout->fields) {
        if (field->name == field_name) {
            return field;
        }
    }
    return NULL;
}

StructLayout* searchStructLayoutByIdentifier(SymbolTable * symbolTable, int identiiferIndex) {
    for (auto pair : *symbolTable->structSymbolTable->table) {
        if (pair.second->identifier_index == identiiferIndex) {
            return pair.second;
        }
    }
    throw runtime_error("struct layout could not be found");
}

// End struct symbol table 

void addPrimitiveSymbols(SymbolTable* symbolTable, string typeStr, Type* type);

SymbolTable* createSymbolTable() {
    SymbolTable* symbolTable = new SymbolTable();
    symbolTable->globalSymbolTable = new map<string, Symbol*>();
    FunctionSymbolTable* fSymbolTable = new FunctionSymbolTable();
    fSymbolTable->table = new map<string, FunctionSymbol*>();
    StructSymbolTable* structSymbolTable = new StructSymbolTable();
    structSymbolTable->table = new map<string, StructLayout*>();
    symbolTable->functionDefThatEscape = new vector<EscapingFunctionSymbol*>();

    map<string, Symbol*>* globalSymbolTable = new map<string, Symbol*>();

    map<FunctionSymbol*, NodeTypeMap*>* nodeTypeMap = new map<FunctionSymbol*, NodeTypeMap*>();

    EnumSymbolTable* enumTable = new EnumSymbolTable();
    enumTable->table = new map<string, EnumSymbol*>();

    symbolTable->enumSymbolTable = enumTable;
    symbolTable->fSymbolTable = fSymbolTable;
    symbolTable->globalSymbolTable = globalSymbolTable;
    symbolTable->structSymbolTable = structSymbolTable;
    symbolTable->nodeTypeMap = nodeTypeMap;
    // Add primitive types
    addPrimitiveSymbols(symbolTable, intReturn.typeStr, &intReturn);
    addPrimitiveSymbols(symbolTable, boolReturn.typeStr, &boolReturn);
    addPrimitiveSymbols(symbolTable, doubleReturn.typeStr, &doubleReturn);
    addPrimitiveSymbols(symbolTable, byteReturn.typeStr, &byteReturn);
    addPrimitiveSymbols(symbolTable, charReturn.typeStr, &charReturn);
    addPrimitiveSymbols(symbolTable, voidReturn.typeStr, &voidReturn);


    return symbolTable;
}

void addFunctionSymbolAsEscaping(SymbolTable * symbolTable, FunctionSymbol * functionSymbol, AstNode * escapingNode) {
    EscapingFunctionSymbol* escapingFunctionSymbol = new EscapingFunctionSymbol();
    escapingFunctionSymbol->escapingFunctionSymbol = functionSymbol;
    escapingFunctionSymbol->escapingNode = escapingNode;
    symbolTable->functionDefThatEscape->push_back(escapingFunctionSymbol);
}

AstFunctionDefinition* getFunctionDef(AstNode* node) {
    if (node->type == FunctionDefinition) {
        return (AstFunctionDefinition*)node;
    }
    if (node->type == ExternalFunctionDefinition) {
        AstExternalFunction* externalFunction = (AstExternalFunction*)node;
        return externalFunction->functionDef;
    }
    assert_never_reaches("getFunctionDef, correct node type is not passed");
}

int call_stack_difference(FunctionSymbol * currentFunctionBody, Symbol * symbolFound) {
    int call_stack_difference = currentFunctionBody->functionDepthLevel - symbolFound->function_depth_level;
    return call_stack_difference;
}

void markSymbolNonLocal(FunctionSymbol * currentFunctionBody, Symbol * symbolFound, AstNode * nodeForSymbol) {
    int difference = call_stack_difference(currentFunctionBody, symbolFound);

    if (difference != 0) {
        NonLocalIdentifier* nonLocal = new NonLocalIdentifier();
        nonLocal->identifier = nodeForSymbol;
        nonLocal->rootSymbol = symbolFound;
        nonLocal->functionDepthLevel = currentFunctionBody->functionDepthLevel;
        currentFunctionBody->nonLocalIdentifiers->push_back(nonLocal);
    }
}

bool isFlagPoly(PolyFlags polyFlag) {
    // One of the following has been set (polyFlag & PolyStruct) || (polyFlag & PolyArray) || (polyFlag & SinglePoly);
    return polyFlag != NoPoly && polyFlag != ResolvedPolyExpansion;
}

enum struct PolyBindingOptions {
    ErorOnPolyBindingError,
    CreateDefaultBinding
};

Type* createTypeFromTypeDecl(AstTypeDecleration* typeDecl, SymbolTable* symbolTable, PolyEnv* polyEnv, PolyBindingOptions bindingOptions);

Type* createTypeFromTypeDecl(AstTypeDecleration* typeDecl, SymbolTable* symbolTable, PolyEnv* polyEnv, PolyBindingOptions bindingOptions) {
    Type* resolvedType = NULL;
    if (isFlagPoly(typeDecl->polyFlags)) {
        if (typeDecl->polyFlags & SinglePoly) {
            // TODO :: this does not handle the case where you can do x = new T where T is resolved to be an int
            // Maybe I just have bindings to thse and it does not matter if a user can do new int():
            if (contains(polyEnv, typeDecl->typeStr)) {
                resolvedType = resolveType(polyEnv, typeDecl->typeStr);
            }
            else {
                if (bindingOptions == PolyBindingOptions::ErorOnPolyBindingError) {
                    emit_error_info(typeDecl, "no poly binding exists :: either add to function args or struct header, if this is a local function, propogate the poly arg to the first global function");
                }
                resolvedType = createPolyType(typeDecl->typeStr);
                addEnv(polyEnv, typeDecl->typeStr, resolvedType);
            }
            // You cannot be a single poly and a struct 
            assert((typeDecl->polyFlags & PolyStruct) == 0);
        }

        // The code is purposefully not "else if" to allow for type expansation
        // A poly struct can be merged into a poly array
        if (typeDecl->polyFlags & PolyStruct) {
            auto structName = typeDecl->typeStr;
            Type* baseTypeDef = getTypeDefinitionByName(symbolTable, structName);
            if (baseTypeDef == NULL) {
                emit_error_info(typeDecl, "base type for poly morphic expansion cannot be found");
            }
            if (baseTypeDef->type_of_type != TypeOfType::StructPolyType) {
                emit_error_info(typeDecl, "param is declared with poly args but base type has no poly headers");
            }

            StructPolyMorphicType* defaultStructType = (StructPolyMorphicType*)baseTypeDef;
            // Create a new struct poly type 
            auto baseType = defaultStructType->baseType;
            auto structPolyType = createStructPolyTypeFromBase(baseType);
            for (auto polyarg : *typeDecl->polyArgs) {
                if (polyarg->polyFlags & SinglePoly) {
                    // If there is already a binding in the poly env for that T, then just get that, else 
                    // return default poly 
                    Type* polyType = NULL;
                    if (contains(polyEnv, polyarg->typeStr)) {
                        polyType = resolveType(polyEnv, polyarg->typeStr);
                    }
                    else {
                        if (bindingOptions == PolyBindingOptions::ErorOnPolyBindingError) {
                            emit_error_info(typeDecl, "no poly binding exists :: either add to function args or struct header, if this is a local function, propogate the poly arg to the first global function");
                        }
                        polyType = createPolyType(polyarg->typeStr);
                        addEnv(polyEnv, polyarg->typeStr, polyType);
                    }
                    addPolyArgToStructPoly(structPolyType, polyType);
                }
                else {
                    if (polyarg->polyFlags & PolyStruct) {
                        auto polyStruct = createTypeFromTypeDecl(polyarg, symbolTable, polyEnv, bindingOptions);
                        addPolyArgToStructPoly(structPolyType, polyStruct);
                    }
                    else {
                        assert(polyarg->polyFlags & NoPoly);
                        auto conreteType = getTypeDefinitionByName(symbolTable, polyarg->typeStr);
                        if (conreteType == NULL) {
                            emit_error_info(typeDecl, "base type for poly arg cannot be found");
                        }
                        addPolyArgToStructPoly(structPolyType, conreteType);
                    }
                }
            }

            // Regardless of the type we always expand the poly
            resolvedType = expandPoly(symbolTable, structName, defaultStructType, structPolyType);
        }

        if (typeDecl->polyFlags & PolyArray) {
            assert(resolvedType != NULL);
            assert(typeDecl->isArray);
            auto num_dimensions = typeDecl->arr_dimensions->size();
            resolvedType = createArrType(resolvedType, num_dimensions);
        }
    }

    else {

        auto baseType = getTypeDefinitionByName(symbolTable, typeDecl->typeStr);
        if (baseType == NULL) {
            emit_error_info(typeDecl, "baseType could not be found for non poly member");
        }
        if (typeDecl->polyFlags & ResolvedPolyExpansion) {
            if (!isTypeStructPoly(baseType)) {
                // todo :: Fix this warning 
                cout << "TODO :: FIX WARNING FOR UMAR :: Type decleration was expanded as part of poly expansion but does not resolve to a struct poly type" << endl;
            }
        }
        else if (typeDecl->polyFlags & NoPoly) {
            if (isTypeStructPoly(baseType)) {
                emit_error_info(typeDecl, "Type decleration has no poly arguments but base type requires it");
            }
        }
        else {
            assert_never_reaches("createTyepFromDecl :: typeDecl->polyFlags is missing a case");
        }

        if (typeDecl->isArray) {
            assert(typeDecl->arr_dimensions != NULL);
            auto num_dimensions = typeDecl->arr_dimensions->size();
            resolvedType = createArrType(baseType, num_dimensions);
        }
        else {
            resolvedType = baseType;
        }
    }
    assert(resolvedType != NULL);
    return resolvedType;
}

void addFullStructDefinitionToSymbolTable(SymbolTable * symbolTable, AstStructNode * structDef) {
    auto structLayoutTable = new StructLayout();
    PolyEnv* polyEnv = createPolyEnv();
    for (auto polyHeader : *structDef->polyHeader) {
        structLayoutTable->polyArgHeader->push_back(polyHeader);
        if (isIdentifierPoly(polyHeader)) {
            auto polyType = createPolyType(polyHeader);
            addEnv(polyEnv, polyHeader, polyType);
        }
    }

    set<string> fieldNames;
    for (auto member : *structDef->members) {
        Type* typeOfMember = createTypeFromTypeDecl(member->typeDecl, symbolTable, polyEnv, PolyBindingOptions::ErorOnPolyBindingError);
        assert(typeOfMember != NULL);
        if (fieldNames.count(member->decl_name) > 0) {
            emit_error_info(member, "struct member has already been declared");
        }
        fieldNames.insert(member->decl_name);
        auto structField = new Field(member->decl_name, typeOfMember);
        structLayoutTable->fields->push_back(structField);
    }
    delete polyEnv;

    // TODO :: This triple indirection is a bit weird, when looking at a dot access
    // I have to first look at the struct table entry, 
    // Then match the field name to the return type and then from return type have to look at 
    // structure table entry again, it would be nice if it just one level 

    // we need two struct layout entries
    // one for the base type and one the full type decleration that can include 
    // eg ::
    // list -> StructLayout{}
    // list<T> -> StructLayout{}
    Type* structType = getTypeDefinitionByName(symbolTable, structDef->name);
    assert(structType != NULL);
    auto typeToString = structType->toString();
    addStructLayoutTableEntry(symbolTable, structType->toString(), structLayoutTable);
    if (typeToString != structDef->name) {
        addStructLayoutTableEntry(symbolTable, structDef->name, structLayoutTable);
    }
}

void addPrimitiveSymbols(SymbolTable * symbolTable, string typeStr, Type * type) {
    Symbol* symbol = new Symbol(typeStr, type);
    symbol->symbolType = SymbolType::GlobalType;
    symbolTable->globalSymbolTable->insert_or_assign(typeStr, symbol);
}

void loadGlobalSymbol(SymbolTable * symbolTable, AstBody * astBody) {
    for (auto statement : *astBody->statements) {
        if (statement->type == StructDef) {
            auto structDef = (AstStructNode*)statement;
            if (isIdentifierPoly(structDef->name)) {
                emit_error_info(structDef, "struct name cannot use reserved poly identifier names [A-Z]");
            }
            auto typeOfStruct = getTypeDefinitionByName(symbolTable, structDef->name);
            if (typeOfStruct != NULL) {
                emit_error_info(structDef, "struct type is already declared");
            }
            addStructAstToSymbolTable(symbolTable, structDef);
        }
        else if (statement->type == Enum) {
            AstEnum* enumNode = (AstEnum*)statement;
            auto existingEnumSymbol = getEnumSymbol(symbolTable, enumNode->enumIdentifier->name);
            if (existingEnumSymbol != NULL) {
                emit_error_info(enumNode, "enum type is already declared");
            }
            auto enumSymbol = createEnumSymbol(enumNode->enumIdentifier->name);
            int intValueForEnumValue = 0;
            for (auto enumValue : *enumNode->enums) {
                if (isInEnum(enumSymbol, enumValue->name)) {
                    emit_error_info(enumValue, "enum value already declared");
                }
                addEnumValueToEnum(enumSymbol, enumValue->name, intValueForEnumValue);
                intValueForEnumValue++;
            }
            // The base return type is assumed to be int? I think I should make this explicit
            EnumType* enumType = creatEnumType(enumSymbol->name);
            enumSymbol->type = enumType;
            addEnumSymbol(symbolTable, enumSymbol);
        }
    }
}

void addSymbolFromFunctionTypeDef(SymbolTable* symbolTable, AstFunctionTypeDef* typeDef) {
    auto polyEnv = createPolyEnv();
    auto lambdaDecl = typeDef->lambda_decl;
    Type* returnType = createTypeFromTypeDecl(lambdaDecl->returnType, symbolTable, polyEnv, PolyBindingOptions::CreateDefaultBinding);
    assert(returnType != NULL);
    if (doesTypeHaveEmbeddedPolyType(returnType)) {
        emit_error_info(typeDef, "function tye definiton must not have poly types");
    }
    vector<Type*>* params = new vector<Type*>();
    for (auto argumentTypeDecl : *lambdaDecl->params) {
        Type* paramType = createTypeFromTypeDecl(argumentTypeDecl, symbolTable, polyEnv, PolyBindingOptions::CreateDefaultBinding);
        assert(paramType != NULL);
        if (doesTypeHaveEmbeddedPolyType(paramType)) {
            emit_error_info(typeDef, "function tye definiton must not have poly types");
        }
        params->push_back(paramType);
    }
    delete polyEnv;
    auto lambdaType = createLambdaType(params, returnType);
    lambdaType->typeDefName = lambdaDecl->name;

    Symbol* typedefSymbol = new Symbol(lambdaDecl->name, lambdaType);
    typedefSymbol->symbolType = SymbolType::LambdaTypeDefinition;
    addGlobalSymbol(symbolTable, typedefSymbol);
}

FunctionSymbol* createFunctionSymbolFromFunctionDefAst(
    SymbolTable * symbolTable,
    AstFunctionDefinition * functionAst,
    string functionName,
    int functionDepthLevel,
    PolyEnv * polyEnv,
    PolyBindingOptions functionParamBindingOptions) {

    // The function name might come from a polymorphic expansion
    if (getFunctionSymbol(symbolTable, functionName) != NULL) {
        emit_error_info(functionAst, "Symbol already exists for function :: " + functionAst->functionName);
    }

    FunctionSymbol* fSymbol = createFunctionSymbol();

    fSymbol->markedAsThrows = functionAst->throwsError;
    fSymbol->functionDef = functionAst;
    fSymbol->functionBody = functionAst->body;
    fSymbol->functionDepthLevel = functionDepthLevel;
    if (fSymbol->functionDepthLevel > 0) {
        fSymbol->isLocal = true;
    }
    fSymbol->name = functionName;
    fSymbol->needsPolyBinding = false;
    fSymbol->numberOfArgs = functionAst->paramaters->size();
    fSymbol->isExternal = functionAst->isExternal;

    // Resolve funtion param types
    for (auto astParam : *functionAst->paramaters) {
        // If the param is a function decleration we will create a function return type from that 
        if (astParam->declType == DeclerationType::Lambda) {
            Type* returnType = createTypeFromTypeDecl(astParam->lambdaFunction->returnType, symbolTable, polyEnv, functionParamBindingOptions);
            assert(returnType != NULL);

            if (doesTypeHaveEmbeddedPolyType(returnType)) {
                fSymbol->needsPolyBinding = true;
            }
            vector<Type*>* params = new vector<Type*>();
            for (auto argumentTypeDecl : *astParam->lambdaFunction->params) {
                Type* paramType = createTypeFromTypeDecl(argumentTypeDecl, symbolTable, polyEnv, functionParamBindingOptions);
                assert(paramType != NULL);
                if (doesTypeHaveEmbeddedPolyType(paramType)) {
                    fSymbol->needsPolyBinding = true;
                }
                params->push_back(paramType);
            }

            auto lambdaType = createLambdaType(params, returnType);
            Type* paramType = lambdaType;
            Symbol* symbol = createLambdaSymbol(astParam->decl_name, paramType);
            addParamToFunctionSymbol(fSymbol, symbol);
        }
        else {
            assert(astParam->declType == DeclerationType::Normal);
            Type* paramType = createTypeFromTypeDecl(astParam->typeDecl, symbolTable, polyEnv, functionParamBindingOptions);
            if (doesTypeHaveEmbeddedPolyType(paramType)) {
                fSymbol->needsPolyBinding = true;
            }
            assert(paramType != NULL); // For now since things are in order kinda this will work
            // We can move this outside the loop, maybe we do not need this check?
            for (auto functionParam : *fSymbol->params) {
                if (functionParam->name == astParam->decl_name) {
                    emit_error_info(astParam, "there is a function param that is redeclared");
                }
            }
            Symbol* symbol = new Symbol(astParam->decl_name, paramType);
            if (isTypeLambda(paramType)) {
                symbol->symbolType = SymbolType::Lambda;
            }
            else {
                symbol->symbolType = SymbolType::Identifier;
            }

            addParamToFunctionSymbol(fSymbol, symbol);
        }
    }
    // Resolve returnType
    Type* returnTypeDeclaredInFunction;
    if (functionAst->returnIsVoid) {
        returnTypeDeclaredInFunction = &voidReturn;
    }
    else {
        //  PolyBindingOptions::ErorOnPolyBindingError There should have been a scoped poly env from the arguments or from the closure 
        Type* functionReturnType = createTypeFromTypeDecl(functionAst->returnTypeDecl, symbolTable, polyEnv, PolyBindingOptions::ErorOnPolyBindingError);
        if (doesTypeHaveEmbeddedPolyType(functionReturnType)) {
            fSymbol->needsPolyBinding = true;
        }
        returnTypeDeclaredInFunction = functionReturnType;

    }
    fSymbol->returnType = returnTypeDeclaredInFunction;
    fSymbol->polyEnv = polyEnv;
    auto typeArgs = new vector<Type*>();
    for (auto param : *fSymbol->params) {
        typeArgs->push_back(param->returnType);
    }
    fSymbol->lambdaType = createLambdaType(typeArgs, fSymbol->returnType);
    return fSymbol;
}

void loadFunctionSymbols(SymbolTable * symbolTable, AstBody * astBody) {


    for (auto statement : *astBody->statements) {
        if (statement->type == FunctionTypeDef) {
            AstFunctionTypeDef* typeDef = (AstFunctionTypeDef*)statement;
            addSymbolFromFunctionTypeDef(symbolTable, typeDef);
        }

        else if (statement->type == FunctionDefinition || statement->type == ExternalFunctionDefinition) {
            AstFunctionDefinition* functionAst = getFunctionDef(statement);
            auto polyEnv = createPolyEnv();
            auto functionSymbol = createFunctionSymbolFromFunctionDefAst(symbolTable, functionAst, functionAst->functionName, GLOBAL_DEPTH_ZERO, polyEnv, PolyBindingOptions::CreateDefaultBinding);
            addFunctionSymbol(symbolTable, functionSymbol, NULL_FRAME_SCOPE);
        }
    }
}

void typeCheckAssignment(AstNode * assignmentNode, Type * leftSide, Type * rightSide) {
    if (!isReturnTypeNull(rightSide)) {
        if (!areReturnTypesEqual(leftSide, rightSide)) {
            if (isTypeArray(leftSide) && isTypeArray(rightSide)) {

                ArrayType* leftArrayType = (ArrayType*)leftSide;
                ArrayType* rigthArrayType = (ArrayType*)rightSide;

                if (leftArrayType->num_dimensions != rigthArrayType->num_dimensions) {
                    emit_error_info(assignmentNode, "Left side of assignment is not equal to the right. Left :: " + leftSide->toString() + " Right :: " + rightSide->toString());
                }

                auto conversionMatch = matchArrayTypeConversion(leftArrayType->baseType, rigthArrayType->baseType);
                if (conversionMatch == NULL) {
                    emit_error_info(assignmentNode, "Left side of assignment is not equal to the right. Left :: " + leftSide->toString() + " Right :: " + rightSide->toString());
                }
            }
            else {
                auto conversionMatch = matchPrimitiveConversion(leftSide, rightSide);
                if (conversionMatch == NULL) {
                    emit_error_info(assignmentNode, "Left side of assignment is not equal to the right. Left :: " + leftSide->toString() + " Right :: " + rightSide->toString());
                }
            }
        }
    }
}

Type* typeCheckReturnStatement(AstReturnStatement * returnStatement, FunctionSymbol * functionOfBody, SymbolTable * symbolTable, PolyEnv * polyEnv, FrameScope * frame_scope) {
    // EXIT EARLY Becuase this condition is good 
    if (returnStatement->expression == NULL && isFunctionVoid(functionOfBody)) {
        return &voidReturn;
    }

    if (returnStatement->expression != NULL && isFunctionVoid(functionOfBody)) {
        emit_error_info(returnStatement, "return must return a type of expression that matches the function");
    }

    if (returnStatement->expression != NULL && isFunctionVoid(functionOfBody)) {
        emit_error_info(returnStatement, "function is void cannot return anything");
    }

    if (returnStatement->expression == NULL && !isFunctionVoid(functionOfBody)) {
        emit_error_info(returnStatement, "function is not void and must return something");
    }

    if (returnStatement->expression != NULL) {
        Type* returnType = inferTypeOfExpression(symbolTable, functionOfBody, returnStatement->expression, polyEnv, frame_scope);
        if (returnType == NULL) {
            emit_error_info(returnStatement, "Return type of return expression cannot be determined");
        }

        if (isReturnTypeNull(returnType)) {
            return &nullReturnType;
        }

        if (!areReturnTypesEqual(returnType, functionOfBody->returnType)) {
            emit_error_info(returnStatement, "Return expression does not match return type of function");
        }
        return returnType;
    }
}

Type* inferTypeOfAssignment(SymbolTable * symbolTable, FunctionSymbol * functionOfBody, AstNode * astNode, PolyEnv * polyEnv, FrameScope * frame_scope) {
    if (astNode == NULL) {
        return NULL;
    }
    assert(astNode->type == Assignment);
    AstAssignementStatement* assignment = (AstAssignementStatement*)astNode;
    Type* returnType = inferTypeOfExpression(symbolTable, functionOfBody, assignment->rightSide, polyEnv, frame_scope);
    if (returnType == NULL) {
        emit_error_info(assignment->rightSide, "cannot infer type of assignment expression");
    }
    if (assignment->rightSide->type == FunctionCall) {
        AstFunctionCall* functionCall = (AstFunctionCall*)assignment->rightSide;
        auto symbol = searchSymbol(symbolTable, frame_scope, functionCall->name);

        // TODO :: Lambda functions do not support multiple return types so this still works fine 
        if (symbol->symbolType == SymbolType::FunctionDefinition) {
            // The function symbol can be the base unexpanded poly so cannot use the return of the function call 
            auto functionSymbol = getFunctionSymbol(symbolTable, functionCall->name);

            if (functionSymbol->markedAsThrows && !isReturnTypeVoid(returnType)) {
                emit_error_info(assignment, "function call returns multiple values");
            }
            // Re assign erro type to be the error 
            if (functionSymbol->markedAsThrows && isReturnTypeVoid(returnType)) {
                returnType = get_err_type(symbolTable);
            }
        }
    }

    bool isTypePresent = isOptionalTypePresent(assignment);
    if (isReturnTypeNull(returnType) && !isTypePresent) {
        emit_error_info(assignment->identifier, "You have to declare a type first before assinging null");
    }
    else if (isTypePresent) {
        auto polyEnv = createPolyEnv();
        Type* declaredType = createTypeFromTypeDecl(assignment->typeDecl, symbolTable, polyEnv, PolyBindingOptions::ErorOnPolyBindingError);
        typeCheckAssignment(assignment, declaredType, returnType);
        delete polyEnv;
        returnType = declaredType; // reassing the return type because return type could be a null
    }

    storeTypeOfNode(symbolTable, functionOfBody, assignment, returnType);
    auto symbolAdded = storeAssignment(returnType, assignment->identifier->name, assignment, frame_scope);

    markSymbolNonLocal(functionOfBody, symbolAdded, assignment);
}

Type* inferTypeOfBooleanExpresssion(SymbolTable * symbolTable,
    FunctionSymbol * functionOfBody,
    AstNode * astNode,
    PolyEnv * polyEnv,
    FrameScope * frame_scope) {

    if (astNode == NULL) {
        return NULL;
    }
    if (astNode->type == Literal) {
        AstLiteralValue* literalValue = (AstLiteralValue*)astNode;
        if (literalValue->valueType != BooleanLiteral) {
            emit_error_info(literalValue, "literal value should be true/false in if expression");
        }
        auto transformedBooleanExpression = createBooleanExpressionNode(literalValue, literalValue, literalValue->literalValue->boolValue ? "==" : "!=");
        literalValue->sub = transformedBooleanExpression;
        return &boolReturn;
    }
    else if (astNode->type == FunctionCall) {
        Type* returnType = inferTypeOfExpression(symbolTable, functionOfBody, astNode, polyEnv, frame_scope);
        if (!isReturnTypeBool(returnType)) {
            emit_error_info(astNode, "functionCall in boolean Expression should resolve");
        }
        return returnType;
    }

    else if (astNode->type != BooleanExpression) {
        emit_error_info(astNode, "conditional expression of if statement should resolve to some kind of boolean expression");
    }

    assert(astNode->type == BooleanExpression);
    AstBooleanExpression* booleanExpression = (AstBooleanExpression*)astNode;
    Type* left_type = inferTypeOfExpression(symbolTable, functionOfBody, booleanExpression->left, polyEnv, frame_scope);
    if (left_type == NULL) {
        emit_error_info(booleanExpression->left, "The left side type of the boolean expression could not be resolved");
    }
    Type* right_type = inferTypeOfExpression(symbolTable, functionOfBody, booleanExpression->right, polyEnv, frame_scope);
    if (right_type == NULL) {
        emit_error_info(booleanExpression->right, "The left side type of the boolean expression could not be resolved");
    }
    storeTypeOfNode(symbolTable, functionOfBody, booleanExpression->left, left_type);
    storeTypeOfNode(symbolTable, functionOfBody, booleanExpression->right, right_type);

    if (booleanExpression->op == "&&" || booleanExpression->op == "||") {
        if (isReturnTypeBool(left_type) && areReturnTypesEqual(left_type, right_type)) {
            return left_type;
        }
        return NULL;
    }

    if ((booleanExpression->op == ">") || (booleanExpression->op == ">=") || (booleanExpression->op == "<") || (booleanExpression->op == "<=")) {

        LRTypeOrder order = getLRPrimitiveTypeTransformation(left_type, right_type);
        if (order == bad_match) {
            emit_error_info(booleanExpression->left, "left side of expr and right side cannot be compared");
            return NULL;
        }
        return &boolReturn;
    }

    // TODO :: For now I will let anything be compared so long as they are the same type
    if (booleanExpression->op == "==" || booleanExpression->op == "!=") {

        // if one of the operands is null then it is a null comparison and we will allow this 
        if (isReturnTypeNull(left_type) || isReturnTypeNull(right_type)) {
            return &boolReturn;
        }

        if (areReturnTypesEqual(left_type, right_type)) {
            return &boolReturn;
        }
        return NULL;
    }

    return NULL;
}

void typeCheckStatement(SymbolTable * symbolTable, AstNode * statement, FunctionSymbol * functionOfBody, PolyEnv * polyEnv, FrameScope * frame_scope);
void typeCheckBody(SymbolTable * symbolTable, AstBody * body, FunctionSymbol * functionOfBody, PolyEnv * polyEnv, FrameScope * frame_scope) {
    enterNewFrame(frame_scope);
    for (auto statement : *body->statements) {
        typeCheckStatement(symbolTable, statement, functionOfBody, polyEnv, frame_scope);
    }
    exitFrame(frame_scope);
}

void typeCheckStatement(SymbolTable * symbolTable,
    AstNode * statement,
    FunctionSymbol * functionOfBody,
    PolyEnv * polyEnv,
    FrameScope * frame_scope) {

    if (statement->type == Match) {
        AstMatchNode* matchNode = (AstMatchNode*)statement;
        Symbol* matchTargetSymbol = searchSymbol(symbolTable, frame_scope, matchNode->matchTarget->name);
        if (matchTargetSymbol == NULL) {
            emit_error_info(matchNode, "match target symbol cannot be found");
        }

        if (!isTypeEnum(matchTargetSymbol->returnType)) {
            emit_error_info(matchNode, "match target has to be of type enum");
        }

        auto enumType = (EnumType*)matchTargetSymbol->returnType;
        auto enumSymbol = getEnumSymbol(symbolTable, enumType->enumName);
        if (enumSymbol == NULL) {
            emit_error_info(matchNode, "enum symbol cannot be found");
        }
        // TODO :: Add set check to make sure all cases are matched 
        for (auto caseBlock : *matchNode->cases) {
            for (auto target : *caseBlock->targets) {
                if (!isInEnum(enumSymbol, target)) {
                    emit_error_info(matchNode, "case target cannot be found on enum");
                }
            }
            typeCheckBody(symbolTable, caseBlock->body, functionOfBody, polyEnv, frame_scope);
        }

        // transfer case statatements into a if else statmenets
        int begin = 0;
        auto firstCaseBlock = matchNode->cases->at(begin);
        // do the rest for the case target matches 
        auto startIfStatement = transformCaseToIfStatement(symbolTable, functionOfBody, matchTargetSymbol, firstCaseBlock, enumSymbol);

        for (size_t i = begin + 1; i < matchNode->cases->size(); i++) {
            auto caseBlock = matchNode->cases->at(i);
            auto ifStatement = transformCaseToIfStatement(symbolTable, functionOfBody, matchTargetSymbol, caseBlock, enumSymbol);
            auto elseIf = createIfElseFromIf(ifStatement);
            addElseIfStatementToIf(startIfStatement, elseIf);
        }
        // Add NULL body because we do not need anything?? Might break something but everything that does not match ends up here and does nothing
        addElseStatementToIf(startIfStatement, NULL);
        addSubtitutionToNode(matchNode, startIfStatement);
        return;
    }

    if (statement->type == Return) {
        AstReturnStatement* returnStatement = (AstReturnStatement*)statement;
        typeCheckReturnStatement(returnStatement, functionOfBody, symbolTable, polyEnv, frame_scope);

        if (returnStatement->expression != NULL && returnStatement->expression->type == FunctionCall) {
            AstFunctionCall* functionCall = (AstFunctionCall*)returnStatement->expression;
            auto functionSymbol = getFunctionSymbol(symbolTable, functionCall->name);
            assert(functionSymbol != NULL); // typeCheckReturnStatement would have validate this already
            if (functionSymbol->markedAsThrows) {
                emit_error_info(statement, "cannot using function call in return expression if function call throws");
            }
        }
        return;
    }

    if (statement->type == Defer) {
        AstDefer* deferStatement = (AstDefer*)statement;
        typeCheckStatement(symbolTable, deferStatement->defferedFunctionCall, functionOfBody, polyEnv, frame_scope);
        FunctionSymbol* deferredFunctionSymbol = getFunctionSymbol(symbolTable, deferStatement->defferedFunctionCall->name);
        assert(deferredFunctionSymbol != NULL);

        if (deferredFunctionSymbol->markedAsThrows) {
            emit_error_info(deferStatement->defferedFunctionCall, "deferred function call cannot throw");
        }

        if (!isReturnTypeVoid(deferredFunctionSymbol->returnType)) {
            emit_error_info(deferStatement->defferedFunctionCall, "deferred function call should be void");
        }
        return;
    }

    if (statement->type == IfStatement) {
        AstIfStatement* ifStatement = (AstIfStatement*)statement;
        Type* returnType = inferTypeOfBooleanExpresssion(symbolTable, functionOfBody, ifStatement->booleanExpression, polyEnv, frame_scope);

        throwIfReturnTypeNotBool(returnType, ifStatement);

        if (ifStatement->ifbody != NULL) {
            typeCheckBody(symbolTable, ifStatement->ifbody, functionOfBody, polyEnv, frame_scope);
        }

        for (auto ifElse : *ifStatement->ifElses) {
            Type* returnType = inferTypeOfBooleanExpresssion(symbolTable, functionOfBody, ifElse->booleanExpression, polyEnv, frame_scope);
            throwIfReturnTypeNotBool(returnType, ifStatement);

            if (ifElse->ifbody != NULL) {
                typeCheckBody(symbolTable, ifElse->ifbody, functionOfBody, polyEnv, frame_scope);
            }
        }

        if (ifStatement->elseBody != NULL) {
            typeCheckBody(symbolTable, ifStatement->elseBody, functionOfBody, polyEnv, frame_scope);
        }
    }

    else if (statement->type == ArraySubscriptAssignment) {
        auto arrSubscriptAssignment = (AstArraySubscriptAssignment*)statement;

        Type* returnTypeOfArrAccess = inferTypeOfExpression(symbolTable, functionOfBody, arrSubscriptAssignment->arrAccess, polyEnv, frame_scope);
        if (isTypeArray(returnTypeOfArrAccess)) {
            emit_error_info(arrSubscriptAssignment, "cannot reassign array, you have to access all dimensions to store a value");
        }

        Type* returnTypeOfAssignment = inferTypeOfExpression(symbolTable, functionOfBody, arrSubscriptAssignment->expression, polyEnv, frame_scope);

        if (!areReturnTypesEqual(returnTypeOfArrAccess, returnTypeOfAssignment)) {
            emit_error_info(arrSubscriptAssignment, "Cannot assign the right side of the expression to this array access");
        }
    }

    else if (statement->type == Assignment) {
        inferTypeOfAssignment(symbolTable, functionOfBody, statement, polyEnv, frame_scope);
    }
    else if (statement->type == ForLoop) {
        AstForLoop* forLoop = (AstForLoop*)statement;
        enterNewFrame(frame_scope);

        if (forLoop->assignment != NULL) {
            inferTypeOfAssignment(symbolTable, functionOfBody, forLoop->assignment, polyEnv, frame_scope);
        }

        Type* booleanType = inferTypeOfBooleanExpresssion(symbolTable, functionOfBody, forLoop->conditional, polyEnv, frame_scope);

        if (booleanType == NULL) {
            emit_error_info(forLoop->conditional, "Boolean expression type cannot be resolved. There must always be some implicit boolean expression");
        }

        if (forLoop->postCondition) {
            inferTypeOfAssignment(symbolTable, functionOfBody, forLoop->postCondition, polyEnv, frame_scope);
        }

        for (auto statement : *forLoop->body->statements) {
            typeCheckStatement(symbolTable, statement, functionOfBody, polyEnv, frame_scope);
        }

        exitFrame(frame_scope);
    }

    else {
        inferTypeOfExpression(symbolTable, functionOfBody, statement, polyEnv, frame_scope);
    }
}

void typeCheckFunctionBody(SymbolTable * symbolTable, FunctionSymbol * fSymbol, PolyEnv * polyEnv, FrameScope * frame_scope) {
    enterNewFrame(frame_scope);
    for (auto param : *fSymbol->params) {
        addSymbolToCurrentFrame(frame_scope, param);
    }

    if (fSymbol->functionBody == NULL && !isFunctionVoid(fSymbol)) {
        emit_error_info(fSymbol->functionDef, "Function is not void and should  return something");
    }

    if (fSymbol->functionBody != NULL) {
        // TODO :: Just a quick hack so last statement actually return somethings 
        bool returnStatementFound = false;
        for (auto statement : *fSymbol->functionBody->statements) {
            if (statement->type == Return) {
                returnStatementFound = true;
            }
            typeCheckStatement(symbolTable, statement, fSymbol, polyEnv, frame_scope);
        }

        if (!isFunctionVoid(fSymbol) && !returnStatementFound) {
            emit_error_info(fSymbol->functionDef, "Function is not void and should  return something");
        }
    }
    exitFrame(frame_scope);
}

void typeCheckAllFunctions(SymbolTable * symbolTable, AstBody * astBody) {
    for (auto statement : *astBody->statements) {
        if (statement->type == FunctionDefinition) {
            AstFunctionDefinition* functionDef = (AstFunctionDefinition*)statement;
            FunctionSymbol* fSymbol = getFunctionSymbol(symbolTable, functionDef->functionName);
            auto frame_scope = createFrameScope(NULL_FRAME_SCOPE);
            typeCheckFunctionBody(symbolTable, fSymbol, fSymbol->polyEnv, &frame_scope);
            run_try_handler_cfg_checks(symbolTable, functionDef);
        }
    }
}

void typeCheck(SymbolTable * symbolTable, AstBody * progam) {

    loadGlobalSymbol(symbolTable, progam);

    // Note :: getStrType(symbolTable) is a hack 
    // The first import that is processed in any file will be the string type 
    // This is so we get native string type into the symbol table 

    // The same is true for future and error

    // I am not a big fan of this as it creates a weird dependancy structures where the type checker depends 
    // on the langauge creating the native types

    // Althgouth I do not have time to do this , there should some stub types for string and error and futures 
    // to solve this kind of circular dependancy
    Type* str_type = getStrType(symbolTable);
    if (getFunctionSymbol(symbolTable, PRINT_F_NAME) == NULL) {
        FunctionSymbol* fPrintStrSymbol = createExternalPrintStrFunction(str_type);
        addFunctionSymbol(symbolTable, fPrintStrSymbol, NULL_FRAME_SCOPE);
    }

    if (getFunctionSymbol(symbolTable, CLOSURE_CREATE_SL_F_NAME) == NULL) {
        FunctionSymbol* fClosureCreate = createExternalSlClosureCreate();
        addFunctionSymbol(symbolTable, fClosureCreate, NULL_FRAME_SCOPE);
    }

    loadFunctionSymbols(symbolTable, progam);
    // Now expand struct definition since partial symbols were allocated 
    for (auto statement : *progam->statements) {
        if (statement->type == StructDef) {
            auto structDef = (AstStructNode*)statement;
            // We can type check this without expansion first at call 
            addFullStructDefinitionToSymbolTable(symbolTable, structDef);
        }
    }

    typeCheckAllFunctions(symbolTable, progam);
}

void storeTypeOfNode(SymbolTable * symbolTable, FunctionSymbol * fSymbol, AstNode * node, Type * type) {
    symbolTable->nodeTypeMap->at(fSymbol)->typeMap->insert_or_assign(node, type);
}

Type* loadTypeOfNode(SymbolTable * symbolTable, FunctionSymbol * functionSymbol, AstNode * node) {
    return symbolTable->nodeTypeMap->at(functionSymbol)->typeMap->at(node);
}

Type* inferTypeOfArithmeticExpresssion(SymbolTable * symbolTable, FunctionSymbol * functioOfBody, AstNode * astNode, PolyEnv * polyEnv, FrameScope * frame_scope) {
    assert(astNode->type == ArithmeticExpression);
    AstArithMeticExpression* arithmeticExpression = (AstArithMeticExpression*)astNode;
    Type* left_type = inferTypeOfExpression(symbolTable, functioOfBody, arithmeticExpression->left, polyEnv, frame_scope);
    Type* right_type = inferTypeOfExpression(symbolTable, functioOfBody, arithmeticExpression->right, polyEnv, frame_scope);

    if (arithmeticExpression->op == "%") {
        if (!isReturnTypeInt(left_type) || !isReturnTypeInt(right_type)) {
            emit_error_info(astNode, "Mod operator requires both operands to be of type int");
        }
    }

    // It does not matter if these types are not correct, just store them anyway
    storeTypeOfNode(symbolTable, functioOfBody, arithmeticExpression->left, left_type);
    storeTypeOfNode(symbolTable, functioOfBody, arithmeticExpression->right, right_type);
    Type* resolvingTypeOfArithmetic = matchPrimitiveConversion(left_type, right_type);
    if (resolvingTypeOfArithmetic == NULL) {
        emit_error_info(arithmeticExpression->left, "operands are not arithmetically resolvable");
    }

    return resolvingTypeOfArithmetic;
}

void typeCheckArrBounds(AstTypeDecleration * typeDecl, SymbolTable * symbolTable,
    FunctionSymbol * functionOfBody,
    Type * baseTypeOfArr,
    PolyEnv * polyEnv,
    FrameScope * frame_scope) {

    assert(typeDecl->isArray == true);
    assert(isTypeArray(baseTypeOfArr));
    // Loop throught all dimensions and make sure that they are resolvable or at least can be type inferred 
    for (auto arrDimension : *typeDecl->arr_dimensions) {
        if (!arrDimension->isSizeKnown) {
            if (arrDimension->dimensionExpression == NULL) {
                emit_error_info(typeDecl, "Dimensions cannot be empty and have to be computable");
            }
            Type* returnTypeOfDimensionExp = inferTypeOfExpression(symbolTable, functionOfBody, arrDimension->dimensionExpression, polyEnv, frame_scope);
            if (returnTypeOfDimensionExp == NULL || !isReturnTypeInt(returnTypeOfDimensionExp)) {
                emit_error_info(arrDimension->dimensionExpression, "Array dimension size has to be resolvable to some int value");
            }
        }
        else {
            if (arrDimension->size == 0) {
                emit_error_info(arrDimension->dimensionExpression, "Array dimension size cannot be zero");
            }
            // TODO :: Unifiy this with some other means so there is only one place that store constant in pool is called from??
            assert(arrDimension->dimensionExpression->type == Literal);
        }
    }
}

Type* inferTypeOfExpressionInner(SymbolTable * symbolTable, FunctionSymbol * functionOfBody, AstNode * node, PolyEnv * polyEnv, FrameScope * frame_scope);

Type* inferTypeOfExpression(SymbolTable * symbolTable,
    FunctionSymbol * functionOfBody,
    AstNode * node,
    PolyEnv * polyEnv,
    FrameScope * frame_scope) {

    Type* returnType = inferTypeOfExpressionInner(symbolTable, functionOfBody, node, polyEnv, frame_scope);
    storeTypeOfNode(symbolTable, functionOfBody, node, returnType);
    return returnType;
}

Type* inferTypeOfArrayAccess(SymbolTable * symbolTable,
    FunctionSymbol * functionOfBody,
    PolyEnv * polyEnv,
    Type * base_type,
    AstArrayAccess * arrayAccess,
    FrameScope * frame_scope) {

    if (!isTypeArray(base_type)) {
        emit_error_info(arrayAccess, "type is not array but it being indexed like one");
    }

    auto arrayType = (ArrayType*)base_type;
    assert(arrayType->type_of_type == TypeOfType::ArrayType);
    size_t num_array_access_dimensions = arrayAccess->subScripts->size();;
    assert(num_array_access_dimensions != 0);

    if (num_array_access_dimensions > arrayType->num_dimensions) {
        emit_error_info(arrayAccess, "Trying to access dimensions that is outof bounds");
    }
    // Make sure all array accesses can be resolved to some kind of decimal number,
    // For now it just hardcode to be int 
    for (auto subscript : *arrayAccess->subScripts) {
        Type* accessType = inferTypeOfExpression(symbolTable, functionOfBody, subscript->expression, polyEnv, frame_scope);
        if (accessType == NULL) {
            emit_error_info(arrayAccess, "Type cannot be resolved for array access");
        }
        if (!isReturnTypeInt(accessType)) {
            emit_error_info(arrayAccess, "array access should be of type int");
        }
    }
    return getTypeFromArrayAccess(arrayType, num_array_access_dimensions);

}

string create_unique_private_f_symbol_name() {
    static int num_private = 0;
    num_private++;
    return "<private_function_" + to_string(num_private) + ">";
}



auto match_expected_type = true;
auto do_not_match_expected_type = false;
auto link_current_frame_scope = true;
auto do_not_link_current_frame_scope = false;

FunctionSymbol* fullTypeCheckOnAnonFunction(SymbolTable* symbolTable,
    PolyEnv* polyEnv,
    FrameScope* frame_scope,
    FunctionSymbol* parent_function,
    AstAnonFunction* anonFunction,
    Type* expectedType,
    bool matchExpectedType,
    bool link_current_frame_scope) {

    if (matchExpectedType) {
        assert(expectedType != NULL);
        if (!isTypeLambda(expectedType)) {
            emit_error_info(anonFunction, "argument is not expecting a lambda function");
        }

        LambdaType* lambda = (LambdaType*)expectedType;
        if (lambda->arguments->size() != anonFunction->functionDef->paramaters->size()) {
            emit_error_info(anonFunction, "number of paramaters in anonymous function is not correct");
        }

        bool return_void_check_equal = isReturnTypeVoid(lambda->returnType) == anonFunction->functionDef->returnIsVoid;
        if (!return_void_check_equal) {
            emit_error_info(anonFunction, "anon function return type does not match expected return type");
        }
    }

    auto anon_function_depth_level = parent_function->functionDepthLevel + 1;

    string unique_name = create_unique_private_f_symbol_name();
    anonFunction->private_name = unique_name;
    auto anonFunctionSymbol = createFunctionSymbolFromFunctionDefAst(symbolTable, anonFunction->functionDef, unique_name, anon_function_depth_level, polyEnv, PolyBindingOptions::ErorOnPolyBindingError);
    if (doesTypeHaveEmbeddedPolyType(anonFunctionSymbol->lambdaType)) {
        emit_error_info(anonFunction, "anonymous functions cannot have poly types");
    }
    addFunctionSymbol(symbolTable, anonFunctionSymbol, frame_scope);

    auto new_frame_sope = createFrameScope(link_current_frame_scope ? frame_scope : NULL_FRAME_SCOPE);
    if (!link_current_frame_scope) {
        new_frame_sope.function_depth_level = anon_function_depth_level;
    }

    typeCheckFunctionBody(symbolTable, anonFunctionSymbol, polyEnv, &new_frame_sope);
    run_try_handler_cfg_checks(symbolTable, anonFunction->functionDef);
    addFunctionSymbolAsEscaping(symbolTable, anonFunctionSymbol, anonFunction);
    return anonFunctionSymbol;
}

FunctionSymbol* typeCheckAnonNoRoutineFunc(
    SymbolTable* symbolTable,
    PolyEnv* polyEnv,
    FrameScope* current_frame_scope,
    FunctionSymbol* parent_function,
    AstNoRoutine* noRoutine) {

    assert(noRoutine->body->type == AnonymousFunction);
    AstAnonFunction* anonFunction = (AstAnonFunction*)noRoutine->body;

    FunctionSymbol * anonFunctionSymbol = fullTypeCheckOnAnonFunction(symbolTable, polyEnv, current_frame_scope, parent_function, anonFunction, NULL, do_not_link_current_frame_scope, do_not_match_expected_type);
    assert(anonFunction->functionDef->paramaters->size() == anonFunctionSymbol->params->size());
    int index = 0;
    for (auto *param : *anonFunctionSymbol->params) {
        Symbol* symbol = searchSymbol(symbolTable, current_frame_scope, param->name);
        if (symbol == NULL) {
            emit_error_info(NULL, "param in no routine capture group cannot be found");
        }

        if (!areReturnTypesEqual(symbol->returnType, param->returnType)) {
            emit_error_info(NULL, "argument type in no routine capture does not match the type found in scope");
        }
        auto num_staic_links = anonFunctionSymbol->functionDepthLevel - symbol->function_depth_level;
        if (num_staic_links != 1) {
            emit_error_info(NULL, "argument in capture group must refer to a variable in the current scope");
        }
        index++;
    }

    return anonFunctionSymbol;
}

Type* inferTypeOfExpressionInner(SymbolTable * symbolTable,
    FunctionSymbol * functionOfBody,
    AstNode * node,
    PolyEnv * polyEnv,
    FrameScope * frame_scope) {

    if (node->type == Null) {
        return &nullReturnType;
    }

    if (node->type == NegativeExpression) {
        auto negativeExpression = (AstNegativeExpression*)node;
        Type* type = inferTypeOfExpression(symbolTable, functionOfBody, negativeExpression->expression, polyEnv, frame_scope);
        if (!isReturnTypeDouble(type) && !isReturnTypeInt(type)) {
            emit_error_info(negativeExpression, "negative exprssion can only be applied to a numerical literal or an arithemtic expression");
        }
        return type;
    }

    if (node->type == PolyClonedFunctionDefinition) {
        // @Hack::PolyClonedFunctionDefinition
        // Note (umar) PolyClonedFunctionDefinition do not need to be type checked again when part of poly expansion
        // However becuase a new frame is created the function symbol needs to be added to the frame so it can be found
        AstPolyClonedFunctionDefinitition* polyClonedFunctionDef = (AstPolyClonedFunctionDefinitition*)node;
        auto functionSymbol = getFunctionSymbol(symbolTable, polyClonedFunctionDef->functionDef->functionName);
        assert(functionSymbol != NULL);
        addFunctionSymbolToFrameScope(symbolTable, functionSymbol, frame_scope);
        return NULL;
    }

    if (node->type == FunctionDefinition) {
        // Local function definition
        AstFunctionDefinition* functionAst = (AstFunctionDefinition*)node;
        auto functionSymbol = createFunctionSymbolFromFunctionDefAst(symbolTable, functionAst, functionAst->functionName, functionOfBody->functionDepthLevel + 1, polyEnv, PolyBindingOptions::ErorOnPolyBindingError);
        functionSymbol->parent = functionOfBody;
        addFunctionSymbol(symbolTable, functionSymbol, frame_scope);

        auto new_frame_sope = createFrameScope(frame_scope);
        typeCheckFunctionBody(symbolTable, functionSymbol, polyEnv, &new_frame_sope);
        run_try_handler_cfg_checks(symbolTable, functionAst);
        return NULL;
    }

    if (node->type == NoRoutine) {
        AstNoRoutine* noRoutine = (AstNoRoutine*)node;
        FunctionSymbol* noRoutineCalledFunctionSymbol = NULL;
        if (noRoutine->coRoutineBodyType == CoRoutineBodyType::Anonymous) {
            assert(noRoutine->body->type == AnonymousFunction);
            noRoutineCalledFunctionSymbol = typeCheckAnonNoRoutineFunc(symbolTable, polyEnv, frame_scope, functionOfBody, noRoutine);
        }
        else {
            emit_error_info(noRoutine, "no routine calls are only supported on anonynmous functions for now");
        }

        Type* futureType = getFutureType(symbolTable);
        assert(futureType->type_of_type == TypeOfType::StructPolyType);
        PolyEnv* polyEnv = createPolyEnv();
        Type* polyResolvedFuture = NULL;
        addEnv(polyEnv, "T", noRoutineCalledFunctionSymbol->returnType);
        // resolves to future<T> where T is the function return type              
        polyResolvedFuture = unifyTypes(futureType, polyEnv);
        delete polyEnv;
        return polyResolvedFuture;
    }

    if (node->type == Yield) {
        return &voidReturn;
    }

    if (node->type == Try) {
        if (!functionOfBody->markedAsThrows) {
            emit_error_info(node, "Function symbol must now throw if you are trying a function call");
        }
        AstTryStatement* astTryStatement = (AstTryStatement*)node;
        Type* inferredType = inferTypeOfExpression(symbolTable, functionOfBody, astTryStatement->functionCall, polyEnv, frame_scope);

        FunctionSymbol* callingFunction = getFunctionSymbol(symbolTable, astTryStatement->functionCall->name);
        if (!callingFunction->markedAsThrows) {
            emit_error_info(node, "Cannot try a function that does not throw");
        }
        return inferredType;
    }

    if (node->type == Throw) {
        AstThrow* astThrow = (AstThrow*)node;
        if (!functionOfBody->markedAsThrows) {
            emit_error_info(node, "add throws clasuse to function definition if you are throwing error");
        }
        Type* returnTypeOfExp = inferTypeOfExpression(symbolTable, functionOfBody, astThrow->functionCall, polyEnv, frame_scope);
        if (returnTypeOfExp == NULL) {
            emit_error_info(astThrow->functionCall, "type of ast throw function cannot be inferred");
        }
        // Fix this ? Why am i comparing the type str
        Type* errType = get_err_type(symbolTable);
        if (!areReturnTypesEqual(returnTypeOfExp, errType)) {
            emit_error_info(astThrow->functionCall, "type of throw is not of type err");
        }
        return returnTypeOfExp;
    }

    if (node->type == Tuple) {
        AstTupleError* tuple = (AstTupleError*)node;
        // Will do a type check on function call;

        auto functionCall = tuple->rightSide;
        FunctionSymbol* fSymbol = getFunctionSymbol(symbolTable, functionCall->name);
        if (fSymbol == NULL) {
            emit_error_info(functionCall, "function symbol cannot be found");
        }
        if (!fSymbol->markedAsThrows) {
            emit_error_info(functionCall, "function call has to to throw in order for to use multiple return types");
        }
        auto inferredReturnTypeOfFunction = inferTypeOfExpression(symbolTable, functionOfBody, tuple->rightSide, polyEnv, frame_scope);

        if (isReturnTypeVoid(inferredReturnTypeOfFunction)) {
            emit_error_info(functionCall, "cannot use a tuple when called function is void");
        }

        Type* errType = get_err_type(symbolTable);
        Type* pairs[2] = { inferredReturnTypeOfFunction,  errType };
        assert(tuple->tuple->size() == 2);
        for (size_t i = 0; i <= tuple->tuple->size() - 1; i++) {
            storeAssignment(pairs[i], tuple->tuple->at(i), tuple, frame_scope);
        }
        return inferredReturnTypeOfFunction;
    }

    if (node->type == FunctionCall) {
        auto functionCall = ((AstFunctionCall*)(node));

        // type_check_break_point(); // if this function call is made in a noC file then we will trigger a break point 
        // this will let us step over the next expression being allocated
        if (functionCall->name == "type_check_break_point") {
            DEBUG_BREAK();
            return &voidReturn;
        }

        Symbol* symbol = searchSymbol(symbolTable, frame_scope, functionCall->name);
        if (symbol == NULL) {
            emit_error_info(functionCall, "function call cannot be resolved");
        }

        if (symbol->symbolType == SymbolType::Identifier) {
            emit_error_info(functionCall, "function call is happening on a symbol that is an indentifier");
        }

        if (symbol->symbolType == SymbolType::LambdaTypeDefinition) {
            emit_error_info(functionCall, "function call is happening on a symbol that is a type definition");
        }

        if (symbol->symbolType == SymbolType::Lambda) {

            auto lambda_symbol = symbol;

            LambdaType* lambdaType = (LambdaType*)symbol->returnType;
            assert(lambdaType->type_of_type == TypeOfType::LambdaType);

            if (functionCall->arguments->size() != lambdaType->arguments->size()) {
                emit_error_info(functionCall, "not enough arguments passed");
            }

            int argCallIndex = 0;
            for (auto expectedType : *lambdaType->arguments) {
                AstNode* argPassed = functionCall->arguments->at(argCallIndex);

                if (isTypeLambda(expectedType)) {
                    if (argPassed->type == AnonymousFunction) {
                        // Note (umar) Lambda symbols cannot have polymorphic types 
                        // This means that we can, yes can, do full type check of anon function
                        auto anonFunction = (AstAnonFunction*)argPassed;
                        fullTypeCheckOnAnonFunction(symbolTable, polyEnv, frame_scope, functionOfBody, anonFunction, expectedType, match_expected_type, link_current_frame_scope);
                        continue;
                    }
                }

                Type * calledArgType = inferTypeOfExpression(symbolTable, functionOfBody, argPassed, polyEnv, frame_scope);
                if (!areReturnTypesEqual(expectedType, calledArgType)) {
                    emit_error_info(argPassed, "function call arg is not correct");
                }
                argCallIndex++;
            }

            tranform_function_call_to_lambda(functionCall, call_stack_difference(functionOfBody, lambda_symbol));
            return lambdaType->returnType;
        }
        assert(symbol->symbolType == SymbolType::FunctionDefinition);
        // This is for global functions and also handle the case of nested local functions 
        auto calledFunctionSymbol = getFunctionSymbol(symbolTable, functionCall->name);
        if (calledFunctionSymbol == NULL) {
            emit_error_info(functionCall, "Function symbol " + functionCall->name + "   does not exist");
        }

        if (calledFunctionSymbol == functionOfBody) {
            functionOfBody->is_recursive = true;
        }

        auto numberOfRegularArgs = calledFunctionSymbol->numberOfArgs;
        if (calledFunctionSymbol->hasVarArgs) {
            numberOfRegularArgs = calledFunctionSymbol->numberOfArgs - 1;
        }
        else {
            if (functionCall->arguments->size() != numberOfRegularArgs) {
                emit_error_info(functionCall, "number of args passed is incorrect");
            }
        }

        vector<Type*> functionArgTypes;
        if (numberOfRegularArgs <= functionCall->arguments->size()) {
            size_t functionSymbolArgIndex = 0;
            for (size_t argCallIndex = 0; argCallIndex < functionCall->arguments->size(); argCallIndex++) {
                // If it has var args reset the index
                if (functionSymbolArgIndex >= functionCall->arguments->size()) {
                    if (!calledFunctionSymbol->hasVarArgs) {
                        emit_error_info(functionCall, "Too many arguements passed");
                    }
                    functionSymbolArgIndex = calledFunctionSymbol->params->size() - 1;
                }

                Type* expectedTypeForArg = calledFunctionSymbol->params->at(functionSymbolArgIndex)->returnType;
                AstNode* argPassed = functionCall->arguments->at(argCallIndex);
                Type* argReturnType = NULL;

                if (argPassed->type == AnonymousFunction) {
                    bool run_match_on_expcted_type = calledFunctionSymbol->needsPolyBinding == false;
                    auto anonFunctionSymbol = fullTypeCheckOnAnonFunction(symbolTable, polyEnv, frame_scope, functionOfBody, (AstAnonFunction*)argPassed, expectedTypeForArg, run_match_on_expcted_type, link_current_frame_scope);
                    argReturnType = anonFunctionSymbol->lambdaType;
                }
                else {
                    argReturnType = inferTypeOfExpression(symbolTable, functionOfBody, argPassed, polyEnv, frame_scope);
                }

                if (argReturnType == NULL) {
                    emit_error_info(argPassed, "function arg is not correct, type cannot be inffered");
                    return NULL;
                }

                if (isTypeAny(expectedTypeForArg)) {
                    continue;
                }
                if (calledFunctionSymbol->needsPolyBinding) {
                    functionArgTypes.push_back(argReturnType);
                }
                else {
                    auto areBaseTypeEqual = areReturnTypesEqual(argReturnType, expectedTypeForArg);
                    if (!areBaseTypeEqual) {
                        emit_error_info(argPassed, "Type does match in one of the arguments");
                    }
                }

                functionSymbolArgIndex++;
            }
        }
        else {
            emit_error_info(functionCall, "Function call " + functionCall->name + " does not match required params");
        }

        // Note :: Even if the calling context is not poly binded we still need poly expansion to resolve generic types 
        if (calledFunctionSymbol->needsPolyBinding) {
            auto polyBindedFunctionSymbol = expandPoly(symbolTable, calledFunctionSymbol, &functionArgTypes, polyEnv, frame_scope);
            // If it polybinding we can substtitue the call 
            if (!polyBindedFunctionSymbol->needsPolyBinding) {

                // TODO :: This is just a sanity check since the merge and unfication would have handled this but I am paranoid 
                int argCallIndex = 0;
                for (auto param : *polyBindedFunctionSymbol->params) {
                    if (!areReturnTypesEqual(param->returnType, functionArgTypes.at(argCallIndex))) {
                        emit_error_info(functionCall->arguments->at(argCallIndex), "function call arg is not correct");
                    }
                    argCallIndex++;
                }

                // If a function does not need poly binding, by definition any calling functions aslo must not require polyBinding
                if (!functionOfBody->needsPolyBinding) {
                    auto replacedFunctionCall = createFunctionCall(polyBindedFunctionSymbol->name);;
                    for (auto arg : *functionCall->arguments) {
                        addArgumentToFunctionCall(replacedFunctionCall, arg);
                    }
                    functionCall->sub = replacedFunctionCall;
                }
            }
            calledFunctionSymbol = polyBindedFunctionSymbol;
        }
        return calledFunctionSymbol->returnType;
    }

    else if (node->type == Identifier) {
        auto identiferNode = (AstIdentifier*)node;
        Symbol* symbol = searchSymbol(symbolTable, frame_scope, identiferNode->name);
        if (symbol != NULL) {
            if (symbol->symbolType == SymbolType::Struct) {
                emit_error_info(identiferNode, "struct symbol is being used as identifier");
            }

            // function definitions are not heap allocated identifiers so require no scoping rules 
            if (symbol->symbolType == SymbolType::FunctionDefinition) {
                auto functionSymbol = getFunctionSymbol(symbolTable, symbol->name);
                addFunctionSymbolAsEscaping(symbolTable, functionSymbol, identiferNode);
                return symbol->returnType;
            }

            markSymbolNonLocal(functionOfBody, symbol, identiferNode);
            return symbol->returnType;
        }
        emit_error_info(identiferNode, "identifer cannot be found");
    }

    else if (node->type == Literal) {
        auto literalValue = (AstLiteralValue*)node;
        if (literalValue->valueType == NumberLiteral) {
            return &intReturn;
        }

        if (literalValue->valueType == BooleanLiteral) {
            return &boolReturn;
        }

        if (literalValue->valueType == StringLiteral) {
            // TODO :: cahce this look up
            // cache future lookup
            // cache error lookup

            // The string has to be copied to avoid infinite recursion with the substitition
            auto copied_literal_st = createLitearlValueNodeForString(literalValue->literalValue->strValue);
            auto native_str_f_call = createFunctionCall(TO_NATIVE_STR);
            addArgumentToFunctionCall(native_str_f_call, copied_literal_st);

            literalValue->sub = native_str_f_call;
            return getStrType(symbolTable);
        }

        if (literalValue->valueType == DoubleLiteral) {
            return &doubleReturn;
        }

        if (literalValue->valueType == CharLiteral) {
            auto char_literal_len = literalValue->literalValue->strValue.length();
            if (char_literal_len > 1) {
                emit_error_info(literalValue, "char literal cannot have len more than 1");
            }
            return &charReturn;
        }

        if (literalValue->valueType == HexLiteral) {
            for (auto character : literalValue->literalValue->strValue) {
                int int_value = hex_to_int(character);
                if (int_value == -1) {
                    emit_error_info(literalValue, "hex string is invalid");
                }
            }
            // We will interpreter the hex value as an int if it smaller than an int
            // TODO :: Get constant from the type layout.h 
            if (literalValue->literalValue->strValue.length() <= 8) {
                return &intReturn;
            }
            // TODO :: Not sure what to do here 
            assert(true == false);
        }
    }
    else if (node->type == ArrayAccess) {
        auto arrayAccess = (AstArrayAccess*)node;
        Symbol* symbol = searchSymbol(symbolTable, frame_scope, arrayAccess->identifier->name);
        if (symbol == NULL) {
            emit_error_info(arrayAccess, "array access symbol cannot be found");
        }

        markSymbolNonLocal(functionOfBody, symbol, arrayAccess);;
        return inferTypeOfArrayAccess(symbolTable, functionOfBody, polyEnv, symbol->returnType, arrayAccess, frame_scope);
    }

    else if (node->type == BooleanExpression) {
        return inferTypeOfBooleanExpresssion(symbolTable, functionOfBody, node, polyEnv, frame_scope);
    }

    else if (node->type == ArithmeticExpression) {
        return inferTypeOfArithmeticExpresssion(symbolTable, functionOfBody, node, polyEnv, frame_scope);
    }

    // The code gen just runs a no op instruction when it sees a break statements that is does not need to run
    else if (node->type == BreakStatement) {
        return &voidReturn;
    }

    else if (node->type == ContinueStatement) {
        return &voidReturn;
    }

    else if (node->type == ObjectCreation) {
        AstObjectCreation* objectCreation = (AstObjectCreation*)node;
        auto typeDecl = objectCreation->typeDecl;
        auto resolvedTypeOfDecl = createTypeFromTypeDecl(typeDecl, symbolTable, polyEnv, PolyBindingOptions::ErorOnPolyBindingError);
        if (isTypeArray(resolvedTypeOfDecl)) {
            typeCheckArrBounds(typeDecl, symbolTable, functionOfBody, resolvedTypeOfDecl, polyEnv, frame_scope);
        }
        else {
            if (isTypePrimitive(resolvedTypeOfDecl)) {
                emit_error_info(typeDecl, "Cannot instantiate object that is type primtive");
            }
        }
        return resolvedTypeOfDecl;
    }

    else if (node->type == DotAssignment) {
        auto dotAssignment = (AstDotAssignment*)node;
        auto returnTypeOfDotAccess = inferTypeOfExpression(symbolTable, functionOfBody, dotAssignment->dotAccess, polyEnv, frame_scope);

        if (returnTypeOfDotAccess == NULL) {
            emit_error_info(dotAssignment, "return type of dot access in assignement could not be resolved");
        }
        if (isTypeEnum(returnTypeOfDotAccess)) {
            emit_error_info(dotAssignment, "you cannot reassign enum values");
        }
        auto returnTypeOfExpression = inferTypeOfExpression(symbolTable, functionOfBody, dotAssignment->expression, polyEnv, frame_scope);
        if (returnTypeOfExpression == NULL) {
            emit_error_info(dotAssignment->expression, "return type of right side of assignement could not be resolved");
        }
        typeCheckAssignment(dotAssignment, returnTypeOfDotAccess, returnTypeOfExpression);
        return returnTypeOfDotAccess;
    }

    else if (node->type == DotAccess) {
        auto dotAccessNode = (AstDotAccess*)node;
        assert(dotAccessNode->accesses->size() > 1);
        auto rootAccess = dotAccessNode->accesses->at(0);
        int begin = 0;

        auto enumSymbol = getEnumSymbol(symbolTable, rootAccess->identifier_access->name);
        if (enumSymbol != NULL) {
            if (dotAccessNode->accesses->size() > 2) {
                emit_error_info(rootAccess, "too many dot accesses on enum symbol");
            }
            auto enumValue = dotAccessNode->accesses->at(begin + 1)->identifier_access;
            if (!isInEnum(enumSymbol, enumValue->name)) {
                emit_error_info(dotAccessNode, "enum value does not exists");
            }

            int value = getIntValueFromEnum(enumSymbol, enumValue->name);
            auto literalAstValueForEnum = createLitearlValueNodeForInt(value);
            dotAccessNode->sub = literalAstValueForEnum;
            return enumSymbol->type;
        }

        // Think about a way to combine this 
        Symbol* rootSymbol = searchSymbol(symbolTable, frame_scope, rootAccess->identifier_access->name);
        if (rootSymbol == NULL) {
            emit_error_info(dotAccessNode, "dot acccess is referencing a root symbol that does not exist");
        }

        // TODO :: Need to add condition where lambda can be in the middle 
        if (rootSymbol->symbolType == SymbolType::Lambda) {
            emit_error_info(dotAccessNode, "dot access cannot happen on a lambda function");
        }

        markSymbolNonLocal(functionOfBody, rootSymbol, dotAccessNode);
        storeTypeOfNode(symbolTable, functionOfBody, rootAccess, rootSymbol->returnType);
        // We iterate from the position that is one ahead of the structLayout we are using to determin the match
        auto last_inferredType = rootSymbol->returnType;
        for (size_t i = 1; i < dotAccessNode->accesses->size(); i++) {
            bool found_field_match = false;
            auto currentAccess = dotAccessNode->accesses->at(i);

            if (isTypeArray(last_inferredType)) {
                // If its not a struct check if the user is asking .len of an array type
                if (currentAccess->identifier_access->name == "len") {
                    last_inferredType = &intReturn;
                    found_field_match = true;
                    continue;
                }
                emit_error_info(currentAccess, "dot access on array expects .len");
            }

            if (isTypePrimitive(last_inferredType)) {
                emit_error_info(currentAccess, "Dot access canont happen on primitive type");
            }

            auto structLayoutToSearchFor = last_inferredType->toString();
            auto structLayout = searchStructLayout(symbolTable, structLayoutToSearchFor);
            if (structLayout == NULL) {
                emit_error_info(currentAccess, "root symbol is expected to be of type struct but cannot be resolved");
            }
            for (auto field : *structLayout->fields) {
                if (field->name == currentAccess->identifier_access->name) {
                    last_inferredType = field->returnType;
                    found_field_match = true;
                    break;
                }
            }
            if (!found_field_match) {
                emit_error_info(dotAccessNode, "Dot access is referencing a field that does not exist");
            }
            storeTypeOfNode(symbolTable, functionOfBody, currentAccess, last_inferredType);

            if (currentAccess->arr_access != NULL) {
                if (!isTypeArray(last_inferredType)) {
                    emit_error_info(dotAccessNode, "Dot access element is subscripting something that is not an array");
                }
                auto arr_access = currentAccess->arr_access;
                // Update last inferred type to be what the array subscript evalates to be 
                last_inferredType = inferTypeOfArrayAccess(symbolTable, functionOfBody, polyEnv, last_inferredType, arr_access, frame_scope);
            }
        }

        if (last_inferredType == NULL) {
            emit_error_info(dotAccessNode, "Could not infer type of dot accesss");
        }

        return last_inferredType;
    }

    throw runtime_error("inferTypeOfExpression has no handler for the node type");
    return NULL;
}

Type* getTypeDefinitionByName(SymbolTable * symbolTable, string name) {
    if (symbolTable->globalSymbolTable->count(name) == 0) {
        return NULL;
    }
    auto symbol = symbolTable->globalSymbolTable->at(name);
    return symbol->returnType;
}

// Merging and unification 

// Merging 
// eg merge(list[T], list[int]) => list[int] 
// eg merge(list[T], list[Z]) => list[Z]
// A merge of types just checks wethere the two type graphs can be subtituted, one generic type graph into one concrete
// type graph
// As the graph is being merged the PolyEnv is built up
// eg T -> int 

// Unification 
// For a given polymorphic type, say T
// Unification retrieves what was built in the poly env during merging 
Type* mergeTypes(Type * genericType, Type * concreteType, PolyEnv * newPolyEnv) {

    if (genericType->type_of_type == TypeOfType::BaseType || genericType->type_of_type == TypeOfType::EnumType) {
        if (!areReturnTypesEqual(genericType, concreteType)) {
            throw runtime_error("arguments cannot be merged, expeting " + genericType->toString() + "  but got " + concreteType->toString());
            return NULL;
        }

        return concreteType;
    }

    if (genericType->type_of_type == TypeOfType::PolyType) {
        PolyType* poly = (PolyType*)genericType;

        // TODO :: This is a broke check because we also need to check wether any of the args contain a poly base type 
        if (concreteType->type_of_type == TypeOfType::PolyType) {
            incrementUnresolvedPolyTypes(newPolyEnv);
        }

        // Note :: Merging on poly type will only support these base types, not sure if other are even needed. I will let something break during testing and add more cases here 
        if (concreteType->type_of_type == TypeOfType::PolyType
            || concreteType->type_of_type == TypeOfType::BaseType
            || concreteType->type_of_type == TypeOfType::StructPolyType
            || concreteType->type_of_type == TypeOfType::LambdaType) {
            if (!contains(newPolyEnv, poly->polyStr)) {
                addEnv(newPolyEnv, poly->polyStr, concreteType);
                return concreteType;
            }
            else {
                Type* existingBinding = resolveType(newPolyEnv, poly->polyStr);
                if (!areReturnTypesEqual(existingBinding, concreteType)) {
                    throw runtime_error("Poly Type argument is being reassiged, was " + existingBinding->toString() + " but now has been changed " + concreteType->toString());
                    return NULL;
                }
                return concreteType;
            }
        }
        else {
            throw runtime_error("Poly arguments are of a completely different sturcture and cannot be unified");
            return NULL; // canot be unified
        }
    }

    if (genericType->type_of_type == TypeOfType::ArrayType) {
        if (concreteType->type_of_type != TypeOfType::ArrayType) {
            throw runtime_error("poly arg expected array but got something else");
            return NULL;
        }
        ArrayType* polyTypeAsArr = (ArrayType*)genericType;
        ArrayType* concretTypeAsArr = (ArrayType*)concreteType;

        if (polyTypeAsArr->num_dimensions != concretTypeAsArr->num_dimensions) {
            throw runtime_error("poly arg dimensions do not mathc");
            return NULL;
        }
        auto merged = mergeTypes(polyTypeAsArr->baseType, concretTypeAsArr->baseType, newPolyEnv);
        if (merged == NULL) {
            return NULL;
        }
        return concreteType;
    }

    if (genericType->type_of_type == TypeOfType::StructPolyType) {

        if (concreteType->type_of_type != TypeOfType::StructPolyType) {
            throw runtime_error("poly arg expected struct");
            return NULL;
        }

        StructPolyMorphicType* returnType1AsStructPoly = (StructPolyMorphicType*)genericType;
        StructPolyMorphicType* returnType2AsStructPoly = (StructPolyMorphicType*)concreteType;

        if (returnType1AsStructPoly->polyTypes->size() != returnType2AsStructPoly->polyTypes->size()) {
            throw runtime_error("struct poly headers do not match");
            return NULL;
        }

        mergeTypes(returnType1AsStructPoly->baseType, returnType2AsStructPoly->baseType, newPolyEnv);

        for (size_t i = 0; i < returnType1AsStructPoly->polyTypes->size(); i++) {
            auto mergedType = mergeTypes(returnType1AsStructPoly->polyTypes->at(i), returnType2AsStructPoly->polyTypes->at(i), newPolyEnv);
            if (mergedType == NULL) {
                return NULL;
            }
        }
        return concreteType;
    }

    if (genericType->type_of_type == TypeOfType::LambdaType) {

        if (concreteType->type_of_type != TypeOfType::LambdaType) {
            throw runtime_error("argument expected to be function type");
            return NULL;
        }

        LambdaType* returnType1AsFunctionType = (LambdaType*)genericType;
        LambdaType* returnType2AsFunctionType = (LambdaType*)concreteType;

        if (returnType1AsFunctionType->arguments->size() != returnType2AsFunctionType->arguments->size()) {
            throw runtime_error("function arg types do not match");
            return NULL;
        }

        mergeTypes(returnType1AsFunctionType->returnType, returnType2AsFunctionType->returnType, newPolyEnv);

        for (size_t i = 0; i < returnType1AsFunctionType->arguments->size(); i++) {
            auto mergedType = mergeTypes(returnType1AsFunctionType->arguments->at(i), returnType2AsFunctionType->arguments->at(i), newPolyEnv);
            if (mergedType == NULL) {
                return NULL;
            }
        }
        return concreteType;
    }
    throw runtime_error("Nothing can be merged, a case maybe missing");
}

ArrayType* unify(ArrayType * arrType, PolyEnv * polyEnv);
StructPolyMorphicType* unify(StructPolyMorphicType * polyType, PolyEnv * polyEnv);
Type* unify(PolyType * polyType, PolyEnv * polyEnv);
LambdaType* unify(LambdaType * polyFunctionType, PolyEnv * polyEnv);
Type* unifyTypes(Type * returnType, PolyEnv * polyEnv) {
    // Does not need unification;
    if (isTypeBaseType(returnType)) {
        return returnType;
    }

    if (isTypeArray(returnType)) {
        ArrayType* arrType = (ArrayType*)returnType;
        return unify(arrType, polyEnv);
    }
    else if (isTypePoly(returnType)) {
        PolyType* polyType = (PolyType*)returnType;
        return unify(polyType, polyEnv);
    }
    else if (isTypeStructPoly(returnType)) {
        StructPolyMorphicType* structPolyType = (StructPolyMorphicType*)returnType;
        return unify(structPolyType, polyEnv);
    }

    else if (isTypeLambda(returnType)) {
        auto lambdaType = (LambdaType*)returnType;
        return unify(lambdaType, polyEnv);
    }
    throw runtime_error("terminal compiler error unify types is at the end of the block");
}

Type* unify(PolyType * polyType, PolyEnv * polyEnv) {
    if (polyEnv->env->count(polyType->polyStr) != 0) {
        return polyEnv->env->at(polyType->polyStr);
    }
    return NULL;
}

ArrayType* unify(ArrayType * arrType, PolyEnv * polyEnv) {
    // TODO :: does thIS needs fixing
    if (isTypePoly(arrType->baseType)) {
        auto polyType = (PolyType*)arrType->baseType;
        auto concreteType = unify(polyType, polyEnv);
        if (concreteType == NULL) {
            return NULL;
        }
        return createArrType(concreteType, arrType->num_dimensions);
    }
    return arrType;
}

LambdaType* unify(LambdaType * polyFunctionType, PolyEnv * polyEnv) {
    auto concreteReturnType = unifyTypes(polyFunctionType->returnType, polyEnv);
    if (concreteReturnType == NULL) {
        return NULL;
    }
    vector<Type*>* unified_args = new vector<Type*>();
    for (auto type : *polyFunctionType->arguments) {
        Type* concreteType = unifyTypes(type, polyEnv);
        if (concreteType == NULL) {
            return NULL;
        }
        unified_args->push_back(concreteType);
    }
    return createLambdaType(unified_args, concreteReturnType);
}

// The struct polyType will have a head definition of the following
// baseType[T]
// we create a non poly typ with tbe base types subtituted with a concret type 
// eg :: T = int, then baseType[T] would be converted to baseType[int]
StructPolyMorphicType* unify(StructPolyMorphicType * polyType, PolyEnv * polyEnv) {

    StructPolyMorphicType* noPoly = new StructPolyMorphicType();
    // We have to create a new type but with conretes added and replaced 
    noPoly->baseType = polyType->baseType;
    vector<Type*>* unifiedTypes = new vector<Type*>();
    for (auto maybePolyType : *polyType->polyTypes) {
        Type* concreteType = unifyTypes(maybePolyType, polyEnv);
        if (concreteType == NULL) {
            return NULL;
        }
        unifiedTypes->push_back(concreteType);
    }
    noPoly->polyTypes = unifiedTypes;
    return noPoly;
}

Type* expandPoly(SymbolTable * symbolTable,
    string structName,
    StructPolyMorphicType * polyStructType,
    StructPolyMorphicType * resolvedType,
    PolyEnv * polyEnv);

Type* expandPoly(SymbolTable * symbolTable,
    string structName,
    StructPolyMorphicType * polyStructType,
    StructPolyMorphicType * resolvedType) {

    PolyEnv* polyEnv = createPolyEnv();
    auto expanded_type = expandPoly(symbolTable, structName, polyStructType, resolvedType, polyEnv);
    delete polyEnv;
    return expanded_type;
}

Type* expandPoly(SymbolTable * symbolTable,
    string structName,
    StructPolyMorphicType * polyStructType,
    StructPolyMorphicType * resolvedType,
    PolyEnv * polyEnv) {

    auto mergedStructHeaderType = mergeTypes(polyStructType, resolvedType, polyEnv);
    auto expanded_type_name = mergedStructHeaderType->toString();

    auto typeOfStruct = getTypeDefinitionByName(symbolTable, expanded_type_name);
    if (typeOfStruct != NULL) {
        return typeOfStruct;
    }
    // Add early to support recursive expansions 
    addStructTypeSymbolTable(symbolTable, expanded_type_name, mergedStructHeaderType);

    auto polyStructLayout = searchStructLayout(symbolTable, structName);
    auto nonPolyStructLayout = new StructLayout();
    for (auto member : *polyStructLayout->fields) {
        Type* typeOfMemberForNonPoly = unifyTypes(member->returnType, polyEnv);
        if (typeOfMemberForNonPoly == NULL) {
            emit_error_info(NULL, "Member \"" + member->name + "\" of Struct \"" + structName + "\" has no poly binding. Missing PolyArg in struct def header <>");
        }

        if (isTypeStructPoly(typeOfMemberForNonPoly)) {
            // Explanation :: As we are iterating over each field we might need to do poly morphic expansation on the unified type 
            // For example if a field was resolved to be node<int> 
            // we need to expand the struct definition for the node<int> 
            // we get the base type "node" and just run the expansation of the resolved type 
            // Note we only do expansions if the member return type is a struct poly 
            if (member->returnType->type_of_type == TypeOfType::StructPolyType) {
                auto baseTypeStr = getBaseType((StructPolyMorphicType*)typeOfMemberForNonPoly)->typeStr;
                auto baseType = getTypeDefinitionByName(symbolTable, baseTypeStr);
                assert(isTypeStructPoly(baseType));
                expandPoly(symbolTable, baseTypeStr, (StructPolyMorphicType*)baseType, (StructPolyMorphicType*)typeOfMemberForNonPoly);
            }
        }
        auto structField = new Field(member->name, typeOfMemberForNonPoly);
        nonPolyStructLayout->fields->push_back(structField);
    }
    // Add everything to symbol table including type and struct layout
    addStructLayoutTableEntry(symbolTable, expanded_type_name, nonPolyStructLayout);
    return mergedStructHeaderType;
}

// Note (umar)
// This does a shallow clone of the ast since, only the object that actually require poly substitution are deep cloned 
AstNode* cloneToNonPoly(AstNode * src, PolyEnv * polyEnv);
AstIfStatement* cloneIfStatement(AstIfStatement * ifStatement, PolyEnv * polyEnv);
AstDotAssignment* clonedDotAssignment(AstDotAssignment * dotAssignment, PolyEnv * polyEnv);
AstAssignementStatement* cloneAssignment(AstAssignementStatement * src, PolyEnv * polyEnv);
AstObjectCreation* cloneObjectCreation(AstObjectCreation * objcreation, PolyEnv * polyEnv);
AstForLoop* cloneForLoop(AstForLoop * forLoop, PolyEnv * polyEnv);
AstForLoop* cloneForLoop(AstForLoop * forLoop, PolyEnv * polyEnv);
AstArraySubscriptAssignment* cloneArraySubscriptAssignment(AstArraySubscriptAssignment * src, PolyEnv * polyEnv);


FunctionSymbol* expandPoly_inner(SymbolTable * symbolTable,
    FunctionSymbol * polyFunctionSymbol,
    vector<Type*>*functionCallArgTypes,
    PolyEnv * polyEnv,
    FrameScope * current_frame_scope);

FunctionSymbol* expandPoly(SymbolTable * symbolTable,
    FunctionSymbol * polyFunctionSymbol,
    vector<Type*>*functionCallArgTypes,
    PolyEnv * polyEnv,
    FrameScope * current_frame_scope) {

    // if the function symbol is local we will use the existing poly env that was built up in the outer scope
    if (polyFunctionSymbol->isLocal) {
        return expandPoly_inner(symbolTable,
            polyFunctionSymbol,
            functionCallArgTypes,
            polyEnv,
            current_frame_scope);
    }

    // For non local function we just use a fresh polyenv 
    polyEnv = createPolyEnv();
    auto expanded_poly_function = expandPoly_inner(symbolTable,
        polyFunctionSymbol,
        functionCallArgTypes,
        polyEnv,
        current_frame_scope);
    delete polyEnv;
    return expanded_poly_function;

}

FunctionSymbol* expandPoly_inner(SymbolTable * symbolTable,
    FunctionSymbol * polyFunctionSymbol,
    vector<Type*>*functionCallArgTypes,
    PolyEnv * polyEnv,
    FrameScope * current_frame_scope) {

    assert(polyEnv != NULL);
    int index = 0;
    FunctionSymbol* nonPolyFunctionSymbol = createFunctionSymbol();
    auto expandedFunctionName = polyFunctionSymbol->name;
    for (auto param : *polyFunctionSymbol->params) {
        auto functionArgType = functionCallArgTypes->at(index);
        auto typeBinding = mergeTypes(param->returnType, functionArgType, polyEnv);
        assert(typeBinding != NULL);
        expandedFunctionName += typeBinding->toString();
        Symbol* symbol = new Symbol(param->name, typeBinding);
        symbol->symbolType = param->symbolType;
        addParamToFunctionSymbol(nonPolyFunctionSymbol, symbol);
        index++;
    }

    auto returnTypeBinded = unifyTypes(polyFunctionSymbol->returnType, polyEnv);
    assert(returnTypeBinded != NULL);
    expandedFunctionName += returnTypeBinded->toString();

    // We exit early if there is already an existing binding; this means we have already expanded the poly before hand
    auto existingBoundFunction = getFunctionSymbol(symbolTable, expandedFunctionName);
    if (existingBoundFunction != NULL) {
        return existingBoundFunction;
    }

    // TODO :: Move this to a straight copy of all the polyfunciton info that applies to the nonpoly
    nonPolyFunctionSymbol->hasVarArgs = polyFunctionSymbol->hasVarArgs;
    nonPolyFunctionSymbol->isExternal = polyFunctionSymbol->isExternal;
    nonPolyFunctionSymbol->returnType = returnTypeBinded;
    nonPolyFunctionSymbol->name = expandedFunctionName;
    nonPolyFunctionSymbol->numberOfArgs = polyFunctionSymbol->numberOfArgs;
    nonPolyFunctionSymbol->markedAsThrows = polyFunctionSymbol->markedAsThrows;
    nonPolyFunctionSymbol->isLocal = polyFunctionSymbol->isLocal;
    nonPolyFunctionSymbol->parent = polyFunctionSymbol->parent;
    nonPolyFunctionSymbol->functionDepthLevel = polyFunctionSymbol->functionDepthLevel;

    auto typeArgs = new vector<Type*>();
    for (auto param : *nonPolyFunctionSymbol->params) {
        typeArgs->push_back(param->returnType);
    }
    nonPolyFunctionSymbol->lambdaType = createLambdaType(typeArgs, nonPolyFunctionSymbol->returnType);

    if (nonPolyFunctionSymbol->params->size() != polyFunctionSymbol->params->size()) {
        throw runtime_error("Not all arguments could be binded");
    }

    // TODO :: What frame scope should be provided here??
    if (nonPolyFunctionSymbol->isLocal) {
        addFunctionSymbol(symbolTable, nonPolyFunctionSymbol, current_frame_scope);
    }
    else {
        addFunctionSymbol(symbolTable, nonPolyFunctionSymbol, NULL_FRAME_SCOPE);
    }

    if (!nonPolyFunctionSymbol->isExternal) {
        bool doFullExpansion = polyEnv->num_unresolved_polyTypes == 0;
        if (doFullExpansion) {
            FrameScope new_frame_scope;
            if (nonPolyFunctionSymbol->isLocal) {
                new_frame_scope = createFrameScope(current_frame_scope);
            }
            else {
                new_frame_scope = createFrameScope(NULL_FRAME_SCOPE);
            }

            AstBody * clonedFunctionBody = (AstBody*)cloneToNonPoly(polyFunctionSymbol->functionBody, polyEnv);
            assert(clonedFunctionBody->type == Body);
            nonPolyFunctionSymbol->functionBody = clonedFunctionBody;
            typeCheckFunctionBody(symbolTable, nonPolyFunctionSymbol, polyEnv, &new_frame_scope);
            nonPolyFunctionSymbol->needsPolyBinding = false;
        }
        else {
            nonPolyFunctionSymbol->needsPolyBinding = true;
        }
    }
    else {
        // TODO :: This should break for external poly functions at some point where await is polymorphically called in 
        // an unsresolved poly body, I am not sure if that can happen??? but for now lets makes sure everything can be resolved
        assert(polyEnv->num_unresolved_polyTypes == 0);
    }
    return nonPolyFunctionSymbol;
}

AstNode* cloneToNonPoly(AstNode * src, PolyEnv * polyEnv) {
    if (src == NULL) {
        throw runtime_error("clone poly src node is null");
    }

    else if (src->type == FunctionCall) {
        return src;
    }

    else if (src->type == ArrayAccess) {
        return src;
    }

    else if (src->type == IfStatement) {
        return cloneIfStatement((AstIfStatement*)src, polyEnv);
    }

    else if (src->type == Literal) {
        return src;
    }

    else if (src->type == ArithmeticExpression) {
        return src;
    }

    else if (src->type == BooleanExpression) {
        return src;
    }

    else if (src->type == Identifier) {
        return src;
    }

    else if (src->type == Throw) {
        return src;
    }

    else if (src->type == Try) {
        return src;
    }

    else if (src->type == DotAssignment) {
        return clonedDotAssignment((AstDotAssignment*)src, polyEnv);
    }

    else if (src->type == Assignment) {
        return cloneAssignment((AstAssignementStatement*)src, polyEnv);
    }

    else if (src->type == ArraySubscriptAssignment) {
        return cloneArraySubscriptAssignment((AstArraySubscriptAssignment*)src, polyEnv);
    }

    else if (src->type == BreakStatement) {
        return src;
    }

    else if (src->type == ContinueStatement) {
        return src;
    }
    else if (src->type == DotAccess) {
        return src;
    }

    else if (src->type == AnonymousFunction) {
        return src;
    }

    else if (src->type == FunctionDefinition) {
        // Note(umar) :: seem comment  @Hack::PolyClonedFunctionDefinition
        return createPolyClonedFunctionDef((AstFunctionDefinition*)src);
    }

    else if (src->type == Body) {
        AstBody* clonedBody = createAstBody();
        AstBody* srcBody = (AstBody*)src;
        for (auto statement : *(srcBody->statements)) {
            auto clonedStatement = cloneToNonPoly(statement, polyEnv);

            addFileInfo(statement, clonedStatement);
            addStatementToBody(clonedBody, clonedStatement);
        }
        return clonedBody;
    }

    else if (src->type == ForLoop) {
        return cloneForLoop((AstForLoop*)src, polyEnv);
    }
    else if (src->type == Null) {
        return src;
    }
    else if (src->type == ObjectCreation) {
        return cloneObjectCreation((AstObjectCreation*)src, polyEnv);
    }
    else if (src->type == Return) {
        return src;
    }

    assert_never_reaches("cloneToNonPoly::unrecognized ast type, you are most likely missing a a case");

}

AstForLoop* cloneForLoop(AstForLoop * forLoop, PolyEnv * polyEnv) {
    auto clonedBody = (AstBody*)cloneToNonPoly(forLoop->body, polyEnv);
    auto cloned = createAstForLoop(forLoop->assignment, forLoop->conditional, forLoop->postCondition, clonedBody);
    return cloned;
}

AstIfStatement* cloneIfStatement(AstIfStatement * ifStatement, PolyEnv * polyEnv) {
    AstBody* clonedBody = NULL;
    if (ifStatement->ifbody != NULL) {
        clonedBody = (AstBody*)cloneToNonPoly(ifStatement->ifbody, polyEnv);
        assert(clonedBody->type == Body);
    }

    auto clonedIf = createAstIfStatement(ifStatement->booleanExpression, clonedBody);
    for (auto if_else : *ifStatement->ifElses) {

        AstIfElse* clonedifElse = new AstIfElse();
        clonedifElse->ifbody = (AstBody*)cloneToNonPoly(if_else->ifbody, polyEnv);
        clonedifElse->booleanExpression = if_else->booleanExpression;

        addElseIfStatementToIf(clonedIf, clonedifElse);
    }

    if (ifStatement->elseBody != NULL) {
        auto clonedElseBody = (AstBody*)cloneToNonPoly(ifStatement->elseBody, polyEnv);
        clonedIf->elseBody = clonedElseBody;
    }
    return clonedIf;
}

AstDotAssignment* clonedDotAssignment(AstDotAssignment * dotAssignment, PolyEnv * polyEnv) {
    auto clonedEpxression = cloneToNonPoly(dotAssignment->expression, polyEnv);
    auto clonedDotAssignment = createDotAssignment(dotAssignment->dotAccess, clonedEpxression);
    return clonedDotAssignment;
}

AstAssignementStatement* cloneAssignment(AstAssignementStatement * src, PolyEnv * polyEnv) {
    auto clonedRValue = cloneToNonPoly(src->rightSide, polyEnv);
    auto cloned = createAstAssignment(src->identifier->name, clonedRValue, src->typeDecl);
    return cloned;
}

AstArraySubscriptAssignment* cloneArraySubscriptAssignment(AstArraySubscriptAssignment * src, PolyEnv * polyEnv) {
    auto clonedRValue = cloneToNonPoly(src->expression, polyEnv);
    auto cloned = createArraySubscriptAssignment(src->arrAccess, clonedRValue);
    return cloned;
}

AstTypeDecleration* clone_type_decl(AstTypeDecleration * typeDecl,
    PolyEnv * polyEnv) {
    if (typeDecl->polyFlags & PolyStruct) {
        vector<AstTypeDecleration*>* clonedPolyArgs = new vector<AstTypeDecleration*>();
        for (auto polyarg : *typeDecl->polyArgs) {
            if (polyarg->polyFlags & SinglePoly) {
                auto resolvedType = resolveType(polyEnv, polyarg->typeStr);
                auto resolvedTypeDecl = createTypeDecleration(resolvedType->toString(), PolyFlags::NoPoly, NULL);
                clonedPolyArgs->push_back(resolvedTypeDecl);
            }
            else if (polyarg->polyFlags & NoPoly) {
                clonedPolyArgs->push_back(polyarg);
            }
            else if (polyarg->polyFlags & PolyStruct) {
                auto resolvedTypeDecl = clone_type_decl(polyarg, polyEnv);
                clonedPolyArgs->push_back(resolvedTypeDecl);
            }
        }
        return createTypeDecleration(typeDecl->typeStr, typeDecl->polyFlags, clonedPolyArgs);
    }

    else if (typeDecl->polyFlags & SinglePoly) {
        auto resolvedType = resolveType(polyEnv, typeDecl->typeStr);
        // Note @hack ResolvedPolyExpansion
        // The single poly itself can resolved to a struct poly type
        // eg :: T -> TreeNode<T> 
        // the optional poly arguments are not rebuilt so we do not want the type checker running a type check on the optional poly arguments
        // ResolvedPolyExpansion lets us know we do not need to do this; nasty hack 
        string cloned_type_name = "";
        if (isTypeLambda(resolvedType)) {
            LambdaType* lambdaType = (LambdaType*)resolvedType;
            cloned_type_name = lambdaType->typeDefName;
        }
        else {
            cloned_type_name = resolvedType->toString();
        }
        return createTypeDecleration(cloned_type_name, PolyFlags::ResolvedPolyExpansion, NULL);
    }

    else if (typeDecl->polyFlags & NoPoly) {
        // Nothing to clone 
        return typeDecl;
    }
    assert_never_reaches("resolve_cloned_type_decl :: poly flag case missing");
}

AstObjectCreation* cloneObjectCreation(AstObjectCreation * objcreation, PolyEnv * polyEnv) {

    auto typeDecl = objcreation->typeDecl;
    // If there are no poly flags then we do not have to clone the object creation since its already typed
    if (typeDecl->polyFlags & NoPoly) {
        return objcreation;
    }
    auto cloned_type_decl = clone_type_decl(typeDecl, polyEnv);
    cloned_type_decl->arr_dimensions = typeDecl->arr_dimensions;
    cloned_type_decl->isArray = typeDecl->isArray;
    if (typeDecl->polyFlags & PolyArray) {
        if (isFlagPoly(cloned_type_decl->polyFlags)) {
            cloned_type_decl->polyFlags |= PolyArray;
        }
    }
    auto cloned = createAstObjectCreation(cloned_type_decl);
    return cloned;
}
