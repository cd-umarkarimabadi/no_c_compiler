#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <string>
#include <vector>
#include <set>
using namespace std;
#include <map>
#include "ast/ast.h"

#define NULL_POLY_ENV NULL
#define NULL_FRAME_SCOPE NULL

#define GLOBAL_DEPTH_ZERO 0

enum struct TypeOfType {
    BaseType,
    ArrayType,
    PolyType, // This is a simple poly type like this [T]
    EnumType,
    StructPolyType, // list[int] something like this
    LambdaType // f:int -> int ;; funciton has one argumen of type int and returns int 
};

struct Type {
    TypeOfType type_of_type;
    virtual string toString() = 0;
};

struct EnumType : public Type {
    string enumName;

    EnumType() {
        this->type_of_type = TypeOfType::EnumType;
    }

    string toString() override {
        return enumName;
    }
};

struct BaseType : public Type {
    bool isPrimitive;
    string typeStr;

    BaseType() {
        this->type_of_type = TypeOfType::BaseType;
    }

    BaseType(string type_str, bool isPrimitive) {
        this->type_of_type = TypeOfType::BaseType;
        this->typeStr = type_str;
        this->isPrimitive = isPrimitive;
    }

    string toString() override {
        return this->typeStr;
    }
};

struct PolyType : public Type {
    string polyStr;

    PolyType() {
        this->type_of_type = TypeOfType::PolyType;
    }
    string toString() override {
        return this->polyStr;
    }
};

struct ArrayType : public Type {
    size_t num_dimensions;
    Type* baseType;

    ArrayType() {
        this->type_of_type = TypeOfType::ArrayType;
    }

    string toString() override {
        return this->baseType->toString() + to_string(this->num_dimensions) + "N"; // todo get dimensions of arr as string
    }
};

struct StructPolyMorphicType : public Type {
    Type* baseType;
    vector<Type*>* polyTypes;

    StructPolyMorphicType() {
        this->type_of_type = TypeOfType::StructPolyType;
        this->polyTypes = new vector<Type*>();
    }

    string toString() override {
        string s = this->baseType->toString();
        s += "<";
        for (auto polyType : *polyTypes) {
            s += polyType->toString();
        }
        s += ">";
        return s;
    }
};

struct LambdaType : public Type {
    Type* returnType;
    vector<Type*>* arguments;
    string typeDefName;

    LambdaType() {
        this->type_of_type = TypeOfType::LambdaType;
    }

    string toString() override {
        if (typeDefName != "") {
            return typeDefName;
        }
        string s = this->returnType->toString();
        s += "<";
        for (auto arg : *this->arguments) {
            s += arg->toString();
        }
        s += ">";
        return s;
    }
};

struct TupleType : public Type {
    Type* type_1;
    Type* type_2;

    string toString() override {
        return this->type_1->toString() + this->type_2->toString();
    }
};

enum struct SymbolType {
    Lambda,
    LambdaTypeDefinition,
    GlobalType,
    Identifier, // TODO :: Rename this 
    Struct,
    FunctionDefinition,
    Enum,
};


struct Scoping_info {
    int num_static_links = 0;
};

struct Symbol {
    int function_depth_level;
    SymbolType symbolType;
    string name;
    Type* returnType;
    Scoping_info scoping_info;

    Symbol(string name, Type* returnType) {
        this->name = name;
        this->returnType = returnType;
    }
};

struct Field {
    string name;
    Type* returnType;
    size_t mem_offset;

    Field(string name, Type* returnType) {
        this->name = name;
        this->returnType = returnType;
    }
};

struct EnumSymbol {
    string name;
    EnumType* type;
    map<string, int>* enums;
};

struct StructLayout {
    int identifier_index;
    size_t total_size_of_struct;
    vector<string>* polyArgHeader;
    vector<Field*>* fields;
    StructLayout() {
        fields = new vector<Field*>();
        polyArgHeader = new vector<string>();
    }
};

struct NonLocalIdentifier {
    AstNode* identifier;
    Symbol* rootSymbol; // The root symbol is where that symbol is declared
    int functionDepthLevel;
    bool doesNonLocalReferenceBoxed;
};

// A poly env is a mapping between a polymorphic type decleration and its concret type 
// eg :: {T -> int, B -> string }
struct PolyEnv {
    int num_unresolved_polyTypes;
    map<string, Type*>* env;

};

struct FrameScope {
    vector<map<string, Symbol*>*>* symbols;
    FrameScope* parent;
    int function_depth_level;
};

struct FunctionSymbol {

    // Exclusive owernship
    bool needsPolyBinding;
    bool isLocal;
    bool needsLambdaLifting;
    int functionDepthLevel;
    string name;
    size_t numberOfArgs;
    bool hasVarArgs;
    vector<Symbol*>* params;
    Type* returnType;
    bool isExternal;
    bool markedAsThrows;
    bool is_recursive;
    AstFunctionDefinition* functionDef;
    AstBody* functionBody;
    PolyEnv* polyEnv; // @hack because I need a refernce to the first ever poly env created 
    LambdaType* lambdaType;

    // Shared ownership
    FunctionSymbol* parent; // For local function binding
    vector<NonLocalIdentifier*>* nonLocalIdentifiers; // TODO :: Should be part of transformation 
};

struct FunctionSymbolTable {
    map<string, FunctionSymbol*>* table;
};

struct EnumSymbolTable {
    map<string, EnumSymbol*>* table;
};

struct StructSymbolTable {
    map<string, StructLayout*>* table;
};

struct EscapingFunctionSymbol {
    AstNode* escapingNode;
    FunctionSymbol* escapingFunctionSymbol;
};


struct NodeTypeMap {
    map<AstNode*, Type*>* typeMap;
};

struct SymbolTable {
    // Required global lifetime 
    FunctionSymbolTable* fSymbolTable;
    StructSymbolTable* structSymbolTable;
    EnumSymbolTable* enumSymbolTable;

    map<string, Symbol*>* globalSymbolTable;
    // The following is needed for code gen and can be deleted after code gen is done
    map<FunctionSymbol*, NodeTypeMap*>* nodeTypeMap;
    vector<EscapingFunctionSymbol*>* functionDefThatEscape;
};


struct TuplePair {
    Type* pair_1;
    Type* pair_2;

    TuplePair(Type* pair_1, Type* pair_2) {
        this->pair_1 = pair_1;
        this->pair_2 = pair_2;
    }
};

enum LRTypeOrder {
    left_i_right_d, // left is int right is double
    left_d_right_i, // left is double right is int
    left_i_right_c, // left is int right is character
    left_c_right_i, // left is char right is int
    same, // both same 
    bad_match,
};

typedef struct primitive_type_conversion {
    Type* left;
    Type* right;
    Type* inferred;
    LRTypeOrder order;

} primitive_type_conversion_t;

extern BaseType voidReturn;
extern BaseType intReturn;
extern BaseType doubleReturn;
extern BaseType boolReturn;
extern BaseType nullReturnType;
extern BaseType byteReturn;
extern BaseType charReturn;
extern BaseType typeAnyReturn;


SymbolTable* createSymbolTable();
void typeCheck(SymbolTable * symTable, AstBody* progam);

LRTypeOrder getLRPrimitiveTypeTransformation(Type* left, Type* right);

void storeTypeOfNode(SymbolTable * symbolTable, FunctionSymbol* fSymbol, AstNode * node, Type * type);
Type* loadTypeOfNode(SymbolTable * symbolTable, FunctionSymbol* fSymbol, AstNode * node);

FunctionSymbol* getFunctionSymbol(SymbolTable* symTable, string functionName);
Type* getTypeDefinitionByName(SymbolTable* symTable, string name);

bool isReturnTypeVoid(Type* returnType);
bool isReturnTypeBool(Type* returnType);
bool isReturnTypeInt(Type* returnType);
bool isReturnTypeDouble(Type* returnType);
bool isTypeArray(Type* type);
bool isTypePrimitive(Type* type);
bool isTypeEnum(Type* type);
bool isTypeLambda(Type* type);

StructLayout* searchStructLayout(SymbolTable* symTable, string symbol);
StructLayout* searchStructLayoutByIdentifier(SymbolTable * symbolTable, int identiiferIndex);
Field* searchField(StructLayout* layout, string field_name);
void addStructLayoutTableEntry(SymbolTable * symbolTable, string structName, StructLayout * layout);

void resolve_closures(SymbolTable * symbolTable);
size_t sizeOfStruct(StructLayout * layout);

int getIntValueFromEnum(EnumSymbol * enumSymbol, string toSearch);

Type* get_err_type(SymbolTable* symbolTable);
void run_try_handler_cfg_checks(SymbolTable* symbolTable, AstFunctionDefinition* functionDef);
void run_try_handler_cfg_checks(SymbolTable* symbolTable, AstBody* body);

void emit_error_info(AstNode* node, string message_err);


void addStructTypeSymbolTable(SymbolTable * symbolTable, string strctName, Type * type);
BaseType* createStructReturnType(string typeStr);

string internal_closure_struct_runtime_name();

#endif // TYPECHECKER_H