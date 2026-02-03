#ifndef AST_H
#define AST_H

#include <string>
#include <vector>
#include <iostream>

using namespace std;

enum PolyFlags {
    NoPoly = 1,
    PolyStruct = (1 << 2),
    PolyArray = (1 << 3),
    SinglePoly = (1 << 4),

    // TODO :: ResolvedPolyExpansion is a big hack, see comment 
    // @hack ResolvedPolyExpansion
    ResolvedPolyExpansion = (1 << 5),
};

inline PolyFlags operator&(PolyFlags a, PolyFlags b) {
    return (PolyFlags)((int)a & (int)b);
}

inline PolyFlags& operator|=(PolyFlags& a, PolyFlags b) {
    a = (PolyFlags)((int)a | (int)b);
    return a;
}

enum NodeType {
    StructDef,
    FunctionCall,
    ClosureCreation,
    FunctionTypeDef,
    LambdaCall,
    FunctionParam,
    AnonymousFunction,
    FunctionDefinition,
    PolyClonedFunctionDefinition, // Note see comment @Hack::PolyClonedFunctionDefinition
    ExternalFunctionDefinition,
    Interface,
    IfStatement,
    Literal,
    ArithmeticExpression,
    BooleanExpression,
    Identifier,
    TypedIdentifier,
    Body,
    Assignment,
    BreakStatement,
    ContinueStatement,
    ForLoop,
    ArrayAccess,
    ArraySubscriptAssignment,
    Return,
    ObjectCreation,
    DotAccess,
    DotAssignment,
    Null,
    Defer,
    Try,
    TryLabel,
    Throw,
    Tuple,
    Match,
    Case,
    Enum,
    TypeDecleration,
    ScopedIdentifier,
    Yield,
    NoRoutine,
    CoRoutineLifted,
    TryCatchAssignment,
    NegativeExpression
};

enum LiteralType {
    NumberLiteral,
    StringLiteral,
    BooleanLiteral,
    DoubleLiteral,
    CharLiteral,
    HexLiteral,
};

struct LiteralValue {
    bool boolValue;
    double doubleValue;
    int intValue;
    string strValue;
};

struct AstNode {
    NodeType type;
    AstNode* sub; // This is a substition field that will be used to replace. The type chcker will use when running transformations
    int line_number;
    int column_number;
    string file_name;
};

struct AstIdentifier : public AstNode {
    string name;
};

struct AstTypedIdentifier : public AstNode {
    string name;
    string type;
};

struct Subscript {
    AstNode* expression;
};

struct AstArrayAccess : public AstNode {
    AstIdentifier* identifier;
    vector<Subscript*>* subScripts;
};

struct AstArraySubscriptAssignment : public AstNode {
    AstArrayAccess* arrAccess;
    AstNode* expression;
};

struct ArrayDimension {
    AstNode* dimensionExpression;
    int size;
    bool isSizeKnown;
};

struct AstYield : public AstNode {

};

struct AstArrayDecleration : public AstNode {
    string type_str;
    bool areAllDimensionResolvable;
    vector<ArrayDimension*>* dimensions; // Can get the num of dimensions from this 
};

struct AstTypeDecleration : public AstNode {
    PolyFlags polyFlags;
    vector<AstTypeDecleration*>* polyArgs;
    string typeStr;
    bool isArray;
    vector<ArrayDimension*>* arr_dimensions;
};

enum struct DeclerationType {
    Lambda,
    Normal,
};

struct AstLambdaDecleration : public AstNode {
    string name;
    vector<AstTypeDecleration*>* params;
    AstTypeDecleration* returnType;
};

struct AstDecleration : public AstNode {
    DeclerationType declType;
    string decl_name;
    AstTypeDecleration* typeDecl;
    AstLambdaDecleration* lambdaFunction;
};

struct AstBody : public AstNode {
    vector<AstNode*>* statements;
};

struct AstFunctionDefinition : public AstNode {
    string functionName;
    vector<AstDecleration*>* paramaters;
    AstTypeDecleration*    returnTypeDecl;
    bool returnIsVoid;
    bool returnIsArr;
    bool throwsError;
    bool hasPolyArgs;
    bool isExternal;
    AstBody* body;
};

// Anon is short for anonymous :: shortened because I cannot spell
struct AstAnonFunction : public AstNode {
    string private_name; //The tye checker transformer  will add this 
    AstFunctionDefinition* functionDef;
};

// @TODO :: HACK
struct AstPolyClonedFunctionDefinitition : public AstNode {
    AstFunctionDefinition* functionDef;
};

struct AstExternalFunction : public AstNode {
    AstFunctionDefinition* functionDef;
};

struct AstInterfaceNode : public AstNode {
    string name;
    vector<AstFunctionDefinition*>* functions;
    void addFunction(AstFunctionDefinition* funcionDef);

};

struct AstFunctionTypeDef : public AstNode {
    AstLambdaDecleration* lambda_decl;
};

struct AstLiteralValue : public AstNode {
    LiteralType valueType;
    LiteralValue* literalValue;
};

struct AstStructNode : public AstNode {
    bool isDefinedAsPoly;
    vector<string>* polyHeader;
    string name;
    vector<AstDecleration*>* members;
    void addDecleration(AstDecleration* decleration);
};

enum struct CoRoutineBodyType {
    Anonymous = 0,
    FunctionCall = 1,
    //ClosureCall = 2
};

struct AstNoRoutine : public AstNode {
    AstNode* body;
    CoRoutineBodyType coRoutineBodyType;
};

struct AstDotElement : public AstNode {
    AstIdentifier* identifier_access;
    AstArrayAccess* arr_access;
};

struct AstDotAccess : public AstNode {
    vector<AstDotElement*>* accesses;
};

struct AstArithMeticExpression : public AstNode {
    string op;
    AstNode* left;
    AstNode* right;
};

struct AstBooleanExpression : public AstNode {
    string op;
    AstNode* left;
    AstNode* right;

};

struct AstIfElse : public AstNode {
    AstNode* booleanExpression;
    AstBody* ifbody;
};

struct AstIfStatement : public AstNode {
    AstNode* booleanExpression;
    AstBody* ifbody;
    vector<AstIfElse*>* ifElses;
    AstBody* elseBody;
};

struct AstAssignementStatement : public AstNode {
    AstIdentifier* identifier;
    AstTypeDecleration* typeDecl;
    AstNode* rightSide;
};

struct AstTryCatchAssignmentStatement : public AstNode {
    AstAssignementStatement* assignment;
};

struct AstContinueStatement : public AstNode {
    AstContinueStatement() {
        this->type = ContinueStatement;
    }
};

struct AstBreakStatement : public AstNode {
    AstBreakStatement() {
        this->type = BreakStatement;
    }
};

struct AstForLoop : public AstNode {
    AstAssignementStatement* assignment;
    AstNode* conditional;
    AstAssignementStatement* postCondition;
    AstBody* body;
};

struct AstForEachStatment : public AstNode {

};

struct AstFunctionCall : public AstNode {
    string name;
    vector<AstNode*>* arguments;
};


struct AstClosureCreation : public AstNode {
    string escapingFunctionName;
};

struct AstLambaCall : public AstNode {
    AstFunctionCall* functionCall;
    int call_stacks_to_walk_up;
};

struct AstTryStatement : public AstNode {
    AstFunctionCall* functionCall;
};

struct AstTryLabel : public AstNode {
    AstTryLabel() {
        this->type = TryLabel;
    }
};

struct AstThrow : public AstNode {
    AstFunctionCall* functionCall;
};

struct AstDefer : public AstNode {
    AstFunctionCall* defferedFunctionCall;
};


struct AstScopedIdentifier : public AstIdentifier {
    int call_stack_difference;
};

struct AstEnum : public AstNode {
    AstIdentifier* enumIdentifier;
    vector<AstIdentifier*>* enums;
};

struct AstCaseBlock : public AstNode {
    vector<string>* targets;
    AstBody* body;
};

struct AstMatchNode : public AstNode {
    AstIdentifier* matchTarget;
    vector<AstCaseBlock*>* cases;
};

struct AstTupleError : public AstNode {
    vector<string>* tuple;
    AstFunctionCall* rightSide;
};

struct AstReturnStatement : public AstNode {
    AstNode* expression;
};

struct AstObjectCreation : public AstNode {
    AstTypeDecleration* typeDecl;
};

struct AstDotAssignment : public AstNode {
    AstDotAccess* dotAccess;
    AstNode* expression;
};

struct AstNegativeExpression : public AstNode {
    AstNode* expression;
};

struct AstNullNode : public AstNode {
    AstNullNode() {
        this->type = Null;
    }
};

#endif
