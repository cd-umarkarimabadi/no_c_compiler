
#ifndef AST_FUN_H
#define AST_FUN_H

#include "ast/ast.h"
#include "tokenizer/tokenizer.h"

AstLiteralValue* createLitearlValueNodeForInt(int value);
AstArithMeticExpression* createAithemticExpressionNode(AstNode* left, AstNode* right, string op);
AstBooleanExpression* createBooleanExpressionNode(AstNode* left, AstNode* right, string op);
AstLiteralValue* createLitearlValueNodeForBool(bool value);
AstLiteralValue* createLitearlValueNodeForString(string literal);
AstLiteralValue* createLiterallValueNodeForDouble(double value);
AstLiteralValue* createLiterallValueNodeForChar(string value);
AstIfStatement* createAstIfStatement(AstNode*  booleanExpression, AstBody* body);
AstIfElse* createIfElseFromIf(AstIfStatement* ifStatement);
void addElseStatementToIf(AstIfStatement* ifStatement, AstBody* body);
void addElseIfStatementToIf(AstIfStatement* ifStatement, AstIfElse* ifElse);
AstInterfaceNode* createAstInterface(string name);
AstBreakStatement* createBreakStatement();
AstContinueStatement* createAstContinueStatement();
AstDecleration* createAstDecleration(string decl_name, AstTypeDecleration* typeDecl);
AstDecleration* createAstDecleration(string decl_name, AstLambdaDecleration* lambdaDecl);
AstLambdaDecleration* createLambdaDecleration();
void addParamToLambdaDecl(AstLambdaDecleration* lambdaDecl, AstTypeDecleration* typeDecl);
AstMatchNode* createAstMatchNode(string targetMatch);
AstCaseBlock* createAstCaseBlock();
void addTargetToCaseBlock(AstCaseBlock* caseblock, string target);
void addCaseBlockToMatchNode(AstMatchNode* matchNode, AstCaseBlock* caseBlock);
AstEnum* createAstEnum(string enunName);
void addEnumValueToEnum(AstEnum* enumNode, AstIdentifier* enumValue);
AstDotAccess* createAstDotAccess(string name);
AstDotAccess* createAstDotAccess(AstDotElement* firstDot);
void appendDotAccess(AstDotAccess* dotAccess, AstDotElement* element);
void addDeclToStruct(AstStructNode* structNode, AstDecleration* decl);
AstBody* createAstBody();
AstStructNode* createStructNode();
void addPolyArg(AstStructNode* structNode, string polyArg);
AstAssignementStatement* createAstAssignment(string identiferName, AstNode* rValue, AstTypeDecleration* optionalType);
AstAssignementStatement* createAstAssignmentForObject(string identiferName, AstObjectCreation* objectCreation, AstTypeDecleration* optionalType);
void addStatementToBody(AstBody* astBody, AstNode* statement);
AstFunctionDefinition* createFunctionDefinition();
void addDeclToFunctionParams(AstFunctionDefinition* functionDef, AstDecleration* decleration);
AstFunctionCall* createFunctionCall(string functionName);
AstClosureCreation* createClosureCreation();
AstLambaCall* createAstLambdaCall(AstFunctionCall* functionCall, int num_static_links_to_walk_up);
AstForEachStatment* createAstForEach();
AstForLoop* createAstForLoop(
    AstAssignementStatement* assignment,
    AstNode* conditional,
    AstAssignementStatement* postCondition,
    AstBody* forLoopBody);

AstForLoop* createInfiniteLoop(AstBody* forLoopBody);
void addArgumentToFunctionCall(AstFunctionCall* functionCall, AstNode* arg);
void addBodyToFunctionDef(AstFunctionDefinition* functionDef, AstBody* body);
AstIdentifier* createAstIdentifier(string name);
void printAst(AstNode* program);
AstArrayDecleration* createAstArrayDecleration(string typeStr);
ArrayDimension* createArrDimension();
void addArrDimensionToArrayDecl(AstArrayDecleration* arrDecl, ArrayDimension* arrDimension);
AstReturnStatement* createAstReturnStatement(AstNode* returnExpression);
AstArrayAccess* createAstArrayAccess();
AstScopedIdentifier* createScopedIdentifier(string identifierName, int num_stack_to_walk_up);
AstArraySubscriptAssignment* createArraySubscriptAssignment(AstArrayAccess* arrayAccess, AstNode* assignmentExpression);
AstObjectCreation* createAstObjectCreation(AstTypeDecleration* typeDecl);
void addPolyArgToObjectCreation(AstObjectCreation* objectCreation, string polyArg);
AstDotAssignment* createDotAssignment(AstDotAccess * dotAccess, AstNode * expression);
AstNullNode* createNullNode();
AstDefer* createAstDefer(AstFunctionCall * deferredFunctionCall);
AstThrow* createAstThrow(AstFunctionCall * functionCall);
AstTryStatement* createAstTry(AstFunctionCall * functionCall);
AstTupleError* createAstTuple();
void addTuple(AstTupleError * tuple, string identifier);
void addFunctionCallToTuple(AstTupleError * tuple, AstFunctionCall * functionCall);
void addFileInfo(TokenStream* tokenStream, AstNode * node, Token * startTokenOfNode);
void addFileInfo(TokenStream* tokenStream, AstNode * node, int line_number, int column_number);
void addFileInfo(AstNode* src, AstNode* target);
AstDotElement* createAstDotElement(string name);
AstDotElement* createAstDotElement(AstArrayAccess* arr_access);
AstDotElement* createAstDotElement(AstScopedIdentifier* scopedIdentifier);
void addSubtitutionToNode(AstNode * src, AstNode * sub);
AstTypeDecleration* createTypeDecleration(string type_name, PolyFlags flags, vector<AstTypeDecleration*>* optionalPolyArg);
AstTypeDecleration* createArrTypeDecleration(AstArrayDecleration* arrDecl);
AstYield* createAstYield();
AstNoRoutine* createPrivateCoRoutine(AstAnonFunction* anonFunction);
AstNoRoutine* createFunctionCallCoRoutine(AstFunctionCall* coRoutineBody);
AstNoRoutine* createFunctionCoRoutine(AstFunctionCall * functionCall);
AstExternalFunction* createExternalFunction(AstFunctionDefinition* functionDef);
AstTryCatchAssignmentStatement* createTryCatchAssignmentStatement(AstAssignementStatement* statement);
AstNegativeExpression* createNegativeExpression(AstNode* expression);
AstPolyClonedFunctionDefinitition* createPolyClonedFunctionDef(AstFunctionDefinition* src);
bool isOptionalTypePresent(AstAssignementStatement* assignement);
AstFunctionTypeDef* createastFunctionTypeDef(AstLambdaDecleration* lambdaDecl);
AstAnonFunction* createAnonFunction();

#endif