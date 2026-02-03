#include "ast/ast.h"
#include "ast/ast_fun.h"
#include <iostream>
#include "tokenizer/tokenizer.h"
using namespace std;

AstBreakStatement* breakStatement = new AstBreakStatement();
AstContinueStatement* continueStatement = new AstContinueStatement();
AstNullNode* nullNode = new AstNullNode();

AstBreakStatement* createBreakStatement() {
    return breakStatement;
}

AstContinueStatement* createAstContinueStatement() {
    return continueStatement;
}

AstNoRoutine* createCoRoutine(AstNode* coRoutineBody, CoRoutineBodyType type) {
    AstNoRoutine* coRoutine = new AstNoRoutine();
    coRoutine->type = NoRoutine;
    coRoutine->body = coRoutineBody;
    coRoutine->coRoutineBodyType = type;
    return coRoutine;
}

AstNoRoutine* createPrivateCoRoutine(AstAnonFunction* anonFunction) {
    return createCoRoutine(anonFunction, CoRoutineBodyType::Anonymous);
}

AstNoRoutine* createFunctionCallCoRoutine(AstFunctionCall* coRoutineBody) {
    return createCoRoutine(coRoutineBody, CoRoutineBodyType::FunctionCall);
}

AstNoRoutine* createFunctionCoRoutine(AstFunctionCall* functionCall) {
    AstNoRoutine* coRoutine = new AstNoRoutine();
    coRoutine->type = NoRoutine;
    coRoutine->body = functionCall;
    coRoutine->coRoutineBodyType = CoRoutineBodyType::FunctionCall;
    return coRoutine;
}

AstExternalFunction* createExternalFunction(AstFunctionDefinition* functionDef) {
    AstExternalFunction* externalFunction = new AstExternalFunction();
    externalFunction->functionDef = functionDef;
    externalFunction->type = ExternalFunctionDefinition;
    return externalFunction;
}

AstYield* createAstYield() {
    AstYield* yield = new AstYield();
    yield->type = Yield;
    return yield;
}

AstTypeDecleration* createTypeDecleration(string type_name,
    PolyFlags flags,
    vector<AstTypeDecleration*>* optionalPolyArg) {

    AstTypeDecleration* typeDecl = new AstTypeDecleration();
    typeDecl->polyFlags = flags;
    typeDecl->type = TypeDecleration;
    typeDecl->polyArgs = optionalPolyArg;
    typeDecl->typeStr = type_name;
    typeDecl->type = TypeDecleration;
    return typeDecl;
}

AstFunctionTypeDef* createastFunctionTypeDef(AstLambdaDecleration* lambdaDecl) {
    AstFunctionTypeDef*  typeDef = new AstFunctionTypeDef();
    typeDef->type = FunctionTypeDef;
    typeDef->lambda_decl = lambdaDecl;
    return typeDef;
}

AstAnonFunction*  createAnonFunction() {
    AstAnonFunction* anon = new AstAnonFunction();
    anon->type = AnonymousFunction;
    return anon;
}

AstDecleration* createAstDecleration(string decl_name, AstTypeDecleration* typeDecl) {
    AstDecleration* decl = new AstDecleration();
    decl->decl_name = decl_name;
    decl->typeDecl = typeDecl;
    decl->declType = DeclerationType::Normal;
    return decl;
}

AstDecleration* createAstDecleration(string decl_name, AstLambdaDecleration* lambdaDecl) {
    AstDecleration* decl = new AstDecleration();
    decl->decl_name = decl_name;
    decl->lambdaFunction = lambdaDecl;
    decl->declType = DeclerationType::Lambda;
    return decl;
}

AstLambdaDecleration* createLambdaDecleration() {
    AstLambdaDecleration* lambdaDecl = new AstLambdaDecleration();
    lambdaDecl->params = new vector<AstTypeDecleration*>();
    return lambdaDecl;
}

void addParamToLambdaDecl(AstLambdaDecleration* lambdaDecl, AstTypeDecleration* typeDecl) {
    lambdaDecl->params->push_back(typeDecl);
}

AstDotElement* createAstDotElement(string name) {
    AstDotElement* element = new AstDotElement();
    element->identifier_access = createAstIdentifier(name);
    return element;
}

AstDotElement* createAstDotElement(AstArrayAccess* arr_access) {
    AstDotElement* element = new AstDotElement();
    element->arr_access = arr_access;
    element->identifier_access = arr_access->identifier;
    return element;
}

AstDotElement* createAstDotElement(AstScopedIdentifier* scopedIdentifier) {
    AstDotElement* element = new AstDotElement();
    element->identifier_access = scopedIdentifier;
    return element;
}

AstDotAccess* createAstDotAccess(AstDotElement* firstDot) {
    AstDotAccess* dotAccess = new AstDotAccess();
    dotAccess->type = DotAccess;
    dotAccess->accesses = new vector<AstDotElement*>();
    dotAccess->accesses->push_back(firstDot);
    return dotAccess;
}

AstDotAccess* createAstDotAccess(string name) {
    AstDotAccess* dotAccess = new AstDotAccess();
    dotAccess->type = DotAccess;
    dotAccess->accesses = new vector<AstDotElement*>();
    dotAccess->accesses->push_back(createAstDotElement(name));
    return dotAccess;
}

void appendDotAccess(AstDotAccess* dotAccess, AstDotElement* element) {
    dotAccess->accesses->push_back(element);
}

void addDeclToStruct(AstStructNode* structNode, AstDecleration* decl) {
    structNode->members->push_back(decl);
}

AstInterfaceNode* createInterfaceNode(string name) {
    AstInterfaceNode* node = new AstInterfaceNode();
    node->name = name;
    node->functions = new vector<AstFunctionDefinition*>();
    return node;
}

AstObjectCreation* createAstObjectCreation(AstTypeDecleration*  typeDecl) {
    AstObjectCreation* objectCreation = new AstObjectCreation();
    objectCreation->type = ObjectCreation;
    objectCreation->typeDecl = typeDecl;
    return objectCreation;
}

AstAssignementStatement* createAstAssignment(string identiferName, AstNode* rValue, AstTypeDecleration* optionalTypeDecl) {
    AstAssignementStatement* assignmentNode = new AstAssignementStatement();
    assignmentNode->identifier = createAstIdentifier(identiferName);
    assignmentNode->typeDecl = optionalTypeDecl;
    assignmentNode->rightSide = rValue;
    assignmentNode->type = Assignment;
    return assignmentNode;
}

AstArrayDecleration* createAstArrayDecleration(string typeStr) {
    AstArrayDecleration* arrDecl = new AstArrayDecleration();
    arrDecl->type_str = typeStr;
    arrDecl->dimensions = new vector<ArrayDimension*>();
    return arrDecl;
}

// TODO :: I needed the cycle in the type checker so I can go back to parent but i think I can just 
// Have the object creation be its own type, very similar to gos := vs = 
AstAssignementStatement* createAstAssignmentForObject(string identiferName,
    AstObjectCreation* objectCreation,
    AstTypeDecleration* optionalTypeDecl) {
    AstAssignementStatement* assignmentNode = new AstAssignementStatement();
    assignmentNode->identifier = createAstIdentifier(identiferName);
    assignmentNode->rightSide = objectCreation;
    assignmentNode->type = Assignment;
    assignmentNode->typeDecl = optionalTypeDecl;
    return assignmentNode;
}

void AstInterfaceNode::addFunction(AstFunctionDefinition* functionDef) {
    this->functions->push_back(functionDef);
}

AstStructNode* createStructNode() {
    AstStructNode* node = new AstStructNode();
    node->type = StructDef;
    node->members = new vector<AstDecleration*>();
    node->isDefinedAsPoly = false;
    node->polyHeader = new vector<string>();
    return node;
}

void addPolyArg(AstStructNode* structNode, string polyArg) {
    structNode->polyHeader->push_back(polyArg);
    structNode->isDefinedAsPoly = true;
}

void AstStructNode::addDecleration(AstDecleration* decleration) {
    this->members->push_back(decleration);
}

AstLiteralValue* createLitearlValueNodeForBool(bool value) {
    AstLiteralValue* node = new AstLiteralValue();
    node->type = Literal;
    node->valueType = BooleanLiteral;
    LiteralValue* literalValue = new LiteralValue();
    literalValue->boolValue = value;
    node->literalValue = literalValue;
    return node;
}

AstLiteralValue* createLitearlValueNodeForString(string literal) {
    AstLiteralValue* node = new AstLiteralValue();
    node->type = Literal;
    node->valueType = StringLiteral;
    LiteralValue* literalValue = new LiteralValue();
    literalValue->strValue = literal;
    node->literalValue = literalValue;
    return node;
}

AstLiteralValue* createLitearlValueNodeForInt(int value) {

    AstLiteralValue* node = new AstLiteralValue();
    node->type = Literal;
    node->valueType = NumberLiteral;
    LiteralValue* literalValue = new LiteralValue();
    literalValue->intValue = value;
    node->literalValue = literalValue;

    return node;
}

AstLiteralValue* createLiterallValueNodeForDouble(double value) {

    AstLiteralValue* node = new AstLiteralValue();
    node->type = Literal;
    node->valueType = DoubleLiteral;
    LiteralValue* literalValue = new LiteralValue();
    literalValue->doubleValue = value;
    node->literalValue = literalValue;
    return node;
}

AstLiteralValue* createLiterallValueNodeForChar(string value) {
    AstLiteralValue* node = new AstLiteralValue();
    node->type = Literal;
    node->valueType = CharLiteral;
    LiteralValue* literalValue = new LiteralValue();
    literalValue->strValue = value;
    node->literalValue = literalValue;
    return node;
}

AstBooleanExpression* createBooleanExpressionNode(AstNode* left, AstNode* right, string op) {
    AstBooleanExpression* node = new AstBooleanExpression();
    node->type = BooleanExpression;
    node->left = left;
    node->right = right;
    node->op = op;
    return node;
}

AstArithMeticExpression* createAithemticExpressionNode(AstNode* left, AstNode* right, string op) {
    AstArithMeticExpression* node = new AstArithMeticExpression();
    node->type = ArithmeticExpression;
    node->left = left;
    node->right = right;
    node->op = op;
    return node;
}

AstIfStatement* createAstIfStatement(AstNode*  booleanExpression, AstBody* body) {
    AstIfStatement* ifStatement = new AstIfStatement();
    ifStatement->type = IfStatement;
    ifStatement->booleanExpression = booleanExpression;
    ifStatement->ifbody = body;
    ifStatement->ifElses = new vector<AstIfElse*>();
    return ifStatement;
}

AstIfElse* createIfElseFromIf(AstIfStatement* ifStatement) {
    AstIfElse* ifElse = new AstIfElse();
    ifElse->ifbody = ifStatement->ifbody;
    ifElse->booleanExpression = ifStatement->booleanExpression;
    return ifElse;
}

void addElseStatementToIf(AstIfStatement* ifStatement, AstBody* body) {
    ifStatement->elseBody = body;
}

void addElseIfStatementToIf(AstIfStatement* ifStatement, AstIfElse* ifElse) {
    ifStatement->ifElses->push_back(ifElse);
}

void addDeclToFunctionParams(AstFunctionDefinition* functionDef, AstDecleration* decleration) {
    functionDef->paramaters->push_back(decleration);
}

AstInterfaceNode* createAstInterface(string name) {
    AstInterfaceNode* node = new AstInterfaceNode();
    node->name = name;
    node->type = Interface;
    node->functions = new vector<AstFunctionDefinition*>();

    return node;
}

AstBody* createAstBody() {
    AstBody* astBody = new AstBody();
    astBody->statements = new vector<AstNode*>();
    astBody->type = Body;
    return astBody;
}

void addStatementToBody(AstBody* astBody, AstNode* statement) {
    astBody->statements->push_back(statement);
}

void addArgumentToFunctionCall(AstFunctionCall* functionCall, AstNode* arg) {
    functionCall->arguments->push_back(arg);
}

AstFunctionDefinition* createFunctionDefinition() {
    AstFunctionDefinition* functionDefinition = new AstFunctionDefinition();
    functionDefinition->type = FunctionDefinition;
    functionDefinition->body = NULL;
    functionDefinition->paramaters = new vector<AstDecleration*>();
    functionDefinition->returnIsVoid = false;
    return functionDefinition;
}

void addBodyToFunctionDef(AstFunctionDefinition* functionDef, AstBody* body) {
    functionDef->body = body;
}


ArrayDimension* createArrDimension() {
    return new ArrayDimension();
}

void addArrDimensionToArrayDecl(AstArrayDecleration* arrDecl, ArrayDimension* arrDimension) {
    arrDecl->dimensions->push_back(arrDimension);
}

AstForLoop* createInfiniteLoop(AstBody* forLoopBody) {
    auto trueNode = createLitearlValueNodeForBool(true);
    AstBooleanExpression* conditional = createBooleanExpressionNode(trueNode, trueNode, "==");

    AstForLoop* forLoop = new AstForLoop();
    forLoop->type = ForLoop;
    forLoop->assignment = NULL;
    forLoop->conditional = conditional;
    forLoop->body = forLoopBody;
    return forLoop;
}

AstForLoop* createAstForLoop(
    AstAssignementStatement* assignment,
    AstNode* conditional,
    AstAssignementStatement* postCondition,
    AstBody* forLoopBody) {


    AstForLoop* forLoop = new AstForLoop();
    forLoop->type = ForLoop;
    forLoop->assignment = assignment;
    forLoop->conditional = conditional;
    forLoop->postCondition = postCondition;
    forLoop->body = forLoopBody;

    return forLoop;
}

AstClosureCreation* createClosureCreation() {
    AstClosureCreation* closureCreation = new AstClosureCreation();
    closureCreation->type = ClosureCreation;
    return closureCreation;
}

AstFunctionCall* createFunctionCall(string functionName) {
    AstFunctionCall* functionCall = new AstFunctionCall();
    functionCall->name = functionName;
    functionCall->arguments = new vector<AstNode*>();
    functionCall->type = FunctionCall;
    return functionCall;
}

AstClosureCreation* closureCreation() {
    AstClosureCreation* closureCreation = new AstClosureCreation();
    closureCreation->type = ClosureCreation;
    return closureCreation;
}

AstLambaCall* createAstLambdaCall(AstFunctionCall* functionCall, int num_call_stacks_to_walk_up) {
    AstLambaCall* lambdaCall = new AstLambaCall();
    lambdaCall->type = LambdaCall;
    lambdaCall->functionCall = functionCall;
    lambdaCall->call_stacks_to_walk_up = num_call_stacks_to_walk_up;
    return lambdaCall;
}

AstIdentifier* createAstIdentifier(string name) {
    AstIdentifier* identifier = new AstIdentifier();
    identifier->name = name;
    identifier->type = Identifier;
    return identifier;
}

AstReturnStatement* createAstReturnStatement(AstNode* returnExpression) {
    AstReturnStatement* statement = new AstReturnStatement();
    statement->type = Return;
    statement->expression = returnExpression;

    return statement;
}

AstDotAssignment* createDotAssignment(AstDotAccess* dotAccess, AstNode* expression) {
    AstDotAssignment* dotAssignment = new AstDotAssignment();
    dotAssignment->type = DotAssignment;
    dotAssignment->dotAccess = dotAccess;
    dotAssignment->expression = expression;
    return dotAssignment;
}

AstArraySubscriptAssignment* createArraySubscriptAssignment(AstArrayAccess* arrayAccess, AstNode* assignmentExpression) {
    AstArraySubscriptAssignment* subscriptAssignment = new AstArraySubscriptAssignment();
    subscriptAssignment->type = ArraySubscriptAssignment;
    subscriptAssignment->arrAccess = arrayAccess;
    subscriptAssignment->expression = assignmentExpression;
    return subscriptAssignment;
}

AstArrayAccess* createAstArrayAccess() {
    AstArrayAccess* access = new AstArrayAccess();
    access->type = ArrayAccess;
    access->subScripts = new vector<Subscript*>();
    return access;
}

AstScopedIdentifier* createScopedIdentifier(string identifierName, int num_stack_to_walk_up) {

    AstScopedIdentifier* scoped_identifier = new AstScopedIdentifier();
    scoped_identifier->name = identifierName;
    scoped_identifier->call_stack_difference = num_stack_to_walk_up;
    scoped_identifier->type = ScopedIdentifier;
    return scoped_identifier;
}

AstNullNode* createNullNode() {
    return nullNode;
}

AstDefer* createAstDefer(AstFunctionCall* functionCall) {
    AstDefer* defer = new AstDefer();
    defer->type = Defer;
    defer->defferedFunctionCall = functionCall;
    return defer;
}

AstTryStatement* createAstTry(AstFunctionCall* functionCall) {
    AstTryStatement* astTry = new AstTryStatement();
    astTry->type = Try;
    astTry->functionCall = functionCall;
    return astTry;
}

AstThrow* createAstThrow(AstFunctionCall* functionCall) {
    AstThrow* astThrow = new AstThrow();
    astThrow->type = Throw;
    astThrow->functionCall = functionCall;
    return astThrow;
}

AstTupleError* createAstTuple() {
    AstTupleError* tuple = new AstTupleError();
    tuple->type = Tuple;
    tuple->tuple = new vector<string>();
    return tuple;
}

AstMatchNode* createAstMatchNode(string targetMatch) {
    AstMatchNode* match = new AstMatchNode();
    match->type = Match;
    auto identifier = createAstIdentifier(targetMatch);
    match->matchTarget = identifier;
    match->cases = new vector<AstCaseBlock*>();
    return match;
}

AstCaseBlock* createAstCaseBlock() {

    AstCaseBlock* caseBlock = new AstCaseBlock();
    caseBlock->type = Case;
    caseBlock->targets = new vector<string>();
    return caseBlock;
}

AstEnum* createAstEnum(string enumName) {
    AstEnum* enumNode = new AstEnum();
    enumNode->type = Enum;
    enumNode->enumIdentifier = createAstIdentifier(enumName);
    enumNode->enums = new vector<AstIdentifier*>();
    return enumNode;
}

AstTryCatchAssignmentStatement* createTryCatchAssignmentStatement(AstAssignementStatement* statement) {
    AstTryCatchAssignmentStatement* tryStatement = new AstTryCatchAssignmentStatement();
    tryStatement->type = TryCatchAssignment;
    tryStatement->assignment = statement;
    return tryStatement;
}

AstNegativeExpression* createNegativeExpression(AstNode* expression) {
    AstNegativeExpression* negativeExpression = new AstNegativeExpression();
    negativeExpression->type = NegativeExpression;
    negativeExpression->expression = expression;
    return negativeExpression;
}

AstPolyClonedFunctionDefinitition* createPolyClonedFunctionDef(AstFunctionDefinition* src) {
    AstPolyClonedFunctionDefinitition* cloned = new AstPolyClonedFunctionDefinitition();
    cloned->type = PolyClonedFunctionDefinition;
    cloned->functionDef = src;
    return cloned;
}

void addEnumValueToEnum(AstEnum* enumNode, AstIdentifier* enumValue) {
    enumNode->enums->push_back(enumValue);
}

void addTargetToCaseBlock(AstCaseBlock* caseblock, string target) {
    caseblock->targets->push_back(target);
}

void addCaseBlockToMatchNode(AstMatchNode* matchNode, AstCaseBlock* caseBlock) {
    matchNode->cases->push_back(caseBlock);
}

void addTuple(AstTupleError* tuple, string identifier) {
    tuple->tuple->push_back(identifier);
}

void addFunctionCallToTuple(AstTupleError* tuple, AstFunctionCall* functionCall) {
    tuple->rightSide = functionCall;
}

void addFileInfo(TokenStream* tokenStream, AstNode* node, Token* startTokenOfNode) {
    node->line_number = startTokenOfNode->line_number;
    node->column_number = startTokenOfNode->colum_number - startTokenOfNode->literal.size();
    node->file_name = tokenStream->file_name;
}

void addFileInfo(TokenStream* tokenStream, AstNode* node, int line_number, int column_number) {
    node->line_number = line_number;
    node->column_number = column_number;
    node->file_name = tokenStream->file_name;
}

void addFileInfo(AstNode* src, AstNode* target) {
    target->line_number = src->line_number;
    target->column_number = src->column_number;
    target->file_name = src->file_name;
}

void addSubtitutionToNode(AstNode* src, AstNode* sub) {
    src->sub = sub;
}

bool isOptionalTypePresent(AstAssignementStatement* assignment) {
    return assignment->typeDecl != NULL;
}
