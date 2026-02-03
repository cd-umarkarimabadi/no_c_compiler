#include "parser/parser.h"
#include "ast/ast_fun.h"
#include <queue>
#include <stack>
#include <vector>
#include <assert.h>
#include <set>


AstFunctionDefinition* parseFunctionDecleration(TokenStream* tokenStream, bool is_exteranal);
AstFunctionDefinition* parseFunctionDecleration(TokenStream* tokenStream);
AstExternalFunction* parseExternalFunction(TokenStream * tokenStream);
AstStructNode* parseStructDecleration(TokenStream * tokenStream);
AstDecleration* parseDeclerationInStruct(TokenStream * tokenStream, AstStructNode* structNode);
AstForLoop* parseForLoop(TokenStream* tokenStream);
AstFunctionTypeDef* parseFunctionTypeDef(TokenStream* tokenStream);
bool parseForLoopConditional(TokenStream* tokenStream);
bool parseIfStatmenet(TokenStream* tokenStream);
bool tryParseFunctionCall(TokenStream* tokenStream);
AstFunctionCall* parseFunctionCall(TokenStream* tokenStream);
AstBody* parseFunctionBody(TokenStream* tokenStream);
bool tryParseBooleanLiteral(TokenStream* tokenStream);
AstLiteralValue* parseBooleanLiteral(TokenStream* tokenStream);
AstNode* parseBooleanExpression(TokenStream* tokenStream, TokenType stopToken);
AstLiteralValue* parseStringLiteral(TokenStream* tokenStream);
bool tryParseStringLiteral(TokenStream* tokenStream);
bool tryParseNumberLiteral(TokenStream* tokenStream);
AstLiteralValue* parseNumberLiteral(TokenStream* tokenStream);
AstForEachStatment* parseForEach(TokenStream * tokenStream);
bool parseFunctionParams(TokenStream* tokenStream, AstFunctionDefinition* functionDef);
bool parseStructBody(TokenStream * tokenStream, AstStructNode* structNode);
AstIfStatement* parseIfStatement(TokenStream* tokenStream);
AstAssignementStatement * parseAssignment(TokenStream * tokenStream);
AstDecleration* parseDecleration(TokenStream* tokenStream);
bool tryParseAssigment(TokenStream * tokenStream);
AstBody* parseIfBody(TokenStream* tokenStream);
AstBody* parseStatements(TokenStream* tokenStream);
AstInterfaceNode* parseInterface(TokenStream* tokenStream);
bool isStatementExpression(TokenStream* tokenStream);
bool isTokenOperator(Token* token);
bool isTokenLogicalOperator(Token* token);
AstLiteralValue* parseNumberLiteral(TokenStream* tokenStream);
void resolveOperands(TokenStream* tokenStream, stack<AstNode*>* operands, stack<Operator_t>* operators);
AstNode* parseExpression(TokenStream* tokenStream, TokenType stopToken);
AstNode* parseExpression(TokenStream* tokenStream, stack<AstNode*>* operands, stack<Operator_t>* operators, TokenType stopToken);
bool tryParseArrayDecleration(TokenStream* tokenStream);

struct ParseArrayDeclResult {
    AstArrayDecleration* array_decl;
    PolyFlags polyFlags;
};

ParseArrayDeclResult* parseArrayDecleration(TokenStream* tokenStream);
AstArrayAccess* parseArrayAccess(TokenStream* tokenStream);
bool tryParseDotAccess(TokenStream * tokenStream);
AstDotAccess* parseDotAccess(TokenStream * tokenStream);
AstObjectCreation* parseObjectCreation(TokenStream* tokenStream);
AstDefer* parseAstDefer(TokenStream* tokenStream);
AstThrow* parseAstThrow(TokenStream* tokenStream);
bool tryParseTuple(TokenStream* tokenStream);
AstTupleError* parseTuple(TokenStream* tokenStream);
AstTryStatement* parseAstTry(TokenStream* tokenStream);
bool isTokenUnary(Token* token);
string unaryToOperator(Token* token);
AstAssignementStatement* parseUnary(TokenStream* tokenStream);
bool tryParseUnary(TokenStream* tokenStream);
AstDotAssignment* createDotAssignmentFromUnary(AstDotAccess* dotAccess, Token* unaryToken);
AstMatchNode* parseMatchNode(TokenStream* tokenStream);
AstEnum* parseEnum(TokenStream* tokenStream);
AstTypeDecleration* parseTypeDecl(TokenStream* tokenStream);
AstYield* parseAstYield(TokenStream* tokenStream);
AstNoRoutine* parseNoRoutine(TokenStream* tokenStream);


// Bidmas is basically the priority level
// Indexes closer to 0 are higher in value // TODO :: Just fix this 
TokenType OperatorPriotyLevel[] = {
    MOD,
    DIVIDE,
    MULTIPLY,
    ADD,
    SUBTRACT,
    LOGICAL_EQUAL,
    LOGICAL_NOT,
    LOGICAL_NOT_EQUAL,
    LOGICAL_LESS_THAN,
    LOGICAL_LESS_THAN_EQUAL,
    LOGICAL_MORE_THAN,
    LOGICAL_MORE_THAN_EQUAL,
    LOGICAL_AND,
    LOGICAL_OR,
};

bool isTokenTypeIdentifer(Token* token);
void advance(TokenStream* stream, set<TokenType>* stopTokens);
vector<string>* collectPolyArgs(TokenStream* tokenStream);
set<TokenType> blockLevelTokens = { STRUCT, FUNCTION, INTERFACE };

void printParseError(Token * token, string file_name, string message) {
    cout << file_name << ":" << token->line_number << ":" << token->colum_number - token->literal.size() << ":" << message << endl;
}

void printError(string message) {
    cout << message << endl;
    cout << endl;
}

bool readSemiColon(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != SEMI_COLON) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected semi colon");
        return false;
    }
    tokenStream->readToken();
    return true;
}

file_parse_unit* create_file_parse_unit(string file_name) {
    file_parse_unit* unit = new file_parse_unit();
    unit->file_name = file_name;
    unit->imports = new vector<string>();
    unit->is_parsing_error = false;
    return unit;
}

file_parse_unit* parseProgram(TokenStream * tokenStream) {
    AstBody* body = createAstBody();
    file_parse_unit* unit = create_file_parse_unit(tokenStream->file_name);
    unit->program = body;

    // Add base dependancies to every file apart from the string file
    // TODO :: Move this to dependancy creator 
    if (tokenStream->file_name != "src/string.noc") {
        unit->imports->push_back("src/string.noc");
        if (tokenStream->file_name != "src/error.noc") {
            unit->imports->push_back("src/error.noc");
        }
    }

    while (tokenStream->canRead()) {
        Token* token = tokenStream->peakToken();
        Token* startToken = tokenStream->peakToken();
        if (token->tokenType == STRUCT) {
            AstStructNode* structNode = parseStructDecleration(tokenStream);
            if (structNode == NULL) {
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            addFileInfo(tokenStream, structNode, startToken);
            addStatementToBody(body, structNode);
            continue;
        }

        if (token->tokenType == TYPEDEF) {
            AstFunctionTypeDef* functionTypeDef = parseFunctionTypeDef(tokenStream);
            if (functionTypeDef == NULL) {
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            addFileInfo(tokenStream, functionTypeDef, startToken);
            addStatementToBody(body, functionTypeDef);
            continue;
        }

        if (token->tokenType == IMPORT) {
            tokenStream->readToken();
            if (tokenStream->peakToken()->tokenType != STRING_LITERAL_TOKEN) {
                printParseError(tokenStream->peakToken(), tokenStream->file_name, "import should be followed by a string literal");
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            unit->imports->push_back(tokenStream->peakToken()->literal);
            tokenStream->readToken();
            continue;
        }

        if (token->tokenType == FUNCTION) {
            AstFunctionDefinition* functionDef = parseFunctionDecleration(tokenStream);
            if (functionDef == NULL) {
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            addFileInfo(tokenStream, functionDef, startToken);
            addStatementToBody(body, functionDef);
            continue;
        }

        if (token->tokenType == EXTERNAL) {
            AstExternalFunction* externalFunction = parseExternalFunction(tokenStream);
            if (externalFunction == NULL) {
                throw runtime_error("failed to parse external function");
            }
            addStatementToBody(body, externalFunction);
            continue;
        }

        if (token->tokenType == INTERFACE) {
            auto interfaceNode = parseInterface(tokenStream);
            if (interfaceNode == NULL) {
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            addStatementToBody(body, interfaceNode);
            continue;
        }

        if (token->tokenType == ENUM) {
            auto enumNode = parseEnum(tokenStream);
            if (enumNode == NULL) {
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            addFileInfo(tokenStream, enumNode, startToken);
            addStatementToBody(body, enumNode);
            continue;
        }

        bool isAssingment = tryParseAssigment(tokenStream);
        if (isAssingment) {
            AstAssignementStatement* assignement = parseAssignment(tokenStream);
            if (assignement == NULL) {
                unit->is_parsing_error = true;
                advance(tokenStream, &blockLevelTokens);
                continue;
            }
            addFileInfo(tokenStream, assignement, startToken);
            addStatementToBody(body, assignement);
            continue;
        }
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "statement not allowed at global scope level");
        throw runtime_error("grammar not recognized");
    }
    return unit;
}

AstInterfaceNode* parseInterface(TokenStream * tokenStream) {
    printParseError(tokenStream->peakToken(), tokenStream->file_name, "interface parsing not supported");
    return NULL;
}

bool isStatementExpression(TokenStream * tokenStream) {

    if (!tokenStream->canRead()) {
        return false;
    }
    return isTokenOperator(tokenStream->peakToken());
}

bool isTokenLogicalOperator(Token * token) {

    static TokenType logicalTokens[] = { LOGICAL_AND,
                                        LOGICAL_OR,
                                        LOGICAL_EQUAL,
                                        LOGICAL_NOT,
                                        LOGICAL_NOT_EQUAL,
                                        LOGICAL_LESS_THAN,
                                        LOGICAL_LESS_THAN_EQUAL,
                                        LOGICAL_MORE_THAN,
                                        LOGICAL_MORE_THAN_EQUAL
    };

    for (auto logicalToken : logicalTokens) {
        if (token->tokenType == logicalToken) {
            return true;
        }
    }
    return false;
}

bool isTokenOperator(Token * token) {

    if (token->tokenType == ADD) {
        return true;
    }
    if (token->tokenType == SUBTRACT) {
        return true;
    }
    if (token->tokenType == MULTIPLY) {
        return true;
    }
    if (token->tokenType == MOD) {
        return true;
    }
    if (token->tokenType == DIVIDE) {
        return true;
    }
    if (token->tokenType == LOGICAL_AND) {
        return true;
    }
    if (token->tokenType == LOGICAL_OR) {
        return true;
    }
    return false;
}

size_t getPriorityLevelFromOperator(Token * token) {
    // The enum is printed in reverse order 
    for (size_t i = 0; i < LEN(OperatorPriotyLevel); i++) {
        if (OperatorPriotyLevel[i] == token->tokenType) {
            return LEN(OperatorPriotyLevel) - i;
        }
    }
    throw runtime_error("Token is not an operator :: this sould not happen");
}

AstNode* createNodeFromOperands(AstNode * firstOperand, AstNode * secondOperand, Operator_t op) {
    // TDODO :: Fix later to get the operator I can just flatten this
    if (op.operatorStr == "+" || op.operatorStr == "-" || op.operatorStr == "*" || op.operatorStr == "/" || op.operatorStr == "%") {
        return createAithemticExpressionNode(firstOperand, secondOperand, op.operatorStr);
    }

    return createBooleanExpressionNode(firstOperand, secondOperand, op.operatorStr);;
}

AstNode* parseExpression(TokenStream * tokenStream, TokenType stopToken) {

    stack<AstNode*> operands = stack<AstNode*>();
    stack<Operator_t> operators = stack<Operator_t>();

    AstNode* expression = parseExpression(tokenStream, &operands, &operators, stopToken);
    return expression;
}

// TOOD :: Need to have a way of differentiating array access from array decleration
bool tryParseArrayAccess(TokenStream * tokenStream) {
    return tryParseArrayDecleration(tokenStream);
}

bool tryParseArrayDecleration(TokenStream * tokenStream) {

    Token* peaked = tokenStream->peakToken();
    if (peaked->tokenType == IDENTIFER || peaked->isTokenTypePrimitve) {
        if (tokenStream->peakNextToken()->tokenType == OPEN_ARR_BRACE) {
            return true;
        }
        return false;
    }

    return false;
}

bool tryParseCharLiteral(TokenStream * tokenStream) {
    if (!tokenStream->canRead()) {
        return false;
    }
    if (tokenStream->peakToken()->tokenType == CHAR_LITERAL_TOKEN) {
        return true;
    }
    return false;
}

AstLiteralValue* parseCharLiteral(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != CHAR_LITERAL_TOKEN) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected char token literal");
        return NULL;
    }
    Token* charLiteralToken = tokenStream->peakToken();
    auto charLiteralNode = createLiterallValueNodeForChar(charLiteralToken->literal);
    tokenStream->readToken();
    addFileInfo(tokenStream, charLiteralNode, charLiteralToken);
    return charLiteralNode;
}

AstNode* parseExpression(TokenStream * tokenStream, stack<AstNode*>*operands, stack<Operator_t>*operators, TokenType stopToken) {

    size_t priotity_depth = LEN(OperatorPriotyLevel);
    size_t priority_scope_level = 0;

    while (true) {

        while (tokenStream->peakToken()->tokenType == OPEN_ROUND_BRACKET) {
            priority_scope_level += priotity_depth;
            tokenStream->readToken();
        }

        Token* startTokenOfResolvedNode = tokenStream->peakToken();
        AstNode* resolved_node = NULL;
        bool negativeExpression = false;
        bool negateExpression = false;

        if (tokenStream->peakToken()->tokenType == SUBTRACT) {
            tokenStream->readToken();
            negativeExpression = true;
        }
        else {
            if (tokenStream->peakToken()->tokenType == LOGICAL_NOT) {
                tokenStream->readToken();
                negateExpression = true;
            }
        }

        if (resolved_node == NULL) {
            if (tryParseFunctionCall(tokenStream)) {
                resolved_node = parseFunctionCall(tokenStream);
                if (resolved_node == NULL) {
                    printError("Error in expression, nothing can be resolved");
                    return NULL;
                }
            }
        }

        if (resolved_node == NULL) {
            if (tryParseArrayAccess(tokenStream)) {
                resolved_node = parseArrayAccess(tokenStream);

                if (resolved_node == NULL) {
                    printError("Error in array decleration, nothing can be resolved");
                    return NULL;
                }
            }
        }

        if (resolved_node == NULL) {
            bool isboolean = tryParseBooleanLiteral(tokenStream);

            if (isboolean) {
                resolved_node = parseBooleanLiteral(tokenStream);
            }
        }

        if (resolved_node == NULL) {
            bool isString = tryParseStringLiteral(tokenStream);

            if (isString) {
                resolved_node = parseStringLiteral(tokenStream);
            }
        }

        if (resolved_node == NULL) {
            bool isNumberLiteral = tryParseNumberLiteral(tokenStream);

            if (isNumberLiteral) {
                resolved_node = parseNumberLiteral(tokenStream);
            }
        }

        if (resolved_node == NULL) {
            if (tryParseCharLiteral(tokenStream)) {
                resolved_node = parseCharLiteral(tokenStream);
            }
        }

        if (resolved_node == NULL) {
            if (tokenStream->peakToken()->tokenType == NULLZ) {
                resolved_node = createNullNode();
                tokenStream->readToken();
            }
        }

        // At this point it has to be an identifer or could be a strut accessor
        if (resolved_node == NULL) {

            if (tryParseDotAccess(tokenStream)) {
                resolved_node = parseDotAccess(tokenStream);
            }

            else {
                if (tokenStream->peakToken()->tokenType == IDENTIFER) {
                    auto identiferToken = tokenStream->peakToken();
                    resolved_node = createAstIdentifier(identiferToken->literal);
                    tokenStream->readToken();
                }
            }
        }

        if (resolved_node == NULL) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "bad expression");
            return NULL;
        }

        if (negativeExpression) {
            resolved_node = createNegativeExpression(resolved_node);
        }
        else {
            if (negateExpression) {
                resolved_node = createBooleanExpressionNode(resolved_node, createLitearlValueNodeForBool(false), "==");;
            }
        }

        addFileInfo(tokenStream, resolved_node, startTokenOfResolvedNode);
        operands->push(resolved_node);

        if (tokenStream->peakToken()->tokenType == stopToken && priority_scope_level == 0) {
            break; // resolve_operands
        }

        bool resolve_operands_now = false;
        while (tokenStream->peakToken()->tokenType == CLOSE_ROUND_BRACKET) {
            if (priority_scope_level <= 0) {
                resolve_operands_now = true;
                break; // resolve operands now and let the calling context handle parsing errors 
            }

            tokenStream->readToken();

            priority_scope_level -= priotity_depth;
            // TODO :: This will break the expression ((1)) because there are not opernads 
            if (operands->empty()) {
                printParseError(tokenStream->peakToken(), tokenStream->file_name, "bad expression");
                return NULL;
            }
            else {
                // resolve the current expression as we are closing the expression
                if (operands->size() > 1) {
                    resolveOperands(tokenStream, operands, operators);
                }
            }
        }

        if (resolve_operands_now) {
            break;
        }

        Token* token = tokenStream->peakToken();
        if (!isTokenLogicalOperator(token) && !isTokenOperator(token)) {
            if (token->tokenType == stopToken) {
                if (priority_scope_level != 0) {
                    printParseError(tokenStream->peakToken(), tokenStream->file_name, "bad expression ) brace has not been closed");
                    return NULL;
                }
                break; // Resolve operands now 
            }
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "bad expression");
            return NULL;
        }

        tokenStream->readToken();

        Operator_t currentOperator = {};
        currentOperator.priotyLevel = getPriorityLevelFromOperator(token) + priority_scope_level;
        currentOperator.operatorStr = tokenToString(token->tokenType);
        currentOperator.scope_group = priority_scope_level;

        // Resolve if at least two operands are on the stack 
        if (operands->size() > 1) {
            Operator_t operator_top = operators->top();
            if (operator_top.priotyLevel >= currentOperator.priotyLevel) {

                operators->pop();
                // TODO :: Merge with operator code, search @link::operatorMerge
                AstNode* first_operand = operands->top();
                operands->pop();
                AstNode*  second_operand = operands->top();
                operands->pop();

                AstNode* node = createNodeFromOperands(second_operand, first_operand, operator_top);
                addFileInfo(tokenStream, node, first_operand->line_number, first_operand->column_number);
                operands->push(node);
            }
        }
        operators->push(currentOperator);
    }
    // Collapse and create ast node 
    resolveOperands(tokenStream, operands, operators);
    AstNode* node = operands->top();
    return node;
}

AstNode* parseBooleanExpressionInForLoop(TokenStream * tokenStream) {
    return parseBooleanExpression(tokenStream, SEMI_COLON);
}

AstNode* parseBooleanExpressionInIf(TokenStream * tokenStream) {
    return parseBooleanExpression(tokenStream, CLOSE_ROUND_BRACKET);
}

AstNode* parseBooleanExpression(TokenStream * tokenStream, TokenType stopToken) {
    return parseExpression(tokenStream, stopToken);
}

void resolveOperands(TokenStream* tokenStream, stack<AstNode*>*operands,
    stack<Operator_t>*operators) {

    if (operands->size() == 1) {
        return;
    }

    size_t currentScopeGroup = operators->top().scope_group;
    while (operators->size() != 0) {
        Operator_t operator_top = operators->top();
        if (operator_top.scope_group != currentScopeGroup) {
            break;
        }

        // TODO :: Merge with operator code, search @link::operatorMerge
        operators->pop();
        AstNode* first_operand = operands->top();
        operands->pop();
        AstNode*  second_operand = operands->top();
        operands->pop();
        AstNode* node = createNodeFromOperands(second_operand, first_operand, operator_top);
        addFileInfo(tokenStream, node, first_operand->line_number, first_operand->column_number);
        operands->push(node);
    }
}

AstLiteralValue* parseNumberLiteral(TokenStream * tokenStream) {
    Token* token = tokenStream->peakToken();
    if (tokenStream->peakToken()->tokenType == NUMBER_LITERAL) {
        AstLiteralValue* node = createLitearlValueNodeForInt(ascii_to_int(token->literal));
        tokenStream->readToken();
        addFileInfo(tokenStream, node, token);
        return node;
    }

    else if (tokenStream->peakToken()->tokenType == REAL_LITERAL) {
        AstLiteralValue* node = createLiterallValueNodeForDouble(ascii_to_double(token->literal));
        tokenStream->readToken();
        addFileInfo(tokenStream, node, token);
        return node;
    }
    printParseError(token, tokenStream->file_name, "unrecognized number litereal");
    return NULL;
}

bool tryParseNumberLiteral(TokenStream * tokenStream) {
    if (!tokenStream->canRead()) {
        return false;
    }
    if (tokenStream->peakToken()->tokenType == NUMBER_LITERAL || tokenStream->peakToken()->tokenType == REAL_LITERAL) {
        return true;
    }
    return false;
}

bool tryParseBooleanLiteral(TokenStream * tokenStream) {

    if (!tokenStream->canRead()) {
        return false;
    }
    Token* token = tokenStream->peakToken();
    if (token->tokenType == TRUE) {
        return true;
    }
    if (token->tokenType == FALSE) {
        return true;
    }

    return false;
}

bool tryParseStringLiteral(TokenStream * tokenStream) {

    if (!tokenStream->canRead()) {
        return false;
    }
    Token* token = tokenStream->peakToken();
    if (token->tokenType == STRING_LITERAL_TOKEN) {
        return true;
    }

    return false;
}

AstLiteralValue* parseBooleanLiteral(TokenStream * tokenStream) {

    Token* token = tokenStream->peakToken();
    tokenStream->readToken();
    if (token->tokenType == TRUE) {
        return createLitearlValueNodeForBool(true);
    }

    if (token->tokenType == FALSE) {
        return createLitearlValueNodeForBool(false);
    }

    throw runtime_error("unrecongized token type for ParseBooleanLiteralUnsafe");
}

AstLiteralValue* parseStringLiteral(TokenStream * tokenStream) {

    Token* strToken = tokenStream->peakToken();

    if (strToken->tokenType == STRING_LITERAL_TOKEN) {
        tokenStream->readToken();
        auto stringNode = createLitearlValueNodeForString(strToken->literal);
        addFileInfo(tokenStream, stringNode, strToken);
        return stringNode;
    }

    throw runtime_error("unrecongized token type for ParseBooleanLiteralUnsafe");
}

AstBody* parseIfBody(TokenStream * tokenStream) {
    AstBody* ifBody = parseStatements(tokenStream);
    if (ifBody == NULL) {
        return NULL;
    }
    return ifBody;
}

AstBody* parseStatements(TokenStream * tokenStream) {

    AstBody* body = createAstBody();
    while (tokenStream->canRead()) {

        Token* token = tokenStream->peakToken();
        // We know we should close parsing the statement since
        // I do not like the fact this the break is here but I have no other way of determining when i should exit 
        if (token->tokenType == CLOSE_CURLY_BRACKET) {
            return body;
        }

        if (token->tokenType == YIELD) {
            AstYield* astYield = parseAstYield(tokenStream);
            if (astYield == NULL) {
                return  NULL;
            }
            addStatementToBody(body, astYield);
            continue;
        }

        if (token->tokenType == DEFER) {
            AstDefer* defer = parseAstDefer(tokenStream);
            if (defer == NULL) {
                return  NULL;
            }
            addStatementToBody(body, defer);
            continue;
        }

        bool isTuple = tryParseTuple(tokenStream);
        if (isTuple) {
            AstTupleError* tuple = parseTuple(tokenStream);

            if (tuple == NULL) {
                return NULL;
            }
            addStatementToBody(body, tuple);
            continue;
        }

        if (token->tokenType == THROW) {
            AstThrow* astThrow = parseAstThrow(tokenStream);
            if (astThrow == NULL) {
                return  NULL;
            }
            addStatementToBody(body, astThrow);
            continue;
        }

        if (token->tokenType == FUNCTION) {
            AstFunctionDefinition* functionDef = parseFunctionDecleration(tokenStream);
            if (functionDef == NULL) {
                return  NULL;
            }
            addStatementToBody(body, functionDef);
            continue;
        }

        if (token->tokenType == TRY) {
            AstTryStatement* astTry = parseAstTry(tokenStream);
            if (astTry == NULL) {
                return  NULL;
            }
            addStatementToBody(body, astTry);
            continue;
        }

        if (token->tokenType == FOR) {
            AstForLoop* forLoop = parseForLoop(tokenStream);
            if (forLoop == NULL) {
                return NULL;
            }
            addStatementToBody(body, forLoop);
            continue;
        }

        if (token->tokenType == NO) {
            AstNoRoutine* coRoutine = parseNoRoutine(tokenStream);
            if (coRoutine == NULL) {
                return NULL;
            }
            addStatementToBody(body, coRoutine);
            continue;
        }

        if (token->tokenType == MATCH) {

            AstMatchNode* matchNode = parseMatchNode(tokenStream);
            if (matchNode == NULL) {
                return NULL;
            }
            addStatementToBody(body, matchNode);
            continue;
        }

        if (token->tokenType == FOR_EACH) {
            AstForEachStatment* forEach = parseForEach(tokenStream);
            if (forEach == NULL) {
                return NULL;
            }

            addStatementToBody(body, forEach);
            continue;
        }

        if (token->tokenType == IF) {
            Token* startToken = tokenStream->peakToken();
            AstIfStatement* ifStatement = parseIfStatement(tokenStream);
            if (ifStatement == NULL) {
                return NULL;
            }

            addFileInfo(tokenStream, ifStatement, startToken);
            addStatementToBody(body, ifStatement);
            continue;
        }

        if (token->tokenType == RETURN) {
            Token* startToken = tokenStream->peakToken();
            tokenStream->readToken();
            if (tokenStream->peakToken()->tokenType == SEMI_COLON) {
                auto retunStatement = createAstReturnStatement(NULL);
                addFileInfo(tokenStream, retunStatement, startToken);
                addStatementToBody(body, retunStatement);
                tokenStream->readToken();
                continue;
            }

            auto returnExpression = parseExpression(tokenStream, SEMI_COLON);
            if (returnExpression == NULL) {
                return NULL;
            }

            bool read_semi = readSemiColon(tokenStream);
            if (!read_semi) {
                return NULL;
            }

            auto retunStatement = createAstReturnStatement(returnExpression);
            addFileInfo(tokenStream, retunStatement, startToken);
            addStatementToBody(body, retunStatement);
            continue;
        }

        if (token->tokenType == BREAK) {
            Token* start = tokenStream->peakToken();
            tokenStream->readToken();// READ BREAK
            bool read = readSemiColon(tokenStream);
            if (!read) {
                return NULL;
            }
            AstBreakStatement* breakSatement = createBreakStatement();
            addStatementToBody(body, breakSatement);
            addFileInfo(tokenStream, breakSatement, start);
            continue;
        }

        if (token->tokenType == CONTINUE) {
            Token* start = tokenStream->peakToken();
            tokenStream->readToken();// READ CONTIONIE
            bool read = readSemiColon(tokenStream);
            if (!read) {
                return NULL;
            }
            AstContinueStatement* continueStatement = createAstContinueStatement();
            addStatementToBody(body, continueStatement);
            addFileInfo(tokenStream, continueStatement, start);
            continue;
        }

        bool isUnary = tryParseUnary(tokenStream);
        if (isUnary) {
            AstAssignementStatement* assignment = parseUnary(tokenStream);
            if (assignment == NULL) {
                return NULL;
            }
            bool readSemi = readSemiColon(tokenStream);
            if (readSemi == false) {
                return NULL;
            }
            addStatementToBody(body, assignment);
            continue;
        }

        bool isAssingment = tryParseAssigment(tokenStream);

        if (isAssingment) {
            AstAssignementStatement* assignment = parseAssignment(tokenStream);

            if (assignment == NULL) {
                return NULL;
            }

            addStatementToBody(body, assignment);
            continue;
        }

        bool isFunctionCallParsed = tryParseFunctionCall(tokenStream);
        if (isFunctionCallParsed) {

            AstFunctionCall* functionCall = parseFunctionCall(tokenStream);

            if (functionCall == NULL) {
                return NULL;
            }

            bool readSemi = readSemiColon(tokenStream);
            if (readSemi == false) {
                return NULL;
            }
            addStatementToBody(body, functionCall);
            continue;
        }

        if (tryParseArrayAccess(tokenStream)) {

            Token* startOfArrAccess = tokenStream->peakToken();
            auto arrayAccessNode = parseArrayAccess(tokenStream);
            if (arrayAccessNode == NULL) {
                return NULL;
            }
            addFileInfo(tokenStream, arrayAccessNode, startOfArrAccess);
            if (tokenStream->peakToken()->tokenType != EQUAL) {
                return NULL;
            }

            tokenStream->readToken();
            auto assignmentExpression = parseExpression(tokenStream, SEMI_COLON);

            if (assignmentExpression == NULL) {
                return NULL;
            }

            bool read_semi = readSemiColon(tokenStream);
            if (!read_semi) {
                return NULL;
            }
            auto arrySubscriptAssignment = createArraySubscriptAssignment(arrayAccessNode, assignmentExpression);
            addFileInfo(tokenStream, arrySubscriptAssignment, startOfArrAccess);
            addStatementToBody(body, arrySubscriptAssignment);
            continue;
        }

        if (tryParseDotAccess(tokenStream)) {

            Token* startTokenOfDotAssignment = tokenStream->peakToken();
            auto dotAccess = parseDotAccess(tokenStream);
            if (dotAccess == NULL) {
                return NULL;
            }

            addFileInfo(tokenStream, dotAccess, startTokenOfDotAssignment);

            if (isTokenUnary(tokenStream->peakToken())) {
                auto dotAssignment = createDotAssignmentFromUnary(dotAccess, tokenStream->peakToken());
                tokenStream->readToken(); // read unary
                bool readSemi = readSemiColon(tokenStream);
                if (!readSemi) {
                    printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected ;");
                    return NULL;
                }

                addStatementToBody(body, dotAssignment);
                continue;
            }
            else {
                if (tokenStream->peakToken()->tokenType != EQUAL) {
                    printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected = in dot assignment");
                    return NULL;
                }
            }
            tokenStream->readToken(); // read eqal
            AstNode* assignmentExpression = NULL;

            if (tokenStream->peakToken()->tokenType == NEW) {
                assignmentExpression = parseObjectCreation(tokenStream);
            }
            else {
                assignmentExpression = parseExpression(tokenStream, SEMI_COLON);
            }

            bool read_semi = readSemiColon(tokenStream);
            if (!read_semi) {
                return NULL;
            }

            if (assignmentExpression == NULL) {
                return NULL;
            }

            auto dotAssignment = createDotAssignment(dotAccess, assignmentExpression);
            addFileInfo(tokenStream, dotAssignment, startTokenOfDotAssignment);
            addStatementToBody(body, dotAssignment);
            continue;
        }

        if (tokenStream->peakToken()->tokenType == SEMI_COLON) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "There is a trailing semi colon that has not been closed");
        }
        else {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "bad statement, statmenet cannot start with this token");
        }

        return NULL;
    }
    // TODO :: If we drop out the loop then this must mean that we could not match anything
    assert(body->statements->size() == 0);
    return body;
}

AstTryStatement* parseAstTry(TokenStream * tokenStream) {
    Token* starToken = tokenStream->peakToken();
    if (tokenStream->peakToken()->tokenType != TRY) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected try");
        return NULL;
    }
    tokenStream->readToken();
    AstFunctionCall* functionCall = parseFunctionCall(tokenStream);
    if (functionCall == NULL) {
        return NULL;
    }
    bool read_semi_colon = readSemiColon(tokenStream);
    if (!read_semi_colon) {
        return NULL;
    }
    auto astTry = createAstTry(functionCall);
    addFileInfo(tokenStream, astTry, starToken);
    return astTry;
}

AstThrow* parseAstThrow(TokenStream * tokenStream) {

    Token* startToken = tokenStream->peakToken();
    if (tokenStream->peakToken()->tokenType != THROW) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected throw");
        return NULL;
    }
    tokenStream->readToken();

    AstFunctionCall* functionCall = parseFunctionCall(tokenStream);
    if (functionCall == NULL) {
        return NULL;
    }
    bool read_semi_colon = readSemiColon(tokenStream);
    if (!read_semi_colon) {
        return NULL;
    }

    auto astThrow = createAstThrow(functionCall);
    addFileInfo(tokenStream, astThrow, startToken);
    return astThrow;
}

AstYield* parseAstYield(TokenStream* tokenStream) {
    if (tokenStream->peakToken()->tokenType != YIELD) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected yeild");
        return NULL;
    }
    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected ( for yield");
        return NULL;
    }
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected ) for yeild");
        return NULL;
    }
    tokenStream->readToken();

    bool read_semi_colon = readSemiColon(tokenStream);
    if (!read_semi_colon) {
        return NULL;
    }
    auto astYield = createAstYield();
    return astYield;
}

AstDefer* parseAstDefer(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != DEFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected defer");
        return NULL;
    }
    tokenStream->readToken();

    AstFunctionCall* functionCall = parseFunctionCall(tokenStream);
    if (functionCall == NULL) {
        return NULL;
    }
    bool read_semi_colon = readSemiColon(tokenStream);
    if (!read_semi_colon) {
        return NULL;
    }
    return createAstDefer(functionCall);
}

bool parseElseIf(TokenStream * tokenStream, AstIfElse * ifElse) {

    if (tokenStream->peakToken()->tokenType != ELSE) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected else");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != IF) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected if");
        return NULL;
    }

    tokenStream->readToken();


    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected (");
        return NULL;
    }

    tokenStream->readToken();

    AstNode * booleanExpression = parseBooleanExpressionInIf(tokenStream);
    bool isboolExpressionValid = booleanExpression != NULL;
    if (!isboolExpressionValid) {
        return false;
    }
    ifElse->booleanExpression = booleanExpression;

    if (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected )");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected {");
        return NULL;
    }

    tokenStream->readToken();
    AstBody* ifBody = parseIfBody(tokenStream);

    if (ifBody == NULL) {
        return false;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
        return NULL;
    }

    tokenStream->readToken();
    ifElse->ifbody = ifBody;
    return true;
}

bool tryPeakElseIf(TokenStream * tokenStream) {

    if (!tokenStream->canPeakNextToken()) {
        return false;
    }

    Token* peaked = tokenStream->peakToken();
    if (peaked->tokenType == ELSE) {
        if (tokenStream->peakNextToken()->tokenType == IF) {
            return true;
        }
        return false;
    }

    return false;

}

bool parseElse(TokenStream * tokenStream, AstIfStatement * ifStatement) {

    if (tokenStream->peakToken()->tokenType == ELSE) {

        tokenStream->readToken();

        if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected defer");
            return false;
        }

        tokenStream->readToken();
        AstBody* body = parseStatements(tokenStream);

        if (body == NULL) {
            return false;
        }

        if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
            return false;
        }

        tokenStream->readToken();
        addElseStatementToIf(ifStatement, body);
    }

    return true;
}

// TODO :: Handle error from parsing, 
// TODO :: The ast if else can come from the parsing rather than outside to keep everything consitent
void parseElseIfStatement(TokenStream * tokenStream, AstIfStatement * ifStatement) {

    while (tryPeakElseIf(tokenStream)) {
        AstIfElse* ifElse = new AstIfElse();
        bool result = parseElseIf(tokenStream, ifElse);

        if (!result) {
            throw runtime_error("parse else if failed");
        }
        addElseIfStatementToIf(ifStatement, ifElse);
    }
}

AstIfStatement* parseIfStatement(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != IF) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected if");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "if should be followed by (");
        return NULL;
    }

    tokenStream->readToken();

    AstNode * booleanExpression = parseBooleanExpressionInIf(tokenStream);
    bool isboolExpressionValid = booleanExpression != NULL;

    if (!isboolExpressionValid) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "if should be closed with )");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "if body should be opened with {");
        return NULL;
    }

    tokenStream->readToken();

    // no body in the if statement
    if (tokenStream->peakToken()->tokenType == CLOSE_CURLY_BRACKET) {
        tokenStream->readToken();
        AstIfStatement* ifStatement = createAstIfStatement(booleanExpression, NULL);

        parseElseIfStatement(tokenStream, ifStatement);

        bool elseParsed = parseElse(tokenStream, ifStatement);

        if (!elseParsed) {
            return NULL;
        }

        return ifStatement;
    }

    AstBody* body = parseIfBody(tokenStream);

    if (body == NULL) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
        return NULL;
    }

    tokenStream->readToken();

    AstIfStatement* ifStatement = createAstIfStatement(booleanExpression, body);

    parseElseIfStatement(tokenStream, ifStatement);

    // Parse final else statement
    bool elseParsed = parseElse(tokenStream, ifStatement);

    if (!elseParsed) {
        return NULL;
    }

    // TODO need to find a way to change this case and suport literal boolean expressions
    // For now I will just hardcode it 
    // Does this comment even make sense?? I think I already handle this
    return ifStatement;
}

AstBody* parseFunctionBody(TokenStream * tokenStream) {

    static auto emptyBody = createAstBody();
    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected { brace");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType == CLOSE_CURLY_BRACKET) {
        tokenStream->readToken();
        return emptyBody;
    }

    AstBody* body = parseStatements(tokenStream);

    if (body == NULL) {
        return NULL;
    }
    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected } brace");
        return NULL;
    }

    tokenStream->readToken();
    return body;
}

bool tryParseFunctionCall(TokenStream * tokenStream) {

    if (!tokenStream->canPeakNextToken()) {
        return false;
    }

    Token* peaked = tokenStream->peakToken();
    if (peaked->tokenType == IDENTIFER) {
        if (tokenStream->peakNextToken()->tokenType == OPEN_ROUND_BRACKET) {
            return true;
        }
        return false;
    }

    return false;
}

AstAnonFunction* parseAnonFunction(TokenStream* tokenStream) {

    AstAnonFunction* anon = createAnonFunction();
    if (tokenStream->peakToken()->tokenType != FUNCTION) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected string identifer for function name");
        return NULL;
    }
    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected string identifer for function name");
        return NULL;
    }
    tokenStream->readToken();

    AstFunctionDefinition* functionDef = createFunctionDefinition();
    bool parsed = parseFunctionParams(tokenStream, functionDef);
    if (!parsed) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        auto returnType = parseTypeDecl(tokenStream);
        if (returnType == NULL) {
            return NULL;
        }
        functionDef->returnTypeDecl = returnType;
        functionDef->returnIsVoid = false;
    }
    else {
        functionDef->returnIsVoid = true;
    }

    if (tokenStream->peakToken()->tokenType == TokenType::COMMA) {
        tokenStream->readToken();
        if (tokenStream->peakToken()->tokenType != TokenType::THROWS) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected throws ");
        }
        tokenStream->readToken();
        functionDef->throwsError = true;
    }

    AstBody* body = parseFunctionBody(tokenStream);
    if (body == NULL) {
        return NULL;
    }
    functionDef->body = body;
    anon->functionDef = functionDef;
    return anon;
}

AstFunctionCall* parseFunctionCall(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected string identifer for function name");
        return NULL;
    }

    Token* startOfFunctionCall = tokenStream->peakToken();
    AstFunctionCall* functionCall = createFunctionCall(tokenStream->peakToken()->literal);
    addFileInfo(tokenStream, functionCall, startOfFunctionCall);
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected ( to open function call");
        return NULL;
    }

    tokenStream->readToken();
    // No args end of function call 
    if (tokenStream->peakToken()->tokenType == CLOSE_ROUND_BRACKET) {
        tokenStream->readToken();
        return functionCall;
    }

    while (tokenStream->canRead()) {

        Token* startOfArg = tokenStream->peakToken();

        AstNode* argument = NULL;
        if (tokenStream->peakToken()->tokenType == TokenType::FUNCTION) {
            argument = parseAnonFunction(tokenStream);
        }
        else {
            argument = parseExpression(tokenStream, COMMA);
        }

        if (argument == NULL) {
            return NULL;
        }

        addFileInfo(tokenStream, argument, startOfArg);
        addArgumentToFunctionCall(functionCall, argument);

        // End of function call 
        if (tokenStream->peakToken()->tokenType == CLOSE_ROUND_BRACKET) {
            tokenStream->readToken();
            break;
        }

        // Not end of function call and continue reading arguments
        if (tokenStream->peakToken()->tokenType != COMMA) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected comma to seperate function args");
            return NULL;
        }
        tokenStream->readToken();
    }
    return functionCall;
}

vector<ArrayDimension*>* resolve_arr_dimensions(TokenStream* tokenStream) {

    bool resolved_dimension = false;
    vector<ArrayDimension*>* dimensions = new vector<ArrayDimension*>();

    while (!resolved_dimension) {
        if (tokenStream->peakToken()->tokenType != OPEN_ARR_BRACE) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "open brace expected for array declertation");
            return NULL;
        }
        ArrayDimension* arrDimension = createArrDimension();
        tokenStream->readToken();

        if (tokenStream->peakToken()->tokenType != CLOSE_ARR_BRACE) {
            AstNode* expression = parseExpression(tokenStream, CLOSE_ARR_BRACE);
            if (expression == NULL) {
                return NULL;
            }
            // Not sure if I have to make it known that the size is known here, wont be able to
            // more complex evaluation this early on
            if (expression->type == Literal) {
                AstLiteralValue* literalNode = (AstLiteralValue*)expression;
                if (literalNode->valueType == NumberLiteral) {
                    arrDimension->isSizeKnown = true;
                    arrDimension->size = literalNode->literalValue->intValue;
                }
            }
            arrDimension->dimensionExpression = expression;
        }

        if (tokenStream->peakToken()->tokenType != CLOSE_ARR_BRACE) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "close brace expected for array decleration");
            return NULL;
        }
        tokenStream->readToken(); // Read the closing brace ]

        dimensions->push_back(arrDimension);
        if (!tokenStream->canRead()) {
            resolved_dimension = true;
        }

        if (tokenStream->peakToken()->tokenType == OPEN_ARR_BRACE) {
            continue;
        }
        resolved_dimension = true;
    }
    return dimensions;
}


AstArrayAccess* parseArrayAccess(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != TokenType::IDENTIFER) {
        return NULL;
    }
    auto identifier = tokenStream->peakToken();
    auto astArrAccess = createAstArrayAccess();
    astArrAccess->identifier = createAstIdentifier(identifier->literal);

    tokenStream->readToken();
    auto dimensions = resolve_arr_dimensions(tokenStream);
    if (dimensions == NULL) {
        return NULL;
    }
    for (auto dimension : *dimensions) {
        Subscript* subscipt = new Subscript();
        subscipt->expression = dimension->dimensionExpression;
        astArrAccess->subScripts->push_back(subscipt);
    }
    delete dimensions;
    return astArrAccess;
}

ParseArrayDeclResult* parseArrayDecleration(TokenStream * tokenStream) {
    Token* startToken = tokenStream->peakToken();
    Token* typeToken = NULL;

    if (!isTokenTypeIdentifer(tokenStream->peakToken())) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "type identifer expected");
        return NULL;
    }

    auto array_decl_result = new ParseArrayDeclResult();

    typeToken = tokenStream->peakToken();
    PolyFlags polyFlag = NoPoly;

    if (isIdentifierPoly(typeToken->literal)) {
        polyFlag = SinglePoly;
    }

    tokenStream->readToken();
    auto arrDecleration = createAstArrayDecleration(typeToken->literal);
    auto dimensions = resolve_arr_dimensions(tokenStream);
    if (dimensions == NULL) {
        return NULL;
    }
    for (auto dimension : *dimensions) {
        addArrDimensionToArrayDecl(arrDecleration, dimension);
    }
    delete dimensions;

    if (polyFlag & SinglePoly) {
        polyFlag |= PolyArray;
    }

    addFileInfo(tokenStream, arrDecleration, startToken);
    array_decl_result->array_decl = arrDecleration;
    array_decl_result->polyFlags = polyFlag;
    return array_decl_result;
}

// NOTE :: There is no semi colon that I read at the end of it
AstObjectCreation* parseObjectCreation(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != NEW) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected new");
        return NULL;
    }

    tokenStream->readToken();
    auto typeDecl = parseTypeDecl(tokenStream);
    if (typeDecl == NULL) {
        return NULL;
    }

    if (!typeDecl->isArray) {

        if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected (");
            return NULL;
        }
        tokenStream->readToken();
        if (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected )");
            return NULL;
        }
        tokenStream->readToken();
    }
    return createAstObjectCreation(typeDecl);
}

AstAssignementStatement* parseAssignment(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected identifer");
        return NULL;
    }

    Token* startTokenOfAssignment = tokenStream->peakToken();
    Token* identiferToken = tokenStream->peakToken();
    tokenStream->readToken();
    AstTypeDecleration* optionalTypeDecl = NULL;
    if (tokenStream->peakToken()->tokenType == COLON_COLON) {
        tokenStream->readToken();
        optionalTypeDecl = parseTypeDecl(tokenStream);
        if (optionalTypeDecl == NULL) {
            return NULL;
        }
    }

    if (tokenStream->peakToken()->tokenType != EQUAL) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected equal in assignement");
        return NULL;
    }

    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType == TRY) {
        auto tryStatment = parseAstTry(tokenStream);
        if (tryStatment == NULL) {
            return NULL;
        }

        auto assignment = createAstAssignment(identiferToken->literal, tryStatment, optionalTypeDecl);
        addFileInfo(tokenStream, assignment, startTokenOfAssignment);
        return assignment;
    }

    if (tokenStream->peakToken()->tokenType == NO) {
        AstNoRoutine* noRoutine = parseNoRoutine(tokenStream);
        if (noRoutine == NULL) {
            return NULL;
        }
        auto assignment = createAstAssignment(identiferToken->literal, noRoutine, optionalTypeDecl);
        addFileInfo(tokenStream, assignment, startTokenOfAssignment);
        return assignment;
    }

    if (tokenStream->peakToken()->tokenType == NEW) {

        AstAssignementStatement* assignment = NULL;
        auto objectCreation = parseObjectCreation(tokenStream);
        if (objectCreation == NULL) {
            return NULL;
        }

        bool readSemi = readSemiColon(tokenStream);
        if (!readSemi) {
            return NULL;
        }
        assignment = createAstAssignmentForObject(identiferToken->literal, objectCreation, optionalTypeDecl);
        return assignment;
    }

    AstNode* expression = parseExpression(tokenStream, SEMI_COLON);
    if (expression == NULL) {
        return NULL;
    }

    bool read = readSemiColon(tokenStream);
    if (!read) {
        return NULL;
    }
    auto assignment = createAstAssignment(identiferToken->literal, expression, optionalTypeDecl);
    addFileInfo(tokenStream, assignment, startTokenOfAssignment);
    return assignment;
}

bool matchTokens(TokenStream* tokenStream, TokenType* expectedTokens, size_t len) {
    for (size_t i = 0; i < len; i++) {
        Token* token = tokenStream->peakAt(i);
        // TODO :: HACK because I have primitives as its own token type, not sure why I have this 
        if (expectedTokens[i] == IDENTIFER) {
            if (!isTokenTypeIdentifer(token)) {
                return false;
            }

        }
        else if (token->tokenType != expectedTokens[i]) {
            return false;
        };
    }
    return true;
}

bool tryParseAssigment(TokenStream * tokenStream) {

    //  try matching somethign like this with a type decl, we let the full parse match look for the equals 
    //  buffer::byte 
    TokenType expectedAssignmentTokens[] = { IDENTIFER, COLON_COLON, IDENTIFER };
    bool tokensMatch = matchTokens(tokenStream, expectedAssignmentTokens, LEN(expectedAssignmentTokens));
    if (tokensMatch) {
        return true;
    }
    else {
        // no type decl
        // try matching somethign like this 
        // buffer = 
        if (tokenStream->peakToken()->tokenType == IDENTIFER) {
            return tokenStream->peakNextToken()->tokenType == EQUAL;
        }
    }

    return false;
}

bool tryParseDotAccess(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType == IDENTIFER) {
        if (tokenStream->canPeakNextToken() && tokenStream->peakNextToken()->tokenType == DOT) {
            return true;
        }
    }

    return false;
}

AstDotAccess* parseDotAccess(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        return NULL;
    }
    AstDotAccess* dotAccess = createAstDotAccess(tokenStream->peakToken()->literal);
    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != DOT) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "Expected dot identifier for dot access");
        return NULL;
    }
    tokenStream->readToken();
    // Read multi dot access that can happen eg 
    // x.y.z etc 
    while (tokenStream->canRead()) {
        if (tokenStream->peakToken()->tokenType != IDENTIFER) {
            // We will assume the chain has finished and return whatever we have built 
            break;
        }
        AstDotElement* dotElement = NULL;
        if (tryParseArrayDecleration(tokenStream)) {
            auto arr_access = parseArrayAccess(tokenStream);
            if (arr_access == NULL) {
                return NULL;
            }
            dotElement = createAstDotElement(arr_access);
        }
        else {
            dotElement = createAstDotElement(tokenStream->peakToken()->literal);
            tokenStream->readToken(); // read identifier
        }
        addFileInfo(tokenStream, dotElement, tokenStream->peakToken());
        appendDotAccess(dotAccess, dotElement);
        if (tokenStream->peakToken()->tokenType != DOT) {
            break;
        }
        tokenStream->readToken();
    }

    return dotAccess;
}

AstTypeDecleration* parseTypeDeclBeforeArrayDef(TokenStream* tokenStream) {
    Token* peakedToken = tokenStream->peakToken();
    Token* startToken = peakedToken;

    PolyFlags polyFlags = NoPoly;
    Token* typeIdentifer = NULL;
    if (isTokenTypeIdentifer(peakedToken)) {
        typeIdentifer = peakedToken;
        tokenStream->readToken();
    }
    else {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "primitive or type identifer expected");
        return NULL;
    }

    if (isIdentifierPoly(typeIdentifer->literal)) {
        polyFlags = SinglePoly;
    }

    vector<AstTypeDecleration*>* optionalPolyArgs = new vector<AstTypeDecleration*>();
    if (tokenStream->peakToken()->tokenType == LOGICAL_LESS_THAN) {

        while (1) {
            if (polyFlags & SinglePoly) {
                printParseError(tokenStream->peakToken(), tokenStream->file_name, "Poly arguments cannot be put on a type that is in the form [A-Z]");
                return NULL;
            }

            tokenStream->readToken();
            polyFlags = PolyStruct;
            auto polyTypeDecl = parseTypeDeclBeforeArrayDef(tokenStream);
            if (polyTypeDecl == NULL) {
                return NULL;
            }
            if (polyTypeDecl->isArray) {
                printParseError(tokenStream->peakToken(), tokenStream->file_name, "Poly Arguments inside <> cannot be of type array");
                return NULL;
            }

            optionalPolyArgs->push_back(polyTypeDecl);

            if (tokenStream->peakToken()->tokenType != COMMA) {
                break;
            }
            tokenStream->readToken(); // comma
        }

        if (tokenStream->peakToken()->tokenType != LOGICAL_MORE_THAN) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "poly args list expected identifier");
            return NULL;
        }
        tokenStream->readToken();
    }

    auto typeDecl = createTypeDecleration(typeIdentifer->literal, polyFlags, optionalPolyArgs);
    addFileInfo(tokenStream, typeDecl, startToken);
    return typeDecl;
}

AstTypeDecleration* parseTypeDecl(TokenStream * tokenStream) {
    auto typeDecl = parseTypeDeclBeforeArrayDef(tokenStream);
    if (typeDecl == NULL) {
        return NULL;
    }
    if (tokenStream->peakToken()->tokenType == OPEN_ARR_BRACE) {
        auto dimensions = resolve_arr_dimensions(tokenStream);
        if (dimensions == NULL) {
            return NULL;
        }
        typeDecl->isArray = true;
        typeDecl->arr_dimensions = dimensions;
        if (typeDecl->polyFlags != NoPoly) {
            typeDecl->polyFlags |= PolyArray;
        }
    }
    return typeDecl;

}

AstLambdaDecleration* parseLambdaDecleration(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected identifer");
        return NULL;
    }
    string lambda_decl_name = tokenStream->peakToken()->literal;
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != COLON_COLON) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected ::");
        return NULL;
    }
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected (");
        return NULL;
    }
    tokenStream->readToken();

    AstLambdaDecleration* lambdaDecl = createLambdaDecleration();

    while (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        auto param = parseTypeDecl(tokenStream);
        addParamToLambdaDecl(lambdaDecl, param);
        if (tokenStream->peakToken()->tokenType != COMMA) {
            if (tokenStream->peakToken()->tokenType == CLOSE_ROUND_BRACKET) {
                break;
            }
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected comma to seperate function params");
            return NULL;
        }
        tokenStream->readToken();
    }
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType == RETURN_ARROW) {
        tokenStream->readToken();
        auto returnType = parseTypeDecl(tokenStream);
        if (returnType == NULL) {
            return NULL;
        }
        lambdaDecl->returnType = returnType;
    }
    lambdaDecl->name = lambda_decl_name;
    return lambdaDecl;
}

AstDecleration* parseDecleration(TokenStream * tokenStream) {
    Token* startToken = tokenStream->peakToken();

    if (tokenStream->peakToken()->tokenType == IDENTIFER && tokenStream->peakNextToken()->tokenType == COLON_COLON) {
        Token* lambdaName = tokenStream->peakToken();

        auto lambdaDecl = parseLambdaDecleration(tokenStream);
        if (lambdaDecl == NULL) {
            return NULL;
        }
        auto decl = createAstDecleration(lambdaName->literal, lambdaDecl);
        addFileInfo(tokenStream, decl, startToken);
        return decl;
    }

    auto typeDecl = parseTypeDecl(tokenStream);
    if (typeDecl == NULL) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        return NULL;
    }

    Token* paramName = tokenStream->peakToken();
    tokenStream->readToken();
    auto decl = createAstDecleration(paramName->literal, typeDecl);
    addFileInfo(tokenStream, decl, startToken);
    return decl;
}

vector<string>* collectPolyArgs(TokenStream * tokenStream) {

    vector<string>* polyArgs = new vector<string>();
    if (tokenStream->peakToken()->tokenType == LOGICAL_LESS_THAN) {
        tokenStream->readToken();

        if (!isTokenTypeIdentifer(tokenStream->peakToken())) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "poly args list expected identifier");
            return NULL;
        }

        while (1) {
            if (!isTokenTypeIdentifer(tokenStream->peakToken())) {
                break;
            }
            Token* polyArgToken = tokenStream->peakToken();
            polyArgs->push_back(polyArgToken->literal);

            tokenStream->readToken(); // read poly arg
            if (tokenStream->peakToken()->tokenType != COMMA) {
                break;
            }
            tokenStream->readToken();
        }

        if (tokenStream->peakToken()->tokenType != LOGICAL_MORE_THAN) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "poly args list expected identifier");
            return NULL;
        }

        tokenStream->readToken();
        return polyArgs;
    }
    else {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected [ to collect poly args");
        return NULL;
    }
}

AstFunctionTypeDef* parseFunctionTypeDef(TokenStream* tokenStream) {
    if (tokenStream->peakToken()->tokenType != TYPEDEF) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected typdef");
        return NULL;
    }
    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != FUNCTION) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected function");
        return NULL;
    }
    tokenStream->readToken();
    auto lambdaDecl = parseLambdaDecleration(tokenStream);
    if (lambdaDecl == NULL) {
        return NULL;
    }
    bool read = readSemiColon(tokenStream);
    if (!read) {
        return NULL;
    }
    return createastFunctionTypeDef(lambdaDecl);
}

AstStructNode* parseStructDecleration(TokenStream * tokenStream) {

    AstStructNode* astStruct = createStructNode();
    if (tokenStream->peakToken()->tokenType != STRUCT) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected struct");
        return NULL;
    }
    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected struct type name");
        return NULL;
    }

    astStruct->name = tokenStream->peakToken()->literal;
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType == LOGICAL_LESS_THAN) {
        Token* startToken = tokenStream->peakToken();
        vector<string>* polyArg = collectPolyArgs(tokenStream);
        if (polyArg == NULL) {
            return NULL;
        }
        for (auto arg : *polyArg) {
            if (!isIdentifierPoly(arg)) {
                printParseError(startToken, tokenStream->file_name, "Poly Args in struct defition should be [A-Z]");
            }
            addPolyArg(astStruct, arg);
        }
        delete polyArg;
    }

    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected {");
        return NULL;
    }

    tokenStream->readToken();

    // This struct decleration has no body
    if (tokenStream->peakToken()->tokenType == CLOSE_CURLY_BRACKET) {
        tokenStream->readToken();
        return astStruct;
    }

    bool parsedStructBody = parseStructBody(tokenStream, astStruct);

    if (!parsedStructBody) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
        return NULL;
    }

    tokenStream->readToken();
    return astStruct;
}

bool parseStructBody(TokenStream * tokenStream, AstStructNode * structNode) {
    while (tokenStream->canRead()) {

        Token* token = tokenStream->peakToken();
        if (token->tokenType == CLOSE_CURLY_BRACKET) {
            return true;
        }

        AstDecleration* decleration = parseDecleration(tokenStream);
        if (decleration == NULL) {
            return false;
        }
        bool readSemi = readSemiColon(tokenStream);
        if (!readSemi) {
            return false;
        }
        addDeclToStruct(structNode, decleration);
    }

    return true;
}

AstEnum* parseEnum(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != ENUM) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected mathc statement");
        return NULL;
    }
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected mathc statement");
        return NULL;
    }

    AstEnum* enumNode = createAstEnum(tokenStream->peakToken()->literal);
    addFileInfo(tokenStream, enumNode, tokenStream->peakToken());
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected mathc statement");
        return NULL;
    }
    tokenStream->readToken();

    while (1) {
        if (tokenStream->peakToken()->tokenType != IDENTIFER) {
            break;
        }
        auto enumValue = createAstIdentifier(tokenStream->peakToken()->literal);
        addFileInfo(tokenStream, enumValue, tokenStream->peakToken());

        addEnumValueToEnum(enumNode, enumValue);
        tokenStream->readToken();
        if (tokenStream->peakToken()->tokenType != COMMA) {
            break;
        }
        tokenStream->readToken();
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected mathc statement");
        return NULL;
    }
    tokenStream->readToken();
    return enumNode;
}

AstMatchNode* parseMatchNode(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != MATCH) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected mathc statement");
        return NULL;
    }
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected identifier statement");
        return NULL;
    }

    Token* matchTargetToken = tokenStream->peakToken();
    AstMatchNode* matchNode = createAstMatchNode(matchTargetToken->literal);
    addFileInfo(tokenStream, matchNode, matchTargetToken);
    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected {");
        return NULL;
    }
    tokenStream->readToken();
    while (1) {
        // parse case statements 
        if (tokenStream->peakToken()->tokenType != CASE) {
            break;
        }
        AstCaseBlock* caseBlock = createAstCaseBlock();
        addFileInfo(tokenStream, caseBlock, tokenStream->peakToken());
        tokenStream->readToken();
        // parse case statements
        while (1) {

            if (tokenStream->peakToken()->tokenType != IDENTIFER) {
                printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected identifier");
                return NULL;
            }

            addTargetToCaseBlock(caseBlock, tokenStream->peakToken()->literal);

            tokenStream->readToken();

            if (tokenStream->peakToken()->tokenType == COLON) {
                tokenStream->readToken();
                break;
            }

            if (tokenStream->peakToken()->tokenType != BAR) {
                printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected | to split case statements or use : to complete case ");
                return NULL;
            }
            tokenStream->readToken();
        }

        if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected open curly");
            return NULL;
        }
        tokenStream->readToken();

        auto casebody = parseStatements(tokenStream);
        if (casebody == NULL) {
            return NULL;
        }

        if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected  } bracket at the end of case statement");
            return NULL;
        }
        tokenStream->readToken();

        caseBlock->body = casebody;
        addCaseBlockToMatchNode(matchNode, caseBlock);
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
        return NULL;
    }
    tokenStream->readToken();
    return matchNode;
}

AstNoRoutine* parseNoRoutine(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != NO) {
        return NULL;
    }
    tokenStream->readToken();


    if (tokenStream->peakToken()->tokenType == TokenType::FUNCTION) {
        auto anonFunction = parseAnonFunction(tokenStream);
        if (anonFunction == NULL) {
            return NULL;
        }
        auto astCoRoutine = createPrivateCoRoutine(anonFunction);
        return astCoRoutine;
    }

    if (tryParseFunctionCall(tokenStream)) {
        AstFunctionCall* functionCall = parseFunctionCall(tokenStream);
        if (functionCall == NULL) {
            printError("no routine is not calling a function call");
            return NULL;
        }
        bool read_semi = readSemiColon(tokenStream);
        if (!read_semi) {
            printError("Error in expression, nothing can be resolved");
            return NULL;
        }

        auto astCoRoutine = createFunctionCallCoRoutine(functionCall);
        return astCoRoutine;
    }

    return NULL;
}

AstForLoop* parseForLoop(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType != FOR) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected for");
        return NULL;
    }

    tokenStream->readToken();
    // parse for loop body
    if (tokenStream->peakToken()->tokenType == OPEN_CURLY_BRACKET) {
        tokenStream->readToken(); // {

        AstBody* forLoopBody = parseStatements(tokenStream);
        if (!forLoopBody) {
            return NULL;
        }
        if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
            return NULL;
        }

        tokenStream->readToken(); // }
        return createInfiniteLoop(forLoopBody);
    }

    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected ( in for");
        return NULL;
    }

    tokenStream->readToken();
    AstAssignementStatement* assignement = parseAssignment(tokenStream);

    if (assignement == NULL) {
        return NULL;
    }

    auto conditionalExpression = parseBooleanExpressionInForLoop(tokenStream);
    if (conditionalExpression == NULL) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected boolean expression but expression could not be resolved");
        return NULL;
    }
    // Parse conditional 
    bool read_semi = readSemiColon(tokenStream);
    if (!read_semi) {
        return NULL;
    }
    AstAssignementStatement* postCondition = NULL;

    if (tryParseUnary(tokenStream)) {
        postCondition = parseUnary(tokenStream);
    }
    else {
        postCondition = parseAssignment(tokenStream);
    }

    if (postCondition == NULL) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected close round brace");
        return NULL;
    }

    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected open curly");
        return NULL;
    }

    tokenStream->readToken();
    auto forLoopBody = parseStatements(tokenStream);
    if (forLoopBody == NULL) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected close curly");
        return NULL;
    }

    tokenStream->readToken();
    return createAstForLoop(assignement, conditionalExpression, postCondition, forLoopBody);
}

bool parseFunctionParams(TokenStream * tokenStream, AstFunctionDefinition * functionDef) {

    while (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        auto start_token = tokenStream->peakToken();
        auto decl = parseDecleration(tokenStream);
        if (decl == NULL) {
            printParseError(start_token, tokenStream->file_name, "failed to parse type decleration in function params");
            return NULL;
        }
        addDeclToFunctionParams(functionDef, decl);
        if (tokenStream->peakToken()->tokenType != COMMA) {
            // Could be the end of the decleration;
            if (tokenStream->peakToken()->tokenType == CLOSE_ROUND_BRACKET) {
                break;
            }
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected comma to seperate function params");
            return false;
        }
        tokenStream->readToken();
    }

    if (!tokenStream->canRead()) {
        return false;
    }

    tokenStream->readToken(); // eat the ) bracket // TODO :: Remove this eat
    return true;
}

AstFunctionDefinition* parseFunctionDecleration(TokenStream * tokenStream) {
    return parseFunctionDecleration(tokenStream, false);
}

AstFunctionDefinition* parseFunctionDecleration(TokenStream * tokenStream, bool is_external) {

    if (tokenStream->peakToken()->tokenType != FUNCTION) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected function");
        return NULL;
    }

    tokenStream->readToken();
    // Could be function name or return type // we do not know at this point 
    AstFunctionDefinition* functionDef = createFunctionDefinition();

    if (tokenStream->peakToken()->tokenType == IDENTIFER && tokenStream->peakNextToken()->tokenType == OPEN_ROUND_BRACKET) {
        functionDef->returnIsVoid = true;
        functionDef->functionName = tokenStream->peakToken()->literal;
        tokenStream->readToken();
    }
    else {
        functionDef->returnIsVoid = false;
        auto returnType = parseTypeDecl(tokenStream);
        if (returnType == NULL) {
            return NULL;
        }
        functionDef->returnTypeDecl = returnType;
        if (tokenStream->peakToken()->tokenType != IDENTIFER) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected function name indetifier");
            return NULL;
        }
        functionDef->functionName = tokenStream->peakToken()->literal;
        tokenStream->readToken();
    }

    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected open brace in function definition");
        return NULL;
    }

    tokenStream->readToken();
    bool isFunctionParamsValid = parseFunctionParams(tokenStream, functionDef);
    if (!isFunctionParamsValid) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType == THROWS) {
        functionDef->throwsError = true;
        tokenStream->readToken();
    }

    if (is_external) {
        if (!readSemiColon(tokenStream)) {
            return NULL;
        }
        return functionDef;
    }

    AstBody* body = parseFunctionBody(tokenStream);
    if (body == NULL) {
        return NULL;
    }

    addBodyToFunctionDef(functionDef, body);
    return functionDef;
}

AstExternalFunction* parseExternalFunction(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != EXTERNAL) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected external function");
        return NULL;
    }
    bool is_external = true;
    tokenStream->readToken();
    AstFunctionDefinition* functionDef = parseFunctionDecleration(tokenStream, is_external);
    if (functionDef == NULL) {
        return NULL;
    }
    functionDef->isExternal = true;
    return createExternalFunction(functionDef);
}

AstForEachStatment* parseForEach(TokenStream * tokenStream) {

    if (tokenStream->peakToken()->tokenType != FOR_EACH) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected foreach");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != OPEN_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected {");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected identifer");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != IN) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected in");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != IDENTIFER) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected identifer");
        return NULL;
    }

    tokenStream->readToken();

    if (tokenStream->peakToken()->tokenType != CLOSE_ROUND_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
        return NULL;
    }

    tokenStream->readToken();
    if (tokenStream->peakToken()->tokenType != OPEN_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected {");
        return NULL;
    }

    tokenStream->readToken();

    // no body
    if (tokenStream->peakToken()->tokenType == CLOSE_CURLY_BRACKET) {
        tokenStream->readToken();
        return NULL;
    }

    bool forBody = parseStatements(tokenStream);

    if (!forBody) {
        return NULL;
    }

    if (tokenStream->peakToken()->tokenType != CLOSE_CURLY_BRACKET) {
        printParseError(tokenStream->peakToken(), tokenStream->file_name, "expected }");
        return NULL;
    }

    tokenStream->readToken();
    return new AstForEachStatment();
}

bool tryParseTuple(TokenStream * tokenStream) {
    if (tokenStream->peakToken()->tokenType == IDENTIFER) {
        if (tokenStream->canPeakNextToken() && tokenStream->peakNextToken()->tokenType == COMMA) {
            return true;
        }
    }
    return false;
}

AstTupleError* parseTuple(TokenStream * tokenStream) {

    Token* startToken = tokenStream->peakToken();
    AstTupleError* astTuple = createAstTuple();
    while (tokenStream->canRead()) {
        if (tokenStream->peakToken()->tokenType != IDENTIFER) {
            printParseError(tokenStream->peakToken(), tokenStream->file_name, "tuple expected identifier");
            return NULL;
        }

        string identifer = tokenStream->peakToken()->literal;
        addTuple(astTuple, identifer);
        tokenStream->readToken();
        if (tokenStream->peakToken()->tokenType != COMMA) {
            if (tokenStream->peakToken()->tokenType == EQUAL) {
                tokenStream->readToken();
                break;
            }
            return NULL;
        }
        tokenStream->readToken();
        if (tokenStream->peakToken()->tokenType == EQUAL) {
            tokenStream->readToken();
            break;
        }
    }
    AstFunctionCall* functioncall = parseFunctionCall(tokenStream);
    if (functioncall == NULL) {
        return NULL;
    }
    addFunctionCallToTuple(astTuple, functioncall);
    if (!readSemiColon(tokenStream)) {
        return NULL;
    }
    addFileInfo(tokenStream, astTuple, startToken);
    return astTuple;
}

void advance(TokenStream * stream, set<TokenType>*stopTokens) {
    while (stream->canRead()) {
        TokenType current = stream->peakToken()->tokenType;
        bool stop = stopTokens->find(current) != stopTokens->end();
        if (stop) {
            break;
        }
        stream->readToken();
    }
}

bool tryParseUnary(TokenStream * tokenStream) {
    return tokenStream->peakToken()->tokenType == IDENTIFER && isTokenUnary(tokenStream->peakNextToken());
}

bool isTokenUnary(Token * token) {
    return token->tokenType == ADD_ADD || token->tokenType == MINUS_MINUS;
}

// Unaries are chaged into 
// i++ => i = i + 1;
// i-- => i = i - 1;
AstAssignementStatement* parseUnary(TokenStream * tokenStream) {
    auto one_literal = createLitearlValueNodeForInt(1);
    auto identiferToken = tokenStream->peakToken();
    auto unaryOperatorToken = tokenStream->peakNextToken();
    auto identifer = createAstIdentifier(identiferToken->literal);
    auto operatorStr = unaryToOperator(unaryOperatorToken);
    AstArithMeticExpression* arithemticExpression = createAithemticExpressionNode(identifer, one_literal, operatorStr);
    auto assignment = createAstAssignment(identiferToken->literal, arithemticExpression, NULL);
    tokenStream->readToken(); // read unary
    tokenStream->readToken(); // read identifer
    return assignment;
}

// Changed to 
// x.y.z++;
// x.y.z = x.y.z + 1;
AstDotAssignment* createDotAssignmentFromUnary(AstDotAccess * dotAccess, Token * unaryToken) {
    auto operatorStr = unaryToOperator(unaryToken);
    auto addOne = createLitearlValueNodeForInt(1);
    AstArithMeticExpression* arithemticExpression = createAithemticExpressionNode(dotAccess, addOne, operatorStr);
    auto dotAssignment = createDotAssignment(dotAccess, arithemticExpression);
    return dotAssignment;
}

string unaryToOperator(Token * token) {

    if (token->tokenType == ADD_ADD) {
        return "+";
    }
    if (token->tokenType == MINUS_MINUS) {
        return "-";
    }
    throw runtime_error("token is not a unary opeartor");
}

bool isIdentifierPoly(string polyArgToken) {
    // @duplicatecode :: polyTyepcheck
    if (polyArgToken.length() == 1) {
        char polyArg = polyArgToken.c_str()[0];
        if (polyArg >= 'A' && polyArg <= 'Z') {
            return true;
        }

        return false;
    }
    return false;
}

bool isTokenTypeIdentifer(Token * token) {
    return token->tokenType == IDENTIFER || token->isTokenTypePrimitve;
}