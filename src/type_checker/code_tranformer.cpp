#include "type_checker/code_transformer.h"
#include "ast/ast_fun.h"
#include "assert.h"
#include <string> 
#include "utils/utils.h"
#include "type_checker/extern_function.h"
#include "type_checker/closure_layout.h"
using namespace std;

void createClosuresUsingStaticLink(SymbolTable* symbolTable);
void create_scope_idenfifers(SymbolTable* symbolTable, FunctionSymbol* functionSymbol);
void transform_to_scope_identifer(NonLocalIdentifier * nonLocal, SymbolTable * symbolTable, FunctionSymbol * functionSymbol);

void resolve_closures(SymbolTable * symbolTable) {
    for (auto kv : *symbolTable->fSymbolTable->table) {
        create_scope_idenfifers(symbolTable, kv.second);
    }

    if (symbolTable->functionDefThatEscape->size() != 0) {
        createClosuresUsingStaticLink(symbolTable);
    }
}

void tranform_function_call_to_lambda(AstFunctionCall* functionCall, int call_stack_difference) {
    functionCall->sub = createAstLambdaCall(functionCall, call_stack_difference);
}

void create_scope_idenfifers(SymbolTable * symbolTable, FunctionSymbol * functionSymbol) {

    if (functionSymbol->isExternal) {
        return;
    }

    if (functionSymbol->is_recursive && functionSymbol->nonLocalIdentifiers->size() > 0) {
        emit_error_info(functionSymbol->functionDef, "recursive functions cannot reference non local identifiers, this is because I cannot determine how many stack frames to go up");
    }

    for (auto nonLocal : *functionSymbol->nonLocalIdentifiers) {
        transform_to_scope_identifer(nonLocal, symbolTable, functionSymbol);
    }
}

void transform_to_scope_identifer(NonLocalIdentifier * nonLocal, SymbolTable * symbolTable, FunctionSymbol * functionSymbol) {

    auto call_stack_difference = (nonLocal->functionDepthLevel - nonLocal->rootSymbol->function_depth_level);
    assert(call_stack_difference != 0);
    auto scoped_identifier = createScopedIdentifier(nonLocal->rootSymbol->name, call_stack_difference);
    storeTypeOfNode(symbolTable, functionSymbol, scoped_identifier, nonLocal->rootSymbol->returnType);

    if (nonLocal->identifier->type == Identifier) {
        AstIdentifier* identifier = (AstIdentifier*)nonLocal->identifier;
        identifier->sub = scoped_identifier;
    }
    else if (nonLocal->identifier->type == Assignment) {
        // assignmentStatement->rightSide will 
        // be transformed if it is has acceses to non local identiifer so I do not have to run a transformation here 
        AstAssignementStatement* assignmentStatement = (AstAssignementStatement*)nonLocal->identifier;
        assignmentStatement->identifier->sub = scoped_identifier;
    }
    else if (nonLocal->identifier->type == DotAccess) {
        AstDotAccess* dotAccess = (AstDotAccess*)nonLocal->identifier;
        auto rootAccess = dotAccess->accesses->at(0);
        rootAccess->sub = scoped_identifier;
    }
    else if (nonLocal->identifier->type == FunctionCall) {
        assert_never_reaches("function calls should never be added here");
    }
    else if (nonLocal->identifier->type == ArrayAccess) {
        AstArrayAccess* access = (AstArrayAccess*)nonLocal->identifier;
        access->identifier->sub = scoped_identifier;
    }
    else if (nonLocal->identifier->type == ArraySubscriptAssignment) {
        AstArraySubscriptAssignment* arr_assignment = (AstArraySubscriptAssignment*)nonLocal->identifier;
        arr_assignment->arrAccess->identifier->sub = scoped_identifier;
    }
    else {
        assert_never_reaches("code_transformer :: There is a node that has not been accounted for ");
    }
}

int stack_difference(int arg1, int arg2) {
    return arg1 - arg2;
}

string internal_closure_struct_runtime_name() {
    static string closure_name = "<internal_closure_struct_def>";
    return closure_name;;
}

void createClosuresUsingStaticLink(SymbolTable * symbolTable) {

    auto closure_type_name = internal_closure_struct_runtime_name();
    auto closureStructLayout = new StructLayout();
    closureStructLayout->fields->push_back(new Field(CLOSURE_FP_FIELD_NAME, &intReturn));
    closureStructLayout->fields->push_back(new Field(CLOSURE_STATIC_LINK_FIELD_NAME, &typeAnyReturn));
    addStructLayoutTableEntry(symbolTable, closure_type_name, closureStructLayout);

    auto closureReturnType = createStructReturnType(closure_type_name);
    addStructTypeSymbolTable(symbolTable, closure_type_name, closureReturnType);

    for (auto escapingFunction : *symbolTable->functionDefThatEscape) {
        auto functionSymbol = escapingFunction->escapingFunctionSymbol;

        functionSymbol->needsLambdaLifting = true;
        auto escapingNode = escapingFunction->escapingNode;
        AstClosureCreation* closureCreation = createClosureCreation();
        closureCreation->escapingFunctionName = escapingFunction->escapingFunctionSymbol->name;

        if (escapingNode->type == Return) {
            assert_never_reaches("there should be no escaping node that is a return statement");
            AstReturnStatement* escapingReturnStatement = (AstReturnStatement*)escapingFunction->escapingNode;
            escapingReturnStatement->expression->sub = closureCreation;
        }
        else if (escapingNode->type == AnonymousFunction) {
            AstAnonFunction* anonFunction = (AstAnonFunction*)escapingNode;
            anonFunction->sub = closureCreation;
        }
        else if (escapingNode->type == Identifier) {
            AstIdentifier* identifier = (AstIdentifier*)escapingNode;
            identifier->sub = closureCreation;
        }
        else {
            assert_never_reaches("complete the rest of the escaping nodes");
        }
    }
}

AstBooleanExpression* createBooleanExpressionForCase(SymbolTable * symbolTable, FunctionSymbol * functionOfBody, AstIdentifier * matchTargtIdentifier, int enumValueAsInt, string op) {
    auto booleanExpression = createBooleanExpressionNode(matchTargtIdentifier, createLitearlValueNodeForInt(enumValueAsInt), op);
    storeTypeOfNode(symbolTable, functionOfBody, booleanExpression->left, &intReturn);
    storeTypeOfNode(symbolTable, functionOfBody, booleanExpression->right, &intReturn);
    return booleanExpression;
}

AstIfStatement* transformCaseToIfStatement(SymbolTable * symbolTable, FunctionSymbol * functionOfBody, Symbol * matchTarget, AstCaseBlock * caseBlock, EnumSymbol * enumSymbol) {

    /*
     match h {
        case a|b|c : {
            // body
        }
    }

    We want to transform each case to the following. The boolean expression builer keeps track of this
    if (h == a || h == b || h ==c ) {
        // body
    }
    */
    // Transform first case h == a 
    int begin = 0;
    auto firstTarget = caseBlock->targets->at(begin);
    int firstTargetAsInt = getIntValueFromEnum(enumSymbol, firstTarget);
    auto targetAsIdentiferNode = createAstIdentifier(matchTarget->name);

    auto booleanExpressionBuilder = createBooleanExpressionForCase(symbolTable, functionOfBody, targetAsIdentiferNode, firstTargetAsInt, "==");

    // Keep building boolean expression if there are targets
    for (size_t i = begin + 1; i < caseBlock->targets->size(); i++) {
        auto target = caseBlock->targets->at(i);
        int targetAsInt = getIntValueFromEnum(enumSymbol, target);
        auto targetBooleanExpression = createBooleanExpressionForCase(symbolTable, functionOfBody, targetAsIdentiferNode, targetAsInt, "==");

        booleanExpressionBuilder = createBooleanExpressionNode(booleanExpressionBuilder, targetBooleanExpression, "||");
        storeTypeOfNode(symbolTable, functionOfBody, booleanExpressionBuilder->left, &boolReturn);
        storeTypeOfNode(symbolTable, functionOfBody, booleanExpressionBuilder->right, &boolReturn);
    }

    return createAstIfStatement(booleanExpressionBuilder, caseBlock->body);
}