#include "ast/ast.h"
#include "collections/collections.h"
#include "type_checker/type_checker.h"
#include "assert.h"
#include "ast/ast_fun.h"

// Note(umar) These are not really basic blocks as described in the literature in compiles
// But the idea is the same but for try handle checking we only need a subset of see the control flow after a function call that throws 
AstTryLabel* tryLabel = new AstTryLabel();

class Block {
public:
    vector<AstNode*>* seq;

    ~Block() {
        if (this->seq) {
            delete this->seq;
        }
    }
};

class BlockList {
public:
    vector<Block*> blocks;
    ~BlockList() {
        for (auto block : this->blocks) {
            delete block;
        }
    }
};

BlockList createNewBlockList() {
    BlockList blockList = BlockList();
    blockList.blocks = vector<Block*>();
    return blockList;
}

void addAstNodeToBlock(BlockList * blockList, AstNode * node) {
    // Dont add any nodes unless there is at least one open block started by a function call
    if (blockList->blocks.size() == 0) {
        return;
    }
    blockList->blocks.back()->seq->push_back(node);
}

void createNewBlockInBlockList(BlockList * blockList) {
    Block* block = new Block();
    block->seq = new vector<AstNode*>();
    blockList->blocks.push_back(block);
}

class BlockSnReader {
public:
    int cursor;
    Block* block;
};

bool doesBlockHaveEnoughNodes(BlockSnReader * reader, size_t required) {
    if ((reader->block->seq->size() - reader->cursor) < required) {
        return false;
    }
    return true;
}

void goBackOne(BlockSnReader * blockSnReader) {
    blockSnReader->cursor = blockSnReader->cursor - 1;
}

AstNode* readNextNode(BlockSnReader * blockSnReader) {
    int cursor = blockSnReader->cursor;
    AstNode* node = blockSnReader->block->seq->at(cursor);
    blockSnReader->cursor += 1;
    return node;
}

BlockSnReader createBlockReader(Block * block) {
    BlockSnReader snReader = BlockSnReader();
    snReader.block = block;
    return snReader;
}

bool isExpressionCatch(AstNode * actual, string expected_err_str_identifer) {

    if (actual->type != IfStatement) {
        return false;
    }
    AstIfStatement* ifStatment = (AstIfStatement*)actual;

    AstNode* ifExpression = ifStatment->booleanExpression;
    if (ifExpression->type != BooleanExpression) {
        return false;
    }
    AstBooleanExpression * actualEpxression = (AstBooleanExpression*)ifStatment->booleanExpression;
    /*
        We are checking that form looks like this, wich is basically a catch statement
        if (err != null)
    */
    if (actualEpxression->op != "!=") {
        return false;
    }
    if (actualEpxression->right->type != Null) {
        return false;
    }
    if (actualEpxression->left->type != Identifier) {
        return false;
    }

    AstNode* left_side = actualEpxression->left;
    if (left_side->type != Identifier) {
        return false;
    }
    AstIdentifier* err_idenfier = (AstIdentifier*)actualEpxression->left;
    if (err_idenfier->name != expected_err_str_identifer) {
        return false;
    }
    return true;
}

FunctionSymbol* resolveFunctionCall(SymbolTable* table, AstFunctionCall* functionCall) {
    if (functionCall->sub != NULL) {
        assert(functionCall->sub->type == FunctionCall);
        AstFunctionCall* sub = (AstFunctionCall*)functionCall->sub;
        return getFunctionSymbol(table, sub->name);
    }
    else {
        return getFunctionSymbol(table, functionCall->name);
    }
}

void typeCheckBlockList(BlockList * blockList, SymbolTable * symbolTable) {

    int min_num_statments_for_try_catch = 2;
    for (auto block : blockList->blocks) {

        BlockSnReader blockReader = createBlockReader(block);
        AstNode * functionCallNode = readNextNode(&blockReader);
        assert(functionCallNode->type == FunctionCall);
        AstFunctionCall* functionCall = (AstFunctionCall*)functionCallNode;
        auto calledFunction = resolveFunctionCall(symbolTable, functionCall);
        // TODO :: For now we just ignore lambda calls 

        /*
        This is the reason why i did not allow lambda functions to throw
        Because the cfg at this point has lost all the type information that was resolved
        For now lambda functions cannot throw anyway so this is okay
        TODO :: I probably could use the node type map to see if the function throws
        if (functionCall->isLambda) {
            continue;
        }
        */

        assert(calledFunction->markedAsThrows);
        if (!doesBlockHaveEnoughNodes(&blockReader, 1)) {
            emit_error_info(functionCall, "Either try catch function call or mark with try");
        }

        AstNode* potentialTry = readNextNode(&blockReader);
        bool isFunctionTried = potentialTry->type == TryLabel;
        if (!isFunctionTried) {
            goBackOne(&blockReader); // Unwind reading the first node 

            if (!doesBlockHaveEnoughNodes(&blockReader, min_num_statments_for_try_catch)) {
                emit_error_info(functionCallNode, "Not enough statments for try catch, mark function call with try if you do not care about error");
            }
            // Get error type from the left side; either type
            // TODO :: Why did i call this expected? It should be actual?
            string err_identifier;
            AstNode* next_node = readNextNode(&blockReader);
            // We dont do try handler if function is called with no routine

            // TODO :: The parser makes sures no routines cannot be tried 

            AstAssignementStatement* identifer_assignement = NULL;
            if (isReturnTypeVoid(calledFunction->returnType)) {
                AstNode* err_assignment_node = next_node;
                if (err_assignment_node->type != Assignment) {
                    emit_error_info(potentialTry, "function throws an error, you should store the error or mark the function call with try");
                }
                identifer_assignement = (AstAssignementStatement*)err_assignment_node;
                err_identifier = identifer_assignement->identifier->name;
            }
            else {
                AstNode* tuple_node = next_node;
                if (tuple_node->type != Tuple) {
                    emit_error_info(potentialTry, "function throws an error, you should store the error or mark the function with try");
                }
                AstTupleError* tuple = (AstTupleError*)tuple_node;
                err_identifier = tuple->tuple->back();
                // Get final type in type and store as identifer 
            }
            AstNode* potentialCatchStatment = readNextNode(&blockReader);
            bool isExpresssionCatch = isExpressionCatch(potentialCatchStatment, err_identifier);
            if (!isExpresssionCatch) {
                emit_error_info(potentialTry, "Expected catch statement but got some other expression");
            }

            // AstTupleError code gens with try catch 
            // Only in the case for a single identifier do we explicitly create a try catch assignment
            if (identifer_assignement != NULL) {
                identifer_assignement->sub = (AstNode*)createTryCatchAssignmentStatement(identifer_assignement);
            }
        }
    }
}

// The try handler cfg checks run on untransformed code 
void append_cfg(SymbolTable* symbolTable, AstNode * node, BlockList* blockList);

void run_try_handler_cfg_checks(SymbolTable* symbolTable, AstFunctionDefinition* functionDef) {
    if (functionDef->body == NULL) {
        return;
    }
    auto blockList = createNewBlockList();
    append_cfg(symbolTable, functionDef->body, &blockList);
    typeCheckBlockList(&blockList, symbolTable);
}

void append_cfg(SymbolTable* symbolTable, AstNode * node, BlockList* blockList) {
    if (node == NULL) {
        return;
    }

    switch (node->type) {
    case Body:
    {
        AstBody* body = (AstBody*)node;
        for (auto statement : *body->statements) {
            append_cfg(symbolTable, statement, blockList);
        }
        break;
    }
    case FunctionCall:
    {
        AstFunctionCall* functionCall = (AstFunctionCall*)node;
        auto functionSymbol = getFunctionSymbol(symbolTable, functionCall->name);

        if (functionSymbol != NULL) {
            if (functionSymbol->markedAsThrows) {
                createNewBlockInBlockList(blockList);
            }
        }
        // In this case the function call could be a lambda symbol and for now lambda functions cannot throw
        addAstNodeToBlock(blockList, functionCall);
        break;
    }
    case Assignment:
    {
        AstAssignementStatement* assignment = (AstAssignementStatement*)node;
        append_cfg(symbolTable, assignment->rightSide, blockList);
        addAstNodeToBlock(blockList, assignment);
        break;
    }
    case Tuple:
    {
        AstTupleError* tuple = (AstTupleError*)node;
        append_cfg(symbolTable, tuple->rightSide, blockList);
        addAstNodeToBlock(blockList, tuple);
        break;
    }
    case IfStatement:
    {
        AstIfStatement* ifStatement = (AstIfStatement*)node;
        addAstNodeToBlock(blockList, ifStatement);
        append_cfg(symbolTable, ifStatement->booleanExpression, blockList);
        append_cfg(symbolTable, ifStatement->ifbody, blockList);
        if (ifStatement->ifElses != NULL) {
            for (auto if_else : *ifStatement->ifElses) {
                append_cfg(symbolTable, if_else, blockList);
            }
        }
        append_cfg(symbolTable, ifStatement->elseBody, blockList);
        break;
    }
    case FunctionDefinition:
    {
        // Do not add local function to try cfg blocks they are called explicitly when the full type check happens 
        return;
    }

    case AnonymousFunction:
    {
        // Do not add anonymous function to try cfg blocks they are called explicitly when the full type check happens 
        // The anon function can only be used inside a function call or for a  no routine
        // Both of these cases wont affect creating the "correct" cfg construction
        return;
    }
    case ForLoop:
    {
        AstForLoop* loop = (AstForLoop*)node;
        append_cfg(symbolTable, loop->body, blockList);
        break;
    }
    case Try:
    {
        AstTryStatement* astTryStatement = (AstTryStatement*)node;
        append_cfg(symbolTable, astTryStatement->functionCall, blockList);
        addAstNodeToBlock(blockList, tryLabel);
        break;
    }
    case NoRoutine:
    {

        AstNoRoutine* noRoutine = (AstNoRoutine*)node;
        addAstNodeToBlock(blockList, noRoutine);
        // No routines are not added 
        // AstNoRoutine* noRoutine = (AstNoRoutine*)node;
        // if (noRoutine->coRoutineBodyType == CoRoutineBodyType::FunctionCall) {
        //     AstFunctionCall* functionCall = (AstFunctionCall*)noRoutine->body;
        //     auto functionSymbol = getFunctionSymbol(symbolTable, functionCall->name);
        //     append_cfg(symbolTable, functionCall, blockList);
        //     addAstNodeToBlock(blockList, noRoutine);
        // }
        // else if (noRoutine->coRoutineBodyType == CoRoutineBodyType::Anonymous) {
        //     append_cfg(symbolTable, noRoutine->body, blockList);
        // }
        // else {
        //     addAstNodeToBlock(blockList, noRoutine);
        // }
        break;
    }

    default:
        addAstNodeToBlock(blockList, node);
        break;
    }
}