#include "ast/ast.h"
#include "ast/ast_fun.h"
#include <assert.h>
#include "type_checker/type_checker.h"
#include "gen/code_gen.h"
#include <iostream>
#include <assert.h>
#include "collections/collections.h"
#include "utils/utils.h"
#include "type_checker/extern_function.h"

string noRoutineStorageName = "<noRoutine>";

Instruction * emit(InstructionStream * stream, Opcode opcode);

void toString(Opcode opcode) {
    switch (opcode) {
    case Opcode::LOAD_ARR_LEN:
        cout << "LOAD_ARR_LEN";
        break;
    case Opcode::LOAD_I:
        cout << "LOAD_I";
        break;
    case Opcode::LOAD_IMMEDIATE:
        cout << "LOAD_IMMEDIATE";
        break;
    case Opcode::STORE:
        cout << "STORE";
        break;
    case Opcode::LOAD_CONST:
        cout << "LOAD_CONST";
        break;
    case Opcode::LOAD_NON_NATIVE_STR_CONST:
        cout << "LOAD_NON_NATIVE_STR_CONST";
        break;
    case Opcode::LOAD_NULL:
        cout << "LOAD_NULL";
        break;
    case Opcode::ADD:
        cout << "ADD";
        break;
    case Opcode::MOD:
        cout << "MOD";
        break;
    case Opcode::SUBTRACT:
        cout << "SUBTRACT";
        break;
    case Opcode::MULTIPLY:
        cout << "MULTIPLY";
        break;
    case Opcode::DIVIDE:
        cout << "DIVIDE";
        break;
    case Opcode::I2D:
        cout << "I2D";
        break;
    case Opcode::I2C:
        cout << "I2C";
        break;
    case Opcode::C2I:
        cout << "C2I";
        break;
    case Opcode::THROW:
        cout << "THROW";
        break;
    case Opcode::NEW_ARR:
        cout << "NEW_ARR";
        break;
    case Opcode::NEW:
        cout << "NEW";
        break;
    case Opcode::RETURN:
        cout << "RETURN";
        break;
    case Opcode::RETURN_FROM_VOID:
        cout << "RETURN_FROM_VOID";
        break;
    case Opcode::JUMP_IF_FALSE:
        cout << "JUMP_IF_FALSE";
        break;
    case Opcode::JUMP:
        cout << "JUMP";
        break;
    case Opcode::DEFER:
        cout << "DEFER";
        break;
    case Opcode::DEFER_FINISH:
        cout << "DEFER_FINISH";
        break;
    case Opcode::CALL:
        cout << "CALL";
        break;
    case Opcode::CALL_LAMBDA:
        cout << "CALL_LAMDA";
        break;
    case Opcode::CALL_WITH_TRY_CATCH:
        cout << "CALL_WITH_TRY_CATCH";
        break;
    case Opcode::LABEL:
        cout << "LABEL";
        break;
    case Opcode::OR:
        cout << "OR";
        break;
    case Opcode::AND:
        cout << "AND";
        break;
    case Opcode::LESS_THAN:
        cout << "LESS_THAN";
        break;
    case Opcode::LESS_THAN_EQUAL:
        cout << "LESS_THAN_EQUAL";
        break;
    case Opcode::MORE_THAN:
        cout << "MORE_THAN";
        break;
    case Opcode::MORE_THAN_EQUAL:
        cout << "MORE_THAN_EQUAL";
        break;
    case Opcode::EQUAL:
        cout << "EQUAL";
        break;
    case Opcode::NOT_EQUAL:
        cout << "NOT_EQUAL";
        break;
    case Opcode::LOAD_FIELD:
        cout << "LOAD_FIELD";
        break;
    case Opcode::STORE_FIELD:
        cout << "STORE_FIELD";
        break;
    case Opcode::STORE_ARR_I:
        cout << "STORE_ARR_I";
        break;
    case Opcode::LOAD_ARR_I:
        cout << "LOAD_ARR_I";
        break;
    case Opcode::ENTER_FOR_BLOCK:
        cout << "ENTER_FOR_BLOCK";
        break;
    case Opcode::FORK:
        cout << "FORK";
        break;
    default:
        cout << "Unknown Opcode";
        break;
    }
}

struct LabelMap {
    // Exclusive owernship
    map<string, int>* labelIpMap;
    vector<Instruction*>* instructionLabelsToResolve;
    stack<stack<Instruction*>*>* resolveBreaks;
    stack<stack<Instruction*>*>* resolveContinues;
    // End Exclusive owernship

    ~LabelMap() {
        if (this->labelIpMap) {
            delete this->labelIpMap;
        }
        if (this->instructionLabelsToResolve) {
            delete this->instructionLabelsToResolve;
        }
        if (this->resolveBreaks) {
            delete this->resolveBreaks;
        }
        if (this->resolveContinues) {
            delete this->resolveContinues;
        }
    }

};

// TODO ::The side effect of adding to resolution map does make things confusing 
int getInstructionPointerFromLabel(LabelMap* labelmap, Instruction* callInstruction) {
    if (labelmap->labelIpMap->count(callInstruction->symbol) == 0) {
        labelmap->instructionLabelsToResolve->push_back(callInstruction);
        return -1;
    }
    return labelmap->labelIpMap->at(callInstruction->symbol);
}

int getLableIp(LabelMap* labelMap, string label) {
    if (labelMap->labelIpMap->count(label) == 0) {
        return -1;
    }
    return labelMap->labelIpMap->at(label);
}

void addLableIp(LabelMap* labelMap, string label, int instructionPointer) {
    labelMap->labelIpMap->insert_or_assign(label, instructionPointer);
}

void gen_internal(InstructionStream * stream, LabelMap* labelMap, SymbolTable* symbolTable, AstNode* node, FunctionSymbol* enclosingFunctionDef, CodeGenConstantPool* constantPool);

InstructionStream* createInstructionStream() {
    InstructionStream* stream = new InstructionStream();
    stream->instructions = new vector<Instruction*>();
    return stream;
}

CodeGenConstantPool* createConsPool() {
    CodeGenConstantPool* pool = new CodeGenConstantPool();
    pool->constantPool = new vector<AstLiteralValue*>();
    pool->str_constantPool = new vector<string>();
    return pool;

}

InstructionStream* createInsStreamFromOffset(InstructionStream* ins, int offset, int lastInsIndx) {
    InstructionStream* stream = new InstructionStream();
    stream->instructions = ins->instructions;
    stream->startInsIndx = offset;
    stream->lastInsIdx = lastInsIndx;
    return stream;
}

// The instruction is a pure trip with optional arguments
void print(InstructionStream* stream) {
    int number = 0;
    for (auto ins : *stream->instructions) {
        cout << number << "  ";
        toString(ins->op_code);
        cout << " :: " << ins->arg1 << "," << ins->arg2 << "," << ins->symbol << endl;
        number++;
    }
}

int createConst(CodeGenConstantPool* constantPool, AstLiteralValue* literal) {
    constantPool->constantPool->push_back(literal);
    return constantPool->constantPool->size() - 1;
}

int createStrConst(CodeGenConstantPool* constantPool, AstLiteralValue* literal) {
    assert(literal->valueType == LiteralType::StringLiteral);
    constantPool->str_constantPool->push_back(literal->literalValue->strValue);
    return constantPool->str_constantPool->size() - 1;
}

int getCurrentOff(InstructionStream* stream) {
    return stream->instructions->size();
}

Instruction* emit(InstructionStream* stream, Opcode opcode, int arg1) {
    Instruction* ins = new Instruction();
    ins->op_code = opcode;
    ins->arg1 = arg1;
    stream->instructions->push_back(ins);
    return ins;
}

Instruction* emit(InstructionStream* stream, Opcode opcode) {
    return emit(stream, opcode, 0);
}

LabelMap createLabelMap() {
    LabelMap labelMap = LabelMap();
    labelMap.instructionLabelsToResolve = new vector<Instruction*>();
    labelMap.labelIpMap = new map<string, int>();
    labelMap.resolveBreaks = new stack<stack<Instruction*>*>();
    labelMap.resolveContinues = new stack<stack<Instruction*>*>();
    return labelMap;
}

void push_new_resolve_breaks_frame(LabelMap* labelMap) {
    labelMap->resolveBreaks->push(new stack<Instruction*>());
}

void push_new_resolve_continues_frame(LabelMap* labelMap) {
    labelMap->resolveContinues->push(new stack<Instruction*>());
}


// TODO :: Move to templates/macors 
void pop_resolve_breaks(LabelMap* labelMap) {
    if (labelMap->resolveBreaks->empty()) {
        assert_never_reaches("pop_resolve_breaks is empty");
    }
    auto top = labelMap->resolveBreaks->top();
    labelMap->resolveBreaks->pop();
    delete top;
}

stack<Instruction*>* top_resolve_breaks(LabelMap* labelMap) {
    if (labelMap->resolveBreaks->empty()) {
        assert_never_reaches("top_resolve_breaks is empty");
    }
    auto top = labelMap->resolveBreaks->top();
    return top;
}

void pop_resolve_continues(LabelMap* labelMap) {
    if (labelMap->resolveContinues->empty()) {
        assert_never_reaches("pop_resolve_continues is empty");
    }
    auto top = labelMap->resolveContinues->top();
    labelMap->resolveContinues->pop();
    delete top;
}

stack<Instruction*>* top_resolve_continues(LabelMap* labelMap) {
    if (labelMap->resolveContinues->empty()) {
        assert_never_reaches("top_resolve_continues is empty");
    }
    auto top = labelMap->resolveContinues->top();
    return top;
}

void gen_code_for_function_def(InstructionStream * stream,
    LabelMap* labelMap,
    SymbolTable* symbolTable,
    FunctionSymbol* functionDef,
    CodeGenConstantPool* constPool) {

    auto labelip = getCurrentOff(stream);
    auto labelInst = emit(stream, Opcode::LABEL);
    labelInst->symbol = functionDef->name;
    addLableIp(labelMap, labelInst->symbol, labelip);
    auto functionBody = functionDef->functionBody;

    // There is no body in external functions 
    if (!functionDef->isExternal) {
        gen_internal(stream, labelMap, symbolTable, functionBody, functionDef, constPool);
    }
    else {
        if (!isReturnTypeVoid(functionDef->returnType)) {
            emit(stream, Opcode::RETURN);
        }
    }

    // Always add return just in case the function is void so we can exit the frame 
    if (functionDef->markedAsThrows && isReturnTypeVoid(functionDef->returnType)) {
        emit(stream, Opcode::LOAD_NULL);
    }
    emit(stream, Opcode::RETURN_FROM_VOID);
}

EntryPoint* createEntryPoint() {
    EntryPoint* entryPoint = new EntryPoint();
    entryPoint->constantPool = createConsPool();
    entryPoint->ins = createInstructionStream();;
    return entryPoint;
}

EntryPoint* gen(SymbolTable* symbolTable) {
    auto mainFunction = getFunctionSymbol(symbolTable, "main");
    if (mainFunction == NULL) {
        throw runtime_error("code_gen::There is no main funtion");
    }

    LabelMap labelMap = createLabelMap();
    EntryPoint* entryPoint = createEntryPoint();
    auto stream = entryPoint->ins;
    auto constantPool = entryPoint->constantPool;

    for (auto functionDef : *symbolTable->fSymbolTable->table) {

        // We cannot generate code for this 
        if (functionDef.second->needsPolyBinding) {
            continue;
        }
        gen_code_for_function_def(stream, &labelMap, symbolTable, functionDef.second, constantPool);
    }

    // Resolve lables that could not be resolved
    // If something cannot be resolved there should be some assertion error 
    for (auto labelsToResolve : *labelMap.instructionLabelsToResolve) {
        if (labelMap.labelIpMap->count(labelsToResolve->symbol) == 0) {
            assert_never_reaches("label map does not have a resolution for symbol :: " + labelsToResolve->symbol + " :: This should have bee resolved as part of code gen");
        }
        int instructioPointer = labelMap.labelIpMap->at(labelsToResolve->symbol);
        labelsToResolve->arg1 = instructioPointer;
    }

    auto ip = getLableIp(&labelMap, "main");
    entryPoint->mainIp = ip;

    stream->lastInsIdx = stream->instructions->size() - 1;
    stream->startInsIndx = entryPoint->mainIp;
    // We should have resolved all breaks
    assert(labelMap.resolveBreaks->empty());
    assert(labelMap.resolveContinues->empty());
    return entryPoint;
}

void modifyJumpTarget(Instruction* jmp, int targetAddress) {
    jmp->arg1 = targetAddress;
}

void modifyJumpTargetToCurOff(Instruction* jmp, InstructionStream* stream) {
    jmp->arg1 = stream->instructions->size();
}

void gen_code_for_function_args(InstructionStream* stream,
    LabelMap* labelMap,
    SymbolTable* symbolTable,
    AstFunctionCall* functionCall,
    FunctionSymbol* enclosingFunctionSymbol,
    CodeGenConstantPool* constantPool) {
    for (int i = functionCall->arguments->size() - 1; i >= 0; i--) {
        gen_internal(stream, labelMap, symbolTable, functionCall->arguments->at(i), enclosingFunctionSymbol, constantPool);
    }
}

void gen_try_catch(InstructionStream* stream, LabelMap* labelMap, SymbolTable* symbolTable, AstFunctionCall* functionCall, FunctionSymbol* enclosingFunctionSymbol, CodeGenConstantPool* constPool) {
    auto functionSymbol = getFunctionSymbol(symbolTable, functionCall->name);
    assert(functionSymbol != NULL);
    gen_code_for_function_args(stream, labelMap, symbolTable, functionCall, enclosingFunctionSymbol, constPool);
    auto tryCatch = emit(stream, Opcode::CALL_WITH_TRY_CATCH);
    tryCatch->symbol = functionCall->name;
    tryCatch->arg1 = getInstructionPointerFromLabel(labelMap, tryCatch);

    return;
}

void gen_store(InstructionStream* stream, AstAssignementStatement* assignment) {
    auto store_ins = emit(stream, Opcode::STORE);
    store_ins->symbol = assignment->identifier->name;
    if (assignment->identifier->sub != NULL) {
        assert(assignment->identifier->sub->type == ScopedIdentifier);
        // There should only be on substitution unless I have done something silly 
        assert(assignment->identifier->sub->sub == NULL);
        AstScopedIdentifier* scopedIdenfier = (AstScopedIdentifier*)assignment->identifier->sub;
        store_ins->arg1 = scopedIdenfier->call_stack_difference;
    }
}

void gen_lr_type_transform(InstructionStream* stream, LabelMap* labelMap, SymbolTable*
    symbolTable, AstNode* left_node,
    AstNode* right_node,
    Type* left_type,
    Type* right_type,
    FunctionSymbol* enclosingFunctionSymbol,
    CodeGenConstantPool* constantPool) {

    LRTypeOrder typeOrder = getLRPrimitiveTypeTransformation(left_type, right_type);

    // TODO :: bad_match seems weird here but I am saying that no type comparison needs to take palce
    // This is the case for reference comparisoon
    if (typeOrder == same || typeOrder == bad_match) {
        gen_internal(stream, labelMap, symbolTable, left_node, enclosingFunctionSymbol, constantPool);
        gen_internal(stream, labelMap, symbolTable, right_node, enclosingFunctionSymbol, constantPool);
    }
    else if (typeOrder == left_d_right_i) {
        gen_internal(stream, labelMap, symbolTable, left_node, enclosingFunctionSymbol, constantPool);
        gen_internal(stream, labelMap, symbolTable, right_node, enclosingFunctionSymbol, constantPool);
        emit(stream, Opcode::I2D);
    }
    else if (typeOrder == left_i_right_d) {
        // Transform left to double 
        gen_internal(stream, labelMap, symbolTable, left_node, enclosingFunctionSymbol, constantPool);
        emit(stream, Opcode::I2D);
        gen_internal(stream, labelMap, symbolTable, right_node, enclosingFunctionSymbol, constantPool);
    }
    else if (typeOrder == left_c_right_i) {
        // Transform left to int 
        gen_internal(stream, labelMap, symbolTable, left_node, enclosingFunctionSymbol, constantPool);
        emit(stream, Opcode::C2I);
        gen_internal(stream, labelMap, symbolTable, right_node, enclosingFunctionSymbol, constantPool);
    }

    else if (typeOrder == left_i_right_c) {
        // Transform right to int 
        gen_internal(stream, labelMap, symbolTable, left_node, enclosingFunctionSymbol, constantPool);
        gen_internal(stream, labelMap, symbolTable, right_node, enclosingFunctionSymbol, constantPool);
        emit(stream, Opcode::C2I);
    }
    else {
        assert_never_reaches("gen_lr_type_transform type order case missing");
    }
}

void gen_load_for_scoped_identifier(InstructionStream* stream, AstNode* node) {
    assert(node->type == ScopedIdentifier);
    AstScopedIdentifier* scoped_identifier = (AstScopedIdentifier*)node;
    // The code transformer should not have added any more transformations;
    assert(scoped_identifier->sub == NULL);
    auto load_ins = emit(stream, Opcode::LOAD_I);
    load_ins->symbol = scoped_identifier->name;
    load_ins->arg1 = scoped_identifier->call_stack_difference;
}

void find_prev_for_block_index(InstructionStream * stream) {
    int index = stream->instructions->size() - 1;
    while (index > 0) {
        auto opcode = stream->instructions->at(index);
        if (opcode->op_code == Opcode::ENTER_FOR_BLOCK) {
            return;
        }
        index--;
    }
    assert_never_reaches("Break statement cannot find for block its connected to, the type checker should have prevented this");
}

void gen_code_for_subscripts(InstructionStream * stream, LabelMap* labelMap, SymbolTable* symbolTable, AstArrayAccess* arr_access, FunctionSymbol* enclosingFunctionSymbol, CodeGenConstantPool* constantPool) {
    // Do it reverse order, the way the stack will be in correct order 
    for (int i = arr_access->subScripts->size() - 1; i >= 0; i--) {
        auto subscriptExpression = arr_access->subScripts->at(i);
        gen_internal(stream, labelMap, symbolTable, subscriptExpression->expression, enclosingFunctionSymbol, constantPool);
    }
}

void gen_code_for_array_access(InstructionStream * stream, LabelMap* labelMap, SymbolTable* symbolTable, AstArrayAccess* arr_access, FunctionSymbol* enclosingFunctionSymbol, CodeGenConstantPool* constantPool) {
    auto arrayAccess = (AstArrayAccess*)arr_access;

    gen_code_for_subscripts(stream, labelMap, symbolTable, arr_access, enclosingFunctionSymbol, constantPool);

    if (arrayAccess->identifier->sub == NULL) {
        auto loadRootObjIns = emit(stream, Opcode::LOAD_I);
        loadRootObjIns->symbol = arrayAccess->identifier->name;
    }
    else {
        gen_load_for_scoped_identifier(stream, arrayAccess->identifier->sub);
    }

    Instruction* load_arr_index = emit(stream, Opcode::LOAD_ARR_I);
    load_arr_index->arg1 = arrayAccess->subScripts->size(); // Add number of dimensions accesssed
    return;
}

void gen_code_for_dot_access(InstructionStream* stream, LabelMap* labelMap, SymbolTable* symbolTable,
    AstDotAccess* astDotAccess,
    size_t num_accesses,
    FunctionSymbol* enclosingFunctionSymbol,
    CodeGenConstantPool* constantPool) {

    if (num_accesses > astDotAccess->accesses->size()) {
        throw runtime_error("gen_code_for_dot_access:: num access cannot be greater than the number of dot accesses in the node");
    }

    auto rootAccess = astDotAccess->accesses->at(0);
    if (rootAccess->sub == NULL) {
        auto loadRootObjIns = emit(stream, Opcode::LOAD_I);
        loadRootObjIns->symbol = rootAccess->identifier_access->name;
    }
    else {
        assert(rootAccess->sub->type == ScopedIdentifier);
        gen_internal(stream, labelMap, symbolTable, rootAccess->sub, enclosingFunctionSymbol, constantPool);
    }
    Type* rootType = loadTypeOfNode(symbolTable, enclosingFunctionSymbol, rootAccess);
    Type* resolvedType = rootType;
    for (size_t i = 1; i < num_accesses; i++) {
        if (isTypeArray(resolvedType)) {
            int num_accesses_left = num_accesses - i;
            assert(num_accesses_left == 1);
            assert(astDotAccess->accesses->back()->identifier_access->name == "len");
            emit(stream, Opcode::LOAD_ARR_LEN);
            return;
        }

        StructLayout* structLayout = searchStructLayout(symbolTable, resolvedType->toString());

        auto accessNode = astDotAccess->accesses->at(i);
        Field* field_match = NULL;
        for (auto field : *structLayout->fields) {
            if (accessNode->identifier_access->name == field->name) {
                field_match = field;
                break;
            }
        }
        if (accessNode->arr_access != NULL) {
            // Note (umar) we cannot call gen_code_for_arr_access since that works for non-dotaccesses
            // We have to roll the generated code ourselves here 

            // TODO :: Get rid of the store <tmp> hack 
            auto load_field_ins = emit(stream, Opcode::LOAD_FIELD);
            load_field_ins->arg1 = field_match->mem_offset;

            auto store_ins = emit(stream, Opcode::STORE);
            store_ins->symbol = "<tmp>";

            auto arrayAccess = accessNode->arr_access;
            gen_code_for_subscripts(stream, labelMap, symbolTable, arrayAccess, enclosingFunctionSymbol, constantPool);

            auto load_tmp = emit(stream, Opcode::LOAD_I);
            load_tmp->symbol = store_ins->symbol;

            Instruction* load_arr_index = emit(stream, Opcode::LOAD_ARR_I);
            load_arr_index->arg1 = arrayAccess->subScripts->size(); // Add number of dimensions accesssed
        }
        else {
            auto load_field_ins = emit(stream, Opcode::LOAD_FIELD);
            load_field_ins->arg1 = field_match->mem_offset;
        }

        if (i != astDotAccess->accesses->size() - 1) {
            resolvedType = field_match->returnType;
        }
    }
}

void gen_code_for_dot_access(InstructionStream* stream, LabelMap* labelMap, SymbolTable* symbolTable,
    AstDotAccess* astDotAccess,
    FunctionSymbol* enclosingFunctionSymbol,
    CodeGenConstantPool* constantPool) {
    gen_code_for_dot_access(stream, labelMap, symbolTable, astDotAccess, astDotAccess->accesses->size(), enclosingFunctionSymbol, constantPool);
}

void gen_internal(InstructionStream * stream, LabelMap * labelMap, SymbolTable * symbolTable, AstNode * node, FunctionSymbol* enclosingFunctionDef, CodeGenConstantPool* constantPool) {

    if (node == NULL) {
        return;
    }

    // If there is a substition node we want to ingore the initial node and sub that
    if (node->sub != NULL) {
        gen_internal(stream, labelMap, symbolTable, node->sub, enclosingFunctionDef, constantPool);
        return;
    }

    if (node->type == Enum) {
        assert(true == false); // Should never be called from here 
        return;
    }

    if (node->type == FunctionDefinition || node->type == PolyClonedFunctionDefinition) {
        // For nested functions we do not generate any code 
        // This is because the the function definition is added as part of the function symbl table and the code will be generated already from that
        return;
    }
    else if (node->type == Body) {
        AstBody* body = (AstBody*)node;
        for (auto statement : *body->statements) {
            // For nested function calls we want to jump over it 
            if (statement->type == FunctionDefinition) {
                Instruction* jpm = emit(stream, Opcode::JUMP);
                gen_internal(stream, labelMap, symbolTable, statement, enclosingFunctionDef, constantPool);
                modifyJumpTargetToCurOff(jpm, stream);
            }
            else {
                gen_internal(stream, labelMap, symbolTable, statement, enclosingFunctionDef, constantPool);
            }
        }
        return;
    }
    else if (node->type == Return) {
        AstReturnStatement* returnStatement = (AstReturnStatement*)node;
        // We will just assume that a return statment with no expression from a void function
        if (returnStatement->expression == NULL) {
            assert(isReturnTypeVoid(enclosingFunctionDef->returnType));
            if (enclosingFunctionDef->markedAsThrows) {
                emit(stream, Opcode::LOAD_NULL);
            }
            emit(stream, Opcode::RETURN_FROM_VOID);
        }
        else {
            assert(!isReturnTypeVoid(enclosingFunctionDef->returnType));
            if (enclosingFunctionDef->markedAsThrows) {
                emit(stream, Opcode::LOAD_NULL);
            }
            gen_internal(stream, labelMap, symbolTable, returnStatement->expression, enclosingFunctionDef, constantPool);
            emit(stream, Opcode::RETURN);
        }
        return;
    }
    else if (node->type == Defer) {
        // The defere has a target of what to skip, the interprete would load this and then jump
        // When the frame is exited, it would then just just load the instruction frame 
        // with start address target branch and end address end branch 
        AstDefer* astDefer = (AstDefer*)node;
        Instruction* deferins = emit(stream, Opcode::DEFER);
        deferins->arg1 = getCurrentOff(stream); // Start of defere
        gen_internal(stream, labelMap, symbolTable, astDefer->defferedFunctionCall, enclosingFunctionDef, constantPool);
        emit(stream, Opcode::DEFER_FINISH); // This is a cheeky hack to stop the interpeter
        deferins->arg2 = getCurrentOff(stream); // end of defer
        return;
    }

    else if (node->type == IfStatement) {
        AstIfStatement* ifStatment = (AstIfStatement*)node;
        gen_internal(stream, labelMap, symbolTable, ifStatment->booleanExpression, enclosingFunctionDef, constantPool);
        Instruction* firstIfJump = emit(stream, Opcode::JUMP_IF_FALSE);
        gen_internal(stream, labelMap, symbolTable, ifStatment->ifbody, enclosingFunctionDef, constantPool);
        Instruction* firstJumpInsideIfBody = emit(stream, Opcode::JUMP);

        vector<Instruction*> instructionJmpFromIfElseBodyToModify;
        instructionJmpFromIfElseBodyToModify.push_back(firstJumpInsideIfBody);

        Instruction* jumpTargetOfPreviousIfElse = firstIfJump;
        for (auto ifElse : *ifStatment->ifElses) {
            // 1. first modify jump instruction from previous if/ifelse block
            modifyJumpTargetToCurOff(jumpTargetOfPreviousIfElse, stream);
            // 2. gen_internal body of if expression
            gen_internal(stream, labelMap, symbolTable, ifElse->booleanExpression, enclosingFunctionDef, constantPool);
            // 3. generate jump target of if expression
            Instruction* jumpTargetOfIfElse = emit(stream, Opcode::JUMP_IF_FALSE);
            jumpTargetOfPreviousIfElse = jumpTargetOfIfElse;
            // 4. generated jump of if body 
            gen_internal(stream, labelMap, symbolTable, ifElse->ifbody, enclosingFunctionDef, constantPool);
            Instruction* jumpIns = emit(stream, Opcode::JUMP);
            instructionJmpFromIfElseBodyToModify.push_back(jumpIns);  // We dont know where these instruction should go 
        }

        modifyJumpTargetToCurOff(jumpTargetOfPreviousIfElse, stream);
        if (ifStatment->elseBody != NULL) {
            gen_internal(stream, labelMap, symbolTable, ifStatment->elseBody, enclosingFunctionDef, constantPool);
        }
        // Update all jumps from all the if elses bodies
        for (auto instruction : instructionJmpFromIfElseBodyToModify) {
            modifyJumpTargetToCurOff(instruction, stream);
        }
        return;
    }

    else if (node->type == ArraySubscriptAssignment) {
        auto arrSubscriptAssignment = (AstArraySubscriptAssignment*)node;
        gen_internal(stream, labelMap, symbolTable, arrSubscriptAssignment->expression, enclosingFunctionDef, constantPool);
        auto arrayAccess = arrSubscriptAssignment->arrAccess;
        // Do it reverse order, the way the stack will be in correct order 
        for (int i = arrayAccess->subScripts->size() - 1; i >= 0; i--) {
            auto subscriptExpression = arrayAccess->subScripts->at(i);
            gen_internal(stream, labelMap, symbolTable, subscriptExpression->expression, enclosingFunctionDef, constantPool);
        }
        if (arrSubscriptAssignment->arrAccess->identifier->sub == NULL) {
            auto loadArrIns = emit(stream, Opcode::LOAD_I);
            loadArrIns->symbol = arrayAccess->identifier->name;
        }
        else {
            gen_load_for_scoped_identifier(stream, arrSubscriptAssignment->arrAccess->identifier->sub);
        }
        auto store_ins = emit(stream, Opcode::STORE_ARR_I); // I need the symbol and the number of dimensions to access 
        store_ins->arg1 = arrayAccess->subScripts->size();
        return;
    }

    else if (node->type == AnonymousFunction) {
        assert_never_reaches("anonymous functions should have been transformed into a closure call and should not be code generated directly");
    }

    else if (node->type == NoRoutine) {
        AstNoRoutine* astCoRoutine = (AstNoRoutine*)node;
        assert(astCoRoutine->body->type == AnonymousFunction);
        assert(astCoRoutine->body->sub->type == ClosureCreation);
        assert(astCoRoutine->body->type == AnonymousFunction);
        AstAnonFunction* anonFunction = (AstAnonFunction*)astCoRoutine->body;
        FunctionSymbol* anonFunctionSymbol = getFunctionSymbol(symbolTable, anonFunction->private_name);
        AstClosureCreation* closureCreation = (AstClosureCreation*)astCoRoutine->body->sub;

        // 1. create closure call on running thread , the astCoRoutine body would have been transformed into a closure creation  
        gen_internal(stream, labelMap, symbolTable, closureCreation, enclosingFunctionDef, constantPool);
        // 2. Store closure call on the running thread in stack storage 
        // 3. Fork it, load the closure and call the closure 
        auto forkIns = emit(stream, Opcode::FORK);
        forkIns->arg1 = anonFunctionSymbol->params->size();

        for (auto param : *anonFunctionSymbol->params) {
            auto load_identifier = emit(stream, Opcode::LOAD_I);
            load_identifier->symbol = param->name;
            load_identifier->arg1 = param->scoping_info.num_static_links;
        }

        auto loadFunction = emit(stream, Opcode::LOAD_I);
        loadFunction->symbol = noRoutineStorageName;
        emit(stream, Opcode::CALL_LAMBDA);
        forkIns->arg2 = getCurrentOff(stream);
        return;
    }

    else if (node->type == BreakStatement) {
        find_prev_for_block_index(stream);
        auto break_ins = emit(stream, Opcode::JUMP);
        auto resolveBreaks = top_resolve_breaks(labelMap);
        resolveBreaks->push(break_ins);
        return;
    }

    else if (node->type == ContinueStatement) {
        find_prev_for_block_index(stream);
        Instruction* continue_ins = emit(stream, Opcode::JUMP);
        auto resolveContinues = top_resolve_continues(labelMap);
        resolveContinues->push(continue_ins);
        return;
    }

    else if (node->type == TryCatchAssignment) {
        AstTryCatchAssignmentStatement* tryCatchAssignement = (AstTryCatchAssignmentStatement*)node;
        assert(tryCatchAssignement->assignment->rightSide->type == FunctionCall);
        AstFunctionCall* functionCall = (AstFunctionCall*)tryCatchAssignement->assignment->rightSide;
        if (functionCall->sub != NULL) {
            assert(functionCall->sub->type == FunctionCall);
            gen_try_catch(stream, labelMap, symbolTable, (AstFunctionCall*)functionCall->sub, enclosingFunctionDef, constantPool);
        }
        else {
            gen_try_catch(stream, labelMap, symbolTable, functionCall, enclosingFunctionDef, constantPool);
        }

        gen_store(stream, tryCatchAssignement->assignment);
        return;
    }

    else if (node->type == Assignment) {
        AstAssignementStatement* assignment = (AstAssignementStatement*)node;
        gen_internal(stream, labelMap, symbolTable, assignment->rightSide, enclosingFunctionDef, constantPool);

        auto left_type = loadTypeOfNode(symbolTable, enclosingFunctionDef, assignment);
        auto right_type = loadTypeOfNode(symbolTable, enclosingFunctionDef, assignment->rightSide);

        LRTypeOrder typeOrder = getLRPrimitiveTypeTransformation(left_type, right_type);
        if (typeOrder == left_d_right_i) {
            emit(stream, Opcode::I2D);
        }
        else if (typeOrder == left_i_right_d) {
            emit(stream, Opcode::D2I);
        }
        else if (typeOrder == left_i_right_c) {
            emit(stream, Opcode::C2I);
        }
        else if (typeOrder == left_c_right_i) {
            emit(stream, Opcode::I2C);
        }
        gen_store(stream, assignment);
        return;
    }

    else if (node->type == ForLoop) {
        AstForLoop* forLoop = (AstForLoop*)node;

        push_new_resolve_breaks_frame(labelMap);
        push_new_resolve_continues_frame(labelMap);

        gen_internal(stream, labelMap, symbolTable, forLoop->assignment, enclosingFunctionDef, constantPool);

        int conditionalPp = getCurrentOff(stream); // conditionalPp ( condtional program pointer )
        gen_internal(stream, labelMap, symbolTable, forLoop->conditional, enclosingFunctionDef, constantPool);
        Instruction* jumpTargetOfIf = emit(stream, Opcode::JUMP_IF_FALSE);

        emit(stream, Opcode::ENTER_FOR_BLOCK);
        gen_internal(stream, labelMap, symbolTable, forLoop->body, enclosingFunctionDef, constantPool);

        size_t post_condition_start = getCurrentOff(stream);
        gen_internal(stream, labelMap, symbolTable, forLoop->postCondition, enclosingFunctionDef, constantPool);
        Instruction* jmpBackToCond = emit(stream, Opcode::JUMP);

        modifyJumpTarget(jmpBackToCond, conditionalPp);
        modifyJumpTargetToCurOff(jumpTargetOfIf, stream);

        auto resolve_breaks_stack = top_resolve_breaks(labelMap);
        while (!resolve_breaks_stack->empty()) {
            auto breakIns = resolve_breaks_stack->top();
            breakIns->arg1 = jumpTargetOfIf->arg1;
            resolve_breaks_stack->pop();
        }
        pop_resolve_breaks(labelMap);

        auto resolve_continues_stack = top_resolve_continues(labelMap);
        while (!resolve_continues_stack->empty()) {
            Instruction* continueins = resolve_continues_stack->top();
            modifyJumpTarget(continueins, post_condition_start);
            resolve_continues_stack->pop();
        }
        pop_resolve_continues(labelMap);

        return;
    }
    else if (node->type == Null) {
        emit(stream, Opcode::LOAD_NULL);
        return;
    }
    else if (node->type == Try) {
        AstTryStatement* tryStatement = (AstTryStatement*)node;
        gen_internal(stream, labelMap, symbolTable, tryStatement->functionCall, enclosingFunctionDef, constantPool);
        return;
    }
    else if (node->type == Throw) {
        AstThrow* throwNode = (AstThrow*)node;
        gen_internal(stream, labelMap, symbolTable, throwNode->functionCall, enclosingFunctionDef, constantPool);
        emit(stream, Opcode::THROW);
        return;
    }
    else if (node->type == Tuple) {
        AstTupleError* tuple = (AstTupleError*)node;
        gen_try_catch(stream, labelMap, symbolTable, tuple->rightSide, enclosingFunctionDef, constantPool);
        auto first = emit(stream, Opcode::STORE); // Store from top of stack 
        first->symbol = tuple->tuple->at(0);
        auto second = emit(stream, Opcode::STORE); // Store from top of frame 
        second->symbol = tuple->tuple->at(1);
        return;
    }

    else if (node->type == LambdaCall) {
        AstLambaCall* lambdaCall = (AstLambaCall*)node;
        for (int i = lambdaCall->functionCall->arguments->size() - 1; i >= 0; i--) {
            gen_internal(stream, labelMap, symbolTable, lambdaCall->functionCall->arguments->at(i), enclosingFunctionDef, constantPool);
        }
        auto loadFunction = emit(stream, Opcode::LOAD_I);
        loadFunction->symbol = lambdaCall->functionCall->name;
        loadFunction->arg1 = lambdaCall->call_stacks_to_walk_up;
        emit(stream, Opcode::CALL_LAMBDA);
        return;
    }

    else if (node->type == ClosureCreation) {
        AstClosureCreation* closureCreation = (AstClosureCreation*)node;
        auto loadImmediate = emit(stream, Opcode::LOAD_IMMEDIATE);
        loadImmediate->symbol = closureCreation->escapingFunctionName;
        loadImmediate->arg1 = getInstructionPointerFromLabel(labelMap, loadImmediate);
        auto callIns = emit(stream, Opcode::CALL);
        callIns->symbol = CLOSURE_CREATE_SL_F_NAME;
        callIns->arg1 = getInstructionPointerFromLabel(labelMap, callIns);
        return;
    }

    else if (node->type == FunctionCall) {
        AstFunctionCall* functionCall = (AstFunctionCall*)node;
        FunctionSymbol* functionSymbol = getFunctionSymbol(symbolTable, functionCall->name);

        gen_code_for_function_args(stream, labelMap, symbolTable, functionCall, enclosingFunctionDef, constantPool);
        if (functionSymbol->name == PRINT_F_NAME) {
            auto num_args = emit(stream, Opcode::LOAD_IMMEDIATE);
            // -1 because first argumnt always has to be supplied
            num_args->arg1 = functionCall->arguments->size() - 1;
        }

        auto callIns = emit(stream, Opcode::CALL);
        callIns->symbol = functionCall->name;
        callIns->arg1 = getInstructionPointerFromLabel(labelMap, callIns);

        return;
    }
    else if (node->type == Identifier) {
        AstIdentifier* identifier = (AstIdentifier*)node;
        auto load_ins = emit(stream, Opcode::LOAD_I);
        load_ins->symbol = identifier->name;
        load_ins->arg1 = 0;
        return;
    }
    else if (node->type == ScopedIdentifier) {
        AstScopedIdentifier* scoped_identifier = (AstScopedIdentifier*)node;
        auto load_ins = emit(stream, Opcode::LOAD_I);
        load_ins->symbol = scoped_identifier->name;
        load_ins->arg1 = scoped_identifier->call_stack_difference;
        return;
    }
    else if (node->type == Literal) {
        AstLiteralValue* literal = (AstLiteralValue*)node;
        if (literal->valueType == StringLiteral) {
            auto const_ins = emit(stream, Opcode::LOAD_NON_NATIVE_STR_CONST);
            int index = createStrConst(constantPool, literal);
            const_ins->arg1 = index;
        }
        else {
            auto const_ins = emit(stream, Opcode::LOAD_CONST);
            int index = createConst(constantPool, literal);
            const_ins->arg1 = index;
        }
        return;
    }
    else if (node->type == ObjectCreation) {
        auto objectCreation = (AstObjectCreation*)node;

        if (objectCreation->typeDecl->isArray) {
            auto dimensions = objectCreation->typeDecl->arr_dimensions;

            for (int i = dimensions->size() - 1; i >= 0; i--) {
                auto dimension = dimensions->at(i);
                if (dimension->isSizeKnown) {
                    auto load_cons = emit(stream, Opcode::LOAD_CONST);
                    assert(dimension->dimensionExpression->type == Literal);
                    AstLiteralValue* literal = (AstLiteralValue*)dimension->dimensionExpression;
                    load_cons->arg1 = createConst(constantPool, literal);
                }
                else {
                    gen_internal(stream, labelMap, symbolTable, dimension->dimensionExpression, enclosingFunctionDef, constantPool);
                }
            }

            auto new_arr_ins = emit(stream, Opcode::NEW_ARR);
            auto type_of_node = loadTypeOfNode(symbolTable, enclosingFunctionDef, objectCreation);
            assert(type_of_node->type_of_type == TypeOfType::ArrayType);
            ArrayType* arrType = (ArrayType*)type_of_node;
            if (isTypeLambda(arrType->baseType)) {
                new_arr_ins->symbol = internal_closure_struct_runtime_name();
            }
            else {
                new_arr_ins->symbol = getTypeDefinitionByName(symbolTable, objectCreation->typeDecl->typeStr)->toString();
            }

            new_arr_ins->arg1 = dimensions->size();
            return;
        }

        auto new_arr_ins = emit(stream, Opcode::NEW);
        auto type_of_object = loadTypeOfNode(symbolTable, enclosingFunctionDef, objectCreation);
        StructLayout* structLayout = searchStructLayout(symbolTable, type_of_object->toString());
        assert(structLayout != NULL);
        new_arr_ins->symbol = type_of_object->toString();
        new_arr_ins->arg1 = structLayout->identifier_index;
        return;
    }
    else if (node->type == DotAccess) {
        auto astDotAccess = (AstDotAccess*)node;
        // There has to be at least a min of two access eg x.y;
        assert(astDotAccess->accesses->size() > 1);
        gen_code_for_dot_access(stream, labelMap, symbolTable, astDotAccess, enclosingFunctionDef, constantPool);
        return;
    }
    else if (node->type == DotAssignment) {
        // Dot assignment is the same as dot access 
        auto astDotAssignment = (AstDotAssignment*)node;
        auto astDotAccess = astDotAssignment->dotAccess;
        gen_internal(stream, labelMap, symbolTable, astDotAssignment->expression, enclosingFunctionDef, constantPool);

        // If dot access is line this [x.y.z] 
        // Then positions look like this [0, n-2, n-1] 
        int num_dot_access = astDotAccess->accesses->size() - 1;
        gen_code_for_dot_access(stream, labelMap, symbolTable, astDotAssignment->dotAccess, num_dot_access, enclosingFunctionDef, constantPool);

        // We have to generated code for the last dot access ourselves in case last dot access is array assignment
        auto second_last = astDotAccess->accesses->size() - 2;
        auto second_last_dot_access = astDotAssignment->dotAccess->accesses->at(second_last);
        Type* type_of_second_last_dot_access = loadTypeOfNode(symbolTable, enclosingFunctionDef, second_last_dot_access);
        StructLayout* structLayout = searchStructLayout(symbolTable, type_of_second_last_dot_access->toString());
        if (structLayout == NULL) {
            throw runtime_error("Struct layout cannot be found for dot acces at position[n-1]");
        }

        auto final_access_node = astDotAccess->accesses->back();
        Field * final_field = NULL;
        for (auto field : *structLayout->fields) {
            if (final_access_node->identifier_access->name == field->name) {
                final_field = field;
                break;
            }
        }
        assert(final_field != NULL);
        if (final_access_node->arr_access != NULL) {
            auto load_field_ins = emit(stream, Opcode::LOAD_FIELD);
            load_field_ins->arg1 = final_field->mem_offset;

            auto store_ins = emit(stream, Opcode::STORE);
            store_ins->symbol = "<tmp>";

            auto arrayAccess = final_access_node->arr_access;
            gen_code_for_subscripts(stream, labelMap, symbolTable, arrayAccess, enclosingFunctionDef, constantPool);

            auto load_tmp = emit(stream, Opcode::LOAD_I);
            load_tmp->symbol = store_ins->symbol;

            auto store_arr_ins = emit(stream, Opcode::STORE_ARR_I);
            store_arr_ins->arg1 = arrayAccess->subScripts->size();
        }

        else {
            auto load_field_ins = emit(stream, Opcode::STORE_FIELD);
            load_field_ins->arg1 = final_field->mem_offset;
        }
        return;
    }

    else if (node->type == ArrayAccess) {
        auto arrayAccess = (AstArrayAccess*)node;
        gen_code_for_array_access(stream, labelMap, symbolTable, arrayAccess, enclosingFunctionDef, constantPool);
        return;
    }
    else if (node->type == ArithmeticExpression) {
        AstArithMeticExpression* expression = (AstArithMeticExpression*)node;
        // todo :: get symbol table 
        Type* left_type = loadTypeOfNode(symbolTable, enclosingFunctionDef, expression->left);
        Type* right_type = loadTypeOfNode(symbolTable, enclosingFunctionDef, expression->right);
        gen_lr_type_transform(stream, labelMap, symbolTable, expression->left, expression->right, left_type, right_type, enclosingFunctionDef, constantPool);

        if (expression->op == "+") {
            emit(stream, Opcode::ADD);
        }
        else if (expression->op == "-") {
            emit(stream, Opcode::SUBTRACT);
        }
        else if (expression->op == "*") {
            emit(stream, Opcode::MULTIPLY);
        }
        else if (expression->op == "/") {
            emit(stream, Opcode::DIVIDE);
        }

        else if (expression->op == "%") {
            emit(stream, Opcode::MOD);
        }
        return;
    }

    else if (node->type == NegativeExpression) {
        AstNegativeExpression* negative = (AstNegativeExpression*)node;
        gen_internal(stream, labelMap, symbolTable, negative->expression, enclosingFunctionDef, constantPool);
        auto immediate = emit(stream, Opcode::LOAD_IMMEDIATE);
        immediate->arg1 = -1;
        emit(stream, Opcode::MULTIPLY);
        return;
    }

    else if (node->type == BooleanExpression) {
        AstBooleanExpression* booleanExp = (AstBooleanExpression*)node;
        Type* left_type = loadTypeOfNode(symbolTable, enclosingFunctionDef, booleanExp->left);
        Type* right_type = loadTypeOfNode(symbolTable, enclosingFunctionDef, booleanExp->right);

        gen_lr_type_transform(stream, labelMap, symbolTable, booleanExp->left, booleanExp->right, left_type, right_type, enclosingFunctionDef, constantPool);

        if (booleanExp->op == "&&") {
            emit(stream, Opcode::AND);
        }
        else if (booleanExp->op == "||") {
            emit(stream, Opcode::OR);
        }
        else if (booleanExp->op == "<") {
            emit(stream, Opcode::LESS_THAN);
        }
        else if (booleanExp->op == "<=") {
            emit(stream, Opcode::LESS_THAN_EQUAL);
        }
        else if (booleanExp->op == ">") {
            emit(stream, Opcode::MORE_THAN);
        }
        else if (booleanExp->op == "<") {
            emit(stream, Opcode::LESS_THAN);
        }
        else if (booleanExp->op == ">=") {
            emit(stream, Opcode::MORE_THAN_EQUAL);
        }
        else if (booleanExp->op == "!=") {
            emit(stream, Opcode::NOT_EQUAL);
        }
        else if (booleanExp->op == "==") {
            emit(stream, Opcode::EQUAL);
        }
        return;
    }
    throw runtime_error("node type missing. you forgot to add a statement");
}