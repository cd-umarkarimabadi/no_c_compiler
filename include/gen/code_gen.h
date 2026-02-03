#ifndef CODE_GEN_H
#define CODE_GEN_H

#include "type_checker/type_checker.h"
#include "ast/ast.h"

extern string noRoutineStorageName;

enum struct Opcode {
    LOAD_ARR_LEN,
    LOAD_I,
    LOAD_I_UNBOXED,
    STORE,
    LOAD_NON_NATIVE_STR_CONST,
    LOAD_CONST,
    LOAD_IMMEDIATE,
    LOAD_NULL,
    ADD,
    SUBTRACT,
    MULTIPLY,
    MOD,
    DIVIDE,
    D2I,
    I2D,
    C2I, // TODO :: We can remove the char object and just use the int runtime object 
    I2C, // TODO :: We can remove the char object and just use the int runtime object 
    THROW,
    NEW_ARR,
    NEW,
    RETURN,
    RETURN_FROM_VOID,
    JUMP_IF_FALSE,
    JUMP,
    DEFER,
    DEFER_FINISH,
    CALL,
    CALL_LAMBDA,
    CALL_WITH_TRY_CATCH,
    LABEL,
    OR,
    AND,
    LESS_THAN,
    LESS_THAN_EQUAL,
    MORE_THAN,
    MORE_THAN_EQUAL,
    EQUAL,
    NOT_EQUAL,
    LOAD_FIELD,
    STORE_FIELD,
    STORE_ARR_I,
    LOAD_ARR_I,
    CONTINUE,
    ENTER_FOR_BLOCK,
    FORK,
};

void toString(Opcode opcode);

struct Instruction {
public:
    Opcode op_code;
    string symbol; // optional cheeky hack, runtime will use to using numbers later
    int arg1;
    int arg2;
};

struct InstructionStream {
public:
    vector<Instruction*>* instructions;
    int startInsIndx;
    int lastInsIdx; // last instruction index
};


struct CodeGenConstantPool {
public:
    vector<AstLiteralValue*>* constantPool;
    vector<string>* str_constantPool;
};

struct EntryPoint {
public:
    int mainIp;
    InstructionStream* ins;
    CodeGenConstantPool* constantPool;
};

InstructionStream* createInstructionStream();
void print(InstructionStream* stream);
EntryPoint* gen(SymbolTable* symbolTable);
InstructionStream* createInsStreamFromOffset(InstructionStream* ins, int offset, int lastInsIndx);

#endif

