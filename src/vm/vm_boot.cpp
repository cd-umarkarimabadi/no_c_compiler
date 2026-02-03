#include "vm/vm_boot.h"
#include "noCThread/noCThread.h"
#include "scheduler/scheduler.h"
#include "vm/noC_vm.h"
#include "vm/runtime_obj.h"
#include "noCThread/noCThread.h"

void createConsantsFromSymbolTable(
    SymbolTable* symbolTable,
    ConstantPool * constantPool,
    CodeGenConstantPool* codeGenConstPool,
    Heap * constant_heap) {

    // TODO :: There is no thread local for the workers and threads since nothing is running at this point
    // If constant heap allocation triggers garbage collection things will break 

    // It is best just to move this to a constant space allocator/ bump allocator   
    falseBoolean = createBoolRuntime(constant_heap, false);
    trueBoolean = createBoolRuntime(constant_heap, true);

    for (auto constantNode : *codeGenConstPool->constantPool) {
        auto constant = (AstLiteralValue*)constantNode;
        NoCRuntimeObjectContainer* runtimeConstant = NULL;
        if (constant->valueType == NumberLiteral) {
            runtimeConstant = (NoCRuntimeObjectContainer*)createIntRuntime(constant_heap, constant->literalValue->intValue);
        }

        if (constant->valueType == BooleanLiteral) {
            runtimeConstant = (NoCRuntimeObjectContainer*)createBoolRuntime(constant_heap, constant->literalValue->boolValue);
        }

        if (constant->valueType == DoubleLiteral) {
            runtimeConstant = (NoCRuntimeObjectContainer*)createDoubleRuntime(constant_heap, constant->literalValue->doubleValue);
        }

        if (constant->valueType == CharLiteral) {
            runtimeConstant = (NoCRuntimeObjectContainer*)createCharRuntime(constant_heap, constant->literalValue->strValue[0]);
        }

        assert(runtimeConstant != NULL);
        constantPool->constantPool->push_back(runtimeConstant);
    }

    for (auto str : *codeGenConstPool->str_constantPool) {
        constantPool->strConstantPool->push_back(str);
    }

    constantPool->out_of_memory = (NoCRuntimeObjectContainer*)create_native_error_obj(symbolTable, constant_heap, "out of memory");
}

void boot(EntryPoint* entryPoint, SymbolTable* symbolTable) {
    Heap* constant_heap = init_heap();
    if (constant_heap == NULL) {
        throw runtime_error("Failed to allocate constants heap");
    }
    ConstantPool* constantPool = new ConstantPool();
    createConsantsFromSymbolTable(symbolTable, constantPool, entryPoint->constantPool, constant_heap);
    entryPoint->ins->startInsIndx = entryPoint->mainIp;
    auto mainThread = createNoCThread(entryPoint->ins, symbolTable, constantPool);
    assert(mainThread->tcb->threadState != NULL);

    Scheduler * scheduler = createScheduler();
    mainThread->tcb->scheduler = scheduler;

    run_scheduler(scheduler, 1, mainThread);
    delete scheduler;
    delete constantPool;
}