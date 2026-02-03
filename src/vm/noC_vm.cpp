#include "vm/noC_vm.h"
#include "assert.h"
#include "vm/printf.h"
#include "allocator/allocator.h"
#include <pthread.h>
#include <unistd.h>
#include "type_checker/type_layout.h"
#include "noCThread/noCThread.h"
#include "string.h"
#include "type_checker/extern_function.h"
#include "utils/utils.h"
#include <fstream>
#include <sys/socket.h>
#include "utils/utils.h"
#include "type_checker/closure_layout.h"
#include "scheduler/sleep_lock_fun.h"
#include "allocator/gc.h"
#include <memory>

struct noCThreadCtx {
    noCThread* thread;
    Scheduler* scheduler;
};


NoCBoolRuntimeObject* falseBoolean = NULL;
NoCBoolRuntimeObject* trueBoolean = NULL;

void runDeffered(noCThreadCtx * thread_ctx, ActivationFrame * frame);
NoCStructContainer* createClosureRuntimeObject(noCThreadCtx * thread_ctx, int closureStructIdentifierIndex, int number_of_args_on_stack);
int get_fp_from_closure(NoCStructContainer* closure);
ActivationFrame* get_static_link_from_closure(NoCStructContainer* closure);
NoCStructContainer* createStaticLinkClosure(noCThreadCtx * thread_ctx);
void call_external_function(noCThreadCtx* thread_ctx, Instruction* instruction);

NoCRuntimeObjectContainer* accessVector(noCThreadCtx * thread_ctx,
    Instruction * instruction);

void addDeferedInstruction(noCThreadCtx * thread_ctx, Instruction * deferred) {
    thread_ctx->thread->tcb->currentActivationFrame->deferredInstructions->push_back(deferred);
}

void handleReturn(noCThreadCtx * thread_ctx);
void handleReturnFromVoid(noCThreadCtx * thread_ctx);

int getCurrentIp(noCThreadCtx * thread_ctx) {
    return thread_ctx->thread->tcb->Ip;
}

void assert_external_handle(NoCRuntimeObjectContainer* object, ExternalHandleType handleType) {
    assert(object->type == RuntimeObjectType::ExternalHandle);
    NoCExternalHandleRuntimeObject* handle = (NoCExternalHandleRuntimeObject*)object;
    assert(handle->handleType == handleType);
}

inline Heap* getHeap(noCThread* noCThread) {
    return noCThread->tcb->heap;
}

SymbolTable* getSymbolTable(noCThread* thread) {
    return thread->tcb->symbolTable;
}

ActivationFrame* defaultStaticLink(noCThread* thread) {
    return thread->tcb->currentActivationFrame;
}

ActivationFrame* createNewActivationFrame(noCThread * thread, ActivationFrame* staticLink, int returnAddress, int throwAddress) {

    ActivationFrame* newFrame = new ActivationFrame();
    newFrame->deferredInstructions = new vector<Instruction*>();
    newFrame->returnAddress = returnAddress;
    newFrame->throwReturnAddress = throwAddress;
    newFrame->storage = new  map<string, NoCRuntimeObjectContainer*>();
    newFrame->previousFrame = thread->tcb->currentActivationFrame;
    newFrame->staticLink = staticLink;
    newFrame->owningThread = thread;

    // Very important that heap reference tracking is created before the frame is set 
    // Otherwise frame would be incorrectly deollacated
    auto frameRefTracker = createExternalHandler(thread->tcb->heap, (void*)newFrame, ExternalHandleType::ActivationFrame);
    newFrame->frameRefTracker = frameRefTracker;
    thread->tcb->currentActivationFrame = newFrame;
    return newFrame;
}

NoCRuntimeObjectContainer* loadConstant(noCThreadCtx * thread_ctx, int index) {
    return thread_ctx->thread->tcb->constPool->constantPool->at(index);
}

ActivationFrame* get_activation_frame(noCThreadCtx * thread_ctx, int num_sl_links_to_walk_up) {
    ActivationFrame* activationFrame = thread_ctx->thread->tcb->currentActivationFrame;
    while (num_sl_links_to_walk_up > 0) {
        activationFrame = activationFrame->staticLink;
        num_sl_links_to_walk_up--;
    }
    return activationFrame;
}

void storeSymbol(noCThreadCtx * thread_ctx, string symbol, NoCRuntimeObjectContainer * obj, int num_sl_links_to_walk_up) {
    ActivationFrame* activationFrame = get_activation_frame(thread_ctx, num_sl_links_to_walk_up);
    activationFrame->storage->insert_or_assign(symbol, obj);
}

// TODO :: Rename this 
void storeSymbolInternal(noCThread * thread, string symbol, NoCRuntimeObjectContainer * obj) {
    thread->tcb->currentActivationFrame->storage->insert_or_assign(symbol, obj);
}

NoCRuntimeObjectContainer* loadSymbol(noCThreadCtx * thread_ctx, string name, int num_sl_links_to_walk_up) {
    ActivationFrame* activationFrame = get_activation_frame(thread_ctx, num_sl_links_to_walk_up);
    return activationFrame->storage->at(name);
}

void push_operand(noCThreadCtx * thread_ctx, NoCRuntimeObjectContainer* obj) {
    if (obj != NULL && obj != &nullObject) {
        pin_object(obj); // TODO  Big pin hack because return values are not safe so this is a hack for now  
    }
    thread_ctx->thread->tcb->operands->push(obj);
}

void push_operand_new(noCThreadCtx * thread_ctx, NoCRuntimeObjectContainer * obj) {
    thread_ctx->thread->tcb->operands->push(obj);
}

NoCRuntimeObjectContainer* pop_operand(noCThreadCtx * thread_ctx) {
    if (thread_ctx->thread->tcb->operands->empty()) {
        throw runtime_error("pop_operand:: attempting to pop operand but it is empty");
    }
    auto top = thread_ctx->thread->tcb->operands->top();
    assert(top != NULL);
    if (top != &nullObject) {
        unpin_object(top); // TODO  Big pin hack because return values are not safe so this is a hack for now  
    }
    thread_ctx->thread->tcb->operands->pop();
    return top;
}

NoCRuntimeObjectContainer* pop_non_null_operand(noCThreadCtx * thread_ctx) {
    auto top = pop_operand(thread_ctx);
    if (top == &nullObject) {
        throw runtime_error("operand cannot be noC null");
    }
    return top;
}

NoCRuntimeObjectContainer* peek_operand(noCThreadCtx * thread_ctx) {
    if (thread_ctx->thread->tcb->operands->empty()) {
        return NULL;
    }
    auto top = thread_ctx->thread->tcb->operands->top();
    return top;
}


// Shameful ugly macros - for shame 

#define RESOLVE_BOOLEAN_FOR_PRIMITIVE(opcode, first_operand, second_operand) \
    ({ \
        NoCBoolRuntimeObject* result; \
        if (opcode == Opcode::LESS_THAN) { \
            result = (second_operand->value < first_operand->value) ? trueBoolean : falseBoolean; \
        } else if (opcode == Opcode::LESS_THAN_EQUAL) { \
            result = (second_operand->value <= first_operand->value) ? trueBoolean : falseBoolean; \
        } else if (opcode == Opcode::MORE_THAN) { \
            result = (second_operand->value > first_operand->value) ? trueBoolean : falseBoolean; \
        } else if (opcode == Opcode::MORE_THAN_EQUAL) { \
            result = (second_operand->value >= first_operand->value) ? trueBoolean : falseBoolean; \
        } else if (opcode == Opcode::NOT_EQUAL) { \
            result = (second_operand->value != first_operand->value) ? trueBoolean : falseBoolean; \
        }else if (opcode == Opcode::EQUAL) { \
            result = (second_operand->value == first_operand->value) ? trueBoolean : falseBoolean; \
        } \
         else { \
            throw runtime_error("unrecognized opcode in function::RESOLVE_BOOLEAN_FOR_LITERAL");  \
        } \
        result; \
    })


#define CONVERT_PRIMTIVE_OPERAND(FROM_RUNTIME_OBJECT_TYPE, FROM_NO_C_OBJ, TO_NOC_OBJ, CREATE_FUNC) \
    { \
        FROM_NO_C_OBJ* from_obj = (FROM_NO_C_OBJ*)pop_operand(thread_ctx); \
        assert(from_obj->header.type == RuntimeObjectType::FROM_RUNTIME_OBJECT_TYPE); \
        TO_NOC_OBJ* converted = CREATE_FUNC(getHeap(thread_ctx->thread), from_obj->value); \
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)converted); \
    }



#define DEFINE_POP_TYPED_OPERAND(TYPE_NAME, RUNTIME_TYPE, RUNTIME_OBJECT_TYPE) \
    RUNTIME_TYPE* pop_##TYPE_NAME##_operand(noCThreadCtx* thread_ctx) { \
        auto top = pop_operand(thread_ctx); \
        if (top == &nullObject) { \
            throw runtime_error("operand cannot be noC null"); \
        } \
        assert(top->type == RuntimeObjectType::RUNTIME_OBJECT_TYPE); \
        return (RUNTIME_TYPE*)top; \
    }

#define DEFINE_PEEK_TYPED_OPERAND(TYPE_NAME, RUNTIME_TYPE, RUNTIME_OBJECT_TYPE) \
    RUNTIME_TYPE* peek_##TYPE_NAME##_operand(noCThreadCtx* thread_ctx) { \
        auto top = peek_operand(thread_ctx); \
        if (top == &nullObject) { \
            throw runtime_error("operand cannot be noC null"); \
        } \
        assert(top->type == RuntimeObjectType::RUNTIME_OBJECT_TYPE); \
        return (RUNTIME_TYPE*)top; \
    }

DEFINE_POP_TYPED_OPERAND(int, NoCIntRuntimeObject, Int)
DEFINE_POP_TYPED_OPERAND(double, NoCDoubleRuntimeObject, Double)
DEFINE_POP_TYPED_OPERAND(bool, NoCBoolRuntimeObject, Bool)
DEFINE_POP_TYPED_OPERAND(vec, NoCVectorRuntimeObject, NDimensionVector)
DEFINE_POP_TYPED_OPERAND(lock, NoCExternalHandleRuntimeObject, ExternalHandle)
DEFINE_POP_TYPED_OPERAND(external, NoCExternalHandleRuntimeObject, ExternalHandle)
DEFINE_POP_TYPED_OPERAND(string, NoCStructContainer, NativeStringStruct)
DEFINE_POP_TYPED_OPERAND(non_native_string, NoCNonNativeStringWrapperRuntimeObject, NonNativeStringWrapper)


DEFINE_PEEK_TYPED_OPERAND(future, NoCExternalHandleRuntimeObject, ExternalHandle)
DEFINE_PEEK_TYPED_OPERAND(lock, NoCExternalHandleRuntimeObject, ExternalHandle)
DEFINE_PEEK_TYPED_OPERAND(int, NoCIntRuntimeObject, Int)


NoCBoolRuntimeObject* resolveBoolean(Opcode opcode, NoCRuntimeObjectContainer * first_operand, NoCRuntimeObjectContainer * second_operand) {

    // @hack 
    // For null comparisions if either operand is the nullObject // then it must be an equal comparison between two objects
    // the type checker will ensure that null comparisons cannot happen for primitive types
    // this is a really lame ass guranteee by the type checker but meh
    if (first_operand == &nullObject || second_operand == &nullObject) {
        assert(opcode == Opcode::EQUAL || opcode == Opcode::NOT_EQUAL);
        bool result = first_operand == second_operand;
        if (opcode == Opcode::NOT_EQUAL) {
            return result ? falseBoolean : trueBoolean;
        }
        if (opcode == Opcode::EQUAL) {
            return result ? trueBoolean : falseBoolean;
        }
    }

    NoCBoolRuntimeObject* result;
    if (first_operand->type == RuntimeObjectType::Int) {

        NoCIntRuntimeObject* first_operand_as_int = (NoCIntRuntimeObject*)first_operand;
        NoCIntRuntimeObject* second_operand_as_int = (NoCIntRuntimeObject*)second_operand;
        result = RESOLVE_BOOLEAN_FOR_PRIMITIVE(opcode, first_operand_as_int, second_operand_as_int);
        return result;
    }

    else if (first_operand->type == RuntimeObjectType::Char) {
        NoCCharRuntimeObject* first_operand_as_char = (NoCCharRuntimeObject*)first_operand;
        NoCCharRuntimeObject* second_operand_as_char = (NoCCharRuntimeObject*)second_operand;
        result = RESOLVE_BOOLEAN_FOR_PRIMITIVE(opcode, first_operand_as_char, second_operand_as_char);
        return result;
    }

    else if (first_operand->type == RuntimeObjectType::Double) {
        NoCDoubleRuntimeObject* first_operand_as_double = (NoCDoubleRuntimeObject*)first_operand;
        NoCDoubleRuntimeObject* second_operand_as_double = (NoCDoubleRuntimeObject*)second_operand;
        result = RESOLVE_BOOLEAN_FOR_PRIMITIVE(opcode, first_operand_as_double, second_operand_as_double);
        return result;
    }

    else if (first_operand->type == RuntimeObjectType::Bool) {
        NoCBoolRuntimeObject* first_operand_as_bool = (NoCBoolRuntimeObject*)first_operand;
        NoCBoolRuntimeObject* second_operand_as_bool = (NoCBoolRuntimeObject*)second_operand;
        // The or only applies to boolean operands 
        if (opcode == Opcode::OR) {
            if (first_operand_as_bool->value || second_operand_as_bool->value) {
                return trueBoolean;
            }
            else {
                return falseBoolean;
            }
        }

        if (opcode == Opcode::AND) {
            if (first_operand_as_bool->value && second_operand_as_bool->value) {
                return trueBoolean;
            }
            else {
                return falseBoolean;
            }
        }
        result = RESOLVE_BOOLEAN_FOR_PRIMITIVE(opcode, first_operand_as_bool, second_operand_as_bool);
        return result;
    }

    else if (opcode == Opcode::EQUAL) {
        // TODO :: Support different types eg int bool, double
        // The  type checker ensures that primtives cannot be null so if there is an operand that is null 
        // Then it means that the user has done a null check, very clunky, think of better way to do this
        bool result = first_operand == second_operand;
        if (result) {
            return trueBoolean;
        }
        else {
            return falseBoolean;
        }

    }
    else if (opcode == Opcode::NOT_EQUAL) {
        bool result = first_operand != second_operand;
        if (result) {
            return trueBoolean;
        }
        else {
            return falseBoolean;
        }
    }
    else if (opcode == Opcode::OR) {
        assert(first_operand->type == RuntimeObjectType::Bool);

        NoCBoolRuntimeObject* first_operand_as_bool = (NoCBoolRuntimeObject*)first_operand;
        NoCBoolRuntimeObject* second_operand_as_bool = (NoCBoolRuntimeObject*)second_operand;
        if (first_operand_as_bool->value || second_operand_as_bool->value) {
            return trueBoolean;
        }
        return falseBoolean;
    }
    else if (opcode == Opcode::AND) {

        NoCBoolRuntimeObject* first_operand_as_bool = (NoCBoolRuntimeObject*)first_operand;
        NoCBoolRuntimeObject* second_operand_as_bool = (NoCBoolRuntimeObject*)second_operand;
        if (first_operand_as_bool->value && second_operand_as_bool->value) {
            return trueBoolean;
        }
        return falseBoolean;;
    }
    else {
        throw runtime_error("unrecognized opcode");
    }
}

// TODO :: Change all of these to macros 
NoCRuntimeObjectContainer* resolveArithmeticDouble(
    noCThreadCtx * thread_ctx,
    Opcode opCode,
    NoCDoubleRuntimeObject * first_operand,
    NoCDoubleRuntimeObject * second_operand) {

    void* result_object = noC_alloc_pin(thread_ctx->thread->tcb->heap, sizeof(NoCDoubleRuntimeObject));
    if (result_object == NULL) {
        throw runtime_error("noC_alloc failed");
    }
    NoCDoubleRuntimeObject* result = (NoCDoubleRuntimeObject*)result_object;
    if (opCode == Opcode::ADD) {
        result->value = second_operand->value + first_operand->value;
    }

    else if (opCode == Opcode::SUBTRACT) {
        result->value = second_operand->value - first_operand->value;
    }

    else if (opCode == Opcode::MULTIPLY) {
        result->value = second_operand->value * first_operand->value;
    }

    else if (opCode == Opcode::DIVIDE) {
        result->value = second_operand->value / first_operand->value;
    }
    else {
        throw runtime_error("unrecognized operator on double");
    }

    NoCRuntimeObjectContainer* result_container = (NoCRuntimeObjectContainer*)result;
    result_container->type = RuntimeObjectType::Double;
    return result_container;
}

NoCRuntimeObjectContainer* resolveArithmeticInt(
    noCThreadCtx * thread_ctx,
    Opcode opCode,
    NoCIntRuntimeObject * first_operand,
    NoCIntRuntimeObject * second_operand) {

    void* result_object = noC_alloc_pin(thread_ctx->thread->tcb->heap, sizeof(NoCIntRuntimeObject));
    if (result_object == NULL) {
        throw runtime_error("noC_alloc failed");
    }
    NoCIntRuntimeObject* result = (NoCIntRuntimeObject*)result_object;
    if (opCode == Opcode::ADD) {
        result->value = second_operand->value + first_operand->value;
    }

    else if (opCode == Opcode::SUBTRACT) {
        result->value = second_operand->value - first_operand->value;
    }

    else if (opCode == Opcode::MULTIPLY) {
        result->value = second_operand->value * first_operand->value;
    }

    else if (opCode == Opcode::DIVIDE) {
        result->value = second_operand->value / first_operand->value;
    }
    else if (opCode == Opcode::MOD) {
        result->value = second_operand->value % first_operand->value;
    }

    else {
        throw runtime_error("unrecognized operator");
    }

    NoCRuntimeObjectContainer* result_container = (NoCRuntimeObjectContainer*)result;
    result_container->type = RuntimeObjectType::Int;
    return result_container;
}

NoCRuntimeObjectContainer* resolveArithmeticChar(
    noCThreadCtx * thread_ctx,
    Opcode opCode,
    NoCCharRuntimeObject * first_operand,
    NoCCharRuntimeObject * second_operand) {

    void* result_object = noC_alloc_pin(thread_ctx->thread->tcb->heap, sizeof(NoCCharRuntimeObject));
    if (result_object == NULL) {
        throw runtime_error("noC_alloc failed");
    }
    NoCCharRuntimeObject* result = (NoCCharRuntimeObject*)result_object;
    if (opCode == Opcode::ADD) {
        result->value = second_operand->value + first_operand->value;
    }

    else if (opCode == Opcode::SUBTRACT) {
        result->value = second_operand->value - first_operand->value;
    }

    else if (opCode == Opcode::MULTIPLY) {
        result->value = second_operand->value * first_operand->value;
    }

    else if (opCode == Opcode::DIVIDE) {
        result->value = second_operand->value / first_operand->value;
    }
    else if (opCode == Opcode::MOD) {
        result->value = second_operand->value % first_operand->value;
    }

    else {
        throw runtime_error("unrecognized operator");
    }

    NoCRuntimeObjectContainer* result_container = (NoCRuntimeObjectContainer*)result;
    result_container->type = RuntimeObjectType::Char;
    return result_container;
}



NoCRuntimeObjectContainer* resolveArithmetic(
    noCThreadCtx * thread_ctx,
    Opcode opCode,
    NoCRuntimeObjectContainer * first_operand,
    NoCRuntimeObjectContainer * second_operand) {
    assert(first_operand->type == second_operand->type);
    switch (first_operand->type) {
    case RuntimeObjectType::Int:
    {
        return resolveArithmeticInt(thread_ctx, opCode, (NoCIntRuntimeObject*)first_operand, (NoCIntRuntimeObject*)second_operand);
    }
    case RuntimeObjectType::Double:
    {
        return resolveArithmeticDouble(thread_ctx, opCode, (NoCDoubleRuntimeObject*)first_operand, (NoCDoubleRuntimeObject*)second_operand);
    }
    case RuntimeObjectType::Char:
    {
        return resolveArithmeticChar(thread_ctx, opCode, (NoCCharRuntimeObject*)first_operand, (NoCCharRuntimeObject*)second_operand);
    }
    default:
        throw runtime_error("operand are not suitable for arithemetic add");
        break;
    }
}

void increment_ip(noCThreadCtx * thread_ctx) {
    thread_ctx->thread->tcb->Ip++;
}

void update_ip(noCThreadCtx * thread_ctx, int ip) {
    thread_ctx->thread->tcb->Ip = ip;
}

void runThreadInner(noCThreadCtx * thread_ctx);

void set_up_first_activation_frame(noCThread* thread) {
    if (thread->tcb->currentActivationFrame == NULL) {
        auto activationFrame = createNewActivationFrame(thread, defaultStaticLink(thread), -1, -1);
        thread->tcb->currentActivationFrame = activationFrame;
    }
    else {
        assert_never_reaches("set_up_first_activation_frame is being called but frame already exists");
    }
}

void throw_error_internal(noCThreadCtx* thread_ctx, NoCStructContainer *error_object);
void throw_error_internal(noCThreadCtx* thread_ctx, string error_msg) {
    auto native_error = create_native_error_obj(getSymbolTable(thread_ctx->thread), getHeap(thread_ctx->thread), error_msg);
    throw_error_internal(thread_ctx, native_error);
}

void throw_error_internal(noCThreadCtx* thread_ctx, NoCStructContainer *error_object) {

    ActivationFrame* current_frame = thread_ctx->thread->tcb->currentActivationFrame;
    int ip_to_restore = 0;
    ActivationFrame* frame_to_restore = NULL;
    ActivationFrame* frame_after_restore = NULL;
    /***
     *  main {
     *       err = calls_a();
     *       if (err != null) {
     *
     *       }
     *
     * }
     *
     * main :: frame ( frame to store :: it has throwReturnAddress != -1  )
     * call_a() :: frame_after_restore
     *
    */

    while (current_frame != NULL) {
        runDeffered(thread_ctx, current_frame);
        if (current_frame->throwReturnAddress != -1) {
            ip_to_restore = current_frame->throwReturnAddress;
            frame_to_restore = current_frame->previousFrame;
            frame_after_restore = current_frame;
            break;
        }
        current_frame = current_frame->previousFrame;
    }

    if (frame_to_restore == NULL) {
        throw NoCUnhandledError((NoCStructContainer*)error_object);
    }

    // restore to frame and ip 
    thread_ctx->thread->tcb->currentActivationFrame = frame_to_restore;
    update_ip(thread_ctx, ip_to_restore);

    // push error object and result 
    if (!isReturnTypeVoid(frame_after_restore->functionSymbol->returnType)) {
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)error_object);
        push_operand(thread_ctx, &nullObject);
    }
    else {
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)error_object);
    }
}

void runThread(noCThread * thread) {
    try {
        update_thread_state_strong(thread, ThreadState_E::Running);
        noCThreadCtx thread_ctx = noCThreadCtx();
        thread_ctx.thread = thread;
        thread_ctx.scheduler = (Scheduler*)thread->tcb->scheduler;
        // First ever frame allocated 
        if (thread_ctx.thread->tcb->currentActivationFrame == NULL) {
            set_up_first_activation_frame(thread_ctx.thread);
        }
        runThreadInner(&thread_ctx);
    }
    catch (const NoCRuntimeException& e) {
        cout << " The runtime through an exception, vThread id :: " << thread->tcb->vThreadId << " failed at ip ::  " << thread->tcb->Ip << "\e[m\n";
        cout << e.what() << endl;
        // Crash if main thread 
        if (thread->parent == NULL) {
            throw e;
        }
        NoCRuntimeObjectContainer* error = NULL;
        try {
            // Try marshalling to native erro if fails just report constant out of memory error 
            error = (NoCRuntimeObjectContainer*)create_native_error_obj(getSymbolTable(thread), getHeap(thread), e.message);
        }
        catch (const NoCRuntimeException& e) {
            cout << "out of memory exception when attempting to marshall NoCRuntimeException to native error, please check error log to see true error" << endl;
            error = thread->tcb->constPool->out_of_memory;
        }
        signal_waiters(thread, &nullObject, error);
    }
    catch (const NoCUnhandledError& e) {
        // TODO :: What happens to the native memory reference when throw an error? Is there still a reference to it 
        cout << " Uncaught noCError object, vThread id :: " << thread->tcb->vThreadId << " failed at ip ::  " << thread->tcb->Ip << "\e[m\n";
        NoCStructContainer* native_str_obj = getStringFromError(e.error_obj);
        print_native_str(native_str_obj);
        cout << endl;
        // If this is main thread then crash the entire program 
        if (thread->parent == NULL) {
            throw e;
        }
        auto error_obj = e.error_obj;
        signal_waiters(thread, &nullObject, (NoCRuntimeObjectContainer*)error_obj);
    }
}

bool isIpValid(noCThread * thread) {
    // If we have returned from the upper most stack 
    if (thread->tcb->Ip == -1) {
        return false;
    }
    // If we are on the the last stack frame check if we are within the bounds
    // Needed for noroutines because they look at the slice of the instruction stream
    if (thread->tcb->currentActivationFrame->returnAddress == -1) {
        return thread->tcb->Ip < thread->tcb->instructionStream->lastInsIdx;
    }
    return true;
}

void check_gc_signal_at_safe_point(noCThread* thread, Instruction* instruction) {

    /*
        The following are gc safe points that wont mess up the object graph
        Before function call
        Before fork
        Before any conditional statements ( this is the case for long while loops )

        Any other instruction can actually be proceded by an allocation ( I have not created a proof for this but really should  )
    */

    // Check gc signal at safe point 
    switch (instruction->op_code) {
    case Opcode::FORK:
    case Opcode::CALL:
    case Opcode::CALL_LAMBDA:
    case Opcode::JUMP_IF_FALSE:
    {
        check_gc_signal(thread);
        break;
    }
    default:break;
    }
}

void store_function_symbol_in_activation_frame(noCThreadCtx* thread_ctx, string fname) {
    auto functionSymbol = getFunctionSymbol(thread_ctx->thread->tcb->symbolTable, fname);
    auto activationFrame = thread_ctx->thread->tcb->currentActivationFrame;
    activationFrame->functionSymbol = functionSymbol;
}

Instruction* getInstructionAt(noCThreadCtx * thread_ctx, int ip) {
    auto insStream = thread_ctx->thread->tcb->instructionStream->instructions;
    if (ip<0 || ip > insStream->size()) {
        throw runtime_error("terminal runtime error :: getInstructionAt provided ip that is out of bounds");
    }

    Instruction* instruction = thread_ctx->thread->tcb->instructionStream->instructions->at(ip);
    return instruction;
}

void runThreadInner(noCThreadCtx * thread_ctx) {

    while (isIpValid(thread_ctx->thread)) {
        int instructionPointer = thread_ctx->thread->tcb->Ip;
        Instruction* instruction = thread_ctx->thread->tcb->instructionStream->instructions->at(instructionPointer);
        Opcode opcode = instruction->op_code;

        check_gc_signal_at_safe_point(thread_ctx->thread, instruction);
        // assert_heap_integrity(getHeap(thread_ctx->thread));

        switch (opcode) {
        case Opcode::LOAD_I:
        {
            NoCRuntimeObjectContainer* loaded = loadSymbol(thread_ctx, instruction->symbol, instruction->arg1);
            push_operand_new(thread_ctx, loaded);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::STORE:
        {
            NoCRuntimeObjectContainer* objToStore = pop_operand(thread_ctx);
            storeSymbol(thread_ctx, instruction->symbol, objToStore, instruction->arg1);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::LOAD_IMMEDIATE:
        {
            auto immediateValue = createIntRuntime(getHeap(thread_ctx->thread), instruction->arg1);
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)immediateValue);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::LOAD_CONST:
        {
            auto constant = loadConstant(thread_ctx, instruction->arg1);
            push_operand(thread_ctx, constant);
            increment_ip(thread_ctx);
            break;
        }

        case Opcode::LOAD_NON_NATIVE_STR_CONST:
        {
            auto index = instruction->arg1;
            string* str = &thread_ctx->thread->tcb->constPool->strConstantPool->at(index);
            NoCNonNativeStringWrapperRuntimeObject* str_wrapper = createNonNativeStringWrapper(getHeap(thread_ctx->thread));
            str_wrapper->str = str;
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)str_wrapper);
            increment_ip(thread_ctx);
            break;

        }
        case Opcode::LOAD_NULL:
        {
            push_operand(thread_ctx, &nullObject);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::ENTER_FOR_BLOCK:
        {
            // The enter for block does nothing but help mark position in the code stream
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::ADD:
        case Opcode::SUBTRACT:
        case Opcode::MULTIPLY:
        case Opcode::DIVIDE:
        case Opcode::MOD:
        {
            auto first_operand = pop_operand(thread_ctx);
            auto second_operand = pop_operand(thread_ctx);
            auto result = resolveArithmetic(thread_ctx, instruction->op_code, first_operand, second_operand);
            push_operand(thread_ctx, result);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::D2I:
        {
            CONVERT_PRIMTIVE_OPERAND(Double, NoCDoubleRuntimeObject, NoCIntRuntimeObject, createIntRuntime);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::I2D:
        {
            CONVERT_PRIMTIVE_OPERAND(Int, NoCIntRuntimeObject, NoCDoubleRuntimeObject, createDoubleRuntime);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::C2I:
        {
            CONVERT_PRIMTIVE_OPERAND(Char, NoCCharRuntimeObject, NoCIntRuntimeObject, createIntRuntime);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::I2C:
        {
            CONVERT_PRIMTIVE_OPERAND(Int, NoCIntRuntimeObject, NoCCharRuntimeObject, createCharRuntime);
            increment_ip(thread_ctx);
            break;
        }

        case Opcode::THROW:
        {
            auto err_obj = pop_operand(thread_ctx);
            assert(err_obj->type == RuntimeObjectType::StructObjType);
            throw_error_internal(thread_ctx, (NoCStructContainer*)err_obj);
            break;
        }
        case Opcode::NEW_ARR:
        {
            string typeStr = instruction->symbol;
            int num_dimensions = instruction->arg1;
            vector<size_t> dimensions;
            for (int i = 1; i <= num_dimensions; i++) {
                auto dimensionSize = (NoCIntRuntimeObject*)pop_operand(thread_ctx);
                assert(dimensionSize->header.type == RuntimeObjectType::Int);

                if (dimensionSize->value < 0) {
                    throw runtime_error("dimension size cannot be smaller than 0");
                }

                dimensions.push_back(dimensionSize->value);
            }
            Type* type = getTypeDefinitionByName(thread_ctx->thread->tcb->symbolTable, typeStr);
            auto arrRuntimeObject = createArrRuntimeObject(getHeap(thread_ctx->thread), type, dimensions);
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)arrRuntimeObject);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::NEW:
        {
            auto structLayout = searchStructLayoutByIdentifier(thread_ctx->thread->tcb->symbolTable, instruction->arg1);
            auto structRuntimeObject = createStructRuntimeObject(getHeap(thread_ctx->thread), structLayout);
            push_operand_new(thread_ctx, (NoCRuntimeObjectContainer*)structRuntimeObject);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::RETURN:
            handleReturn(thread_ctx);
            break;
        case Opcode::RETURN_FROM_VOID:

            handleReturnFromVoid(thread_ctx);
            break;
        case Opcode::JUMP_IF_FALSE:
        {
            // Have tochange ip 
            // Check if operand is false 
            auto top = pop_operand(thread_ctx);
            assert(top->type == RuntimeObjectType::Bool);
            auto booleanObj = (NoCBoolRuntimeObject*)top;
            if (!booleanObj->value) {
                update_ip(thread_ctx, instruction->arg1);
            }
            else {
                increment_ip(thread_ctx);
            }
            break;
        }
        case Opcode::JUMP:
        {
            update_ip(thread_ctx, instruction->arg1);
            break;
        }
        case Opcode::DEFER:
        {
            // Jump passed the instructions to not execute
            addDeferedInstruction(thread_ctx, instruction);
            update_ip(thread_ctx, instruction->arg2);
            break;
        }
        case Opcode::DEFER_FINISH:
        {
            // Exit now to
            return;
        }
        case Opcode::CALL_LAMBDA:
        {
            auto top = pop_operand(thread_ctx);
            if (top == &nullObject) {
                throw_error_internal(thread_ctx, "attempted to call lambda on a null object");
                break;
            }
            assert(top->type == RuntimeObjectType::Closure);
            NoCStructContainer* closureObject = (NoCStructContainer*)top;
            int currentIp = getCurrentIp(thread_ctx);
            int returnaddress = currentIp + 1;

            // We use the static link of the current closure when calling this function
            auto functionPointer = get_fp_from_closure(closureObject);
            assert(functionPointer != -1);
            auto staticLink = get_static_link_from_closure(closureObject);
            createNewActivationFrame(thread_ctx->thread, staticLink, returnaddress, -1);
            update_ip(thread_ctx, functionPointer);
            break;
        }
        case Opcode::CALL:
        {
            // There is not catch statments so we will put -1 on the throw address   
            int currentIp = getCurrentIp(thread_ctx);
            int returnaddress = currentIp + 1;
            createNewActivationFrame(thread_ctx->thread, defaultStaticLink(thread_ctx->thread), returnaddress, -1);
            update_ip(thread_ctx, instruction->arg1);
            break;
        }
        case Opcode::CALL_WITH_TRY_CATCH:
        {
            // Create new activation frame with return address 
            int returnAddress = getCurrentIp(thread_ctx) + 1;
            int tryCatchReturnAdress = returnAddress;
            createNewActivationFrame(thread_ctx->thread, defaultStaticLink(thread_ctx->thread), returnAddress, tryCatchReturnAdress);
            update_ip(thread_ctx, instruction->arg1);
            break;
        }
        case Opcode::FORK:
        {
            auto num_symbols_to_capture = instruction->arg1;
            auto forkedThreadIpStart = thread_ctx->thread->tcb->Ip + num_symbols_to_capture + 1;

            auto lastInsIp = instruction->arg2;
            auto newInsStream = createInsStreamFromOffset(thread_ctx->thread->tcb->instructionStream, forkedThreadIpStart, lastInsIp);
            auto forked_thread = createThreadFromParent(thread_ctx->thread, newInsStream);
            set_up_first_activation_frame(forked_thread);
            auto noRoutineClosure = pop_operand(thread_ctx);
            assert(noRoutineClosure->type == RuntimeObjectType::Closure);
            storeSymbolInternal(forked_thread, noRoutineStorageName, noRoutineClosure);

            int current_ip = getCurrentIp(thread_ctx);


            // I have to pop the instructions in reverse 
            for (int i = num_symbols_to_capture; i >= 1; i--) {
                Instruction* load_ins = getInstructionAt(thread_ctx, current_ip + i);
                assert(load_ins->op_code == Opcode::LOAD_I);
                auto captured_value = loadSymbol(thread_ctx, load_ins->symbol, load_ins->arg1);
                // Hack :: The captured need to be stored in the activation frame to keep the refernces alive when GC happens
                // They also need to be pushed on the stack so when the no routine closure is called it will pop them off the stack properly
                storeSymbolInternal(forked_thread, load_ins->symbol, captured_value);
                forked_thread->tcb->operands->push(captured_value);
            }

            update_ip(thread_ctx, lastInsIp);
            /*
                It should not matter if the scheduled thread causes a GC before we set the future to be part of the operand
                This is because whenever GC should triggered only when we are at a safe point
                If GC is triggerred it will wait for other threads to reach a safe point
            */
            schedule_new_thread(thread_ctx->scheduler, forked_thread);
            auto future = createExternalHandler(getHeap(thread_ctx->thread), forked_thread, ExternalHandleType::Future);
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)future);

            break;
        }
        case Opcode::LABEL:
        {
            store_function_symbol_in_activation_frame(thread_ctx, instruction->symbol);
            auto activation_frame = thread_ctx->thread->tcb->currentActivationFrame;
            auto functionSymbol = activation_frame->functionSymbol;

            // read comment @gc_safety_hack
            if (functionSymbol->name == "printf") {
                assert(functionSymbol->isExternal);
                call_external_function(thread_ctx, instruction);
                break;
            }

            assert(functionSymbol->hasVarArgs == false);

            for (int i = 0; i < functionSymbol->numberOfArgs; i++) {
                auto operand = pop_operand(thread_ctx);
                auto param = functionSymbol->params->at(i);
                activation_frame->storage->insert_or_assign(param->name, operand);
                // todo :: super hack!! This is because arithmetic objects allocate a result object before the frame is set up
                // The code gen is done the wrong way round
                // We should set up the frame, then evaluate each arg, assign reference, and do the same for each argument 
                if (operand != &nullObject) {
                    unpin_object(operand);
                }
            }

            if (functionSymbol->isExternal) {
                // TODO :: @gc_safety_hack
                // This is a big hack since the external code works my popping from the operands stack
                // It should work by reading from the storage but I dont have time to refactor that code 
                // We need to keep the objects rooted in the activation frames so they can be traced
                // This does not support varargs but varargs is only used by print and should never try to allocate any objects 
                for (int i = functionSymbol->numberOfArgs - 1; i >= 0; i--) {
                    auto param = functionSymbol->params->at(i);
                    auto operand = activation_frame->storage->at(param->name);
                    push_operand(thread_ctx, operand);
                }
                call_external_function(thread_ctx, instruction);
            }
            else {
                increment_ip(thread_ctx);
            }
            break;
        }
        case Opcode::OR:
        case Opcode::AND:
        case Opcode::LESS_THAN:
        case Opcode::LESS_THAN_EQUAL:
        case Opcode::MORE_THAN:
        case Opcode::MORE_THAN_EQUAL:
        case Opcode::EQUAL:
        case Opcode::NOT_EQUAL:
        {
            auto first_operand = pop_operand(thread_ctx);
            auto second_operand = pop_operand(thread_ctx);
            auto result = resolveBoolean(instruction->op_code, first_operand, second_operand);
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)result);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::LOAD_ARR_LEN:
        {
            auto operand = pop_operand(thread_ctx);
            if (operand == &nullObject) {
                throw runtime_error("Null pointer exception :: Attempting to get len on a null pointer");
            }
            assert(operand->type == RuntimeObjectType::NDimensionVector);
            NoCVectorRuntimeObject* vector = (NoCVectorRuntimeObject*)operand;
            size_t len = get_len_of_dimension(vector);
            auto runtimelen = (NoCRuntimeObjectContainer*)createIntRuntime(getHeap(thread_ctx->thread), len);
            push_operand(thread_ctx, runtimelen);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::LOAD_FIELD:
        {
            auto value_to_load_from = pop_operand(thread_ctx);
            if (value_to_load_from == &nullObject) {
                throw runtime_error("Attempting to load a field from a null pointer");
            }
            assert(value_to_load_from->type == RuntimeObjectType::StructObjType || value_to_load_from->type == RuntimeObjectType::NativeStringStruct);
            auto mem_offset = instruction->arg1;
            auto value_loaded = load_object_from_offset((NoCStructContainer*)value_to_load_from, mem_offset);
            assert(value_loaded != NULL);
            push_operand(thread_ctx, value_loaded);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::STORE_FIELD:
        {
            auto value_to_store_into = pop_operand(thread_ctx);
            if (value_to_store_into == &nullObject) {
                throw runtime_error("Attempting to store a field into a null pointer");
            }
            assert(value_to_store_into->type == RuntimeObjectType::StructObjType);
            auto value_to_assign = pop_operand(thread_ctx);
            assert(value_to_assign != NULL);
            NoCStructContainer* structContainer = (NoCStructContainer*)value_to_store_into;
            int field_offset = instruction->arg1;
            structContainer->fields[field_offset] = (char*)value_to_assign;
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::STORE_ARR_I:
        {
            auto array = peek_operand(thread_ctx);
            if (array == &nullObject) {
                throw runtime_error("Null pointer exception :: Attempting to access array on null object");
            }

            auto vec = pop_vec_operand(thread_ctx);
            vector<int> indexes;
            int num_dimensions = instruction->arg1;
            for (int i = 0; i < num_dimensions; i++) {
                auto index = (NoCIntRuntimeObject*)pop_operand(thread_ctx);
                assert(index->header.type == RuntimeObjectType::Int);
                indexes.push_back(index->value);
            }
            // The type checker should enforce
            assert(vec->slice.num_dimensions == num_dimensions);

            NoCRuntimeObjectContainer* objectToStore = pop_operand(thread_ctx);
            storeValueInVector(vec, objectToStore, &indexes);
            increment_ip(thread_ctx);
            break;
        }
        case Opcode::LOAD_ARR_I:
        {
            auto array = peek_operand(thread_ctx);
            if (array == &nullObject) {
                throw runtime_error("Null pointer exception :: Attempting to access array on null object");
            }
            auto loadedArr = accessVector(thread_ctx, instruction);
            push_operand(thread_ctx, loadedArr);
            increment_ip(thread_ctx);
            break;
        }
        default:
            throw runtime_error("unknown opcode ");
            break;
        }
    }

    auto result = peek_operand(thread_ctx);
    if (result == NULL) {
        signal_waiters(thread_ctx->thread, &nullObject, &nullObject);
    }
    else {
        pop_operand(thread_ctx);
        signal_waiters(thread_ctx->thread, result, &nullObject);
    }
    thread_set_status_finished(thread_ctx->thread);
}
void handleReturn(noCThreadCtx * thread_ctx) {
    auto currentReturnValue = pop_operand(thread_ctx);
    runDeffered(thread_ctx, thread_ctx->thread->tcb->currentActivationFrame);
    // TODO :: This seems hacky but I have no way of knowing if I should return one or two 
    if (thread_ctx->thread->tcb->currentActivationFrame->throwReturnAddress != -1) {
        push_operand(thread_ctx, &nullObject);
        push_operand(thread_ctx, currentReturnValue);
    }
    else {
        push_operand(thread_ctx, currentReturnValue);
    }
    thread_ctx->thread->tcb->Ip = thread_ctx->thread->tcb->currentActivationFrame->returnAddress;
    thread_ctx->thread->tcb->currentActivationFrame = thread_ctx->thread->tcb->currentActivationFrame->previousFrame;
}

void handleReturnFromVoid(noCThreadCtx * thread_ctx) {
    // Check if there are any deffered instructions and handle them 
    runDeffered(thread_ctx, thread_ctx->thread->tcb->currentActivationFrame);
    // Update activation frame and pop the current one 
    thread_ctx->thread->tcb->Ip = thread_ctx->thread->tcb->currentActivationFrame->returnAddress;
    thread_ctx->thread->tcb->currentActivationFrame = thread_ctx->thread->tcb->currentActivationFrame->previousFrame;
}

void runDeffered(noCThreadCtx * thread_ctx, ActivationFrame * frame) {

    // Restore the frame and run the thread inner
    thread_ctx->thread->tcb->currentActivationFrame = frame;
    for (auto deferredInstruction : *frame->deferredInstructions) {
        thread_ctx->thread->tcb->Ip = deferredInstruction->arg1;
        runThreadInner(thread_ctx);
    }
}

NoCRuntimeObjectContainer* accessVector(noCThreadCtx * thread_ctx,
    Instruction * instruction) {

    auto vec = pop_vec_operand(thread_ctx);
    vector<int> indexes;
    int num_dimensions = instruction->arg1;
    for (int i = 0; i < num_dimensions; i++) {
        auto index = (NoCIntRuntimeObject*)pop_operand(thread_ctx);
        assert(index->header.type == RuntimeObjectType::Int);
        indexes.push_back(index->value);
    }

    if (num_dimensions == vec->slice.num_dimensions) {
        return accessValueInVector(vec,
            &indexes);
    }

    return accessVectorSliceFromOffset(vec, &indexes, getHeap(thread_ctx->thread));
}

NoCStructContainer* createStaticLinkClosure(noCThreadCtx * thread_ctx) {
    noCThread* thread = thread_ctx->thread;
    auto symbolTable = thread_ctx->thread->tcb->symbolTable;
    auto closureStructLayout = searchStructLayout(symbolTable, internal_closure_struct_runtime_name());
    assert(closureStructLayout->fields->size() == 2);
    auto closureRuntimeObj = createClosureObject(getHeap(thread_ctx->thread), closureStructLayout);

    auto lambdaIp_index = getFieldIndex(closureStructLayout, CLOSURE_FP_FIELD_NAME);
    auto staticLink_index = getFieldIndex(closureStructLayout, CLOSURE_STATIC_LINK_FIELD_NAME);

    // Sanity asserts
    assert(staticLink_index > lambdaIp_index);

    auto lambdaIp = pop_operand(thread_ctx);

    closureRuntimeObj->fields[lambdaIp_index] = (char*)lambdaIp;
    assert(lambdaIp->type == RuntimeObjectType::Int);

    // Note (umar) thread->tcb->currentActivationFrame->previousFrame
    // The previous frame because the external function createStaticLinkClosure still creates an activation frame
    // However, the code transformation code that transforms non local identifiers does not know about this "extra" frame
    // This is why the previous frame is used 
    ActivationFrame * staticLink = thread->tcb->currentActivationFrame->previousFrame;
    closureRuntimeObj->fields[staticLink_index] = (char*)staticLink;
    return closureRuntimeObj;
}

// TODO :: Move these to some layout file and move away from hardcoded indexes 
ActivationFrame* get_static_link_from_closure(NoCStructContainer * closure) {

    assert(closure->header.type == RuntimeObjectType::Closure);
    char* static_link_mem_addres = closure->fields[1];
    ActivationFrame* staticLink = (ActivationFrame*)static_link_mem_addres;
    return staticLink;
}

int get_fp_from_closure(NoCStructContainer * closure) {
    assert(closure->header.type == RuntimeObjectType::Closure);
    char* fp_mem_adrress = closure->fields[0];
    NoCIntRuntimeObject* fp = (NoCIntRuntimeObject*)fp_mem_adrress;
    assert(fp->header.type == RuntimeObjectType::Int);
    return fp->value;
}

BoxedDescriptor* alloc_pinned_boxed_fd(Heap* heap, int fd) {
    BoxedDescriptor* boxedFd = (BoxedDescriptor*)noC_alloc_pin(heap, sizeof(BoxedDescriptor));
    if (boxedFd == NULL) {
        throw runtime_error("boxed fd allocation failures");
    }
    boxedFd->fd = fd;
    return boxedFd;
}

void call_external_function(noCThreadCtx * thread_ctx, Instruction * instruction) {

    if (instruction->symbol == PRINT_F_NAME) {
        auto num_args = pop_int_operand(thread_ctx);
        vector<NoCRuntimeObjectContainer*> formatArgs = vector<NoCRuntimeObjectContainer*>(num_args->value);
        auto formatStringRuntime = (NoCStructContainer*)pop_operand(thread_ctx);
        assert(formatStringRuntime->header.type == RuntimeObjectType::NativeStringStruct);

        for (int i = 0; i <= num_args->value - 1; i++) {
            auto operand = pop_operand(thread_ctx);
            formatArgs.at(i) = operand;
        }
        printStr(formatStringRuntime, &formatArgs);
    }
    else if (instruction->symbol == TO_NATIVE_STR) {
        auto non_native_str = pop_non_native_string_operand(thread_ctx);
        auto nonNativeStr = mapCStringToNativeString(getSymbolTable(thread_ctx->thread), getHeap(thread_ctx->thread), non_native_str->str);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)nonNativeStr);
    }
    else if (instruction->symbol == GET_V_THREAD_F_NAME) {
        auto vThread = createIntRuntime(getHeap(thread_ctx->thread), thread_ctx->thread->tcb->vThreadId);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)vThread);
    }
    else if (instruction->symbol == CLOSURE_CREATE_SL_F_NAME) {
        auto closureRuntimeObject = createStaticLinkClosure(thread_ctx);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)closureRuntimeObject);
    }
    else if (instruction->symbol == SLEEP_F_NAME) {
        auto sleep_seconds = pop_int_operand(thread_ctx);
        assert(sleep_seconds->header.type == RuntimeObjectType::Int);
        // Note :: We will incremement the ip before going to sleep so when we wake up we do not try to sleep again
        increment_ip(thread_ctx);
        native_sleep(thread_ctx->thread, sleep_seconds->value);
    }
    else if (instruction->symbol == CREATE_MUTEX_F_NAME) {
        auto lock = create_sleep_lock();
        auto handleObject = createExternalHandler(getHeap(thread_ctx->thread), (void*)lock, ExternalHandleType::Lock);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)handleObject);
    }
    else if (instruction->symbol == LOCK_F_NAME) {
        auto lock_runtime_obj = peek_lock_operand(thread_ctx);
        assert(lock_runtime_obj->handleType == ExternalHandleType::Lock);
        sleep_lock* lock = (sleep_lock*)lock_runtime_obj->handle;;
        sleep_lock_lock(lock, thread_ctx->thread);

        // If we do not have the lock sleep lock puts us to sleep so we can only pop operand after 
        pop_operand(thread_ctx);
    }
    else if (instruction->symbol == UNLOCK_F_NAME) {
        auto lock_runtime_obj = pop_lock_operand(thread_ctx);
        assert(lock_runtime_obj->handleType == ExternalHandleType::Lock);
        sleep_lock* lock = (sleep_lock*)lock_runtime_obj->handle;;
        sleep_lock_unlock(lock, thread_ctx->thread);
    }

    else if (instruction->symbol == TRY_LOCK_F_NAME) {
        auto lock_runtime_obj = pop_lock_operand(thread_ctx);
        assert(lock_runtime_obj->handleType == ExternalHandleType::Lock);

        sleep_lock* lock = (sleep_lock*)lock_runtime_obj->handle;;
        bool result = try_lock_sleep_lock(lock, thread_ctx->thread);
        if (result) {
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)trueBoolean);
        }
        else {
            push_operand(thread_ctx, (NoCRuntimeObjectContainer*)falseBoolean);
        }

    }
    // TODO :: Because of the generic overload we have to do a contains comparison
    // TODO :: Move to using base name 
    else if (instruction->symbol.find(AWAIT_F_NAME) != string::npos) {
        // We peek the opearnd because we the execution wait will suspend the current thread at this ip
        // and when added back to run queue it will resume from this ip trying to acquire the wait lock again so we need to maintain the stack
        auto future_runtime_obj = peek_future_operand(thread_ctx);
        assert(future_runtime_obj->handleType == ExternalHandleType::Future);
        NoCExternalHandleRuntimeObject* future_handle = (NoCExternalHandleRuntimeObject*)future_runtime_obj;
        noCThread* target = (noCThread*)future_handle->handle;;
        await(thread_ctx->thread, target);
        // TODO :: If future does not return a value what should be returned??
        await_handle_t* await_handle = (await_handle_t*)target->tcb->await_handle;
        assert(await_handle->result != NULL);
        pop_operand(thread_ctx);

        // The thread we awaited on has failed 
        if (await_handle->error != &nullObject) {
            throw_error_internal(thread_ctx, (NoCStructContainer*)await_handle->error);
            return;
        }
        // instruction.symbol is the generic await function which is poly expanded, however the functions body is this external funciton 
        auto await_funcitionSymbol = getFunctionSymbol(getSymbolTable(thread_ctx->thread), instruction->symbol);
        if (isReturnTypeVoid(await_funcitionSymbol->returnType)) {
            push_operand(thread_ctx, await_handle->error);
        }
        else {
            push_operand(thread_ctx, await_handle->error);
            push_operand(thread_ctx, await_handle->result);
        }

    }
    else if (instruction->symbol == FILE_OPEN) {
        // TODO run these static asserts on debug 
        auto external_f_open = getFunctionSymbol(getSymbolTable(thread_ctx->thread), instruction->symbol);
        assert(external_f_open != NULL);
        if (!external_f_open->markedAsThrows) {
            assert_never_reaches("function should be marked as throws");
        }

        auto file_name_str = pop_string_operand(thread_ctx);
        auto file_permissions_str = pop_string_operand(thread_ctx);

        char* file_name_as_c_array = convertNativeStringToCString(file_name_str);
        char* file_permissions_as_c_array = convertNativeStringToCString(file_permissions_str);

        FILE* file = fopen(file_name_as_c_array, file_permissions_as_c_array);
        delete file_name_as_c_array;
        delete file_permissions_as_c_array;
        if (file == NULL) {
            throw_error_internal(thread_ctx, "failed to open file");
            return;
        }
        NoCExternalHandleRuntimeObject* external_handle = createExternalHandler(getHeap(thread_ctx->thread), file, ExternalHandleType::File);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)external_handle);
    }

    else if (instruction->symbol == READ_FILE) {

        // TODO run these static asserts on debug 
        auto external_f_open = getFunctionSymbol(getSymbolTable(thread_ctx->thread), instruction->symbol);
        assert(external_f_open != NULL);
        if (!external_f_open->markedAsThrows) {
            assert_never_reaches("function should be marked as throws");
        }

        auto file_operand = pop_external_operand(thread_ctx);
        assert(file_operand->handleType == ExternalHandleType::File);
        FILE* file = (FILE*)file_operand->handle;

        auto num_bytes_operand = pop_int_operand(thread_ctx);
        size_t num_bytes_requested = safeCast(num_bytes_operand->value);

        char* c_buffer = new char[num_bytes_requested];
        size_t bytes_read = fread(c_buffer, 1, num_bytes_requested, file);

        if (bytes_read != num_bytes_requested) {
            if (ferror(file)) {
                delete[]c_buffer;
                throw_error_internal(thread_ctx, "error reading file");
                return;
            }
        }
        auto noCByteVector = createNoCByteVector(getHeap(thread_ctx->thread), bytes_read, getSymbolTable(thread_ctx->thread));
        map_char_buffer_to_native_vector(noCByteVector, c_buffer, bytes_read);
        delete[]c_buffer;
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)noCByteVector);
    }
    else if (instruction->symbol == WRITE_FILE) {

        auto external_f_open = getFunctionSymbol(getSymbolTable(thread_ctx->thread), instruction->symbol);
        assert(external_f_open != NULL);
        if (!external_f_open->markedAsThrows) {
            assert_never_reaches("function should be marked as throws");
        }

        auto file_operand = pop_external_operand(thread_ctx);
        assert(file_operand->handleType == ExternalHandleType::File);
        FILE* file = (FILE*)file_operand->handle;

        auto byte_vector = pop_vec_operand(thread_ctx);
        char* c_buffer = map_native_byte_vector_to_c_buffer(byte_vector);
        size_t bytes_written = fwrite(c_buffer, 1, byte_vector->len, file);
        delete c_buffer;

        if (bytes_written != byte_vector->len) {
            if (ferror(file)) {
                throw_error_internal(thread_ctx, "failed to write all bytes");
                return;
            }
            throw_error_internal(thread_ctx, "failed to write all the bytes");
            return;
        }
    }

    // Socket 
    else if (instruction->symbol == CREATE_SOCKET) {
        int socket_fd = create_socket();
        if (socket_fd == -1) {
            throw_error_internal(thread_ctx, "failed to create socket");
            return;
        }

        BoxedDescriptor* boxedFd = alloc_pinned_boxed_fd(getHeap(thread_ctx->thread), socket_fd);
        auto handler = createExternalHandler(getHeap(thread_ctx->thread), boxedFd, ExternalHandleType::Socket);
        unpin_object((void*)boxedFd);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)handler);
    }
    else if (instruction->symbol == CREATE_SERVER_SOCKET) {
        int socket_fd = create_socket();
        if (socket_fd == -1) {
            throw_error_internal(thread_ctx, "failed to create socket");
            return;
        }

        auto port_operand = pop_int_operand(thread_ctx);
        int result = server_socket_listen(thread_ctx->thread, socket_fd, port_operand->value);
        if (result == -1) {
            throw_error_internal(thread_ctx, "server socket listen failed");
            return;
        }

        BoxedDescriptor* boxedFd = alloc_pinned_boxed_fd(getHeap(thread_ctx->thread), socket_fd);
        auto handler = createExternalHandler(getHeap(thread_ctx->thread), boxedFd, ExternalHandleType::Socket);
        unpin_object((void*)boxedFd);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)handler);
    }

    else if (instruction->symbol == SOCKET_ACCEPT) {

        auto server_socket_operand = pop_external_operand(thread_ctx);
        int sock_fd = get_socket_fd(server_socket_operand);

        io_accept_status accept_status = accept_on_socket(sock_fd);

        if (accept_status.status == io_success::WOULD_BLOCK) {
            auto restore_operands = vector<NoCRuntimeObjectContainer*>();
            restore_operands.push_back((NoCRuntimeObjectContainer*)server_socket_operand);
            park_thread_on_fd(thread_ctx->thread, sock_fd, &restore_operands);
            return; // will never reach 
        }

        if (accept_status.status == io_success::FAILED) {
            throw_error_internal(thread_ctx, "socket accept failed");
            return;
        }

        assert(accept_status.status == io_success::SUCCESS);
        assert(accept_status.client_fd != -1);

        BoxedDescriptor* boxedFd = alloc_pinned_boxed_fd(getHeap(thread_ctx->thread), accept_status.client_fd);
        auto handler = createExternalHandler(getHeap(thread_ctx->thread), boxedFd, ExternalHandleType::Socket);
        unpin_object((void*)boxedFd);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)handler);
    }
    else if (instruction->symbol == CONNECT_SOCKET) {

        auto scocket_operand = pop_external_operand(thread_ctx);
        auto address_operand = pop_string_operand(thread_ctx);
        auto port_operand = pop_int_operand(thread_ctx);

        int sock_fd = get_socket_fd(scocket_operand);
        char* c_str = convertNativeStringToCString(address_operand);
        int result = socket_connect(thread_ctx->thread, sock_fd, c_str, port_operand->value);
        delete c_str;
        if (result == -1) {
            throw_error_internal(thread_ctx, "failed to connect to socket");
            return;
        };
    }
    else if (instruction->symbol == CLOSE_SOCKET) {
        auto socket_operand = pop_external_operand(thread_ctx);
        int socket_fd = get_socket_fd(socket_operand);
        close_socket(thread_ctx->thread, socket_fd);
    }

    else if (instruction->symbol == READ_SOCKET) {
        auto socket_operand = pop_external_operand(thread_ctx);
        auto num_bytes_operand = pop_int_operand(thread_ctx);
        int socket_fd = get_socket_fd(socket_operand);
        // When the socket is ready this will be unblock 
        size_t num_bytes = safeCast(num_bytes_operand->value);
        char* c_buffer = new char[num_bytes];
        io_read_write_status io_status = io_on_socket(io_type::READ, socket_fd, c_buffer, num_bytes);

        if (io_status.status == io_success::FAILED) {
            delete[]c_buffer;
            throw_error_internal(thread_ctx, "failed to read from socket");
            return;
        }

        if (io_status.status == io_success::WOULD_BLOCK) {
            delete[]c_buffer;
            // park on unready socket 
            auto restore_operands = vector<NoCRuntimeObjectContainer*>();
            restore_operands.push_back((NoCRuntimeObjectContainer*)socket_operand);
            restore_operands.push_back((NoCRuntimeObjectContainer*)num_bytes_operand);
            park_thread_on_fd(thread_ctx->thread, socket_fd, &restore_operands);
            return; // will never reach 
        }

        size_t bytes_transferred_st = safeCast(io_status.bytes_transferred);
        auto noCByteVector = createNoCByteVector(getHeap(thread_ctx->thread), io_status.bytes_transferred, getSymbolTable(thread_ctx->thread));
        map_char_buffer_to_native_vector(noCByteVector, c_buffer, bytes_transferred_st);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)noCByteVector);
        delete[]c_buffer;
    }
    else if (instruction->symbol == WRITE_SOCKET) {

        auto socket_operand = pop_external_operand(thread_ctx);
        int socket_fd = get_socket_fd(socket_operand);
        auto byte_buffer = pop_vec_operand(thread_ctx);
        auto offset_operand = pop_int_operand(thread_ctx);
        size_t len_of_vector = get_len_of_vector(byte_buffer);
        char* c_write_buffer = map_native_byte_vector_to_c_buffer(byte_buffer, offset_operand->value);

        io_read_write_status io_status = io_on_socket(io_type::WRITE, socket_fd, c_write_buffer, len_of_vector);
        if (io_status.status == io_success::FAILED) {
            delete[]c_write_buffer;
            throw_error_internal(thread_ctx, "failed to write to socket");
            return;
        }

        if (io_status.status == io_success::WOULD_BLOCK) {
            delete[]c_write_buffer;
            auto restore_operands = vector<NoCRuntimeObjectContainer*>();
            restore_operands.push_back((NoCRuntimeObjectContainer*)socket_operand);
            restore_operands.push_back((NoCRuntimeObjectContainer*)byte_buffer);
            restore_operands.push_back((NoCRuntimeObjectContainer*)offset_operand);
            park_thread_on_fd(thread_ctx->thread, socket_fd, &restore_operands);
            return; // will never reach 
        }
        auto bytes_written = createIntRuntime(getHeap(thread_ctx->thread), io_status.bytes_transferred);
        push_operand(thread_ctx, (NoCRuntimeObjectContainer*)bytes_written);
    }

    else if (instruction->symbol == CLOSE_FILE) {
        auto file_runtime_obj = pop_external_operand(thread_ctx);
        assert(file_runtime_obj->handleType == ExternalHandleType::File);
        FILE* file = (FILE*)file_runtime_obj->handle;
        fclose(file);
    }

    else if (instruction->symbol == GC_COLLECT) {
        auto log_level_operand = pop_int_operand(thread_ctx);
        GC_DEBUG_LOG log_level = (GC_DEBUG_LOG)log_level_operand->value;
        garbage_collect(log_level);
    }
    else if (instruction->symbol == GC_STATS) {
        print_heap_stats(getHeap(thread_ctx->thread));
    }

    else {
        throw runtime_error("external binding cannot be found for extern function");
    }

    increment_ip(thread_ctx);
}