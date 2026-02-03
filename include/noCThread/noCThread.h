#ifndef NOCTHREAD_H
#define NOCTHREAD_H

using namespace std;

#include <csetjmp>
#include <pthread.h>
#include "type_checker/type_checker.h"
#include "gen/code_gen.h"
#include "allocator/allocator.h"
#include "vm/runtime_obj.h"
#include "vm/pool.h"
#include "collections/collections.h"
#include <atomic>
#include <assert.h>
#include <scheduler/scheduler_base.h>
#include <scheduler/await_handle_base.h>

enum struct ThreadState_E {
    Created,
    Ready,
    Waiting,
    WaitingLock,
    Runnable,
    Running,
    Parked,
    Signaled,
    Finished,
};

// [0:32] Holds the state from ThreadState_E
// [32:64] Holds a version for concurrent modification 
typedef uint64_t ThreadState;

//struct StaticLink;
struct noCThread;

// TODO Move variable to unique pointers to make destructor look clear 
struct ActivationFrame {

    // Exclusive ownership
    map<string, NoCRuntimeObjectContainer*>* storage;
    vector<Instruction*>* deferredInstructions;

    // Shared 
    FunctionSymbol* functionSymbol;
    ActivationFrame* previousFrame;
    ActivationFrame* staticLink;
    int returnAddress;
    int throwReturnAddress;
    noCThread* owningThread;
    NoCExternalHandleRuntimeObject* frameRefTracker;

    ~ActivationFrame() {
        delete this->storage;
        delete this->deferredInstructions;
    }
};

// TODO Move variable to unique pointers to make destructor look clear 
struct noCTCB /*noCTCB=noCThreadControlBlock*/ {

    // Exclusive ownership
    int vThreadId;
    int workerIndex;
    int Ip;
    stack<NoCRuntimeObjectContainer*>* operands;
    atomic<ThreadState>* threadState;
    InstructionStream* instructionStream;

    // Shared 
    SymbolTable* symbolTable;
    ActivationFrame* currentActivationFrame;
    ConstantPool* constPool; // TODO :: Remove this reference, should actually be innited at boot time and be global 
    bool hasNoRefrence; // TODO :: Hack think about how to remove this 
    Heap* heap;
    Scheduler* scheduler;
    await_handle_t* await_handle;

    ~noCTCB() {
        delete this->operands;
        delete this->threadState;
    }
};

struct noCThread {

    // Exclusive owernship 
    noCTCB* tcb;

    // Shared 
    noCThread* parent; // The thread is was forked from 
    NoCExternalHandleRuntimeObject* heapRefTracker;

    ~noCThread() {
        if (this->tcb) {
            delete this->tcb;
        }
    }
};

bool threadStateAtLeast(noCThread* thread, ThreadState target);
ThreadState_E getThreadState(noCThread* thread);

ActivationFrame* createNewActivationFrame(noCThread * thread, ActivationFrame* staticLink, int returnAddress, int throwAddress);

noCThread* createNoCThread(
    InstructionStream* ins,
    SymbolTable* symbolTable,
    ConstantPool* constantPool);

void thread_set_status_finished(noCThread * thread);
bool update_thread_state_weak(noCThread * thread, ThreadState_E state);
void update_thread_state_strong(noCThread * thread, ThreadState_E state);

#endif // NOCTHREAD_H