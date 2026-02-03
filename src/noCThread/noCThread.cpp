#include "noCThread/noCThread.h"
#include "scheduler/sleep_lock_fun.h"
#include <atomic>
#include "utils/utils.h"
atomic<int> globalVThreadCounter = 1;

noCThread * createNoCThread(
    InstructionStream * ins,
    SymbolTable * symbolTable,
    ConstantPool * constantPool) {

    auto fork_thread = new noCThread();
    fork_thread->tcb = new noCTCB();
    // <Created,0> 
    fork_thread->tcb->threadState = new atomic<ThreadState>(0);

    // TODO :: This can overflow but so unlikely 
    fork_thread->tcb->vThreadId = globalVThreadCounter.fetch_add(1);
    auto fork_thread_tcb = fork_thread->tcb;
    fork_thread_tcb->symbolTable = symbolTable;
    fork_thread_tcb->instructionStream = ins;
    fork_thread_tcb->Ip = ins->startInsIndx;
    fork_thread->tcb->constPool = constantPool;
    fork_thread->tcb->operands = new stack<NoCRuntimeObjectContainer*>();
    fork_thread->tcb->await_handle = create_await_handle();
    return fork_thread;
}

ThreadState_E getThreadState(noCThread* thread) {
    // TODO Can use memory order relaxed on this ?? 
    ThreadState current_ts = thread->tcb->threadState->load();
    uint32_t state_value = (uint32_t)(current_ts & 0xFFFFFFFFULL);
    return (ThreadState_E)state_value;
}

bool threadStateAtLeast(noCThread* thread, ThreadState_E target) {
    auto threadState = getThreadState(thread);
    return threadState >= target;
}

// This is essentially a load linked stored conditional which begs the question should I just use that to solve the ABA problem 
bool update_thread_state(noCThread * thread, ThreadState_E state, bool retry) {

    ThreadState current_ts = thread->tcb->threadState->load();
    for (;;) {
        uint32_t new_state_value_e = (uint32_t)state;
        uint32_t new_version = ((uint32_t)(current_ts >> 32)) + 1;

        ThreadState new_thread_state = 0;
        new_thread_state |= (uint32_t)new_state_value_e;
        new_thread_state |= (uint64_t)new_version << 32;

        bool swapped = thread->tcb->threadState->compare_exchange_strong(current_ts, new_thread_state);
        if (!swapped && retry) {
            continue;
        }
        return swapped;
    }

    return true;
}

void update_thread_state_strong(noCThread * thread, ThreadState_E state) {
    bool retry = true;
    bool result = update_thread_state(thread, state, retry);
    assert(result == true);
}

bool update_thread_state_weak(noCThread * thread, ThreadState_E state) {
    bool retry = false;
    return update_thread_state(thread, state, retry);
}

void thread_set_status_finished(noCThread * thread) {
    // Two instance of this task should never be running; if we fail to set the state then we know that 
    // some other worker is running this task also 
    bool updated = update_thread_state_weak(thread, ThreadState_E::Finished);
    if (!updated) {
        assert_never_reaches("Thread state being concurrently modified when finishing thread, should never happen");
    }
}