#include "scheduler/scheduler.h"
#include "scheduler/sleep_lock.h"
#include "scheduler/sleep_lock_fun.h"
#include "vm/noC_vm.h"
#include "utils/utils.h"


#define ACQUIRE_SPIN_LOCK(lock)                      \
    for (;;) {                                       \
        int spin_lock_free = lock->load();\
        while (spin_lock_free != MUTEX_FREE) {       \
        }                                            \
        int mutex_free = MUTEX_FREE;                 \
        int mutex_locked = MUTEX_LOCKED;             \
        if (lock->compare_exchange_strong(\
                mutex_free, mutex_locked)) {         \
            break;                                   \
        }                                            \
    }

#define RELEASE_SPIN_LOCK(lock)                      \
    do {                                             \
        lock->store(MUTEX_FREE);                     \
    } while (0)

sleep_lock* create_sleep_lock() {
    sleep_lock* lock = new sleep_lock();
    lock->mutex = new atomic<int>(0);
    lock->spin_lock = new atomic<int>(0);
    lock->wait_queue = new queue<noCThread*>();
    return lock;
}

bool try_lock_sleep_lock(sleep_lock * lock, noCThread * acquring_thread) {
    int mutex_free = MUTEX_FREE;
    int mutex_locked = acquring_thread->tcb->vThreadId;
    // This thread already has the lock
    if (lock->mutex->load() == mutex_locked) {
        return true;
    }
    return lock->mutex->compare_exchange_strong(mutex_free, mutex_locked);
}

void sleep_lock_lock(sleep_lock * lock, noCThread * acquring_thread) {
    int mutex_free = MUTEX_FREE;
    int mutex_locked = acquring_thread->tcb->vThreadId;
    // This thread already has the lock
    if (lock->mutex->load() == mutex_locked) {
        return;
    }
    if (lock->mutex->compare_exchange_strong(mutex_free, mutex_locked)) {
        return;
    }

    // Spin lock acquire 
    ACQUIRE_SPIN_LOCK(lock->spin_lock);
    // TODO :: This optimisation does create a starvation problem 
    // This means that arriving threads will be given higher priority if the mutex is unlocked 
    // waiting threads will not be given a chance. At some point the priority should invert  
    mutex_free = MUTEX_FREE;
    mutex_locked = acquring_thread->tcb->vThreadId;
    if (lock->mutex->compare_exchange_strong(mutex_free, mutex_locked)) {
        RELEASE_SPIN_LOCK(lock->spin_lock);
        return;
    }
    else {
        lock->wait_queue->push(acquring_thread);
        bool updated = update_thread_state_weak(acquring_thread, ThreadState_E::Waiting);
        if (!updated) {
            assert_never_reaches("Thread state being concurrently modified when waiting thread, should never happen");
        }
        // Release spin lock before parking thread 
        // Parking thread should atomically modify the thread state 
        RELEASE_SPIN_LOCK(lock->spin_lock);
        park_thread(acquring_thread);
    }
}

void sleep_lock_unlock(sleep_lock * lock, noCThread* owning_thraed) {
    int mutex_locked = owning_thraed->tcb->vThreadId;
    if (lock->mutex->load() != mutex_locked) {
        throw runtime_error("runtime error :: unlockign a thread that is not owned");
    }

    lock->mutex->store(MUTEX_FREE);
    ACQUIRE_SPIN_LOCK(lock->spin_lock);
    if (lock->wait_queue->empty()) {
        RELEASE_SPIN_LOCK(lock->spin_lock);
        return;
    }

    // TODO :: Is there a need to wake up run all threads??
    noCThread* thread = lock->wait_queue->front();
    lock->wait_queue->pop();
    resume_thread(thread);
    RELEASE_SPIN_LOCK(lock->spin_lock);
}

await_handle_t* create_await_handle() {
    await_handle_t* await_handle = new await_handle_t();
    await_handle->spin_lock = new atomic<int>(0);
    await_handle->wait_queue = new queue<noCThread*>();
    await_handle->error = &nullObject;
    await_handle->result = &nullObject;
    return await_handle;
}

void await(noCThread * src, noCThread * awaitOn) {
    if (src == awaitOn) {
        assert_never_reaches("wait:: src and awaitOn are the same, this is impossible");
    }
    await_handle_t* await_handle = (await_handle_t*)awaitOn->tcb->await_handle;
    if (threadStateAtLeast(awaitOn, ThreadState_E::Signaled)) {
        assert(await_handle->result != NULL);
        return;
    }

    ACQUIRE_SPIN_LOCK(await_handle->spin_lock);
    if (threadStateAtLeast(awaitOn, ThreadState_E::Signaled)) {
        RELEASE_SPIN_LOCK(await_handle->spin_lock);
        return;
    }
    await_handle->wait_queue->push(src);
    await_handle->num_waiters++;
    RELEASE_SPIN_LOCK(await_handle->spin_lock);

    // The parking of the thread does not happen atomically with respect to any other operation
    // See comment inside park_thread for explanation
    park_thread(src);
}

void update_thread_state(noCThread * thread, ThreadState_E threadState) {
    // Two instance of this task should be running; if we fail to set the state then we know that 
    // some other worker is running this task also 
    bool updated = update_thread_state_weak(thread, threadState);
    if (!updated) {
        assert_never_reaches("Thread state being concurrently modified when updating thread, should never happen");
    }
}

void signal_waiters(noCThread * src, NoCRuntimeObjectContainer* result, NoCRuntimeObjectContainer* err) {
    // Note :: The problem with signal is that i have to acquire the lock even thought it might be a fire and forget no routine 
    // Through type analysis we can detect this :: for example if the no routine is just fired without storing the future
    // Then at the fork call we can indicate that non one will await, because no one can, this will remove the need to ever call this function 
    /*
    no fire_and_forget() {
        // Some proceessing is happening of results I do not care about
    }
    */
    await_handle_t* await_handle = (await_handle_t*)src->tcb->await_handle;

    ACQUIRE_SPIN_LOCK(await_handle->spin_lock);

    await_handle->result = result;
    await_handle->error = err;

    if (await_handle->num_waiters == 0) {
        update_thread_state(src, ThreadState_E::Signaled);
        RELEASE_SPIN_LOCK(await_handle->spin_lock);
        return;
    }

    update_thread_state(src, ThreadState_E::Signaled);
    for (;;) {
        if (await_handle->num_waiters == 0) {
            break;
        }
        await_handle->num_waiters--;
        noCThread* thread = await_handle->wait_queue->front();
        await_handle->wait_queue->pop();
        resume_thread(thread);
    }
    RELEASE_SPIN_LOCK(await_handle->spin_lock);
}