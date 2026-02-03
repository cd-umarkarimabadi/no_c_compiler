#ifndef SLEEP__LOCK_PRIM_H
#define SLEEP__LOCK_PRIM_H

#include "scheduler/scheduler.h"

#define MUTEX_FREE 0
#define MUTEX_LOCKED 1

struct sleep_lock {
    atomic<int>* mutex;
    atomic<int>* spin_lock;
    queue<noCThread*>* wait_queue;
    ~sleep_lock() {
        if (mutex) {
            delete mutex;
        }
        if (spin_lock) {
            delete spin_lock;
        }
        if (wait_queue) {
            delete wait_queue;
        }
    }
};

struct await_handle_t {
    atomic<int>* spin_lock;
    // protected by spin lock
    queue<noCThread*>* wait_queue;
    int num_waiters;
    NoCRuntimeObjectContainer* result; // this contains the result of the final value on the stack 
    NoCRuntimeObjectContainer* error;
    // end protected by spin lock
};

#endif // SCHEDULER_LOCK_H