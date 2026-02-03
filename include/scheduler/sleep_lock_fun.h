#ifndef SCHEDULER_LOCK_FUN_H
#define SCHEDULER_LOCK_FUN_H
#include "scheduler/sleep_lock.h"
// sleep lock 
sleep_lock* create_sleep_lock();
void sleep_lock_lock(sleep_lock * lock, noCThread * acquring_thread);
bool try_lock_sleep_lock(sleep_lock * lock, noCThread * acquring_thread);
void sleep_lock_unlock(sleep_lock * lock, noCThread * owning_thread);
// await_handle
await_handle_t* create_await_handle();
void await(noCThread * src, noCThread * awaitOn);
void signal_waiters(noCThread * src, NoCRuntimeObjectContainer* result, NoCRuntimeObjectContainer* err);

#endif // SCHEDULER_LOCK_FUN_H