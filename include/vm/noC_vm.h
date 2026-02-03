#ifndef NOCVM_H
#define NOCVM_H

#include "vm/runtime_obj.h"
#include "gen/code_gen.h"
#include "stack"
#include "allocator/allocator.h"
#include "scheduler/scheduler.h"
#include "noCThread/noCThread.h"

void runThread(noCThread* thread);

#endif // NOCVM_H