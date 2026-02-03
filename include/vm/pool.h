#ifndef VM_POOL_H
#define VM_POOL_H

#include "collections/collections.h"
#include "vm/runtime_obj.h"

struct ConstantPool {
    // Exclusive ownership
    vector<NoCRuntimeObjectContainer*>* constantPool;
    vector<string>* strConstantPool;
    // Shared owernship
    NoCRuntimeObjectContainer* out_of_memory;

    ConstantPool() {
        this->constantPool = new vector<NoCRuntimeObjectContainer*>();
        this->strConstantPool = new vector<string>();
    }

    ~ConstantPool() {
        delete this->constantPool;
        delete this->strConstantPool;

    }
};

#endif //VM_POOL_H