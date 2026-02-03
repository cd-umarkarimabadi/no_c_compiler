#include "type_checker/type_checker.h"
#include "type_checker/type_layout.h"
#include "vm/runtime_obj.h"
#include <assert.h>
#include "utils/utils.h"

int getFieldIndex(StructLayout* layout, string field_name) {
    int index = 0;
    for (auto field : *layout->fields) {
        if (field->name == field_name) {
            return index;
        }
        index++;
    }
    assert_never_reaches("field cannot be found");
}

size_t get_pointer_size(RuntimeBackend backend) {
    if (backend == RuntimeBackend::NOC_VIRTUAL) {
        no_c_field* ptr;
        return sizeof(ptr);
    }
    throw runtime_error("backend type not suppported");
}

size_t sizeOfStruct(StructLayout* layout) {
    assert(layout->total_size_of_struct != 0);
    return layout->total_size_of_struct;
}

void configure_layout(SymbolTable* symbolTable, RuntimeBackend backend) {
    if (backend != RuntimeBackend::NOC_VIRTUAL) {
        throw runtime_error("backend type not suppported");
    }

    for (auto structPair : *symbolTable->structSymbolTable->table) {
        auto structLayout = structPair.second;
        int field_index = 0;

        if (structLayout->fields == NULL) {
            continue;
        }
        for (auto field : *structLayout->fields) {
            field->mem_offset = field_index;
            field_index++;
        }
        structLayout->total_size_of_struct = sizeof(NoCStructContainer) + (field_index * get_pointer_size(backend));
    }
}