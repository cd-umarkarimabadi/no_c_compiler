#ifndef TYPELAYOUT_H
#define TYPELAYOUT_H

#include "type_checker/type_checker.h"

#define POINTER_SIZE 64
typedef char no_c_field;

enum struct RuntimeBackend {
    ARM_64,
    NOC_VIRTUAL
};

void configure_layout(SymbolTable* symbolTable, RuntimeBackend backend);
size_t sizeOfStruct(StructLayout* layout);
size_t get_pointer_size(RuntimeBackend backend);
int getFieldIndex(StructLayout* layout, string field_name);

#endif // TYPELAYOUT_H