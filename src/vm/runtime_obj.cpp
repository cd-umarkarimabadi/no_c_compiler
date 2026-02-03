#include "vm/runtime_obj.h"
#include "assert.h"
#include "type_checker/type_checker.h"
#include "allocator/allocator.h"
#include "type_checker/type_layout.h"
#include <string.h>
#include "utils/utils.h"
#include "assert.h"

// The null object was created to be able to carry on handling the case where the stack opearands actually have 
// real Null values wich indicates some stack corruption
NoCRuntimeObjectContainer nullObject = NoCRuntimeObjectContainer(RuntimeObjectType::Null);

struct vec_offset {
    int offset;
    RuntimeDimension* tailDimension;
};

vec_offset calculate_vector_access_offset(NoCVectorRuntimeObject * vec,
    vector<int>* indexes) {

    assert(indexes->size() <= vec->slice.num_dimensions);

    int offset = vec->slice.startingOffset;
    RuntimeDimension* tailDimension;
    tailDimension = vec->slice.dimension;
    // Here we are calucalte the lenght of dimension k, formally
    // off(dim_1) = offset
    // off(dim_k) = size_k-1 * offset
    // size_k = (size_k_1 * size_k-1 ... * size_0)
    for (size_t i = 0; i < indexes->size(); i++) {
        // TODO :: These can be cached on object creation because we would know all dimensions len 
        // So we wont need to calculate them again 
        int dim_len = 1;
        // Calculate size of the current dimension you're on 
        auto dimension = tailDimension->next;
        while (dimension != NULL) {
            dim_len = dimension->size * dim_len;
            dimension = dimension->next;
        }
        offset += indexes->at(i) * dim_len;
        tailDimension = tailDimension->next;
    }

    // Note(umar) The vec always the len of the entire N,M vector 
    if (offset < 0 || offset >= vec->len) {
        throw NoCRuntimeException("index out of bounds");
    }

    return vec_offset{
        .offset = offset,
        .tailDimension = tailDimension
    };
}


void storeValueInVector(NoCVectorRuntimeObject * vec,
    NoCRuntimeObjectContainer* objToStore,
    vector<int>* indexes) {

    vec_offset offset = calculate_vector_access_offset(vec, indexes);
    vec->start_of_vec[offset.offset] = (no_c_field*)objToStore;
}

NoCRuntimeObjectContainer* accessValueInVector(NoCVectorRuntimeObject * vec,
    vector<int>* indexes) {

    vec_offset offset = calculate_vector_access_offset(vec, indexes);
    return (NoCRuntimeObjectContainer*)vec->start_of_vec[offset.offset];
}

NoCRuntimeObjectContainer* accessVectorSliceFromOffset(NoCVectorRuntimeObject * vec,
    vector<int>* indexes,
    Heap* heap) {

    vec_offset offset = calculate_vector_access_offset(vec, indexes);

    //Create new slice from the current slice, and the offset will be the calcuated offset 
    size_t vec_size = sizeof(NoCVectorRuntimeObject);
    NoCVectorRuntimeObject* offsetedVecObj = (NoCVectorRuntimeObject*)noC_alloc(heap, vec_size);
    if (offsetedVecObj == NULL) {
        throw NoCRuntimeException("failed to allocate vecRuntimeObj");
    }
    offsetedVecObj->header.type = RuntimeObjectType::NDimensionVector;
    offsetedVecObj->slice.startingOffset = offset.offset;
    offsetedVecObj->slice.num_dimensions = vec->slice.num_dimensions - indexes->size();
    offsetedVecObj->slice.dimension = offset.tailDimension;
    offsetedVecObj->start_of_vec = vec->start_of_vec;
    offsetedVecObj->len = vec->len; // Note(umar) The vec always the len of the entire N,M vector 
    return (NoCRuntimeObjectContainer*)offsetedVecObj;

}

NoCIntRuntimeObject* createIntRuntime(Heap* heap, int result) {
    NoCIntRuntimeObject* obj = (NoCIntRuntimeObject*)noC_alloc(heap, sizeof(NoCIntRuntimeObject));
    if (obj == NULL) {
        throw NoCRuntimeException("failed to allocate NoCIntRuntimeObject");
    }
    obj->header.type = RuntimeObjectType::Int;
    obj->value = result;
    return obj;
}

NoCBoolRuntimeObject* createBoolRuntime(Heap* heap, bool result) {
    NoCBoolRuntimeObject* obj = (NoCBoolRuntimeObject*)noC_alloc(heap, sizeof(NoCBoolRuntimeObject));
    if (obj == NULL) {
        throw NoCRuntimeException("failed to allocate NoCIntRuntimeObject");
    }
    obj->header.type = RuntimeObjectType::Bool;
    obj->value = result;
    return obj;
}

NoCExternalHandleRuntimeObject* createExternalHandler(Heap* heap, void* handle, ExternalHandleType handleType) {
    NoCExternalHandleRuntimeObject* obj = (NoCExternalHandleRuntimeObject*)noC_alloc(heap, sizeof(NoCExternalHandleRuntimeObject));
    if (obj == NULL) {
        throw NoCRuntimeException("failed to allocate NoCExternalHandleRuntimeObject");
    }
    obj->handleType = handleType;
    obj->header.type = RuntimeObjectType::ExternalHandle;
    obj->handle = handle;
    return obj;
}

NoCDoubleRuntimeObject* createDoubleRuntime(Heap* heap, double result) {
    NoCDoubleRuntimeObject* obj = (NoCDoubleRuntimeObject*)noC_alloc(heap, sizeof(NoCDoubleRuntimeObject));
    if (obj == NULL) {
        throw NoCRuntimeException("failed to allocate NoCIntRuntimeObject");
    }
    obj->header.type = RuntimeObjectType::Double;
    obj->value = result;
    return obj;
}

NoCCharRuntimeObject* createCharRuntime(Heap* heap, char value) {
    NoCCharRuntimeObject* obj = (NoCCharRuntimeObject*)noC_alloc(heap, sizeof(NoCCharRuntimeObject));
    if (obj == NULL) {
        throw NoCRuntimeException("failed to allocate NoCCharRuntimeObject");
    }
    obj->header.type = RuntimeObjectType::Char;
    obj->value = value;
    return obj;
}

NoCNonNativeStringWrapperRuntimeObject* createNonNativeStringWrapper(Heap* heap) {

    NoCNonNativeStringWrapperRuntimeObject* native_wrapper = (NoCNonNativeStringWrapperRuntimeObject*)noC_alloc(heap, sizeof(NoCNonNativeStringWrapperRuntimeObject));
    if (native_wrapper == NULL) {
        throw runtime_error("failed to allocated native wrapper");
    }
    native_wrapper->header.type = RuntimeObjectType::NonNativeStringWrapper;
    return native_wrapper;
}

NoCRuntimeObjectContainer* getDefaultRuntimeObject(Type * type, Heap* heap) {

    if (type->type_of_type == TypeOfType::BaseType) {
        auto basicType = (BaseType*)type;
        if (basicType->typeStr == intReturn.typeStr) {
            return (NoCRuntimeObjectContainer*)createIntRuntime(heap, 0);
        }

        else if (basicType->typeStr == boolReturn.typeStr) {
            return (NoCRuntimeObjectContainer*)createBoolRuntime(heap, false);
        }

        else if (basicType->typeStr == doubleReturn.typeStr) {
            return (NoCRuntimeObjectContainer*)createDoubleRuntime(heap, 0.0);
        }

        else if (basicType->typeStr == byteReturn.typeStr) {
            return (NoCRuntimeObjectContainer*)createCharRuntime(heap, 0);
        }
        else if (basicType->typeStr == charReturn.typeStr) {
            return (NoCRuntimeObjectContainer*)createCharRuntime(heap, 0);
        }
        return &nullObject; // Struct are based type but default to null 
    }

    if (isTypeEnum(type)) {
        return (NoCRuntimeObjectContainer*)createIntRuntime(heap, 0);
    }
    // For any other type just return null; this included poly types
    return &nullObject;
}

NoCVectorRuntimeObject* createArrRuntimeObject(
    Heap* heap,
    Type * type,
    vector<size_t> dimensions
) {

    size_t begin = 0;
    size_t total_size_off_arr = dimensions.at(begin);
    for (size_t i = 1; i < dimensions.size(); i++) {
        total_size_off_arr *= dimensions.at(i);
    }
    size_t vector_size = sizeof(NoCVectorRuntimeObject) + (get_pointer_size(RuntimeBackend::NOC_VIRTUAL) * total_size_off_arr);
    NoCVectorRuntimeObject* vecRuntimeObj = (NoCVectorRuntimeObject*)noC_alloc_pin(heap, vector_size);
    if (vecRuntimeObj == NULL) {
        throw NoCRuntimeException("failed to allocate vecRuntimeObj");
    }

    vecRuntimeObj->header.type = RuntimeObjectType::NDimensionVector;
    // Set up slice 
    vecRuntimeObj->slice.startingOffset = 0;
    vecRuntimeObj->slice.num_dimensions = dimensions.size();
    RuntimeDimension* rootDimension = (RuntimeDimension*)noC_alloc_pin_non_managed(heap, sizeof(RuntimeDimension));
    if (rootDimension == NULL) {
        throw NoCRuntimeException("failed to allocate root dimension");
    }
    pin_object(rootDimension);

    rootDimension->size = dimensions.at(begin);
    vecRuntimeObj->slice.dimension = rootDimension;
    vecRuntimeObj->slice.num_dimensions = dimensions.size();
    auto currentDimension = vecRuntimeObj->slice.dimension;
    // Copy dimension information to slice 
    for (size_t i = 1; i < dimensions.size(); i++) {
        RuntimeDimension* nextDimension = (RuntimeDimension*)noC_alloc_pin_non_managed(heap, sizeof(RuntimeDimension));
        if (nextDimension == NULL) {
            throw NoCRuntimeException("failed to allocate next dimension");
        }
        pin_object(nextDimension);
        nextDimension->size = dimensions.at(i);
        currentDimension->next = nextDimension;
        currentDimension = currentDimension->next;
    }
    // Fill buffer with the number of object requires 
    // For a n dimensional vector this will work
    no_c_field* real_arr_element_address = (no_c_field*)(&vecRuntimeObj->start_of_vec) + sizeof(no_c_field*);
    vecRuntimeObj->start_of_vec = (no_c_field**)(real_arr_element_address);
    no_c_field** array_offset = vecRuntimeObj->start_of_vec;
    for (size_t i = 0; i < total_size_off_arr; i++) {
        auto defaultPrimitiveObject = getDefaultRuntimeObject(type, heap);
        array_offset[i] = (no_c_field*)defaultPrimitiveObject;
    }

    // Safe to unpin all objects now 
    unpin_object(vecRuntimeObj);
    auto dimension = vecRuntimeObj->slice.dimension;
    while (dimension != NULL) {
        unpin_object(dimension);
        dimension = dimension->next;
    }
    vecRuntimeObj->len = total_size_off_arr;

    return vecRuntimeObj;
}

NoCStructContainer* createStructRuntimeObject(Heap * heap, StructLayout * layout) {
    size_t sizeofStruct = sizeOfStruct(layout);
    NoCStructContainer* structObject = (NoCStructContainer*)noC_alloc_pin(heap, sizeofStruct);
    if (structObject == NULL) {
        throw NoCRuntimeException("failed to allocate NoCStructContainer");
    }
    structObject->header.type = RuntimeObjectType::StructObjType;
    structObject->header.type_number = layout->identifier_index;
    int index = 0;
    for (auto field : *layout->fields) {
        NoCRuntimeObjectContainer* defaultObject = getDefaultRuntimeObject(field->returnType, heap);
        structObject->fields[index] = (no_c_field*)defaultObject;
        index++;
    }
    unpin_object(structObject);
    return structObject;
}

// Hack because I do no want to create another container so we reuse the struct conatiner and just change the head type 
NoCStructContainer* createClosureObject(Heap * heap, StructLayout * closureLayout) {
    NoCStructContainer* closureContainer = createStructRuntimeObject(heap, closureLayout);
    closureContainer->header.type = RuntimeObjectType::Closure;
    return closureContainer;
}

NoCRuntimeObjectContainer* load_object_from_offset(NoCStructContainer * structContainer, size_t field_offset) {
    NoCRuntimeObjectContainer* childRuntimeObject = (NoCRuntimeObjectContainer*)structContainer->fields[field_offset];
    return childRuntimeObject;
}

size_t get_len_of_vector(NoCVectorRuntimeObject * vectorRuntime) {
    size_t len = 1;
    RuntimeDimension* dimension = vectorRuntime->slice.dimension;
    while (dimension != NULL) {
        len *= dimension->size;
        dimension = dimension->next;
    }
    return len;
}

// TOOD :: Confusing with get_len_of_vector
size_t get_len_of_dimension(NoCVectorRuntimeObject * vectorRuntime) {
    return vectorRuntime->slice.dimension->size;
}

void map_char_buffer_to_native_vector(NoCVectorRuntimeObject * vectorRuntime,
    char* bytebuffer,
    size_t buffer_len) {

    char** array_offset = vectorRuntime->start_of_vec;
    // I should only be mapping to the lowest level of the N dimensional array
    assert(vectorRuntime->slice.num_dimensions == 1);
    size_t len_of_vec = get_len_of_vector(vectorRuntime);
    assert(buffer_len <= len_of_vec);
    for (size_t i = 0; i < buffer_len; i++) {
        char val = bytebuffer[i];
        NoCRuntimeObjectContainer* element = (NoCRuntimeObjectContainer*)array_offset[i];
        assert(element->type == RuntimeObjectType::Char);
        NoCCharRuntimeObject* charRuntimeObject = (NoCCharRuntimeObject*)element;
        charRuntimeObject->value = val;
    }
}

// TODO RENAME THIS 
char* map_native_byte_vector_to_c_buffer(NoCVectorRuntimeObject * vectorRuntime) {
    return map_native_byte_vector_to_c_buffer(vectorRuntime, 0);
}

char* map_native_byte_vector_to_c_buffer(NoCVectorRuntimeObject * vectorRuntime, int offset) {
    if (offset < 0 || offset > vectorRuntime->len) {
        throw NoCRuntimeException("map_native_byte_vector_to_char_buffer :: bad offset");
    }
    size_t offset_st = safeCast(offset);

    no_c_field** array_offset = vectorRuntime->start_of_vec;
    // I should only be mapping to the lowest level of the N dimensional array
    assert(vectorRuntime->slice.num_dimensions == 1);
    char* output_buffer = new char[vectorRuntime->len - offset_st];
    if (output_buffer == NULL) {
        throw NoCRuntimeException("map_native_byte_vector_to_char_buffer :: Failed to allocate output buffer");
    }
    for (size_t i = offset_st; i < vectorRuntime->len; i++) {
        NoCRuntimeObjectContainer* element = (NoCRuntimeObjectContainer*)array_offset[i];
        NoCCharRuntimeObject* charRuntimeObject = (NoCCharRuntimeObject*)element;
        assert(element->type == RuntimeObjectType::Char);
        output_buffer[i] = charRuntimeObject->value;
    }
    return output_buffer;
}

// TODO :: Why is function needed, I should probably just have one byte array
NoCVectorRuntimeObject* createNoCByteVector(Heap * heap, int num_bytes, SymbolTable * symbolTable) {
    size_t num_bytes_st = safeCast(num_bytes);
    Type* byteType = getTypeDefinitionByName(symbolTable, "byte");
    vector<size_t> dimensions;
    dimensions.push_back(num_bytes_st);
    NoCVectorRuntimeObject*  byteVector = createArrRuntimeObject(heap, byteType, dimensions);
    return byteVector;
}

char* get_thread_pointer_in_closure(NoCStructContainer * container) {
    assert(container->header.type == RuntimeObjectType::Closure);
    char* thread_p = container->fields[2];
    return thread_p;
}

size_t get_size_of_smallest_object() {
    return sizeof(NoCRuntimeObjectContainer);
}

NoCStructContainer* mapCStringToNativeString(SymbolTable* symbolTable, Heap* heap, string* non_native_str) {
    auto non_native_str_len = non_native_str->length();
    auto dimensions = vector<size_t>();
    dimensions.push_back(non_native_str_len);
    auto native_char_array = createArrRuntimeObject(heap, &charReturn, dimensions);
    if (native_char_array == NULL) {
        throw NoCRuntimeException("failed to allocate native char array");
    }

    for (size_t i = 0; i < non_native_str_len; i++) {
        NoCCharRuntimeObject* charRuntimeObject = (NoCCharRuntimeObject*)native_char_array->start_of_vec[i];
        charRuntimeObject->value = non_native_str->at(i);
    }

    pin_object(native_char_array);
    auto str_struct_layout = searchStructLayout(symbolTable, "string");
    assert(str_struct_layout != NULL);

    auto native_str = createStructRuntimeObject(heap, str_struct_layout);
    if (native_str == NULL) {
        throw NoCRuntimeException("failed to allocate native str object");
    }
    unpin_object(native_char_array);
    native_str->header.type = RuntimeObjectType::NativeStringStruct;
    native_str->fields[0] = (char*)native_char_array;
    return native_str;
}

NoCStructContainer* create_native_error_obj(SymbolTable* symbolTable, Heap* heap, string err_str) {
    auto err_struct_layout = searchStructLayout(symbolTable, "err");
    assert(err_struct_layout != NULL);
    auto err_msg_field = searchField(err_struct_layout, "err_msg");
    assert(err_msg_field != NULL);
    auto err_runtime_object = createStructRuntimeObject(heap, err_struct_layout);
    if (err_runtime_object == NULL) {
        throw NoCRuntimeException("faile to allocate eror object");
    }
    pin_object(err_runtime_object);
    auto err_msg = mapCStringToNativeString(symbolTable, heap, &err_str);
    if (err_msg == NULL) {
        throw NoCRuntimeException("failed to allocate string object");
    }
    err_runtime_object->fields[err_msg_field->mem_offset] = (char*)err_msg;
    unpin_object(err_runtime_object);
    return err_runtime_object;
}

NoCVectorRuntimeObject* getCharVector(NoCStructContainer* stringContainer) {
    assert(stringContainer->header.type == RuntimeObjectType::NativeStringStruct);
    NoCStructContainer* stringStruct = (NoCStructContainer*)stringContainer;
    NoCVectorRuntimeObject* vec = (NoCVectorRuntimeObject*)stringStruct->fields[0];
    return vec;
}

NoCStructContainer* getStringFromError(NoCStructContainer* error) {
    return (NoCStructContainer*)error->fields[0];
}

char* convertNativeStringToCString(NoCStructContainer* native_string) {
    assert(native_string->header.type == RuntimeObjectType::NativeStringStruct);
    auto char_vector = getCharVector(native_string);
    char* c_buffer = map_native_byte_vector_to_c_buffer(char_vector);
    return c_buffer;
}

int getIntFromNativeInt(NoCRuntimeObjectContainer* native_int) {
    assert(native_int->type == RuntimeObjectType::Int);
    NoCIntRuntimeObject* int_casted = (NoCIntRuntimeObject*)native_int;
    return int_casted->value;
}

int get_socket_fd(NoCExternalHandleRuntimeObject* native_socket) {
    assert(native_socket->handleType == ExternalHandleType::Socket);
    BoxedDescriptor* boxed_desc = (BoxedDescriptor*)native_socket->handle;
    return boxed_desc->fd;
}
