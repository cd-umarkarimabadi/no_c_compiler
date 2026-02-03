#ifndef RUNTIME_OBJ_H
#define RUNTIME_OBJ_H

#include <string>
#include <vector>
#include <iostream>
#include <map>
#include <vector>
#include "type_checker/type_checker.h"
#include "allocator/allocator.h"
#include "type_checker/type_layout.h"
using namespace std;

enum struct ExternalHandleType {
    Lock,
    Future,
    File,
    Socket,
    ActivationFrame,
    NoCThread,
};

enum struct RuntimeObjectType {
    NDimensionVector,
    Int,
    Bool,
    Double,
    Char,
    NativeStringStruct,
    NonNativeStringWrapper,
    StructObjType,
    ExternalHandle,
    Closure,
    Null,
};

struct BoxedDescriptor {
    int fd;
};

struct RuntimeDimension {
public:
    int size;
    RuntimeDimension* next;

    RuntimeDimension(int size) {
        this->size = size;
    }
};

struct NoCRuntimeObjectContainer {
    RuntimeObjectType type;
    int type_number;
    NoCRuntimeObjectContainer() {}
    NoCRuntimeObjectContainer(RuntimeObjectType type) {
        this->type = type;
    }
};

struct NoCCharRuntimeObject {
public:
    NoCRuntimeObjectContainer header;
    char value;
};

struct NoCIntRuntimeObject {
public:
    NoCRuntimeObjectContainer header;
    int value;
};

struct NoCDoubleRuntimeObject {
public:
    NoCRuntimeObjectContainer header;
    double value;
};

struct NoCBoolRuntimeObject {
public:
    NoCRuntimeObjectContainer header;
    bool value;
};

struct NoCExternalHandleRuntimeObject {
    NoCRuntimeObjectContainer header;
    ExternalHandleType handleType;
    void* handle;
};

struct NoCFileHandle {

};

// A slice represents an offset into a contiguous buffer that the runtime will use to do vector access
// The starting offset determines where to start offset calucation from 
struct Slice {
public:
    int num_dimensions;
    int startingOffset;
    RuntimeDimension* dimension;
};

struct NoCVectorRuntimeObject {
    NoCRuntimeObjectContainer header;
    size_t len;
    Slice slice;
    no_c_field** start_of_vec;
};

struct NoCNonNativeStringWrapperRuntimeObject {
    NoCRuntimeObjectContainer header;
    string* str;
};

struct NoCStructContainer {
    NoCRuntimeObjectContainer header;
    no_c_field* fields[0];
};

struct NoCRuntimeException : public exception {
public:
    string message;
public:
    NoCRuntimeException(string err) : message(err) {}
    const char* what() const noexcept override {
        return message.c_str();
    }
};

// todo this should be tracked 
struct NoCUnhandledError : public exception {
public:
    NoCStructContainer* error_obj;
    NoCUnhandledError(NoCStructContainer* error_obj) : error_obj(error_obj) {}
    const char* what() const noexcept override {
        return "noC unhandled error";
    }
};

size_t get_size_of_smallest_object();
NoCIntRuntimeObject* createIntRuntime(Heap* heap, int resutl);
NoCBoolRuntimeObject* createBoolRuntime(Heap* heap, bool result);
NoCDoubleRuntimeObject* createDoubleRuntime(Heap* heap, double result);

NoCNonNativeStringWrapperRuntimeObject* createNonNativeStringWrapper(Heap* heap);

NoCStructContainer* createStructRuntimeObject(Heap* heap, StructLayout * layout);
NoCStructContainer* createClosureObject(Heap* heap, StructLayout* closureLayout);
NoCCharRuntimeObject* createCharRuntime(Heap* heap, char value);
NoCExternalHandleRuntimeObject* createExternalHandler(Heap* heap, void* handle, ExternalHandleType handleType);

NoCVectorRuntimeObject* createArrRuntimeObject(
    Heap* heap,
    Type * type,
    vector<size_t> dimensions
);

void storeValueInVector(NoCVectorRuntimeObject * vec,
    NoCRuntimeObjectContainer* objToStore,
    vector<int>* indexes);

NoCRuntimeObjectContainer* accessValueInVector(NoCVectorRuntimeObject * vec,
    vector<int>* indexes);

NoCRuntimeObjectContainer* accessVectorSliceFromOffset(NoCVectorRuntimeObject * vec,
    vector<int>* indexes,
    Heap* heap);

NoCRuntimeObjectContainer* accessVectorSliceFromOffset(NoCVectorRuntimeObject * vec,
    vector<int>* indexes,
    Heap* heap);

NoCRuntimeObjectContainer* getDefaultRuntimeObject(Type * type, Heap* heap);
NoCRuntimeObjectContainer* load_object_from_offset(NoCStructContainer* structContainer, size_t field_offset);

extern NoCRuntimeObjectContainer nullObject;

size_t sizeof_runtime_obj(NoCRuntimeObjectContainer* obj);
size_t get_len_of_vector(NoCVectorRuntimeObject* vector);
size_t get_len_of_dimension(NoCVectorRuntimeObject* vectorRuntime);

void map_char_buffer_to_native_vector(NoCVectorRuntimeObject * vectorRuntime,
    char* bytebuffer,
    size_t buffer_len);

char* map_native_byte_vector_to_c_buffer(NoCVectorRuntimeObject * vectorRuntime, int offset);
char* map_native_byte_vector_to_c_buffer(NoCVectorRuntimeObject * vectorRuntime);
NoCVectorRuntimeObject* createNoCByteVector(Heap* heap, int num_bytes, SymbolTable * symbolTable);
NoCStructContainer* mapCStringToNativeString(SymbolTable* symbolTable, Heap* heap, string* non_native_str);

NoCStructContainer* create_native_error_obj(SymbolTable* symbolTable, Heap* heap, string err_str);
NoCStructContainer* getStringFromError(NoCStructContainer* error);

char* get_thread_pointer_in_closure(NoCStructContainer* container);
NoCVectorRuntimeObject* getCharVector(NoCStructContainer* stringContainer);
char* convertNativeStringToCString(NoCStructContainer* native_string);
int getIntFromNativeInt(NoCRuntimeObjectContainer* native_int);
int get_socket_fd(NoCExternalHandleRuntimeObject* native_socket);

extern NoCBoolRuntimeObject* falseBoolean;
extern NoCBoolRuntimeObject* trueBoolean;

#endif