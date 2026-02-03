#include "vm/printf.h"
#include "vm/runtime_obj.h"
#include <vector>
#include <string>
using namespace std;

enum FormatState {
    Read,
    ReadObject,
    Escape,
};

void print_native_str(NoCStructContainer * str) {
    NoCVectorRuntimeObject* str_vec = (NoCVectorRuntimeObject*)str->fields[0];
    for (size_t i = 0; i < str_vec->len; i++) {
        auto char_runtime_object = (NoCCharRuntimeObject*)str_vec->start_of_vec[i];
        char current_char = char_runtime_object->value;
        cout << current_char;
    }
}

void print_runtime_object(NoCRuntimeObjectContainer* obj) {

    if (obj->type == RuntimeObjectType::Int) {
        NoCIntRuntimeObject* casted = (NoCIntRuntimeObject*)(obj);
        cout << casted->value;
    }
    else if (obj->type == RuntimeObjectType::Double) {
        NoCDoubleRuntimeObject* casted = (NoCDoubleRuntimeObject*)(obj);
        cout << casted->value;
    }
    else if (obj->type == RuntimeObjectType::Bool) {
        NoCBoolRuntimeObject* casted = (NoCBoolRuntimeObject*)(obj);
        if (casted->value) {
            cout << "true";
        }
        else {
            cout << "false";
        }
    }
    else if (obj->type == RuntimeObjectType::Char) {
        NoCCharRuntimeObject* casted = (NoCCharRuntimeObject*)(obj);
        cout << casted->value;
    }
    else if (obj->type == RuntimeObjectType::NativeStringStruct) {
        NoCStructContainer* str = (NoCStructContainer*)obj;
        print_native_str(str);
    }
    else if (obj->type == RuntimeObjectType::StructObjType) {
        cout << obj;
    }
    else if (obj == &nullObject) {
        cout << "null";
    }
    else {
        throw runtime_error("print not supported on this object type");
    }
}

NoCRuntimeObjectContainer* get_object_from_args(vector<NoCRuntimeObjectContainer*>* obj, size_t index) {
    if (index >= obj->size()) {
        return NULL;
    }
    return obj->at(index);
}

void printStr(NoCStructContainer * format_str, vector<NoCRuntimeObjectContainer*>* args) {

    NoCVectorRuntimeObject* format_str_vec = (NoCVectorRuntimeObject*)format_str->fields[0];

    FormatState state = Read;
    NoCCharRuntimeObject* current_char_runtime_object;
    char current_char;
    size_t read_obj_index = 0;

    for (size_t i = 0; i < format_str_vec->len; i++) {
        current_char_runtime_object = (NoCCharRuntimeObject*)format_str_vec->start_of_vec[i];
        current_char = current_char_runtime_object->value;
        switch (state) {
        case ReadObject:
        {
            if (current_char == 'd' || current_char == 's' || current_char == 'c') {
                auto runtimeObject = get_object_from_args(args, read_obj_index);
                if (runtimeObject != NULL) {
                    print_runtime_object(runtimeObject);
                }
                else {
                    cout << "null";
                }
                read_obj_index++;
            }

            else if (current_char == 'p') {
                auto runtimeObject = get_object_from_args(args, read_obj_index);
                if (runtimeObject != NULL) {
                    if (runtimeObject == &nullObject) {
                        cout << "null";
                    }
                    else {
                        cout << runtimeObject;
                    }

                }
                read_obj_index++;
            }

            else if (current_char == '%') {
                state = ReadObject;
            }

            else {
                cout << current_char;
            }
            state = Read;
            break;
        }
        case Escape:
        {
            if (current_char == 'n') {
                cout << endl;
            }
            state = Read;
            break;
        }
        case Read:
        {
            if (current_char == '%') {
                state = ReadObject;
            }
            else if (current_char == '\\') {
                state = Escape;
            }
            else {
                cout << current_char;
            }
            break;
        }
        }
    }
    cout << flush;
}
