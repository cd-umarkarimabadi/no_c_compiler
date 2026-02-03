#ifndef PRINTF_INTERP_H
#define PRINTF_INTERP_H

#include "vm/runtime_obj.h"

void printStr(NoCStructContainer * str, vector<NoCRuntimeObjectContainer*>* args);
void print_native_str(NoCStructContainer * str);

#endif // EXTERNAL_INTERP_H