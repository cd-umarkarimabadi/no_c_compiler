#ifndef EXTERNAL_FUNCTION_H
#define EXTERNAL_FUNCTION_H
#include "type_checker/type_checker.h"

#define PRINT_F_NAME "printf"
#define GET_V_THREAD_F_NAME "get_vthreadid"
#define CLOSURE_CREATE_F_NAME "closure_create"
#define CLOSURE_CREATE_SL_F_NAME "closure_sl_create" // SL = static link
#define SLEEP_F_NAME "sleep"
#define CREATE_MUTEX_F_NAME "create_mutex"
#define TRY_LOCK_F_NAME "try_lock"
#define LOCK_F_NAME "lock"
#define UNLOCK_F_NAME "unlock"
#define YIELD_F_NAME "yield"
#define AWAIT_F_NAME "await"
#define FUTURE_STRUCT_NAME "future"
#define TO_BYTE_ARRAY "to_byte_array"
#define GC_COLLECT "gc_collect"
#define GC_STATS "gc_stats"
#define TO_NATIVE_STR "to_native_string"

// File functions names
#define FILE_OPEN "open_file"
#define READ_FILE "read_file"
#define WRITE_FILE "write_file"
#define CLOSE_FILE "close_file"
// end file function name 

// Socket functions 
#define CREATE_SOCKET "create_socket"
#define CREATE_SERVER_SOCKET "create_server_socket"
#define CONNECT_SOCKET "connect_socket"
#define CLOSE_SOCKET "close_socket"
#define READ_SOCKET "read_socket"
#define WRITE_SOCKET "write_socket_base"
#define SOCKET_ACCEPT "accept" 
// end socket functions
FunctionSymbol* createExternalPrintStrFunction(Type* str_type);
FunctionSymbol* createExternalSlClosureCreate();

#endif // EXTERNAL_FUNCTION_H