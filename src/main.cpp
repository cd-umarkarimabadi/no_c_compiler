#include <iostream>
#include <tokenizer/tokenizer.h>
#include <parser/parser.h>
#include "utils/utils.h"
#include <string>
#include "vm/noC_vm.h"
#include "ast/ast_fun.h"
#include "type_checker/type_checker.h"
#include "allocator/allocator.h"
#include "gen/code_gen.h"
#include "type_checker/type_layout.h"
#include "vm/vm_boot.h"
#include <iostream>
#include <csignal>
#include <execinfo.h>
#include <unistd.h>
#include "dependancy_graph/dependancy_graph.h"
#include "file_utilities/file_utils.h"

void seg_fault_handler(int sig) {
    void *array[10];
    size_t size;
    size = backtrace(array, 10);
    cerr << "Error: signal " << sig << ":\n";
    backtrace_symbols_fd(array, size, STDERR_FILENO);
    exit(1);
}

int main(int argc, char **argv) {
    signal(SIGSEGV, seg_fault_handler);

    try {
        // Parse and tokenize 
        vector<string>* noc_files = NULL;
        if (argc == 2) {
            noc_files = new vector<string>();
            noc_files->push_back(string(argv[1]));
        }
        else if (argc == 1) {
            noc_files = read_noc_files_from_current_dir();
        }
        else {
            throw runtime_error("too many arguments passed");
        }

        auto parse_unites = new vector<file_parse_unit*>();
        bool is_parsing_errors = false;
        for (auto nocfilename : *noc_files) {
            auto f_open_result = open_file(nocfilename);
            TokenStream* tokenStream = tokenize(nocfilename, f_open_result.buffer, f_open_result.len);
            file_parse_unit* file_unit = parseProgram(tokenStream);
            if (file_unit == NULL) {
                throw runtime_error("Terminal error :: FAILED TO PARSE");
                return 1;
            }
            if (file_unit->program == NULL) {
                throw runtime_error("Teriminal error :: FAILED TO PARSE");
                return 1;
            }
            is_parsing_errors = file_unit->is_parsing_error || is_parsing_errors;
            parse_unites->push_back(file_unit);
            close_file(&f_open_result);

            delete tokenStream;
        }

        delete noc_files;

        if (is_parsing_errors) {
            throw runtime_error("Parsing errors detected, please fix");
        }

        // Type check 
        SymbolTable* symbolTable = createSymbolTable();
        queue<file_parse_unit*>* depedancy_order = create_depedancy_order(parse_unites);
        while (!depedancy_order->empty()) {
            auto unit = depedancy_order->front();
            depedancy_order->pop();
            typeCheck(symbolTable, unit->program);
        }

        // Note(umar) We resolve the closures at the end after all the functions have been added 
        resolve_closures(symbolTable);
        delete depedancy_order;
        configure_layout(symbolTable, RuntimeBackend::NOC_VIRTUAL);

        // Gen code 
        auto entryPoint = gen(symbolTable);
        if (entryPoint->mainIp == -1) {
            throw runtime_error("main entry point cannot be found");
        }
        cout << "Main Entry Point :: " << entryPoint->mainIp << endl;
        print(entryPoint->ins);
        cout << "=====RUNTIME=====" << endl;

        // Boot vm and run code 
        boot(entryPoint, symbolTable);
        return 0;
    }
    catch (const exception &e)
    {
        cout << "\e[0;41m ERR ::  " << e.what() << "\e[m\n";
        throw e;
    }
}
