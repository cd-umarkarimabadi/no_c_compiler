#ifndef FIEL_UTILS_H
#define FIEL_UTILS_H
#include <stddef.h>
#include <string>
#include "collections/collections.h"
#include <filesystem>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdio.h>
#include <iostream>
namespace fs = std::filesystem;
using namespace std;

struct f_open_result {
    char* buffer;
    size_t len;
};

f_open_result open_file(string file_name);
void close_file(f_open_result* result);
vector<string>* read_noc_files_from_current_dir();

#endif // #ifndef FIEL_UTILS_H


