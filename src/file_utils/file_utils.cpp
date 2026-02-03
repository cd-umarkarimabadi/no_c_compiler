#include "file_utilities/file_utils.h"
using namespace std;

bool ends_with(string str, string suffix) {
    if (suffix.length() > str.length()) {
        return false;
    }
    return (str.compare(str.length() - suffix.length(), suffix.length(), suffix) == 0);
}

string strip_dot_slash(string path) {
    if (path.length() >= 2 && path.substr(0, 2) == "./") {
        return path.substr(2);
    }
    return path;
}

void read_noc_files_from_current_dir_helper(string directory, vector<string>* noc_files);

vector<string>* read_noc_files_from_current_dir() {
    vector<string>* noc_files = new vector<string>();
    read_noc_files_from_current_dir_helper(".", noc_files);
    bool found_main = false;

    for (auto noc_file : *noc_files) {
        if (noc_file == "main.noc") {
            found_main = true;
            break;
        }
    }
    if (!found_main) {
        throw runtime_error("no main file found");
    }
    return noc_files;
}


void read_noc_files_from_current_dir_helper(string directory, vector<string>* noc_files) {
    static auto noc_file_suffix = ".noc";
    try {
        for (const auto& entry : fs::directory_iterator(directory)) {
            if (fs::is_regular_file(entry.status())) {
                auto file_name_with_realtive_path = entry.path().string();
                if (ends_with(file_name_with_realtive_path, noc_file_suffix)) {
                    noc_files->push_back(
                        strip_dot_slash(file_name_with_realtive_path)
                    );
                }
            }
            else if (fs::is_directory(entry.status())) {
                read_noc_files_from_current_dir_helper(entry.path().string(), noc_files);
            }
        }
    }
    catch (const fs::filesystem_error& e) {
        cerr << "Error accessing directory: " << e.what() << endl;
        throw e;
    }
}

f_open_result open_file(string file_name) {
    struct stat s;
    int result = stat(file_name.c_str(), &s);
    if (result != 0) {
        throw runtime_error("failed to get file_size for file :: " + file_name);
    }
    size_t file_size = s.st_size;

    if (file_size == 0) {
        return f_open_result{
            .buffer = NULL,
            .len = file_size
        };
    }
    FILE* file = fopen(file_name.c_str(), "r");
    if (file == NULL) {
        throw runtime_error("faield to open file");
    }
    int fd = fileno(file);
    char* file_buf = (char*)mmap(0, file_size, PROT_READ, MAP_FILE | MAP_PRIVATE, fd, 0);
    if (file_buf == NULL) {
        fclose(file);
        throw runtime_error("failed to mmap file");
    }
    fclose(file);
    return f_open_result{
        .buffer = file_buf,
        .len = file_size
    };
}


void close_file(f_open_result* f_open_result) {
    if (f_open_result->buffer == NULL) {
        return;
    }
    int result = munmap(f_open_result->buffer, f_open_result->len);
    if (result != 0) {
        throw runtime_error("close_file failed to munmap");
    }
}
