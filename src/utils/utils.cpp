#include "utils/utils.h"
#include <iostream>
#include "assert.h"


int pow(int num, int raise) {
    if (raise == 0) {
        return 1;
    }
    int result = num;
    for (int i = 1; i < raise; i++) {
        result = result * num;
    }
    return result;
}

double ascii_to_double(string literal) {
    double r = atof(literal.c_str());
    return r;
}

int ascii_to_int(string literal) {
    int return_value = 0;
    int raise = literal.size() - 1;

    const char* c_str = literal.c_str();

    bool isnegative = false;
    int begin = 0;
    int end = literal.size() - 1;

    if (c_str[0] == '-') {
        isnegative = true;
        begin = 1;
    }
    int exponent = pow(10, raise);
    for (int i = begin; i <= end; i++) {
        int toNum = c_str[i] - '0';
        return_value += toNum * exponent;
        raise = raise - 1;
        exponent = pow(10, raise);
    }
    if (isnegative) {
        return 0 - return_value;
    }
    return return_value;
}

int hex_to_int(char hex_char) {
    static char hex_array[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9','A','B', 'C', 'D','E', 'F' };
    if (hex_char >= '0' && hex_char <= '9') {
        int hex_index = hex_char - '0';
        return hex_array[hex_index];
    }
    else if (hex_char >= 'A' && hex_char <= 'F') {
        int hex_index = (hex_char - 'A') + 9;
        return hex_array[hex_index];
    }
    return -1;
}

void assert_never_reaches(string message) {
    cout << message << endl;
    assert(true == false);
}

size_t safeCast(int value) {
    if (value < 0) {
        throw out_of_range("Negative value cannot be safely converted to size_t");
    }
    return (size_t)(value);
}