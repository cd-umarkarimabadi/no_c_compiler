#ifndef UTILS_H
#define UTILS_H

#include <string>
#include "assert.h"
using namespace std;

#define LEN(array) (sizeof((array)) / sizeof((array)[0]))

int ascii_to_int(string literal);
double ascii_to_double(string literal);
int hex_to_int(char hex_char);

void assert_never_reaches(string message);
size_t safeCast(int value);


#endif // UITLS_H