#ifndef TOKENIZER_H
#define TOKENIZER_H

#include<string>
#include <vector>
#include <utils/utils.h>
using namespace std;

// literal types
enum TokenizerState {
    ReadNormal,
    Comment,
    Done
};

enum TokenType {
    INT,
    BYTE,
    UINT,
    BOOL,
    DOUBLE,
    FLOAT,
    STRING_LITERAL_TOKEN,
    CHAR_LITERAL_TOKEN,
    CHAR,
    BREAK,
    CONTINUE,
    STRUCT,
    TYPE,
    NEW,
    ALIAS,
    VOID,
    NULLZ,
    IMPORT,
    YIELD,
    EQUAL,
    RETURN,
    DEFER,
    UNDERSCORE,
    RETURN_ARROW,
    FOR,
    NO,
    FOR_EACH,
    IN,
    IF,
    ELSE,
    OPEN_CURLY_BRACKET,
    CLOSE_CURLY_BRACKET,
    OPEN_ARR_BRACE,
    CLOSE_ARR_BRACE,
    OPEN_ROUND_BRACKET,
    CLOSE_ROUND_BRACKET,
    SEMI_COLON,
    DOT,
    COLON,
    COLON_COLON,
    COMMA,
    IDENTIFER,
    TRUE,
    FALSE,
    TYPEDEF,
    CASE,


    LOGICAL_AND,
    LOGICAL_OR,
    LOGICAL_EQUAL,
    LOGICAL_NOT,
    LOGICAL_NOT_EQUAL,
    LOGICAL_LESS_THAN,
    LOGICAL_LESS_THAN_EQUAL,
    LOGICAL_MORE_THAN,
    LOGICAL_MORE_THAN_EQUAL,

    BAR,
    AND,
    MOD,

    ADD,
    ADD_ADD,
    MINUS_MINUS,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,


    FUNCTION,
    EXTERNAL,


    NUMBER_LITERAL,
    REAL_LITERAL,

    INTERFACE,

    THROWS,
    THROW,
    MATCH,
    ENUM,
    TRY,
    UNKNOWN,
    END_OF_STREAM // Signified the end of file
};


struct Token {
    TokenType tokenType;
    bool isTokenTypePrimitve;
    string literal;
    size_t colum_number;
    size_t line_number;

    Token(TokenType tokenType, string literal, bool isTokenTypePrimitive) {
        this->tokenType = tokenType;
        this->literal = literal;
        this->isTokenTypePrimitve = isTokenTypePrimitive;
    }

    Token(TokenType tokenType, string literal) {
        this->tokenType = tokenType;
        this->literal = literal;
        this->isTokenTypePrimitve = false;
    }
};

string tokenToString(TokenType tokenType);

struct TokenStream {
    vector<Token*>* tokenStream;
    string file_name;
    size_t position;
    TokenStream(string file_name, vector<Token*>* tokens) {
        this->tokenStream = tokens;
        this->file_name = file_name;
        this->position = 0;
    }

    ~TokenStream() {
        if (this->tokenStream != NULL) {
            for (auto token : *this->tokenStream) {
                delete token;
            }
            delete this->tokenStream;
        }
    }

    Token* readToken();
    Token* peakToken();
    Token* peakNextToken();
    bool canPeakNextToken();
    bool canRead(size_t offset);
    Token* peakAt(size_t offset);
    vector<Token*>* getTokenStream();
    bool canRead();
    void print_token_stream();

};

TokenStream* tokenize(string file_name, char* file_buffer, size_t file_len);

#endif  // TOKENIZER_H