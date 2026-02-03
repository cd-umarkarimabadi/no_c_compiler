#include <tokenizer/tokenizer.h>
#include "utils/utils.h"
#include<string>
#include <iostream>
#include <vector>
#include <iterator> 
#include <sstream>
using namespace std;

#define LEN(array) (sizeof((array)) / sizeof((array)[0]))

struct CharacterStream {
    int position;
    int streamLen;
    char* characters;
    int current_line_number;
    int current_column_number;

    CharacterStream(char* characters, int len) {
        this->characters = characters;
        this->streamLen = len;
        this->position = 0;
        this->current_line_number = 1;
        this->current_column_number = 1;
    }
};


bool canRead(CharacterStream* charStream) {
    return charStream->position < charStream->streamLen;
}

int getCurrentIndex(CharacterStream* charStream) {
    return charStream->position;
}

char peakChar(CharacterStream* charStream) {
    return charStream->characters[charStream->position];
}

char* getRawSubStreamSafe(CharacterStream* charStream, int startIndex) {
    return &charStream->characters[startIndex];
}

char readChar(CharacterStream* charStream) {
    if (charStream->position >= charStream->streamLen) {
        throw "Stream no longer has any data, you should be canRead before reading";
    }
    char toReturn = charStream->characters[charStream->position];
    charStream->position = charStream->position + 1;
    if (toReturn == '\n') {
        charStream->current_line_number++;
        charStream->current_column_number = 0;
    }
    else {
        charStream->current_column_number++;
    }
    return toReturn;
}

struct OperatorMapping {
    char literal;
    TokenType tokenType;
};

static OperatorMapping* operatorAsciiMap[126];

bool itIsPrimitve = true;
// literal types
char INT_LITERAL[] = "int";
char DOUBLE_LITERAL[] = "double";
char CHAR_LITERAL[] = "char";
char BYTE_LITERAL[] = "byte";
char VOID_LITERAL[] = "void";
char BOOL_LITERAL[] = "bool";
char FLOAT_LITERAL[] = "float";
char NULL_LITERAL[] = "null";
char STRUCT_LITERAL[] = "struct";
char NEW_LITERAL[] = "new";
char BREAK_LITERAL[] = "break";
char CONTINUE_LITERAL[] = "continue";
char YIELD_LITERAL[] = "yield";
char NO_LITERAL[] = "no";

char FOR_LITERAL[] = "for";
char TRY_LITERAL[] = "try";
char FOR_EACH_LITERAL[] = "foreach";
char IN_LITERAL[] = "in";
char ELSE_LITERAL[] = "else";
char IF_LITERAL[] = "if";
char FUNCTION_ARROW_LITERAL[] = "=>";
char RETURN_ARROW_LITERAL[] = "->";
char FUNCTION_DECLARATION_LITERAL[] = "function";
char EXTERNAL_FUNCTION_LITERAL[] = "external";
char TRUE_LITERAL[] = "true";
char TYPE_DEF_LITERAL[] = "typedef";
char FALSE_LITERAL[] = "false";
char THROWS_LITERAL[] = "throws";
char THROW_LITERAL[] = "throw";
char MATCH_LITERAL[] = "match";
char CASE_LITERAL[] = "case";
char ENUM_LITERAL[] = "enum";
char RETURN_LITERAL[] = "return";
char DEFER_LITERAL[] = "defer";
char IMPORT_LITERAL[] = "import";
char INTERFACE_LITERAL[] = "interface";
char SEMI_COLON_LITERAL = ';';
char COMMA_LITERAL = ',';
char DOT_LITERAL = '.';
char COLON_LITERAL = ':';
char OPEN_CURLY_BRACKET_LITERAL = '{';
char CLOSE_CURLY_BRACKET_LITERAL = '}';
char OPEN_BRACE_LIERAL = '[';
char CLOSE_BRACE_LIERAL = ']';
char OPEN_ROUND_BRACE_LITERAL = '(';
char CLOSE_ROUND_BRACE_LITERAL = ')';
char LESS_THAN_LITERAL = '<';
char MORE_THAN_LITERAL = '>';
char EQUAL_LITERAL = '=';
char BAR_LITERAL = '|';
char AND_LITERAL = '&';
char MOD_LITERAL = '%';
char SPACE_LITERAL[] = " ";
char ADD_LITERAL = '+';
char ADD_ADD_LITERAL[] = "++";
char MINUS_MINUS_LITERAL[] = "--";
char SUBSTRACT_LITERAL = '-';
char MULTIPLICATION_LITERAL = '*';
char DIVIDE_LITERAL = '/';
char LOGICAL_AND_LITERAL[] = "&&";
char LOGICAL_OR_LITERAL[] = "||";
char LOGICAL_NOT_LITERAL = '!';
char LOGICAL_EQUAL_LITERAL[] = "==";
char LOGICAL_LESS_THAN_EQUAL_LITERAL[] = "<=";
char LOGICAL_MORE_THAN_EQUAL_LITERAL[] = ">=";
char LOGICAL_NOT_EQUAL_LITERAL[] = "!=";
char COMMENT_LITERAL[] = "//";
char COLON_COLON_LITERAL[] = "::";

string tokenToString(TokenType tokenType) {
    switch (tokenType) {
    case MOD: return "%";
    case INT: return "int";
    case BYTE: return "byte";
    case UINT: return "uint";
    case BOOL: return "bool";
    case DOUBLE: return "double";
    case FLOAT: return "float";
    case STRING_LITERAL_TOKEN: return "string_literal";
    case CHAR: return "char";
    case STRUCT: return "struct";
    case TYPE: return "type";
    case ALIAS: return "alias";
    case VOID: return "void";
    case NULLZ: return "null";
    case IMPORT: return "import";
    case LOGICAL_LESS_THAN: return "<";
    case LOGICAL_LESS_THAN_EQUAL: return "<=";
    case LOGICAL_MORE_THAN: return ">";
    case LOGICAL_MORE_THAN_EQUAL: return ">=";
    case LOGICAL_EQUAL: return "==";
    case LOGICAL_NOT_EQUAL: return "!=";
    case LOGICAL_NOT: return "!";
    case LOGICAL_OR: return "||";
    case LOGICAL_AND: return "&&";
    case TRY: return "try";
    case EQUAL: return "=";
    case NO: return "no";
    case UNDERSCORE: return "_";
    case RETURN_ARROW: return "->";
    case DEFER: return "defer";
    case FOR: return "for";
    case IF: return "if";
    case ELSE: return "else";
    case OPEN_CURLY_BRACKET: return "{";
    case CLOSE_CURLY_BRACKET: return "}";
    case OPEN_ARR_BRACE: return "[";
    case CLOSE_ARR_BRACE: return "]";
    case OPEN_ROUND_BRACKET: return "(";
    case CLOSE_ROUND_BRACKET: return ")";
    case SEMI_COLON: return ";";
    case COMMA: return ",";
    case DOT: return ".";
    case COLON: return ":";
    case IDENTIFER: return "IDENTIFER";
    case ADD: return "+";
    case ADD_ADD: return "++";
    case MINUS_MINUS: return "--";
    case SUBTRACT: return "-";
    case MULTIPLY: return "*";
    case DIVIDE: return "/";
    case UNKNOWN: return "UNKNOWN";
    case NUMBER_LITERAL: return "NUMBER_LITERAL";
    case REAL_LITERAL: return "REAL_LITERAL";
    case FUNCTION: return "function";
    case NEW: return "new";
    case BREAK: return "break";
    case CONTINUE: return "continue";
    case FOR_EACH: return "foreach";
    case IN: return "in";
    case TRUE: return "true";
    case FALSE: return "false";
    case INTERFACE: return "interface";
    case RETURN: return "return";
    case THROWS: return "throws";
    default: return "UNKNOWN MAPPING";
    }
}

string convertToString(char* a, int size)
{
    int i;
    string s = "";
    for (i = 0; i < size; i++) {
        s = s + a[i];
    }
    return s;
}

bool isCharSpeechMark(char character) {
    return character == '"';
}

bool isCharSingleQuote(char character) {
    return character == '\'';
}

bool isCharALetter(char character) {
    if (character >= 'A' && character <= 'Z') {
        return true;
    }
    if (character >= 'a' && character <= 'z') {
        return true;
    }

    return false;
}
bool isCharANumber(char character) {

    if (character >= '0' && character <= '9') {
        return true;
    }
    return false;
}


bool isCharacterOperator(char character) {
    int index = (int)character;
    if (index < 0 || index > LEN(operatorAsciiMap) - 1) {
        return false;
    }
    auto mapping = operatorAsciiMap[index];
    return mapping != NULL;
}

void printArr(char* arr, size_t len) {
    for (size_t i = 0; i < len; i++) {
        cout << arr[i] << "";
    }
    cout << endl;
}

bool isArrEqual(char* source, size_t sourceLen, char des[], size_t destLen) {
    if (sourceLen != destLen) {
        return false;
    }

    for (size_t i = 0; i < destLen; i++) {
        if (source[i] != des[i]) {
            return false;
        }
    }

    return true;
}

void emit_tokenizer_error(int line_number, int column_number, string file_name, string err_msg) {
    ostringstream oss;
    oss << file_name << ":"
        << line_number << ":"
        << column_number << ": "
        << err_msg;
    throw runtime_error(oss.str());
}

void emit_tokenizer_error(CharacterStream* char_stream, string file_name, string err_msg) {
    emit_tokenizer_error(char_stream->current_line_number, char_stream->current_column_number, file_name, err_msg);
}

Token* addTokenForIdentifer(
    CharacterStream* stream,
    vector<Token*>* ouput_token_stream,
    char* subStr,
    size_t subStrLen) {

    Token* token = new Token(IDENTIFER, convertToString(subStr, subStrLen), false);
    token->line_number = stream->current_line_number;
    token->colum_number = stream->current_column_number;
    ouput_token_stream->push_back(token);
    return token;
}

Token* addTokenForType(
    CharacterStream* stream,
    vector<Token*>* ouput_token_stream,
    TokenType tokenType,
    char* subStr,
    size_t subStrLen) {

    Token* token = new Token(tokenType, convertToString(subStr, subStrLen), false);
    token->line_number = stream->current_line_number;
    token->colum_number = stream->current_column_number;
    ouput_token_stream->push_back(token);
    return token;
}

Token* addTokenForNumberLiteral(CharacterStream* charStream,
    vector<Token*>* ouput_token_stream,
    TokenType tokenType,
    char* subStr,
    size_t subStrLen) {

    if (tokenType != NUMBER_LITERAL && tokenType != REAL_LITERAL) {
        cout << "token type must be NUMBER_LITERAL or REAL_LITERAL" << endl;

        throw "token type must be NUMBER_LITERAL or REAL_LITERAL";
    }

    Token* token = new Token(tokenType, convertToString(subStr, subStrLen), false);
    token->line_number = charStream->current_line_number;
    token->colum_number = charStream->current_column_number;
    ouput_token_stream->push_back(token);
    return token;
}

Token* addTokenForSingleOperator(CharacterStream* charStream, vector<Token*>*ouput_token_stream, char character) {
    size_t index = (size_t)character;
    OperatorMapping* mapping = operatorAsciiMap[index];

    if (mapping == NULL) {
        cout << "mapping for single character " << character << " does not exist. This should not happen";
        throw "addTokenForSingleOperator throws error";
    }

    Token* token = new Token(mapping->tokenType, string(1, mapping->literal), false);
    token->line_number = charStream->current_line_number;
    token->colum_number = charStream->current_column_number;
    ouput_token_stream->push_back(token);
    return token;
}

Token* searchKeyWordToken(char* subStr, size_t subStrLen, size_t startingPos) {

    static const struct KeywordMapping {
        char* literal;
        size_t length;
        TokenType type;
        bool isPrimitive;
    } keywords[] = {
        // Keywords
        {NEW_LITERAL, LEN(NEW_LITERAL) - 1, NEW, false},
        {BREAK_LITERAL, LEN(BREAK_LITERAL) - 1, BREAK, false},
        {CONTINUE_LITERAL, LEN(CONTINUE_LITERAL) - 1, CONTINUE, false},
        {TRUE_LITERAL, LEN(TRUE_LITERAL) - 1, TRUE, false},
        {FALSE_LITERAL, LEN(FALSE_LITERAL) - 1, FALSE, false},
        {TYPE_DEF_LITERAL, LEN(TYPE_DEF_LITERAL) - 1, TYPEDEF, false},

        {LOGICAL_AND_LITERAL, LEN(LOGICAL_AND_LITERAL) - 1, LOGICAL_AND, false},
        {LOGICAL_OR_LITERAL, LEN(LOGICAL_OR_LITERAL) - 1, LOGICAL_OR, false},
        {LOGICAL_EQUAL_LITERAL, LEN(LOGICAL_EQUAL_LITERAL) - 1, LOGICAL_EQUAL, false},
        {LOGICAL_LESS_THAN_EQUAL_LITERAL, LEN(LOGICAL_LESS_THAN_EQUAL_LITERAL) - 1, LOGICAL_LESS_THAN_EQUAL, false},
        {LOGICAL_MORE_THAN_EQUAL_LITERAL, LEN(LOGICAL_MORE_THAN_EQUAL_LITERAL) - 1, LOGICAL_MORE_THAN_EQUAL, false},
        {LOGICAL_NOT_EQUAL_LITERAL, LEN(LOGICAL_NOT_EQUAL_LITERAL) - 1, LOGICAL_NOT_EQUAL, false},


        {ADD_ADD_LITERAL, LEN(ADD_ADD_LITERAL) - 1, ADD_ADD, false},
        {MINUS_MINUS_LITERAL, LEN(MINUS_MINUS_LITERAL) - 1, MINUS_MINUS, false},


        {DOUBLE_LITERAL, LEN(DOUBLE_LITERAL) - 1, DOUBLE, true},
        {BYTE_LITERAL, LEN(BYTE_LITERAL) - 1, BYTE, true},
        {CHAR_LITERAL, LEN(CHAR_LITERAL) - 1, CHAR, true},
        {INT_LITERAL, LEN(INT_LITERAL) - 1, INT, true},
        {FLOAT_LITERAL, LEN(FLOAT_LITERAL) - 1, FLOAT, true},


        {RETURN_LITERAL, LEN(RETURN_LITERAL) - 1, RETURN, false},
        {DEFER_LITERAL, LEN(DEFER_LITERAL) - 1, DEFER, false},
        {VOID_LITERAL, LEN(VOID_LITERAL) - 1, VOID, false},
        {NULL_LITERAL, LEN(NULL_LITERAL) - 1, NULLZ, false},
        {FOR_LITERAL, LEN(FOR_LITERAL) - 1, FOR, false},
        {FOR_EACH_LITERAL, LEN(FOR_EACH_LITERAL) - 1, FOR_EACH, false},
        {IN_LITERAL, LEN(IN_LITERAL) - 1, IN, false},
        {IF_LITERAL, LEN(IF_LITERAL) - 1, IF, false},
        {ELSE_LITERAL, LEN(ELSE_LITERAL) - 1, ELSE, false},


        {STRUCT_LITERAL, LEN(STRUCT_LITERAL) - 1, STRUCT, false},
        {INTERFACE_LITERAL, LEN(INTERFACE_LITERAL) - 1, INTERFACE, false},
        {ENUM_LITERAL, LEN(ENUM_LITERAL) - 1, ENUM, false},

        {FUNCTION_DECLARATION_LITERAL, LEN(FUNCTION_DECLARATION_LITERAL) - 1, FUNCTION, false},
        {FUNCTION_ARROW_LITERAL, LEN(FUNCTION_ARROW_LITERAL) - 1, RETURN_ARROW, false},
        {RETURN_ARROW_LITERAL, LEN(RETURN_ARROW_LITERAL) - 1, RETURN_ARROW, false},
        {EXTERNAL_FUNCTION_LITERAL, LEN(EXTERNAL_FUNCTION_LITERAL) - 1, EXTERNAL, false},

        {THROWS_LITERAL, LEN(THROWS_LITERAL) - 1, THROWS, false},
        {TRY_LITERAL, LEN(TRY_LITERAL) - 1, TRY, false},
        {THROW_LITERAL, LEN(THROW_LITERAL) - 1, THROW, false},

        {MATCH_LITERAL, LEN(MATCH_LITERAL) - 1, MATCH, false},
        {CASE_LITERAL, LEN(CASE_LITERAL) - 1, CASE, false},
        {YIELD_LITERAL, LEN(YIELD_LITERAL) - 1, YIELD, false},

        {NO_LITERAL, LEN(NO_LITERAL) - 1, NO, false},
        {COLON_COLON_LITERAL, LEN(COLON_COLON_LITERAL) - 1, COLON_COLON, false},
        {IMPORT_LITERAL, LEN(IMPORT_LITERAL) - 1, IMPORT, false}
    };

    for (auto& keyword : keywords) {
        if (isArrEqual(subStr, subStrLen, keyword.literal, keyword.length)) {
            string tokenStr = convertToString(subStr, subStrLen);
            Token* token = NULL;
            if (keyword.isPrimitive) {
                token = new Token(keyword.type, tokenStr, itIsPrimitve);
            }
            else {
                token = new Token(keyword.type, tokenStr, startingPos);
            }
            return token;
        }
    }

    return NULL;

};

Token* addTokenForKeyWord(CharacterStream* charStream, vector<Token*>* ouput_token_stream, char* subStr, size_t subStrLen, size_t startingPosition) {
    Token* token = searchKeyWordToken(subStr, subStrLen, startingPosition);
    if (token == NULL) {
        return NULL;
    }
    token->line_number = charStream->current_line_number;
    token->colum_number = charStream->current_column_number;
    ouput_token_stream->push_back(token);
    return token;
}

bool isNotTokenizable(char character) {
    return character == ' ' || character == '\n' || character == '\t';
}

void eatNonTokenizableCharacters(CharacterStream* charStream) {
    while (canRead(charStream)) {
        auto peakedChar = peakChar(charStream);
        if (isNotTokenizable(peakedChar)) {
            readChar(charStream);
        }
        else {
            break;
        }
    }
}

void eatExcessSemiColons(CharacterStream* charStream) {
    while (canRead(charStream)) {
        auto peakedChar = peakChar(charStream);
        if (peakedChar == ';') {
            readChar(charStream);
        }
        else {
            break;
        }
    }
}

bool isCharADot(char character) {
    return character == DOT_LITERAL;
}

bool peakCharIsNumber(CharacterStream* charStream) {

    if (canRead(charStream)) {
        char character = peakChar(charStream);
        return isCharANumber(character);
    }
    return false;
}

bool isComment(char* comment, size_t comment_len) {
    if (isArrEqual(comment, comment_len, COMMENT_LITERAL, LEN(COMMENT_LITERAL) - 1)) {
        return true;
    }

    return false;
}

TokenStream* tokenize(string file_name, char* file_buffer, size_t len) {

    auto ouput_token_stream = new vector<Token*>();

    auto charStreamOb = CharacterStream(file_buffer, len);
    auto charStream = &charStreamOb;
    TokenizerState state = ReadNormal;

    // TODO :: There are opeartors in this map that are not even operattors? Should be reanmed 
    static OperatorMapping operatorCharacters[24] = {
        {SEMI_COLON_LITERAL, SEMI_COLON},
        {COMMA_LITERAL, COMMA },
        {DOT_LITERAL, DOT},
        {COLON_LITERAL, COLON },
        {OPEN_CURLY_BRACKET_LITERAL, OPEN_CURLY_BRACKET },
        {CLOSE_CURLY_BRACKET_LITERAL, CLOSE_CURLY_BRACKET},
        {OPEN_BRACE_LIERAL, OPEN_ARR_BRACE },
        {CLOSE_BRACE_LIERAL, CLOSE_ARR_BRACE },
        {OPEN_ROUND_BRACE_LITERAL, OPEN_ROUND_BRACKET },
        {CLOSE_ROUND_BRACE_LITERAL, CLOSE_ROUND_BRACKET },
        {LESS_THAN_LITERAL, LOGICAL_LESS_THAN},
        {MORE_THAN_LITERAL, LOGICAL_MORE_THAN},
        {ADD_LITERAL, ADD},
        {SUBSTRACT_LITERAL, SUBTRACT},
        {MULTIPLICATION_LITERAL, MULTIPLY,},
        {DIVIDE_LITERAL, DIVIDE,},
        {LOGICAL_NOT_LITERAL, LOGICAL_NOT },
        {EQUAL_LITERAL, EQUAL},
        {BAR_LITERAL, BAR},
        {AND_LITERAL, AND},
        {MOD_LITERAL, MOD},
    };

    // Set up ascii map for operators
    for (size_t i = 0; i < LEN(operatorAsciiMap); i++) {
        operatorAsciiMap[i] = NULL;
    }

    for (size_t i = 0; i < LEN(operatorCharacters); i++) {
        OperatorMapping mapping = operatorCharacters[i];
        size_t index = (size_t)mapping.literal;
        operatorAsciiMap[index] = &operatorCharacters[i];
    }

    while (state != Done) {
        switch (state) {
        case ReadNormal:
        {
            while (canRead(charStream)) {
                char character = peakChar(charStream);
                if (isCharSingleQuote(character)) {
                    readChar(charStream);
                    if (isCharSingleQuote(peakChar(charStream))) {
                        emit_tokenizer_error(charStream, file_name, "Single quaote should contain at lease one character");
                    }

                    char* charSubStr = getRawSubStreamSafe(charStream, getCurrentIndex(charStream));
                    readChar(charStream);
                    if (!isCharSingleQuote(peakChar(charStream))) {
                        emit_tokenizer_error(charStream, file_name, "single quote should be closed");
                    }
                    readChar(charStream);
                    addTokenForType(charStream, ouput_token_stream, TokenType::CHAR_LITERAL_TOKEN, charSubStr, 1);
                    continue;
                }

                // @bug if the speech mark is not closed everything breaks 
                else if (isCharSpeechMark(character)) {
                    int line_number_of_open = charStream->current_line_number;
                    int column_number_of_open = charStream->current_column_number;

                    int startIndexOfStr = getCurrentIndex(charStream);
                    readChar(charStream);
                    while (canRead(charStream)) {
                        character = peakChar(charStream);
                        if (isCharSpeechMark(character)) {
                            break;
                        }
                        readChar(charStream);
                    }
                    char* subStr = getRawSubStreamSafe(charStream, startIndexOfStr + 1);
                    size_t subStrLen = getCurrentIndex(charStream) - 1 - startIndexOfStr;

                    if (isCharSpeechMark(peakChar(charStream))) {
                        readChar(charStream);
                    }
                    else {
                        emit_tokenizer_error(line_number_of_open, column_number_of_open, file_name, "speech marsh should have been closed");
                    }
                    // TODO :: Whenever you make the token insder the linnumber and column number
                    Token* token = new Token(STRING_LITERAL_TOKEN, convertToString(subStr, subStrLen), false);
                    token->line_number = charStream->current_line_number;
                    token->colum_number = charStream->current_column_number;
                    ouput_token_stream->push_back(token);
                }

                else if (isCharALetter(character)) {

                    int startIndexOfToken = getCurrentIndex(charStream);
                    readChar(charStream);

                    while (canRead(charStream)) {
                        character = peakChar(charStream);
                        if (isCharALetter(character)) {
                            readChar(charStream);
                        }
                        else {
                            // Allow underscores as part of identifers
                            if (character == '_') {
                                readChar(charStream);
                                continue;
                            }
                            break;
                        }
                    }

                    char* subStr = getRawSubStreamSafe(charStream, startIndexOfToken);
                    size_t subStrLen = getCurrentIndex(charStream) - startIndexOfToken;

                    // if the token is null we will assume it has to be an identifer
                    Token* token = addTokenForKeyWord(charStream, ouput_token_stream, subStr, subStrLen, startIndexOfToken);

                    if (token == NULL) {
                        auto token = addTokenForIdentifer(charStream, ouput_token_stream, subStr, subStrLen);
                        if (token == NULL) {
                            emit_tokenizer_error(charStream, file_name, "token for identifier should never be empty but is");
                        }
                    }
                    eatNonTokenizableCharacters(charStream);
                }

                // Resolve if the character and can be a two character literal 
                else if (isCharacterOperator(character)) {
                    int startIndexOfToken = getCurrentIndex(charStream);
                    readChar(charStream);

                    if (canRead(charStream)) {
                        // Try resolving two character symbol 
                        char nextSymbol = peakChar(charStream);
                        char two_chars_from_stream[2] = { character , nextSymbol };

                        if (isComment(two_chars_from_stream, 2)) {
                            state = Comment;
                            readChar(charStream);
                            break;
                        }

                        Token* token = addTokenForKeyWord(charStream, ouput_token_stream, two_chars_from_stream, 2, startIndexOfToken);
                        if (token == NULL) {
                            // We know it is just the one identifer eg !
                            token = addTokenForSingleOperator(charStream, ouput_token_stream, character);
                            if (token->tokenType == SEMI_COLON) {
                                eatExcessSemiColons(charStream);
                            }
                        }
                        else {
                            // Forward the head becuase I peaked token
                            readChar(charStream);
                        }
                    }
                    else {
                        addTokenForSingleOperator(charStream, ouput_token_stream, character);
                    }

                    eatNonTokenizableCharacters(charStream);
                }
                else if (isCharANumber(character)) {

                    TokenType numberType = TokenType::NUMBER_LITERAL;
                    Token* tokenDot = NULL;

                    int startIndexOfToken = getCurrentIndex(charStream);
                    readChar(charStream);

                    while (canRead(charStream)) {
                        character = peakChar(charStream);
                        if (isCharANumber(character)) {
                            readChar(charStream);
                        }

                        else if (isCharADot(character)) {
                            readChar(charStream);
                            // I do not care if I have seen a dot -- I will fix the real literal later in the program
                            // When I resolve the types
                            if (peakCharIsNumber(charStream)) {
                                numberType = TokenType::REAL_LITERAL;
                                continue;
                            }
                            else {
                                tokenDot = new Token(DOT, ".", false);
                                break;
                            }
                        }
                        else {
                            break;
                        }
                    }
                    char* subStr = getRawSubStreamSafe(charStream, startIndexOfToken);
                    size_t subStrLen = getCurrentIndex(charStream) - startIndexOfToken;
                    addTokenForNumberLiteral(charStream, ouput_token_stream, numberType, subStr, subStrLen);
                    // Could have resolved a trailing tokenDot
                    if (tokenDot != NULL) {
                        ouput_token_stream->push_back(tokenDot);
                    }

                }
                else if (isNotTokenizable(character)) {
                    readChar(charStream);
                    eatNonTokenizableCharacters(charStream);
                }
                else {
                    emit_tokenizer_error(charStream, file_name, "unrecgonized token");
                }
            }

            if (!canRead(charStream)) {
                state = Done;
            }

            break;
        }
        case Comment:
        {
            while (canRead(charStream)) {
                if (peakChar(charStream) != '\n') {
                    readChar(charStream);
                }
                else {
                    readChar(charStream);
                    state = ReadNormal;
                    break;
                }
            }
            if (!canRead(charStream)) {
                state = Done;
            }
            break;
        }
        case Done:
        {
            break;
        }
        }
    }

    TokenStream* stream = new TokenStream(file_name, ouput_token_stream);
    return stream;
}