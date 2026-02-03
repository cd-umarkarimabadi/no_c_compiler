#include <iostream>
#include "tokenizer/tokenizer.h"

Token endOfStream(END_OF_STREAM, "", 0);

Token* TokenStream::peakAt(size_t offset) {
    if (!this->canRead(offset)) {
        return &endOfStream;
    }
    return this->tokenStream->at(this->position + offset);
}

Token* TokenStream::peakToken() {
    if (!this->canRead()) {
        return &endOfStream;
    }
    return this->tokenStream->at(this->position);
}

Token* TokenStream::peakNextToken() {
    if (!this->canPeakNextToken()) {
        return &endOfStream;
    }

    return this->tokenStream->at(this->position + 1);
}

Token* TokenStream::readToken() {
    Token* token = tokenStream->at(this->position);
    this->position++;
    return token;
}

bool TokenStream::canRead(size_t offset) {
    return this->position + offset < this->tokenStream->size();
}

bool TokenStream::canRead() {
    return this->position < this->tokenStream->size();
}

void TokenStream::print_token_stream() {
    for (auto token : *this->tokenStream) {
        cout << tokenToString(token->tokenType) << ",";
    }
    cout << endl;
}

bool TokenStream::canPeakNextToken() {
    return this->position + 1 < this->tokenStream->size();
}