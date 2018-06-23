#include <string>

// Lexer {{{

// The Lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,

    // Commands
    tok_def = -2,
    tok_extern = -3,

    // Primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal; // Filled in if tok_number

// gettok - Return the next token from standard input.
static int gettok() {
    static int LastChar = ' ';

    while (isspace(LastChar) != 0) {
        LastChar = getchar();
    }

    if (isalpha(LastChar) != 0) { // Identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = std::to_string(LastChar);
        while (isalnum((LastChar = getchar())) != 0) {
            IdentifierStr += std::to_string(LastChar);
        }

        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern") {
            return tok_extern;
        }
        return tok_identifier;
    }

    if (isdigit(LastChar) != 0 || LastChar == '.') { // Number: [0-9.]+
        std::string NumStr;
        do {
            NumStr += std::to_string(LastChar);
            LastChar = getchar();
        } while (isdigit(LastChar) != 0 || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (LastChar == '#') { // Comment until end of line.
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok();
        }
    }

    if (LastChar == EOF) {
        return tok_eof;
    }

    auto ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

// }}}

// vim:foldmethod=marker:foldlevel=0:ts=4:sts=4:sw=4:et
