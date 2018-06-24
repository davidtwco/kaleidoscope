#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>
#include "../include/KaleidoscopeJIT.h"

namespace {
    class PrototypeAST;
    class ExprAST;
}

static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
struct DebugInfo {
    llvm::DICompileUnit *TheCU;
    llvm::DIType *DblTy;
    std::vector<llvm::DIScope *> LexicalBlocks;

    void emitLocation(ExprAST *AST);
    llvm::DIType *getDoubleTy();
} KSDbgInfo;

struct SourceLocation {
    int Line;
    int Col;
};
static SourceLocation CurLoc;
static SourceLocation LexLoc = {1, 0};

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

    // Control
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10,

    // Operators
    tok_binary = -11,
    tok_unary = -12,

    // Var definition
    tok_var = -13,
};

std::string getTokName(int Tok) {
    switch (Tok) {
    case tok_eof:
        return "eof";
    case tok_def:
        return "def";
    case tok_extern:
        return "extern";
    case tok_identifier:
        return "identifier";
    case tok_number:
        return "number";
    case tok_if:
        return "if";
    case tok_then:
        return "then";
    case tok_else:
        return "else";
    case tok_for:
        return "for";
    case tok_in:
        return "in";
    case tok_binary:
        return "binary";
    case tok_unary:
        return "unary";
    case tok_var:
        return "var";
    }
    return std::string(1, (char)Tok);
}

static int advance() {
    int LastChar = getchar();

    if (LastChar == '\n' || LastChar == '\r') {
        LexLoc.Line++;
        LexLoc.Col = 0;
    } else
        LexLoc.Col++;
    return LastChar;
}

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal; // Filled in if tok_number

/// gettok - Return the next token from standard input.
static int gettok() {
    static char LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar)) {
        LastChar = advance();
    }

    CurLoc = LexLoc;

    if (isalpha(LastChar)) { // Identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while (isalnum((LastChar = advance()))) {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def") { return tok_def; }
        if (IdentifierStr == "extern") { return tok_extern; }
        if (IdentifierStr == "if") { return tok_if; }
        if (IdentifierStr == "then") { return tok_then; }
        if (IdentifierStr == "else") { return tok_else; }
        if (IdentifierStr == "for") { return tok_for; }
        if (IdentifierStr == "in") { return tok_in; }
        if (IdentifierStr == "binary") { return tok_binary; }
        if (IdentifierStr == "unary") { return tok_unary; }
        if (IdentifierStr == "var") { return tok_var; }
        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = advance();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (LastChar == '#') {
        // Comment until end of line.
        do {
            LastChar = advance();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok();
        }
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF) {
        return tok_eof;
    }

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = advance();
    return ThisChar;
}
// }}}

// Abstract Syntax Tree {{{

namespace {

llvm::raw_ostream &indent(llvm::raw_ostream &O, int size) {
    return O << std::string(size, ' ');
}

/// ExprAST - Base class for all expression nodes.
class ExprAST {
  SourceLocation Loc;

public:
  ExprAST(SourceLocation Loc = CurLoc) : Loc(Loc) {}
  virtual ~ExprAST() {}

  int getLine() const { return Loc.Line; }
  int getCol() const { return Loc.Col; }

  virtual llvm::Value *codegen() = 0;
  virtual llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) {
      return out << ':' << getLine() << ':' << getCol() << '\n';
  }
};

/// NumberExprAST - Expression class for numeric literals like "1.0";
class NumberExprAST : public ExprAST {
    double Val;

public:
    explicit NumberExprAST(double Val) : Val(Val) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        return ExprAST::dump(out << Val, ind);
    }
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
    std::string Name;

public:
    explicit VariableExprAST(SourceLocation Loc, std::string Name)
        : ExprAST(Loc), Name(std::move(Name)) { }

    const std::string &getName() const { return Name; }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        return ExprAST::dump(out << Name, ind);
    }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(SourceLocation Loc, char op, std::unique_ptr<ExprAST> LHS,
                  std::unique_ptr<ExprAST> RHS)
        : ExprAST(Loc), Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        ExprAST::dump(out << "binary" << Op, ind);
        LHS->dump(indent(out, ind) << "LHS:", ind + 1);
        RHS->dump(indent(out, ind) << "RHS:", ind + 1);
        return out;
    }
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;

public:
    CallExprAST(SourceLocation Loc, std::string Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : ExprAST(Loc), Callee(std::move(Callee)), Args(std::move(Args)) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        ExprAST::dump(out << "call " << Callee, ind);
        for (const auto &Arg : Args)
            Arg->dump(indent(out, ind + 1), ind + 1);
        return out;
    }
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;

public:
    IfExprAST(SourceLocation Loc, std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
              std::unique_ptr<ExprAST> Else)
        : ExprAST(Loc), Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        ExprAST::dump(out << "if", ind);
        Cond->dump(indent(out, ind) << "Cond:", ind + 1);
        Then->dump(indent(out, ind) << "Then:", ind + 1);
        Else->dump(indent(out, ind) << "Else:", ind + 1);
        return out;
    }
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
    std::string VarName;
    std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
    ForExprAST(std::string VarName, std::unique_ptr<ExprAST> Start,
               std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
               std::unique_ptr<ExprAST> Body)
        : VarName(std::move(VarName)), Start(std::move(Start)),
          End(std::move(End)), Body(std::move(Body)) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        ExprAST::dump(out << "for", ind);
        Start->dump(indent(out, ind) << "Cond:", ind + 1);
        End->dump(indent(out, ind) << "End:", ind + 1);
        Step->dump(indent(out, ind) << "Step:", ind + 1);
        Body->dump(indent(out, ind) << "Body:", ind + 1);
        return out;
    }
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
    char Opcode;
    std::unique_ptr<ExprAST> Operand;

public:
    UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
        : Opcode(Opcode), Operand(std::move(Operand)) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        ExprAST::dump(out << "unary" << Opcode, ind);
        Operand->dump(out, ind + 1);
        return out;
    }
};

/// VarExprAST - Expression class for var/in.
class VarExprAST : public ExprAST {
    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
    std::unique_ptr<ExprAST> Body;

public:
    VarExprAST(std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
               std::unique_ptr<ExprAST> Body)
        : VarNames(std::move(VarNames)), Body(std::move(Body)) { }

    llvm::Value *codegen() override;
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) override {
        ExprAST::dump(out << "var", ind);
        for (const auto &NamedVar : VarNames)
            NamedVar.second->dump(indent(out, ind) << NamedVar.first << ':', ind + 1);
        Body->dump(indent(out, ind) << "Body:", ind + 1);
        return out;
    }
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
    bool IsOperator;
    unsigned Precedence; // Precedence if a binary op.
    int Line;

public:
    PrototypeAST(SourceLocation Loc, std::string name,
                 std::vector<std::string> Args,
                 bool IsOperator = false, unsigned Prec = 0)
        : Name(std::move(name)), Args(std::move(Args)),
          IsOperator(IsOperator), Precedence(Prec), Line(Loc.Line) { }

    llvm::Function *codegen();
    const std::string &getName() const { return Name; }

    bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
    bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

    char getOperatorName() const {
        assert(isUnaryOp() || isBinaryOp());
        return Name[Name.size() - 1];
    }

    unsigned getBinaryPrecedence() const { return Precedence; }
    int getLine() const { return Line; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) { }

    llvm::Function *codegen();
    llvm::raw_ostream &dump(llvm::raw_ostream &out, int ind) {
        indent(out, ind) << "FunctionAST\n";
        ++ind;
        indent(out, ind) << "Body:";
        return Body ? Body->dump(out, ind) : out << "null\n";
    }
};

} // End anonymous namespace.

/// CurTok/getNextToken - Provide a simple token buffer. CurTok is the current
/// token the parser is looking at. getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() {
    return CurTok = gettok();
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "LogError: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = llvm::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // Consume the number.
    return std::move(Result);
}

static std::unique_ptr<ExprAST> ParseExpression();

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // Consume '('.
    auto V = ParseExpression();
    if (!V) {
        return nullptr;
    }

    if (CurTok != ')') {
        return LogError("expected ')'");
    }
    getNextToken(); // Consume ')'.
    return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;

    SourceLocation LitLoc = CurLoc;

    getNextToken(); // Eat identifier.

    if (CurTok != '(') { // Simple variable ref.
        return llvm::make_unique<VariableExprAST>(LitLoc, IdName);
    }

    // Call.
    getNextToken(); // Eat '('.
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression()) {
                Args.push_back(std::move(Arg));
            } else {
                return nullptr;
            }

            if (CurTok == ')') {
                break;
            }

            if (CurTok != ',') {
                return LogError("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }

    getNextToken(); // Eat ')'.

    return llvm::make_unique<CallExprAST>(LitLoc, IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfThenExpr() {
    SourceLocation IfLoc = CurLoc;
    getNextToken(); // Eat the 'if'.

    // Condition.
    auto Cond = ParseExpression();
    if (Cond == nullptr) {
        return nullptr;
    }

    if (CurTok != tok_then) {
        return LogError("expected then");
    }
    getNextToken(); // Eat the 'then'.

    auto Then = ParseExpression();
    if (Then == nullptr) {
        return nullptr;
    }

    if (CurTok != tok_else) {
        return LogError("expected else");
    }
    getNextToken(); // Eat the 'else'.

    auto Else = ParseExpression();
    if (Else == nullptr) {
        return nullptr;
    }

    return llvm::make_unique<IfExprAST>(IfLoc, std::move(Cond), std::move(Then),
                                        std::move(Else));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr() {
    getNextToken(); // Eat the 'for'.

    if (CurTok != tok_identifier) {
        return LogError("expected identifier after for");
    }

    std::string IdName = IdentifierStr;
    getNextToken(); // Eat identifier.

    if (CurTok != '=') {
        return LogError("expected '=' after for");
    }
    getNextToken(); // Eat '='.

    auto Start = ParseExpression();
    if (Start == nullptr) {
        return nullptr;
    }
    if (CurTok != ',') {
        return LogError("expected ',' after start value");
    }
    getNextToken(); // Eat ','.

    auto End = ParseExpression();
    if (End == nullptr) {
        return nullptr;
    }

    // The step value is optional.
    std::unique_ptr<ExprAST> Step;
    if (CurTok == ',') {
        getNextToken(); // Eat ','.
        Step = ParseExpression();
        if (Step == nullptr) {
            return nullptr;
        }
    }

    if (CurTok != tok_in) {
        return LogError("expected 'in' after for");
    }
    getNextToken(); // Eat 'in'.

    auto Body = ParseExpression();
    if (Body == nullptr) {
        return nullptr;
    }

    return llvm::make_unique<ForExprAST>(IdName, std::move(Start),
                                         std::move(End), std::move(Step),
                                         std::move(Body));
}

/// varexpr ::= 'var' identifier ('=' expression)?
///                   (',' identifier ('=' expression)?)* 'in' expression
static std::unique_ptr<ExprAST> ParseVarExpr() {
    getNextToken(); // Eat the 'var'.

    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

    // At least one variable name is required.
    if (CurTok != tok_identifier) {
        return LogError("expected identifier after var");
    }

    while (true) {
        std::string Name = IdentifierStr;
        getNextToken(); // Eat identifier.

        // Read optional initializer.
        std::unique_ptr<ExprAST> Init;
        if (CurTok == '=') {
            getNextToken(); // Eat the '='.

            Init = ParseExpression();
            if (Init == nullptr) {
                return nullptr;
            }
        }

        VarNames.push_back(std::make_pair(Name, std::move(Init)));

        // End of var list, exit loop.
        if (CurTok != ',') {
            break;
        }
        getNextToken(); // Eat the ','.

        if (CurTok != tok_identifier) {
            return LogError("expected identifier list after var");
        }
    }

    // At this point, we have to have 'in'.
    if (CurTok != tok_in) {
        return LogError("expected 'in' keyword after 'var'");
    }
    getNextToken(); // Eat 'in'.

    auto Body = ParseExpression();
    if (Body == nullptr) {
        return nullptr;
    }

    return llvm::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
///   ::= varexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
    default:
        return LogError("Unknown token when expecting an expression.");
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case tok_if:
        return ParseIfThenExpr();
    case tok_for:
        return ParseForExpr();
    case tok_var:
        return ParseVarExpr();
    }
}

/// unary
///   ::= primary
///   ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary() {
    // If the current token is not an operator, it must be a primary expr.
    if (!isascii(CurTok) || CurTok == '(' || CurTok == ',') {
        return ParsePrimary();
    }

    // If this is a unary operator, read it.
    int Opc = CurTok;
    getNextToken();
    if (auto Operand = ParseUnary()) {
        return llvm::make_unique<UnaryExprAST>(Opc, std::move(Operand));
    }
    return nullptr;
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (isascii(CurTok) == 0) {
        return -1;
    }

    // Make sure it's a declared binop.
    auto TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) {
        return -1;
    }
    return TokPrec;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);

/// expression
///   ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParseUnary();
    if (!LHS) {
        return nullptr;
    }

    return ParseBinOpRHS(0, std::move(LHS));
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence;
    while (true) {
        auto TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec) {
            return LHS;
        }

        // Okay, we know this is a binop.
        auto BinOp = CurTok;
        SourceLocation BinLoc = CurLoc;
        getNextToken(); // Eat binop.

        // Parse the primary expression after the binary operator.
        auto RHS = ParseUnary();
        if (!RHS) {
            return nullptr;
        }

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }

        // Merge LHS/RHS.
        LHS = llvm::make_unique<BinaryExprAST>(BinLoc, BinOp, std::move(LHS),
                                               std::move(RHS));
    } // Loop around to the top of the while loop.
}

/// prototype
///   ::= id '(' id* ')'
///   ::= binary LETTER number? '(' id, id ')'
///   ::= unary LETTER (id)
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    std::string FnName;

    SourceLocation FnLoc = CurLoc;

    unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
    unsigned BinaryPrecedence = 30;

    switch (CurTok) {
    default:
        return LogErrorP("Expected function name in prototype.");
    case tok_identifier:
        FnName = IdentifierStr;
        Kind = 0;
        getNextToken();
        break;
    case tok_unary:
        getNextToken();
        if (!isascii(CurTok)) {
            return LogErrorP("Expected unary operator.");
        }
        FnName = "unary";
        FnName += (char)CurTok;
        Kind = 1;
        getNextToken();
        break;
    case tok_binary:
        getNextToken();
        if (isascii(CurTok) == 0) {
            return LogErrorP("Expected binary operator.");
        }
        FnName = "binary";
        FnName += (char)CurTok;
        Kind = 2;
        getNextToken();

        // Read the precedence if present.
        if (CurTok == tok_number) {
            if (NumVal < 1 || NumVal > 100) {
                return LogErrorP("Invalid precedence: must be 1..100");
            }
            BinaryPrecedence = (unsigned)NumVal;
            getNextToken();
        }
        break;
    }

    if (CurTok != '(') {
        return LogErrorP("Expected '(' in prototype.");
    }

    // Read the list of argument names.
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') {
        return LogErrorP("Expected ')' in prototype.");
    }

    // Success!
    getNextToken(); // Eat ')'.

    // Verify right number of names for operator.
    if (Kind && ArgNames.size() != Kind) {
        return LogErrorP("Invalid number of operands for operator.");
    }

    return llvm::make_unique<PrototypeAST>(FnLoc, FnName,
                                           std::move(ArgNames), Kind != 0,
                                           BinaryPrecedence);
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken(); // Eat 'def'.
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpression()) {
        return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// external := 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // Eat extern.
    return ParsePrototype();
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    SourceLocation FnLoc = CurLoc;
    if (auto E = ParseExpression()) {
        // Make an anonymous proto.
        auto Proto = llvm::make_unique<PrototypeAST>(FnLoc, "main",
                                                     std::vector<std::string>());
        return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }

    return nullptr;
}

// }}}

// Debug Info Support {{{

static std::unique_ptr<llvm::DIBuilder> DBuilder;

llvm::DIType *DebugInfo::getDoubleTy() {
    if (DblTy) {
        return DblTy;
    }

    DblTy = DBuilder->createBasicType("double", 64, llvm::dwarf::DW_ATE_float);
    return DblTy;
}

void DebugInfo::emitLocation(ExprAST *AST) {
    if (!AST) {
        return Builder.SetCurrentDebugLocation(llvm::DebugLoc());
    }

    llvm::DIScope *Scope;
    if (LexicalBlocks.empty())
        Scope = TheCU;
    else
        Scope = LexicalBlocks.back();
    Builder.SetCurrentDebugLocation(
            llvm::DebugLoc::get(AST->getLine(), AST->getCol(), Scope));
}

static llvm::DISubroutineType *CreateFunctionType(unsigned NumArgs, llvm::DIFile *Unit) {
    llvm::SmallVector<llvm::Metadata *, 8> EltTys;
    llvm::DIType *DblTy = KSDbgInfo.getDoubleTy();

    // Add the result type.
    EltTys.push_back(DblTy);

    for (unsigned i = 0, e = NumArgs; i != e; ++i)
        EltTys.push_back(DblTy);

    return DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
}

// }}}

// Code Generation {{{

static std::unique_ptr<llvm::Module> TheModule;
static std::map<std::string, llvm::AllocaInst *> NamedValues;
static std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

llvm::Value *LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function. This is used for mutable variables, etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                                const std::string &VarName) {
    llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                           TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(llvm::Type::getDoubleTy(TheContext), 0,
                             VarName.c_str());
}

llvm::Value *NumberExprAST::codegen() {
    KSDbgInfo.emitLocation(this);
    return llvm::ConstantFP::get(TheContext, llvm::APFloat(Val));
}

llvm::Value *VariableExprAST::codegen() {
    // Look this variable up in the function.
    llvm::Value *V = NamedValues[Name];
    if (V == nullptr) {
        LogErrorV("Unknown variable name.");
    }

    KSDbgInfo.emitLocation(this);
    // Load the value.
    return Builder.CreateLoad(V, Name.c_str());
}

llvm::Function *getFunction(std::string Name);

llvm::Value *BinaryExprAST::codegen() {
    KSDbgInfo.emitLocation(this);

    // Special case '=' because we don't want to emit the LHS as an expression.
    if (Op == '=') {
        // Assignment requires the LHS to be an identifier.
        VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(LHS.get());
        if (LHS == nullptr) {
            return LogErrorV("destination of '=' must be a variable");
        }

        // Codegen the RHS.
        llvm::Value *Val= RHS->codegen();
        if (Val == nullptr) {
            return nullptr;
        }

        // Look up the name.
        llvm::Value *Variable = NamedValues[LHSE->getName()];
        if (Variable == nullptr) {
            return LogErrorV("unknown variable name");
        }

        Builder.CreateStore(Val, Variable);
        return Val;
    }

    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if ((L == nullptr) || (R == nullptr)) {
        return nullptr;
    }

    switch(Op) {
    case '+':
        return Builder.CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder.CreateFSub(L, R, "subtmp");
    case '*':
        return Builder.CreateFMul(L, R, "multmp");
    case '<':
        L = Builder.CreateFCmpULT(L, R, "cmptmp");
        return Builder.CreateUIToFP(L, llvm::Type::getDoubleTy(TheContext),
                                    "booltmp");
    default:
        break;
    }

    // If it wasn't a builtin binary operator, it must be a user-defined one. Emit
    // a call to it.
    llvm::Function *F = getFunction(std::string("binary") + Op);
    assert(F != nullptr && "binary operator not found!");

    llvm::Value *Ops[2] = { L, R };
    return Builder.CreateCall(F, Ops, "binop");
}

llvm::Value *CallExprAST::codegen() {
    KSDbgInfo.emitLocation(this);

    // Look up the name in the global module table.
    llvm::Function *CalleeF = getFunction(Callee);
    if (CalleeF == nullptr) {
        return LogErrorV("Unknown function referenced.");
    }

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size()) {
        return LogErrorV("Incorrect # arguments passed.");
    }

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (ArgsV.back() == nullptr) {
            return nullptr;
        }
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Value *IfExprAST::codegen() {
    KSDbgInfo.emitLocation(this);

    llvm::Value *CondV = Cond->codegen();
    if (CondV == nullptr) {
        return nullptr;
    }

    // Convert condition to a bool by comparing non-equal to 0.0.
    CondV = Builder.CreateFCmpONE(
        CondV, llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), "ifcond");

    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create basic blocks for the then and else cases. Insert the 'then' block at the
    // end of the function.
    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else");
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(TheContext, "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    // Emit 'then' value.
    Builder.SetInsertPoint(ThenBB);

    llvm::Value *ThenV = Then->codegen();
    if (ThenV == nullptr) {
        return nullptr;
    }

    Builder.CreateBr(MergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    ThenBB = Builder.GetInsertBlock();

    // Emit 'else' block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);

    llvm::Value *ElseV = Else->codegen();
    if (ElseV == nullptr) {
        return nullptr;
    }

    Builder.CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder.GetInsertBlock();

    // Emit 'merge' block.
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    llvm::PHINode *PN = Builder.CreatePHI(llvm::Type::getDoubleTy(TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

llvm::Value *ForExprAST::codegen() {
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create an alloca for the variable in the entry block.
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

    KSDbgInfo.emitLocation(this);

    // Emit the start code first, without the 'variable' in scope.
    llvm::Value *StartVal = Start->codegen();
    if (StartVal == nullptr) {
        return nullptr;
    }

    // Store the value into the alloca.
    Builder.CreateStore(StartVal, Alloca);

    // Make the new basic block for the loop header, inserting after current
    // block.
    llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "loop", TheFunction);

    // Insert an explicit fall through from the current block to the LoopBB.
    Builder.CreateBr(LoopBB);

    // Start insertion in LoopBB.
    Builder.SetInsertPoint(LoopBB);

    // Within the loop, the variable is defined equal to the PHI node. If it
    // shadows an existing variable, we have to restore it, so save it now.
    llvm::AllocaInst *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Alloca;

    // Emit the body of the loop. This, like any other expr, can change the
    // current BB. Note that we ignore the value computed by the body, but don't
    // allow an error.
    if (Body->codegen() == nullptr) {
        return nullptr;
    }

    // Emit the step value.
    llvm::Value *StepVal = nullptr;
    if (Step != nullptr) {
        StepVal = Step->codegen();
        if (StepVal == nullptr) {
            return nullptr;
        }
    } else {
        StepVal = llvm::ConstantFP::get(TheContext, llvm::APFloat(1.0));
    }

    // Compute the end condition.
    llvm::Value *EndCond = End->codegen();
    if (EndCond == nullptr) {
        return nullptr;
    }

    // Reload, increment, and restore the alloca. This handles the case where
    // the body of the loop mutates the variable.
    llvm::Value *CurVar = Builder.CreateLoad(Alloca);
    llvm::Value *NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
    Builder.CreateStore(NextVar, Alloca);

    // Convert condition to a bool by comparing non-equal to 0.0.
    EndCond = Builder.CreateFCmpONE(
        EndCond, llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), "loopcond");

    // Create the "after loop" block and insert it.
    llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(TheContext, "afterloop", TheFunction);

    // Insert the conditional branch into the end of LoopEndBB.
    Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

    // Any new code will be inserted in AfterBB.
    Builder.SetInsertPoint(AfterBB);

    // Restore the unshadowed variable.
    if (OldVal != nullptr) {
        NamedValues[VarName] = OldVal;
    } else {
        NamedValues.erase(VarName);
    }

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(TheContext));
}

llvm::Value *UnaryExprAST::codegen() {
    llvm::Value *OperandV = Operand->codegen();
    if (OperandV == nullptr) {
        return nullptr;
    }

    llvm::Function *F = getFunction(std::string("unary") + Opcode);
    if (F == nullptr) {
        return LogErrorV("Unknown unary operator.");
    }

    KSDbgInfo.emitLocation(this);
    return Builder.CreateCall(F, OperandV, "unop");
}

llvm::Value *VarExprAST::codegen() {
    std::vector<llvm::AllocaInst *> OldBindings;

    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Register all variables and emit their initializer.
    for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
        const std::string &VarName = VarNames[i].first;
        ExprAST *Init = VarNames[i].second.get();

        // Emit the initializer before adding the variable to scope, this prevents
        // the initializer from referencing the variable itself, and permits stuff
        // like this:
        //  var a = 1 in
        //    var a = a in ...   # refers to outer 'a'.
        llvm::Value *InitVal;
        if (Init) {
            InitVal = Init->codegen();
            if (InitVal == nullptr) {
                return nullptr;
            }
        } else { // If not specified, use 0.0.
            InitVal = llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0));
        }

        llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
        Builder.CreateStore(InitVal, Alloca);

        // Remember the old variable binding so that we can restore the binding
        // when we unrecurse.
        OldBindings.push_back(NamedValues[VarName]);

        // Remember this binding.
        NamedValues[VarName] = Alloca;
    }

    KSDbgInfo.emitLocation(this);

    // Codegen the body, now that all vars are in scope.
    llvm::Value *BodyVal = Body->codegen();
    if (BodyVal == nullptr) {
        return nullptr;
    }

    // Pop all our variables from scope.
    for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
        NamedValues[VarNames[i].first] = OldBindings[i];
    }

    // Return the body computation.
    return BodyVal;
}

llvm::Function *PrototypeAST::codegen() {
    std::vector<llvm::Type *> Doubles(Args.size(),
                                      llvm::Type::getDoubleTy(TheContext));

    llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(TheContext),
                                                     Doubles, false);

    llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                               Name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
        Arg.setName(Args[Idx++]);
    }

    return F;
}

llvm::Function *FunctionAST::codegen() {
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    llvm::Function *TheFunction = getFunction(P.getName());
    if (TheFunction == nullptr) {
        return nullptr;
    }

    if (TheFunction == nullptr) {
        TheFunction = Proto->codegen();
    }

    if (TheFunction == nullptr) {
        return nullptr;
    }

    if (!TheFunction->empty()) {
        return (llvm::Function*)LogErrorV("Function cannot be redefined");
    }

    // If this is an operator, install it.
    if (P.isBinaryOp()) {
        BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();
    }

    // Create a new basic block to start insertion into.
    auto *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    // Create a subprogram DIE for this function.
    llvm::DIFile *Unit = DBuilder->createFile(KSDbgInfo.TheCU->getFilename(),
                                              KSDbgInfo.TheCU->getDirectory());
    llvm::DIScope *FContext = Unit;
    unsigned LineNo = P.getLine();
    unsigned ScopeLine = LineNo;
    llvm::DISubprogram *SP = DBuilder->createFunction(
        FContext, P.getName(), llvm::StringRef(), Unit, LineNo,
        CreateFunctionType(TheFunction->arg_size(), Unit),
        false /* internal linkage */, true /* definition */, ScopeLine,
        llvm::DINode::FlagPrototyped, false);
    TheFunction->setSubprogram(SP);

    // Push the current scope.
    KSDbgInfo.LexicalBlocks.push_back(SP);

    // Unset the location for the prologue emission (leading instructions with no
    // location in a function are considered part of the prologue and the debugger
    // will run past them when breaking on a function)
    KSDbgInfo.emitLocation(nullptr);

    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    unsigned ArgIdx = 0;
    for (auto &Arg : TheFunction->args()) {
        // Create an alloca for this variable.
        llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

        // Create a debug descriptor for the variable.
        llvm::DILocalVariable *D = DBuilder->createParameterVariable(
            SP, Arg.getName(), ++ArgIdx, Unit, LineNo, KSDbgInfo.getDoubleTy(),
            true);

        DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
                                llvm::DebugLoc::get(LineNo, 0, SP),
                                Builder.GetInsertBlock());

        // Store the intial value into the alloca.
        Builder.CreateStore(&Arg, Alloca);

        // Add arguments to the variable symbol table.
        NamedValues[Arg.getName()] = Alloca;

    }

    KSDbgInfo.emitLocation(Body.get());

    if (llvm::Value *RetVal = Body->codegen()) {
        // Finish off the function.
        Builder.CreateRet(RetVal);

        // Pop off the lexical block for the function.
        KSDbgInfo.LexicalBlocks.pop_back();

        // Validate the generated code, checking for consistency.
        llvm::verifyFunction(*TheFunction);

        return TheFunction;
    }

    // Pop off the lexical block for the function since we added it
    // unconditionally.
    KSDbgInfo.LexicalBlocks.pop_back();

    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    return nullptr;
}

// }}}

// JIT and Optimizer {{{

void InitializeModule(void) {
    // Open a new module.
    TheModule = llvm::make_unique<llvm::Module>("jit", TheContext);
    TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());
}

llvm::Function *getFunction(std::string Name) {
    // First, see if the function has already been added to the current module.
    if (auto *F = TheModule->getFunction(Name)) {
        return F;
    }

    // If not, check whether we can codegen the declaration from some existing
    // prototype.
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end()) {
        return FI->second->codegen();
    }

    // If no existing prototype exists, return null.
    return nullptr;
}

// }}}

// Library Functions {{{

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
    fputc((char)X, stderr);
    return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
    fprintf(stderr, "%f\n", X);
    return 0;
}

// }}}

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (FnAST->codegen() == nullptr) {
            fprintf(stderr, "Error generating code for definition");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        if (ProtoAST->codegen() == nullptr) {
            fprintf(stderr, "Error generating code for extern");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr()) {
        if (FnAST->codegen() == nullptr) {
            fprintf(stderr, "Error generating code for top level expr");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
    while (true) {
        switch (CurTok) {
        case tok_eof:
            return;
        case ';': // Ignore top-level semicolons.
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

int main() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    getNextToken();

    TheJIT = llvm::make_unique<llvm::orc::KaleidoscopeJIT>();
    InitializeModule();

    // Add the current debug info version into the module.
    TheModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                             llvm::DEBUG_METADATA_VERSION);

    // Darwin only supports dwarf2.
    if (llvm::Triple(llvm::sys::getProcessTriple()).isOSDarwin())
        TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

    // Construct the DIBuilder, we do this here because we need the module.
    DBuilder = llvm::make_unique<llvm::DIBuilder>(*TheModule);

    // Create the compile unit for the module.
    // Currently down as "fib.ks" as a filename since we're redirecting stdin
    // but we'd like actual source locations.
    KSDbgInfo.TheCU = DBuilder->createCompileUnit(
        llvm::dwarf::DW_LANG_C, DBuilder->createFile("fib.ks", "."),
        "Kaleidoscope Compiler", 0, "", 0);

    // Run the main "interpreter loop" now.
    MainLoop();

    // Finalize the debug info.
    DBuilder->finalize();

    // Print out all of the generated code.
    TheModule->print(llvm::errs(), nullptr);

    return 0;
}

// vim:foldmethod=marker:foldlevel=0:ts=4:sts=4:sw=4:et
