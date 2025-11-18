#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

/////////////////////////////////////////////////////////////// Lexer

// Will return tokens [0 - 255] if unknown character, else it will return one of these
enum Token
{
    tok_eof = -1,

    // Commands
    tok_def = -2,
    tok_extern = -3,

    // Primary
    tok_identifier = -4,
    tok_number = 05,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

/// gettok - Return the next token from std input
static int gettok()
{
    static int LastChar = ' ';

    // Skip over whitespace
    while (isspace(LastChar))
    {
        LastChar = getchar();
    }

    // Recognize keywords and identifiers (must start with a letter, but can include digits)
    if (isalpha(LastChar))
    { // [a-zA-Z]
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar()))) //[a-zA-Z0-9]
        {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def")
        {

            // fprintf(stderr, "D %s\n", IdentifierStr.c_str());
            return tok_def;
        }
        if (IdentifierStr == "extern")
        {
            // fprintf(stderr, "E %s\n", IdentifierStr.c_str());
            return tok_extern;
        }
        // fprintf(stderr, "I %s\n", IdentifierStr.c_str());
        return tok_identifier;
    }

    // Number
    if (isdigit(LastChar) || LastChar == '.')
    { /// Extend to error handle multiple decimal points
        // fprintf(stderr, "N");
        std::string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    // Comment
    if (LastChar == '#')
    {
        // fprintf(stderr, "#");
        //  Line comment
        do
        {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF)
        {
            return gettok();
        }
    }

    // End of File
    if (LastChar == EOF)
    {
        // fprintf(stderr, "E");
        return tok_eof;
    }

    // Otherwise, return ascii value
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

/////////////////////////////////////////////////////////////// Parser

/// ExprAST - Base class expresison node
class ExprAST
{
public:
    virtual ~ExprAST() = default;
    virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression node for numeric literals, like "1.0"
class NumberExprAST : public ExprAST
{
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}
    Value *codegen() override;
};

/// VariableExprAST - Expression node for referencing a variable, like "myVar"
class VariableExprAST : public ExprAST
{
    std::string Name;

public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    Value *codegen() override;
};

// BinaryExprAST - Expression node for a binary operator, like "a + b"
class BinaryExprAST : public ExprAST
{
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    Value *codegen() override;
};

// CallEsprAst - Expression node for function calls
class CallExprAST : public ExprAST
{
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args) : Callee(Callee), Args(std::move(Args)) {}
    Value *codegen() override;
};

/// PrototypeAST - Node represents the "prototype" for a function, caputring its name and argument names (and therefor the number of arguments)
class PrototypeAST
{
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &Name, std::vector<std::string> Args) : Name(Name), Args(std::move(Args)) {}
    Function *codegen();
    const std::string &getName() const { return Name; }
};

// FunctionAST - Node represents a function definition
class FunctionAST
{
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body) : Proto(std::move(Proto)), Body(std::move(Body)) {}
    Function *codegen();
};

// Simple token buffer
static int CurTok;
static int getNextToken()
{
    return CurTok = gettok();
}

/// LogError - error handling helper functions
std::unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

// Number
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // Consume the number
    return std::move(Result);
}

// Parenthesis
static std::unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken();
    auto V = ParseExpression();
    if (!V)
    {
        return nullptr;
    }

    if (CurTok != '(')
    {
        return LogError("Expected ')'");
    }
    getNextToken();
    return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
    std::string IdName = IdentifierStr;

    getNextToken();
    // Check if not function call
    if (CurTok != '(')
    {
        // Create a variable node
        return std::make_unique<VariableExprAST>(IdName);
    }

    getNextToken();
    std::vector<std::unique_ptr<ExprAST>> Args;
    // Get all arguments of the call
    if (CurTok != ')')
    {
        while (true)
        {
            if (auto Arg = ParseExpression())
            {
                Args.push_back(std::move(Arg));
            }
            else
            {
                return nullptr;
            }

            if (CurTok == ')')
            {
                break;
            }

            if (CurTok != ',')
            {
                return LogError("Expected '(' or ',' in arguments list)");
            }
            getNextToken();
        }
    }

    getNextToken();

    // Create a call node inculding the function name and arguments
    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {
    default:
        return LogError("Unknown token when expecting an expression");
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    }
}

static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence()
{
    if (!isascii(CurTok))
    {
        return -1;
    }

    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
    {
        return -1;
    }
    return TokPrec;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS)
{
    while (true)
    {
        int TokPrec = GetTokPrecedence();

        // if the new token precidence is less than the LHS precidence, return the LHS as a finished node
        if (TokPrec < ExprPrec)
        {
            return LHS;
        }

        int BinOp = CurTok;
        getNextToken();

        // Get the next parsed section as inital RHS
        auto RHS = ParsePrimary();
        if (!RHS)
        {
            return nullptr;
        }

        int NextPrec = GetTokPrecedence();

        // If the current precidence is less than the next, recursivly expand the RHS node as far as it'll go
        if (TokPrec < NextPrec)
        {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
            {
                return nullptr;
            }
        }

        // Update the LHS node to be (current LHS) (operator) (RHS) and see if it becomes a sub node in the next loop
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

static std::unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParsePrimary();
    if (!LHS)
    {
        return nullptr;
    }
    return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<PrototypeAST> ParsePrototype()
{
    // fprintf(stdout, "TestING c\n");
    if (CurTok != tok_identifier)
    {
        // fprintf(stdout, "TestING d\n");
        //  fprintf(stderr, "T: %d C: %c", CurTok, CurTok);
        return LogErrorP("Expected function name in prototype");
    }
    // Get the name of the function
    std::string FnName = IdentifierStr;
    getNextToken();
    // fprintf(stdout, "TestING e\n");
    if (CurTok != '(')
    {
        // fprintf(stdout, "TestING f\n");
        //  fprintf(stderr, "T: %d C: %c", CurTok, CurTok);
        return LogErrorP("Expected '(' in prototype");
    }
    // fprintf(stdout, "TestING g\n");
    //  Get all the argument names
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
    {
        ArgNames.push_back(IdentifierStr);
    }
    // Get the close Parenthesi
    // fprintf(stdout, "TestING h\n");
    if (CurTok != ')')
    {
        // fprintf(stdout, "TestING i\n");
        //  fprintf(stderr, "T: %d C: %c", CurTok, CurTok);
        return LogErrorP("Expected ')' in prototype");
    }
    // fprintf(stdout, "TestING j\n");
    getNextToken();
    // Return a node containing a function name and its argument names
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

static std::unique_ptr<FunctionAST> ParseDefinition()
{

    // fprintf(stdout, "TestING!");
    getNextToken();
    // fprintf(stdout, "TestINGA");
    //  Get the prototype
    auto Proto = ParsePrototype();
    if (!Proto)
    {
        // fprintf(stdout, "TestING B");
        return nullptr;
    }

    // fprintf(stdout, "TestING2");
    //  Get the following expression
    if (auto E = ParseExpression())
    {
        // fprintf(stdout, "TestING");
        //  Return a function node pairing the prototype with the follwing expression as its definition
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

// Extern Prototype
static std::unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken();
    return ParsePrototype();
}

//
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // Create an anonymous proto
        auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        // Return the top node by pairing the anonymous proto with the top level expression
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/////////////////////////////////////////////////////////////// Code Gen
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;

Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
}

Value *NumberExprAST::codegen()
{
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen()
{

    Value *V = NamedValues[Name];
    if (!V)
    {
        LogErrorV("Unknown variable name");
    }
    return V;
}

Value *BinaryExprAST::codegen()
{
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R)
    {
        return nullptr;
    }

    switch (Op)
    {
    case '+':
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
        return Builder->CreateFMul(L, R, "multmp");
    case '/':
        return Builder->CreateFDiv(L, R, "divtmp");
    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmpltmp");
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    case '>':
        L = Builder->CreateFCmpUGT(L, R, "cmpgtmp");
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        return LogErrorV("invalid binary operator");
    }
}

Value *CallExprAST::codegen()
{
    Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF)
    {
        return LogErrorV("Unknown function referenced");
    }

    if (CalleeF->arg_size() != Args.size())
    {
        return LogErrorV("Incorect number of arguments passed");
    }

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
    {
        ArgsV.push_back(Args[i]->codegen());
        if (!Args.back())
        {
            return nullptr;
        }
    }
    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen()
{
    std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments
    unsigned Idx = 0;
    for (auto &Arg : F->args())
    {
        Arg.setName(Args[Idx++]);
    }
    return F;
}

Function *FunctionAST::codegen()
{
    Function *TheFunction = TheModule->getFunction(Proto->getName());

    if (!TheFunction)
    {
        TheFunction = Proto->codegen();
    }

    if (!TheFunction)
    {
        return nullptr;
    }

    if (!TheFunction->empty())
    {
        return (Function *)LogErrorV("Function cannot be redefined.");
    }

    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
    {
        NamedValues[std::string(Arg.getName())] = &Arg;
    }

    if (Value *RetVal = Body->codegen())
    {
        // Finish off the function
        Builder->CreateRet(RetVal);
        // Validate the generated code, checking for consistancy
        verifyFunction(*TheFunction);

        return TheFunction;
    }

    // Error reading body, remove funciton
    TheFunction->eraseFromParent();
    return nullptr;
}

/////////////////////////////////////////////////////////////// Top Level Parser and JIT Driver

static void InitializeModule()
{
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);

    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition()
{
    if (auto FnAST = ParseDefinition())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    }
    else
    {
        // Skip token for error recovery
        getNextToken();
    }
}

static void HandleExtern()
{
    if (auto ProtoAST = ParseExtern())
    {
        if (auto *FnIR = ProtoAST->codegen())
        {
            fprintf(stderr, "Read extern:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    }
    else
    {
        // Skip token for error recovery
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    if (auto FnAST = ParseTopLevelExpr())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read top-level expression:");
            FnIR->print(errs());
            fprintf(stderr, "\n");

            FnIR->eraseFromParent();
        }
    }
    else
    {
        // Skip token for error recovery
        getNextToken();
    }
}

static void DriverLoop()
{
    while (true)
    {
        fprintf(stderr, "ready> ");
        switch (CurTok)
        {
        case tok_eof:
            return;
        case ';': // ignore top-level semicolons
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

int main()
{
    // Set up binary operator precedence
    BinopPrecedence['<'] = 10;
    BinopPrecedence['>'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;
    BinopPrecedence['/'] = 40;

    // Prime first token
    fprintf(stderr, "ready> ");
    getNextToken();

    // Make the module, which holds all the code
    InitializeModule();

    // Run the driver
    DriverLoop();

    // Print out the generated code
    TheModule->print(errs(), nullptr);

    return 0;
}
