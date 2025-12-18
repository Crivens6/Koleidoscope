#include "include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;
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
    tok_number = -5,

    tok_if = -6,
    tok_then = -7,
    tok_else = -8,

    tok_for = -9,
    tok_in = -10
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
            return tok_def;
        }
        if (IdentifierStr == "extern")
        {
            return tok_extern;
        }
        if(IdentifierStr == "if"){
            return tok_if;
        }
        if(IdentifierStr == "then"){
            return tok_then;
        }
        if(IdentifierStr == "else"){
            return tok_else;
        }
        if(IdentifierStr == "for"){
            return tok_for;
        }
        if(IdentifierStr == "in"){
            return tok_in;
        }
        return tok_identifier;
    }

    // Number
    if (isdigit(LastChar) || LastChar == '.')
    { /// Extend to error handle multiple decimal points
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
        return tok_eof;
    }

    // Otherwise, return ascii value
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

/////////////////////////////////////////////////////////////// Parser

/// ExprAST - Base class expression node
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

/// IfExprAST - Expression class for if/then/else
class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then,Else;
    
    public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, std::unique_ptr<ExprAST> Else)
    :Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)){}
    Value *codegen() override;
};

/// @brief  ForExprAST - Expression class for for/in
class ForExprAST : public ExprAST {
    std:: string VarName;
    std::unique_ptr<ExprAST> Start, End, Step, Body;

    public:
    ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start, std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step, std::unique_ptr<ExprAST> Body)
    : VarName(VarName), Start(std::move(Start)), End(std::move(End)), Step(std::move(Step)), Body(std::move(Body)){}
    Value *codegen() override;
};

/// PrototypeAST - Node represents the "prototype" for a function, capturing its name and argument names (and therefor the number of arguments)
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

    if (CurTok != ')')
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
                return LogError("Expected ')' or ',' in arguments list)");
            }
            getNextToken();
        }
    }

    getNextToken();

    // Create a call node including the function name and arguments
    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParseIfExpr(){
    getNextToken(); //eat 'if'

    auto Cond = ParseExpression();
    if(!Cond){
        return nullptr;
    }

    if(CurTok != tok_then){
        return LogError("expected then");
    }
    getNextToken(); // eat 'then'

    auto Then = ParseExpression();
    if(!Then){
        return nullptr;
    }

    if(CurTok != tok_else){
        return LogError("expected else");
    }
    getNextToken();
    
    auto Else = ParseExpression();
    if(!Else){
        return nullptr;
    }

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),std::move(Else));
}

// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)?
static std::unique_ptr<ExprAST> ParseForExpr(){
    getNextToken();

    if(CurTok != tok_identifier){
        return LogError("expected identifier after for");
    }

    std::string IdName = IdentifierStr;
    getNextToken();

    if(CurTok != '='){
        return LogError("expected '=' after for");
    }
    getNextToken();

    auto Start = ParseExpression();
    if(!Start){
        return nullptr;
    }
    if(CurTok != ','){
        return LogError("expected ',' after for start value");
    }
    getNextToken();

    auto End = ParseExpression();
    if(!End){
        return nullptr;
    }

    // The Step value is optional
    std::unique_ptr<ExprAST> Step;
    if(CurTok == ','){
        getNextToken();
        Step = ParseExpression();
        if(!Step){
            return nullptr;
        }
    }

    if(CurTok != tok_in){
        return LogError("expected 'in' after for");
    }
    getNextToken();

    auto Body = ParseExpression();
    if(!Body){
        return nullptr;
    }
    return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End), std::move(Step), std::move(Body));
}

static std::unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {
    default:
        fprintf(stderr, "%s\n", IdentifierStr.c_str());
        return LogError("Unknown token when expecting an expression");
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case tok_if:
        return ParseIfExpr();
    case tok_for:
        return ParseForExpr();
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

        // if the new token precedence is less than the LHS precedence, return the LHS as a finished node
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

        // If the current precedence is less than the next, recursively expand the RHS node as far as it'll go
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
    if (CurTok != tok_identifier)
    {
        return LogErrorP("Expected function name in prototype");
    }
    // Get the name of the function
    std::string FnName = IdentifierStr;
    getNextToken();
    if (CurTok != '(')
    {
        return LogErrorP("Expected '(' in prototype");
    }
    //  Get all the argument names
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
    {
        ArgNames.push_back(IdentifierStr);
    }
    // Get the close Parenthesis
    if (CurTok != ')')
    {
        return LogErrorP("Expected ')' in prototype");
    }
    getNextToken();
    // Return a node containing a function name and its argument names
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

static std::unique_ptr<FunctionAST> ParseDefinition()
{

    getNextToken();
    //  Get the prototype
    auto Proto = ParsePrototype();
    if (!Proto)
    {
        return nullptr;
    }

    //  Get the following expression
    if (auto E = ParseExpression())
    {
        //  Return a function node pairing the prototype with the following expression as its definition
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
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());  /// CHANGED in step 3 from step 2 without instruction
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
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;


Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
}

Function *getFunction(std::string Name){
    // Check current module for function
    if(auto *F = TheModule->getFunction(Name)){
        return F;
    }

    // Check if we can codegen from existing declaration
    auto FI = FunctionProtos.find(Name);
    if(FI!=FunctionProtos.end()){
        return FI->second->codegen();
    }

    // Does not exist
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

Value *IfExprAST::codegen(){
    Value *CondV = Cond->codegen();
    if(!CondV){
        return nullptr;
    }
    // Convert condition to a bool by comparing non-equal to 0.0
    CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Crete blocks fo the 'then' and 'else' cases. Insert the 'then' block at the end of the function
    BasicBlock *ThenBB= BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    Builder->CreateCondBr(CondV,ThenBB, ElseBB);

    //Emit 'then' value
    Builder->SetInsertPoint(ThenBB);

    Value *ThenV = Then->codegen();
    if(!ThenV){
        return nullptr;
    }

    Builder->CreateBr(MergeBB);
    // Codegen of "Then" can change the current block, update thenBB for the PHI
    ThenBB=Builder->GetInsertBlock();

    //Emit 'else' value
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);

    Value *ElseV = Else->codegen();
    if(!ElseV){
        return nullptr;
    }

    Builder->CreateBr(MergeBB);
    // Codegen of "Then" can change the current block, update thenBB for the PHI
    ElseBB=Builder->GetInsertBlock();

    // Emit merge block
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

Value *ForExprAST::codegen(){
    //Emit the start code first, without 'variable' in scope
    Value *StartVal = Start->codegen();
    if(!StartVal){
        return nullptr;
    }



    // Make the new basic block for the loop header, inserting after current block
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder->GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    //Insert an explicit fall through from the current block to the LoopBB
    Builder->CreateBr(LoopBB);

    // Start insertion in LoopBB
    Builder->SetInsertPoint(LoopBB);

    // Start the PHI node with an entry for Start
    PHINode *Variable = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
    Variable->addIncoming(StartVal, PreheaderBB);

    // Within the loop, the variable is defined equal to the PHI node. If it shadow an existing variable, we have to restore it, so save it now
    Value *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;

    // Emit the boy of the loop. This, like any other epr, can change the current BB. Note that we ignore the value computed by the body, but don't allow an error
    if(!Body->codegen()){
        return nullptr;
    }

    // emit the step value
    Value *StepVal = nullptr;
    if(Step) {
        StepVal = Step->codegen();
        if(!StepVal){
            return nullptr;
        
        }
    }else {
        // If not specified, use 1.0
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

    // Compute the end condition
    Value *EndCond = End->codegen();
    if(!EndCond){
        return nullptr;
    }

    // Convert condition to a bool by comparing non-equal to 0.0
    EndCond = Builder->CreateFCmpONE(EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

    // Create the "after loop" block and insert it
    BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop:", TheFunction);

    // Insert the conditional branch into the end of LoopEndBB
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

    // Any new code will be inserted in After BB
    Builder->SetInsertPoint(AfterBB);

    // Add a new entry to the PHI node for the backedge
    Variable->addIncoming(NextVar, LoopEndBB);

    // Restore the unshadowed variable
    if(OldVal){
        NamedValues[VarName] = OldVal;
    }else{
        NamedValues.erase(VarName);
    }

    // for expr always returns 0.0
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));

}

Value *CallExprAST::codegen()
{
    // Lookup name in global module table
    Function *CalleeF = getFunction(Callee);
    if (!CalleeF)
    {
        return LogErrorV("Unknown function referenced");
    }

    if (CalleeF->arg_size() != Args.size())
    {
        return LogErrorV("Incorrect number of arguments passed");
    }

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
    {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
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

Function *FunctionAST::codegen() {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());
  if (!TheFunction)
    return nullptr;

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args())
    NamedValues[std::string(Arg.getName())] = &Arg;

  if (Value *RetVal = Body->codegen()) {
    // Finish off the function.
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    // Run the optimizer on the function.
    TheFPM->run(*TheFunction, *TheFAM);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

/////////////////////////////////////////////////////////////// Top Level Parser and JIT Driver

static void InitializeModuleAndManagers(void)
{
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    // Create new pass and analysis managers
    TheFPM = std::make_unique<FunctionPassManager>(); ////////////// ERROR, Changed
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*TheContext, /*DebugLogging*/ true);

    TheSI->registerCallbacks(*ThePIC, TheMAM.get());


    //Add transform passes
    // Simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->addPass(InstCombinePass());

    // Reassociate expressions
    TheFPM->addPass(ReassociatePass());

    // Eliminate Common SubExpressions
    TheFPM->addPass(GVNPass());

    // Simplify the control flow graph (deleting unreachable blocks, etc)
    TheFPM->addPass(SimplifyCFGPass());

    // Register analysis passes used in these transform passes.
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM,*TheFAM,*TheCGAM, *TheMAM);

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
            ExitOnErr(TheJIT->addModule(ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndManagers();
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
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
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
    if (auto FnAST = ParseTopLevelExpr()) {
        if (FnAST->codegen()) {
            // Create a ResourceTracker to track JIT'd memory allocated to our
            // anonymous expression -- that way we can free it after executing.
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();

            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndManagers();

            // Search the JIT for the __anon_expr symbol.
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

            //assert(ExprSymbol && "Function not found");                            //////////////// ERRORs, CHANGED removed from step 4 final

            // Get the symbol's address and cast it to the right type (takes no arguments,
            // returns a double) so we can call it as a native function
            double (*FP)() = ExprSymbol.toPtr<double (*)()>(); ///CHANGED from ExprSymbol.getAddress().toPtr<double (*)()>()
            fprintf(stderr, "Evaluated to %f\n", FP());

            //Delete the anonymous epression module from the JIT
            ExitOnErr(RT->remove());
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

int main()
{
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();


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

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    // Make the module, which holds all the code
    InitializeModuleAndManagers();

    // Run the driver
    DriverLoop();

    // Print out the generated code
    //TheModule->print(errs(), nullptr);

    return 0;
}
