#ifndef LOX_ASTNODE_H
#define LOX_ASTNODE_H

namespace lox {

enum class AstType {
  binary, unary, prefix, suffix, boolean, integer, fractional,
  string, identifier, grouping, assignment, var, exprsmt, print, block
};

namespace exp {
using LongInt_t = long long;
//////////////////////////////////////////////////////////////////////////////
///AST Expr Node class, evaluate() method return type
//////////////////////////////////////////////////////////////////////////////
struct ErrorReported {};
struct Assignment {};
struct VarIdentifier { std::string_view sview; };
struct StrLiteral { std::string_view sview; };

using evalRet_t = std::variant<
    ErrorReported,    ///Stored on Error Detection. Type used to unwind stack
    Assignment,       ///Stored on Assignment evaluation.
    bool,             ///bool literals are stored.
    long long,        ///Integer literals are stored
    double,           ///Fractional literals are stored
    std::string,      ///rvalue string literals Ex: print "Hello + "World";
    VarIdentifier,    ///Variable names. Value of variable stored in VarEnv
    StrLiteral        ///lvalue string literals Ex: var s = "Hello World";
>;
}///end of namespace exp

namespace smt {
enum class Status_e {
  Success, Failure
};

struct SmtEval {
  Status_e flag{Status_e::Success};
};

struct IfTaken {
  Status_e flag{Status_e::Success};
};

using evalRet_t = std::variant<
    SmtEval,
    IfTaken
>;

}///end of namespace smt

}///end of namespace lox

#endif //LOX_ASTNODE_H
