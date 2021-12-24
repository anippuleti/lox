#ifndef LOX_ASTNODE_H
#define LOX_ASTNODE_H

namespace lox {

using llint = long long;

//////////////////////////////////////////////////////////////////////////////
///Expression/Statement classes, evaluate() return type
//////////////////////////////////////////////////////////////////////////////
struct ErrorReported {};  ///Type for propagating Error occurrence.
struct Assignment {};     ///Assignment type indication
struct NilValue {};       ///Nil value indication type
struct VarIdentifier {    ///Variable Identifier type
  std::string_view sview;
};
struct StrLiteral {       ///lvalue string literals Ex: var s = "Hello World";
  std::string_view sview;
};

///bool             ///bool literals are stored.
///long long        ///Integer literals are stored
///double           ///Fractional literals are stored
///std::string      ///rvalue string literals Ex: print "Hello + "World";

using ExprRet_t = std::variant<
    bool,
    llint,
    double,
    std::string,
    StrLiteral,
    VarIdentifier,
    NilValue,
    Assignment,
    ErrorReported
>;

enum class Status_e {
  pass, fail
};

enum class IfTaken {
  yes, no
};

}///end of namespace lox

#endif //LOX_ASTNODE_H
