#ifndef LOX_EXPRNODES_H
#define LOX_EXPRNODES_H

#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <memory>
#include <cmath>
#include "token.h"
#include "vecWrapper.h"

namespace lox {

class VarEnv;
class ErrorHandler;

namespace exp {

using LongInt_t   = long long;
//////////////////////////////////////////////////////////////////////////////
///AST Expr Node class, evaluate() method return type
//////////////////////////////////////////////////////////////////////////////
struct ErrorReported { };
struct Assignment    { };
struct VarIdentifier { std::string_view sview; };
struct StrLiteral    { std::string_view sview; };

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

struct DivOpRes {
  using D = double;
  using L = LongInt_t;
  double decimal;
  double fractional;
  const bool isFloatingOp;

  inline DivOpRes(D numerator, D denominator):
    decimal{numerator / denominator},
    fractional{0},
    isFloatingOp{true} { }

  inline DivOpRes(D numerator, L denominator):
      decimal{numerator / static_cast<double>(denominator)},
      fractional{0},
      isFloatingOp{true} { }

  inline DivOpRes(L numerator, D denominator):
      decimal{static_cast<double>(numerator) / denominator},
      fractional{0},
      isFloatingOp{true} { }

  inline DivOpRes(L numerator, L denominator):
      decimal{0.0},
      fractional{0.0},
      isFloatingOp{false}
  {
    auto res = [] (auto a, auto b) {
      return static_cast<double>(a) / static_cast<double>(b);
    };
    fractional = std::modf(res(numerator, denominator), &decimal);
  }
};
}/// end of namespace exp

//////////////////////////////////////////////////////////////////////////////
///Abstract AST Node Expr classes
//////////////////////////////////////////////////////////////////////////////
class Expr {
 public:
  Expr();
  virtual ~Expr() noexcept;

  Expr(Expr const&)                = delete;
  Expr& operator=(Expr const& )    = delete;
  Expr(Expr&& ) noexcept           = delete;

  [[nodiscard]] virtual  exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                              = 0;
  virtual void toStr(std::string& strm)            const = 0;
  [[nodiscard]] virtual TokenRange getTokenRange() const = 0;
};

//////////////////////////////////////////////////////////////////////////////
///Concrete AST Node Expr classes
//////////////////////////////////////////////////////////////////////////////

class Binary : public Expr {
 private:
  std::unique_ptr<Expr> m_lhs;
  std::unique_ptr<Expr> m_rhs;
  lox::TokenRange       m_range;
  Token_e m_token;

 public:
  Binary(
      std::unique_ptr<Expr> left,
      Token_e               token,
      std::unique_ptr<Expr> right,
      TokenRange&&          range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                      override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class Unary : public Expr {
  std::unique_ptr<Expr> m_rhs;
  lox::TokenRange       m_range;
  Token_e m_token;

 public:
  Unary(
      Token_e token,
      std::unique_ptr<Expr> rhs,
      TokenRange&&          range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& err)                         override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class Prefix : public Expr {
 private:
  std::unique_ptr<Expr> m_rhs;
  lox::TokenRange       m_range;
  Token_e m_token;

 public:
  Prefix(
      Token_e token,
      std::unique_ptr<Expr> rhs,
      TokenRange&&          range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                      override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class Suffix : public Expr {
 private:
  std::unique_ptr<Expr> m_lhs;
  lox::TokenRange       m_range;
  Token_e               m_token;

 public:
  Suffix(
      std::unique_ptr<Expr> lhs,
      Token_e token,
      TokenRange&&          range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                      override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class LRExpr : public Expr {
 private:
  lox::TokenRange       m_range;

 public:
  explicit LRExpr(TokenRange&& range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                      override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class Grouping : public Expr {
 private:
  std::unique_ptr<Expr> m_expr;
  lox::TokenRange       m_range;

 public:
  Grouping(
      std::unique_ptr<Expr> expr,
      TokenRange&&          range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                      override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class Assignment : public Expr {
 private:
  std::unique_ptr<Expr> m_lhs;
  std::unique_ptr<Expr> m_rhs;
  TokenRange            m_range;
  Token_e               m_token;

 public:
  Assignment(
      std::unique_ptr<Expr> lhs,
      Token_e               token,
      std::unique_ptr<Expr> rhs,
      TokenRange&&          range);

  [[nodiscard]] exp::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl)                      override;
  void toStr(std::string& strm)            const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

}//end of namespace lox
#endif //LOX_EXPRNODES_H
