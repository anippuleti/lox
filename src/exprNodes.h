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
#include "astNode.h"

namespace lox {

class VarEnv;
class ErrorHandler;
class Expr;
using Expr_t = std::unique_ptr<Expr>;

namespace exp {

struct DivOpRes {
  using D = double;
  using L = llint;
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
///Expression Types classes
///Expr is the abstract base class for all expressions with below interface.
///Expression type concept must provide below methods.
///
///  ExprRet_t   evaluate(VarEnv& , ErrorHandler& )
///  TokenRange  getTokenRange() const noexcept
///
//////////////////////////////////////////////////////////////////////////////

class Expr {
 public:
  [[nodiscard]] virtual ExprRet_t  evaluate(VarEnv&, ErrorHandler& ) = 0;
  [[nodiscard]] virtual TokenRange getTokenRange() const noexcept    = 0;
};

class LRExpr : public Expr {
 private:
  TokenRange m_range;

 public:
  explicit LRExpr(TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class Prefix : public Expr {
 private:
  Expr_t     m_rhs;
  TokenRange m_range;
  Token_e    m_token;

 public:
  Prefix(
      Token_e      token,
      Expr_t       rhs,
      TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class Suffix : public Expr {
 private:
  Expr_t     m_lhs;
  TokenRange m_range;
  Token_e    m_token;

 public:
  Suffix(
      Expr_t       lhs,
      Token_e      token,
      TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class Unary : public Expr {
  Expr_t     m_rhs;
  TokenRange m_range;
  Token_e    m_token;

 public:
  Unary(
      Token_e      token,
      Expr_t       rhs,
      TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& err) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class Binary : public Expr {
 private:
  Expr_t     m_lhs;
  Expr_t     m_rhs;
  TokenRange m_range;
  Token_e    m_token;

 public:
  Binary(
      Expr_t       left,
      Token_e      token,
      Expr_t       right,
      TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class Grouping : public Expr {
 private:
  Expr_t     m_expr;
  TokenRange m_range;

 public:
  Grouping(
      Expr_t       expr,
      TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class AssignExpr : public Expr {
 public:
  Expr_t     m_lhs;
  Expr_t     m_rhs;
  TokenRange m_range;
  Token_e    m_token;

 public:
  AssignExpr(
      Expr_t       assign_lhs,
      Token_e      token,
      Expr_t       assign_rhs,
      TokenRange&& range);

  [[nodiscard]] ExprRet_t evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

}//end of namespace lox
#endif //LOX_EXPRNODES_H
