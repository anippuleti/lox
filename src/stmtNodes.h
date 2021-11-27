#ifndef LOX_STMTNODES_H
#define LOX_STMTNODES_H

#include <string>
#include <vector>
#include <memory>
#include "token.h"
#include "vecWrapper.h"

namespace lox {
class VarEnv;
class ErrorHandler;
class Expr;

//////////////////////////////////////////////////////////////////////////////
///Abstract AST Node Stmt classes
//////////////////////////////////////////////////////////////////////////////
class Stmt {
 public:
  Stmt();
  virtual ~Stmt() noexcept;

  Stmt(Stmt const& )                = delete;
  Stmt& operator=(Stmt& )           = delete;
  Stmt(Stmt&& ) noexcept            = delete;
  Stmt& operator=(Stmt&& ) noexcept = delete;

  [[nodiscard]] virtual bool evaluate(VarEnv& env, ErrorHandler& errHdl) = 0;
                virtual void toStr(std::string& strm)        const = 0;
  [[nodiscard]] virtual TokenRange getTokenRange()           const = 0;
};

//////////////////////////////////////////////////////////////////////////////
///Concrete AST Node Stmt classes
//////////////////////////////////////////////////////////////////////////////
class VarDecl : public Stmt {
  std::unique_ptr<Expr> m_identifier;
  std::unique_ptr<Expr> m_initializer;
  TokenRange            m_range;

 public:
  VarDecl(
      std::unique_ptr<Expr> id,
      std::unique_ptr<Expr> val,
      TokenRange&&          range);

  VarDecl(
      std::unique_ptr<Expr> id,
      TokenRange&&          range);

  [[nodiscard]] bool evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  void toStr(std::string& strm) const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class ExprStmt : public Stmt {
  std::unique_ptr<Expr> m_expression;
  TokenRange            m_range;

 public:
  ExprStmt(
      std::unique_ptr<Expr> expr,
      TokenRange&&          range
  );

  [[nodiscard]] bool evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  void toStr(std::string& strm) const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class PrintDecl : public Stmt {
  std::vector<std::unique_ptr<Expr>> m_exprs;
  TokenRange                         m_range;

 public:
  PrintDecl(
      std::vector<std::unique_ptr<Expr>>&& exprs,
      TokenRange&&                         range);

  [[nodiscard]] bool evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  void toStr(std::string& strm) const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

class BlockDecl : public Stmt {
  std::vector<std::unique_ptr<Stmt>> m_stmts;
  TokenRange                         m_range;

 public:
  BlockDecl(
      std::vector<std::unique_ptr<Stmt>>&& stmts,
      TokenRange&&                         range);

  [[nodiscard]] bool evaluate(VarEnv& env, ErrorHandler& errHdl) override;
  void toStr(std::string& strm) const override;
  [[nodiscard]] TokenRange getTokenRange() const override;
};

}

#endif //LOX_STMTNODES_H
