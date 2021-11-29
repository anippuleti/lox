#ifndef LOX_STMTNODES_H
#define LOX_STMTNODES_H

#include <string>
#include <vector>
#include <memory>
#include "token.h"
#include "vecWrapper.h"
#include "astNode.h"

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

  [[nodiscard]] virtual smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) = 0;
  [[nodiscard]] virtual AstType getType()          const noexcept = 0;
  [[nodiscard]] virtual TokenRange getTokenRange() const noexcept = 0;
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

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class ExprStmt : public Stmt {
  std::unique_ptr<Expr> m_expression;
  TokenRange            m_range;

 public:
  ExprStmt(
      std::unique_ptr<Expr> expr,
      TokenRange&&          range
  );

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class PrintDecl : public Stmt {
  std::vector<std::unique_ptr<Expr>> m_exprs;
  TokenRange                         m_range;

 public:
  PrintDecl(
      std::vector<std::unique_ptr<Expr>>&& exprs,
      TokenRange&&                         range);

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class BlockDecl : public Stmt {
  std::vector<std::unique_ptr<Stmt>> m_stmts;
  TokenRange                         m_range;

 public:
  BlockDecl(
      std::vector<std::unique_ptr<Stmt>>&& stmts,
      TokenRange&&                         range);

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class IfStmt : public Stmt {
  std::unique_ptr<Expr> m_condition;
  std::unique_ptr<Stmt> m_block;
  TokenRange            m_range;

 public:
  IfStmt(
      std::unique_ptr<Expr> cond,
      std::unique_ptr<Stmt> blk,
      TokenRange&&          range);

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class ElseStmt : public Stmt {
  std::unique_ptr<Stmt> m_block;
  TokenRange            m_range;

 public:
  ElseStmt(
      std::unique_ptr<Stmt> blk,
      TokenRange&&          range);

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class IfElseStmt : public Stmt {
  std::unique_ptr<Stmt>   m_if_clause;
  std::unique_ptr<Stmt> m_else_clause;
  TokenRange                m_range;

 public:
  IfElseStmt(
      std::unique_ptr<Stmt>   ifcls,
      std::unique_ptr<Stmt> elsecls,
      TokenRange&&              m_range);

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

class IfElseIfStmt : public Stmt {
  std::vector<std::unique_ptr<Stmt>> m_if_cluaseq;
  std::unique_ptr<Stmt>            m_else_clause;
  TokenRange                           m_range;

 public:
  IfElseIfStmt(
      std::vector<std::unique_ptr<Stmt>>&& ifq,
      std::unique_ptr<Stmt>            else_clause,
      TokenRange&&                     range);

  IfElseIfStmt(
      std::vector<std::unique_ptr<Stmt>>&& ifq,
      TokenRange&&                     range);

  [[nodiscard]] smt::evalRet_t evaluate(
      VarEnv& env,
      ErrorHandler& errHdl) override;
  [[nodiscard]] AstType getType()          const noexcept override;
  [[nodiscard]] TokenRange getTokenRange() const noexcept override;
};

}//end of namespace lox

#endif //LOX_STMTNODES_H
