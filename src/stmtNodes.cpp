#include <exception>
#include <iostream>
#include "stmtNodes.h"
#include "exprNodes.h"
#include "varEnv.h"
#include "errorHandler.h"

lox::Stmt::Stmt()           = default;
lox::Stmt::~Stmt() noexcept = default;

//////////////////////////////////////////////////////////////////////////////
namespace smtnms {
//////////////////////////////////////////////////////////////////////////////

void errOnVarDecl(
    lox::ErrorHandler&  errHdl,
    lox::Token_itr      loc)
{
  errHdl.recordErrMsg(
      [] (auto& msg) {
        msg += "illegal assignment operation observed while ";
        msg += "rvalue computation is expected";
      },
      loc
  );
}

void errExpectedVar(
    lox::ErrorHandler& errHdl,
    lox::Token_itr     loc)
{
  errHdl.recordErrMsg(
      [loc] (auto& msg) {
        msg += "expected variable identifier but received ";
        lox::toStr(msg, loc->m_type);
      },
      loc
  );
}

  class PrintElem {
    lox::VarEnv&        m_env;
    lox::ErrorHandler&  m_err;
    lox::Token_itr      m_loc;
   public:
    bool m_success{true};

    PrintElem(
        lox::VarEnv&        env,
        lox::ErrorHandler&  err,
        lox::Token_itr      loc):
      m_env{env},
      m_err{err},
      m_loc{loc}
    {

    }

    void operator() (std::unique_ptr<lox::Expr>& elem)
    {
      std::visit(
          [this] (auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, lox::exp::ErrorReported> ||
                          std::is_same_v<T, lox::exp::Assignment>) {
              m_success = false;
            } else if constexpr (std::is_same_v<T, lox::exp::VarIdentifier>) {
              std::visit(
                  [this] (auto&& inArg) {
                    using namespace lox::var;
                    using U = std::decay_t<decltype(inArg)>;
                    if constexpr (std::is_same_v<U, std::monostate>)
                      m_success = false;
                    else if constexpr (std::is_same_v<U, NilState>)
                      std::cout << "nil";
                    else if constexpr (std::is_same_v<U, bool>)
                      std::cout << std::boolalpha << inArg;
                    else
                      std::cout << inArg;
                  },
                  std::as_const(m_env).get(arg.sview, m_err, m_loc)
              );
            } else if constexpr (std::is_same_v<T, lox::exp::StrLiteral>) {
              std::cout << arg.sview;
            } else if constexpr (std::is_same_v<T, bool>) {
              std::cout << std::boolalpha << arg;
            } else {
              std::cout << arg;
            }
          },
          elem->evaluate(m_env, m_err)
      );
    }
  };
//////////////////////////////////////////////////////////////////////////////
} ///end of namespace smtnms
//////////////////////////////////////////////////////////////////////////////

lox::VarDecl::VarDecl(
    std::unique_ptr<Expr> id,
    std::unique_ptr<Expr> val,
    TokenRange&&          range):
  Stmt(),
  m_identifier{std::move(id)},
  m_initializer{std::move(val)},
  m_range{range}
{

}

lox::VarDecl::VarDecl(
    std::unique_ptr<Expr> id,
    TokenRange&&          range):
  Stmt(),
  m_identifier{std::move(id)},
  m_initializer{nullptr},
  m_range{range}
{

}

bool lox::VarDecl::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto var = m_identifier->evaluate(env, errHdl);
  if (!std::holds_alternative<exp::VarIdentifier>(var)) {
    smtnms::errExpectedVar(errHdl, m_range.cur_loc());
    return false;
  }

  //if initializer is not present, set it to Nil
  if (!m_initializer) {
    if (!env.push(std::get<exp::VarIdentifier>(
        var).sview, errHdl, m_range.cur_loc()))
      return false;
    return true;
  }

  using namespace smtnms;
  auto innrVist = [&env, &errHdl, var, this] (auto&& arg) -> bool {
    return std::visit(
        [&env, &errHdl, var, this] (auto&& inArg) -> bool {
          using U = std::decay_t<decltype(inArg)>;
          if constexpr (std::is_same_v<U, std::monostate>) {
            return false;
          } else {
            return env.push(
                std::get<exp::VarIdentifier>(var).sview,
                inArg,
                errHdl,
                m_range.cur_loc()
            );
          }
        },
        std::as_const(env).get(arg.sview, errHdl, m_range.cur_loc())
    );
  };

  return std::visit(
      [&env, &errHdl, var, this, innrVist] (auto&& arg) -> bool {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, exp::ErrorReported>) {
          return false;
        } else if constexpr (std::is_same_v<T, exp::Assignment>) {
          errOnVarDecl(errHdl, m_range.cur_loc());
          return false;
        } else if constexpr (std::is_same_v<T, exp::VarIdentifier>) {
          return innrVist(arg);
        } else if constexpr (std::is_same_v<T, exp::StrLiteral>) {
          return env.push(
              std::get<exp::VarIdentifier>(var).sview,
              arg.sview,
              errHdl,
              m_range.cur_loc()
          );
        } else {
          return env.push(
              std::get<exp::VarIdentifier>(var).sview,
              arg,
              errHdl,
              m_range.cur_loc()
          );
        }
      },
      m_initializer->evaluate(env, errHdl)
  );
}

void lox::VarDecl::toStr(std::string& strm) const
{
  strm += "var ";
  m_identifier->toStr(strm);
  strm += " = ";
  m_initializer->toStr(strm);
  strm += '\n';
}

lox::TokenRange lox::VarDecl::getTokenRange() const { return m_range; }

lox::ExprStmt::ExprStmt(
    std::unique_ptr<Expr> expr,
    TokenRange&&          range):
  Stmt(),
  m_expression{std::move(expr)},
  m_range{range}
{

}

bool lox::ExprStmt::evaluate(
    VarEnv &env,
    ErrorHandler &errHdl)
{
  auto res = m_expression->evaluate(env, errHdl);
  if (std::holds_alternative<exp::ErrorReported>(res))
    return false;
  return true;
}

void lox::ExprStmt::toStr(std::string& strm) const
{
  strm += "Expr ( ";
  m_expression->toStr(strm);
  strm += ")\n";
}

lox::TokenRange lox::ExprStmt::getTokenRange() const { return m_range; }

lox::PrintDecl::PrintDecl(
    std::vector<std::unique_ptr<Expr>>&& exprs,
    TokenRange&&                         range):
    Stmt(),
    m_exprs{std::move(exprs)},
    m_range{range}
{

}

bool lox::PrintDecl::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto res = std::for_each(
      m_exprs.begin(),
      m_exprs.end(),
      smtnms::PrintElem(env, errHdl, m_range.cur_loc())
  );
  std::cout << std::endl;
  return res.m_success;
}

void lox::PrintDecl::toStr(std::string& strm) const
{
  strm += "print ";
  for (auto& elem : m_exprs) {
    elem->toStr(strm);
    strm += ", ";
  }
  strm += '\n';
}

lox::TokenRange lox::PrintDecl::getTokenRange() const { return m_range; }

lox::BlockDecl::BlockDecl(
    std::vector<std::unique_ptr<Stmt>>&& stmts,
    TokenRange&&                         range):
  Stmt(),
  m_stmts{std::move(stmts)},
  m_range{range}
{

}

bool lox::BlockDecl::evaluate(
    VarEnv& env, ErrorHandler& errHdl)
{
  env.allocBlk();
  for (auto& elem : m_stmts) {
    if (!elem->evaluate(env, errHdl)){
      env.deallocBlk();
      return false;
    }
  }
  env.deallocBlk();
  return true;
}

void lox::BlockDecl::toStr(std::string& strm) const
{
  strm += "{\n";
  for (auto& elem : m_stmts) {
    elem->toStr(strm);
    strm += '\n';
  }
  strm += "}\n";
}

lox::TokenRange lox::BlockDecl::getTokenRange() const { return m_range; }
