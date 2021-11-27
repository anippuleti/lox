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

lox::smt::evalRet_t lox::VarDecl::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto var = m_identifier->evaluate(env, errHdl);
  if (!std::holds_alternative<exp::VarIdentifier>(var)) {
    smtnms::errExpectedVar(errHdl, m_range.cur_loc());
    return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
  }

  //if initializer is not present, set it to Nil
  if (!m_initializer) {
    if (!env.push(std::get<exp::VarIdentifier>(
        var).sview, errHdl, m_range.cur_loc()))
      return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
    return smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}};
  }

  using namespace smtnms;
  auto innrVist = [&env, &errHdl, var, this] (auto&& arg) -> smt::evalRet_t {
    return std::visit(
        [&env, &errHdl, var, this] (auto&& inArg) -> smt::evalRet_t {
          using U = std::decay_t<decltype(inArg)>;
          if constexpr (std::is_same_v<U, std::monostate>) {
            return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
          } else {
            return env.push(
                std::get<exp::VarIdentifier>(var).sview,
                inArg,
                errHdl,
                m_range.cur_loc()
            ) ? smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}}
              : smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
          }
        },
        std::as_const(env).get(arg.sview, errHdl, m_range.cur_loc())
    );
  };

  return std::visit(
      [&env, &errHdl, var, this, innrVist] (auto&& arg) -> smt::evalRet_t {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, exp::ErrorReported>) {
          return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
        } else if constexpr (std::is_same_v<T, exp::Assignment>) {
          errOnVarDecl(errHdl, m_range.cur_loc());
          return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
        } else if constexpr (std::is_same_v<T, exp::VarIdentifier>) {
          return innrVist(arg);
        } else if constexpr (std::is_same_v<T, exp::StrLiteral>) {
          return env.push(
              std::get<exp::VarIdentifier>(var).sview,
              arg.sview,
              errHdl,
              m_range.cur_loc()
          ) ? smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}}
            : smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
        } else {
          return env.push(
              std::get<exp::VarIdentifier>(var).sview,
              arg,
              errHdl,
              m_range.cur_loc()
          ) ? smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}}
            : smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
        }
      },
      m_initializer->evaluate(env, errHdl)
  );
}

lox::AstType lox::VarDecl::getType() const { return AstType::var; }
lox::TokenRange lox::VarDecl::getTokenRange() const { return m_range; }

lox::ExprStmt::ExprStmt(
    std::unique_ptr<Expr> expr,
    TokenRange&&          range):
  Stmt(),
  m_expression{std::move(expr)},
  m_range{range}
{

}

lox::smt::evalRet_t lox::ExprStmt::evaluate(
    VarEnv &env,
    ErrorHandler &errHdl)
{
  auto res = m_expression->evaluate(env, errHdl);
  if (std::holds_alternative<exp::ErrorReported>(res))
    return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
  return smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}};
}

lox::AstType lox::ExprStmt::getType() const { return AstType::exprsmt; }
lox::TokenRange lox::ExprStmt::getTokenRange() const { return m_range; }

lox::PrintDecl::PrintDecl(
    std::vector<std::unique_ptr<Expr>>&& exprs,
    TokenRange&&                         range):
    Stmt(),
    m_exprs{std::move(exprs)},
    m_range{range}
{

}

lox::smt::evalRet_t lox::PrintDecl::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto res = std::for_each(
      m_exprs.begin(),
      m_exprs.end(),
      smtnms::PrintElem(env, errHdl, m_range.cur_loc())
  );
  std::cout << std::endl;
  return res.m_success ? smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}}
                       : smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
}

lox::AstType lox::PrintDecl::getType() const { return AstType::print; }
lox::TokenRange lox::PrintDecl::getTokenRange() const { return m_range; }

lox::BlockDecl::BlockDecl(
    std::vector<std::unique_ptr<Stmt>>&& stmts,
    TokenRange&&                         range):
  Stmt(),
  m_stmts{std::move(stmts)},
  m_range{range}
{

}

lox::smt::evalRet_t lox::BlockDecl::evaluate(
    VarEnv& env, ErrorHandler& errHdl)
{
  env.allocBlk();
  for (auto& elem : m_stmts) {
    auto res = elem->evaluate(env, errHdl);
    if (std::holds_alternative<smt::SmtEval>(res)) {
      if (std::get<smt::SmtEval>(res).flag == smt::Status_e::Failure) {
        env.deallocBlk();
        return res;
      }
    } else {
      return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
    }
  }
  env.deallocBlk();
  return smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}};
}

lox::AstType lox::BlockDecl::getType() const { return AstType::block; }
lox::TokenRange lox::BlockDecl::getTokenRange() const { return m_range; }
