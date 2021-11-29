#include <exception>
#include <iostream>
#include <cassert>
#include "usrTraits.h"
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

void errOnIfCondEval(
    lox::ErrorHandler&         errHdl,
    lox::exp::evalRet_t const& res,
    lox::TokenRange            tokens)
{
  auto ret_type = [](auto const& res) {
    return std::visit(
        [] (auto const& val) -> std::string {
          using T = std::decay_t<decltype(val)>;
          using namespace lox::exp;
          using namespace lox::traits;
          if constexpr (std::is_same_v<T, ErrorReported> ||
                        std::is_same_v<T, Assignment>    ||
                        std::is_same_v<T, VarIdentifier>)
            return "unexpected operation within if loop";
          else if constexpr (std::is_same_v<T, LongInt_t> ||
                             std::is_same_v<T, double>)
            return "number integer or double";
          else if constexpr (std::is_same_v<T, std::string> ||
                             std::is_same_v<T, StrLiteral> )
            return "string";
          else if constexpr (std::is_same_v<T, bool>)
            assert(0);
          else
            static_assert(always_false_v<T>, "Must not hit this statement");
          return " unknown type ";
        },
        res
    );
  };

  auto loc = std::find_if(
      tokens.cur_loc(),
      tokens.end(),
      [] (auto const& tkn) { return tkn.m_type == lox::Token_e::identifier; }
  );
  assert(loc != tokens.end());
  errHdl.recordErrMsg(
      [&res, ret_type] (auto& msg) {
        msg += "could not convert ";
        msg += ret_type(res);
        msg += " to bool";
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

lox::AstType lox::VarDecl::getType() const noexcept
{
  return AstType::var;
}
lox::TokenRange lox::VarDecl::getTokenRange() const noexcept
{
  return m_range;
}

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

lox::AstType lox::ExprStmt::getType() const noexcept
{
  return AstType::exprsmt;
}

lox::TokenRange lox::ExprStmt::getTokenRange() const noexcept
{
  return m_range;
}

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

lox::AstType lox::PrintDecl::getType() const noexcept
{
  return AstType::print;
}

lox::TokenRange lox::PrintDecl::getTokenRange() const noexcept
{
  return m_range;
}

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
    } else if (std::holds_alternative<smt::IfTaken>(res)) {
      env.deallocBlk();
      return res;
    } else {
      env.deallocBlk();
      return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
    }
  }
  env.deallocBlk();
  return smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}};
}

lox::AstType lox::BlockDecl::getType() const noexcept
{
  return AstType::block;
}

lox::TokenRange lox::BlockDecl::getTokenRange() const noexcept
{
  return m_range;
}

lox::IfStmt::IfStmt(
    std::unique_ptr<Expr> cond,
    std::unique_ptr<Stmt> blk,
    TokenRange &&range):
  m_condition{std::move(cond)},
  m_block{std::move(blk)},
  m_range{range}
{

}

lox::smt::evalRet_t lox::IfStmt::evaluate(
    VarEnv&  env,
    ErrorHandler& errHdl)
{
  auto res = m_condition->evaluate(env, errHdl);
  auto bool_lmd =
      [&env, &errHdl, this] (auto&& res) -> std::pair<smt::Status_e, bool> {
        if (std::holds_alternative<bool>(res)) {
          return {smt::Status_e::Success, std::get<bool>(res)};

        } else if (std::holds_alternative<exp::VarIdentifier>(res)) {
          auto fres = env.get(
              std::get<exp::VarIdentifier>(res).sview,
              errHdl,
              m_range.cur_loc()
          );
          if (std::holds_alternative<bool>(fres))
            return {smt::Status_e::Success, std::get<bool>(fres)};
        }
        return {smt::Status_e::Failure, false};
  };

  using namespace smt;
  if (auto v = bool_lmd(res); v.first == Status_e::Success && v.second) {
    auto in_res = m_block->evaluate(env, errHdl);

    if (std::holds_alternative<smt::SmtEval>(in_res)) {
      if (std::get<smt::SmtEval>(in_res).flag == smt::Status_e::Success)
        return smt::evalRet_t{smt::IfTaken{smt::Status_e::Success}};
      else
        return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};

    } else if (std::holds_alternative<smt::IfTaken>(in_res)) {
      if (std::get<smt::IfTaken>(in_res).flag == smt::Status_e::Success)
        return smt::evalRet_t{smt::IfTaken{smt::Status_e::Success}};
      else
        return smt::evalRet_t{smt::IfTaken{smt::Status_e::Failure}};
    } else {
      assert(0);
    }

  } else if (v.first == Status_e::Success && !v.second) {
    return smt::evalRet_t{smt::IfTaken{smt::Status_e::Failure}};
  }
  return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
}

lox::AstType lox::IfStmt::getType() const noexcept
{
  return AstType::ifsmt;
}

lox::TokenRange lox::IfStmt::getTokenRange() const noexcept
{
  return m_range;
}

lox::ElseStmt::ElseStmt(
    std::unique_ptr<Stmt> blk,
    TokenRange &&range):
  m_block{std::move(blk)},
  m_range{range}
{

}

lox::smt::evalRet_t lox::ElseStmt::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto res = m_block->evaluate(env, errHdl);
  if (std::holds_alternative<smt::SmtEval>(res) &&
      std::get<smt::SmtEval>(res).flag == smt::Status_e::Failure)
  {
    return smt::evalRet_t{smt::SmtEval{smt::Status_e::Failure}};
  }
  return smt::evalRet_t{smt::SmtEval{smt::Status_e::Success}};
}

lox::AstType lox::ElseStmt::getType() const noexcept
{
  return AstType::elsesmt;
}

lox::TokenRange lox::ElseStmt::getTokenRange() const noexcept
{
  return m_range;
}

lox::IfElseStmt::IfElseStmt(
    std::unique_ptr<Stmt>   ifcls,
    std::unique_ptr<Stmt> elsecls,
    TokenRange&&              range):
  m_if_clause{std::move(ifcls)},
  m_else_clause{std::move(elsecls)},
  m_range{range}
{

}

lox::smt::evalRet_t lox::IfElseStmt::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  using namespace smt;
  auto if_res = m_if_clause->evaluate(env, errHdl);
  if (std::holds_alternative<IfTaken>(if_res)) {
    if (std::get<IfTaken>(if_res).flag == Status_e::Success) {
      return evalRet_t{SmtEval{Status_e::Success}};

    } else {
      auto else_res = m_else_clause->evaluate(env, errHdl);
      if (std::holds_alternative<SmtEval>(else_res) &&
          std::get<SmtEval>(else_res).flag == Status_e::Failure)
        return evalRet_t{SmtEval{Status_e::Failure}};
      return evalRet_t{SmtEval{Status_e::Success}};
    }
  }
  return evalRet_t{SmtEval{Status_e::Failure}};
}

lox::AstType lox::IfElseStmt::getType() const noexcept
{
  return AstType::ifelsesmt;
}

lox::TokenRange lox::IfElseStmt::getTokenRange() const noexcept
{
  return m_range;
}

lox::IfElseIfStmt::IfElseIfStmt(
    std::vector<std::unique_ptr<Stmt>>&& ifq,
    std::unique_ptr<Stmt> else_clause,
    TokenRange&& range):
  m_if_cluaseq{std::move(ifq)},
  m_else_clause{std::move(else_clause)},
  m_range{range}
{

}

lox::IfElseIfStmt::IfElseIfStmt(
    std::vector<std::unique_ptr<Stmt>>&& ifq,
    TokenRange&& range):
    m_if_cluaseq{std::move(ifq)},
    m_else_clause{nullptr},
    m_range{range}
{

}

lox::smt::evalRet_t lox::IfElseIfStmt::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  using namespace smt;
  auto res_vld = [](auto& var) {
    return std::holds_alternative<IfTaken>(var) &&
           std::get<IfTaken>(var).flag == Status_e::Success;
  };

  for (auto& elem : m_if_cluaseq) {
    auto if_res = elem->evaluate(env, errHdl);
    if (res_vld(if_res))
      return evalRet_t{IfTaken{Status_e::Success}};
    else if (std::holds_alternative<SmtEval>(if_res))
      return evalRet_t{SmtEval{Status_e::Failure}};
  }

  if (m_else_clause) {
    auto else_res = m_else_clause->evaluate(env, errHdl);
    if (std::get<SmtEval>(else_res).flag == Status_e::Failure)
      return evalRet_t{IfTaken{Status_e::Failure}};
    return evalRet_t{SmtEval{Status_e::Success}};
  }
  return evalRet_t{IfTaken{Status_e::Success}};
}

lox::AstType lox::IfElseIfStmt::getType() const noexcept
{
  return AstType::ifelseifsmt;
}

lox::TokenRange lox::IfElseIfStmt::getTokenRange() const noexcept
{
  return m_range;
}
