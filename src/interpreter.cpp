#include <memory>
#include "interpreter.h"
#include "stmtNodes.h"
#include "exprNodes.h"
#include "varEnv.h"

namespace iprnms {
using Vwrap = lox::VecWrapper<std::unique_ptr<lox::Stmt>, int>;
using Ustmt = std::unique_ptr<lox::Stmt>;
using Venv  = lox::VarEnv;
using Herr  = lox::ErrorHandler;

bool eval_impl(
    Vwrap eval_ast,
    Venv& var_env,
    Herr& err_hdl)
{
  for (auto& elem : eval_ast) {
    auto res = elem->evaluate(var_env, err_hdl);
    if (!res)
      return true;
  }
  return false;
}

}///end of namespace iprnms

lox::Interpreter::Interpreter(ErrorHandler& err_hdl): m_err_hdl(err_hdl) { }
lox::Interpreter::~Interpreter() = default;

bool lox::Interpreter::evaluate(iprnms::Vwrap eval_ast) const
{
  VarEnv m_env;
  m_env.allocBlk();
  auto res =  iprnms::eval_impl(eval_ast, m_env, m_err_hdl);
  m_env.deallocBlk();
  return res;
}