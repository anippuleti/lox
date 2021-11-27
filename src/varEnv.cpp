#include <stdexcept>
#include <algorithm>
#include "varEnv.h"
#include "errorHandler.h"

namespace detail {

void errReDeclVar(
    lox::ErrorHandler&  errHdl,
    std::string_view    varName,
    lox::var::Token_itr loc)
{
  errHdl.recordErrMsg(
      [varName](auto& msg) {
        msg += "redeclaration of ";
        msg += varName;
      },
      loc
  );
}
} //end of namespace

using env = lox::VarEnv;
lox::VarEnv::VarEnv()           = default;
lox::VarEnv::~VarEnv() noexcept = default;

bool env::Node::put(std::string_view sv, bool val)
{
  if (auto itr = find(sv); itr == end()) {
    m_var.emplace_back(sv, val);
    return true;
  }
  return false;
}

 bool env::Node::put(std::string_view sv, env::Lint_t val)
{
  if (auto itr = find(sv); itr == end()) {
    m_var.emplace_back(sv, val);
    return true;
  }
  return false;
}

bool env::Node::put(std::string_view sv, double val)
{
  if (auto itr = find(sv); itr == end()) {
    m_var.emplace_back(sv, val);
    return true;
  }
  return false;
}

bool env::Node::put(std::string_view sv, std::string_view val) {
  if (auto itr = find(sv); itr == end()) {
    m_var.emplace_back(sv, std::string{val.begin(), val.end()});
    return true;
  }
  return false;
}

bool env::Node::put(std::string_view sv, std::string&& val) {
  if (auto itr = find(sv); itr == end()) {
    m_var.emplace_back(sv, std::move(val));
    return true;
  }
  return false;
}

bool env::Node::put(std::string_view sv, var::NilState&& val) {
  if (auto itr = find(sv); itr == end()) {
    m_var.emplace_back(sv, val);
    return true;
  }
  return false;
}

env::Node::iterator env::Node::find(std::string_view sv)
{
  return std::find_if(
      m_var.begin(),
      m_var.end(),
      [sv](auto const& var) { return var.first == sv; }
  );
}

env::Node::const_iterator env::Node::find(std::string_view sv) const
{
  return std::find_if(
      m_var.cbegin(),
      m_var.cend(),
      [sv](auto const& var) { return var.first == sv; }
  );
}

env::Node::iterator env::Node::end()        { return m_var.end(); }
env::Node::const_iterator env::Node::cend() const { return m_var.cend(); }
void env::allocBlk()   { m_env.emplace_back(); }
void env::deallocBlk() { m_env.pop_back();     }

env::Node::iterator env::find(std::string_view id)
{
  for (auto itr = m_env.rbegin(); itr != m_env.rend(); ++itr) {
    if (auto vitr = itr->find(id); vitr != itr->end()) {
      return vitr;
    }
  }
  return m_env.front().end();
}

env::Node::const_iterator env::find(std::string_view id) const
{
  for (auto itr = m_env.rbegin(); itr != m_env.rend(); ++itr) {
    if (auto vitr = itr->find(id); vitr != itr->cend()) {
      return vitr;
    }
  }
  return m_env.front().cend();
}

bool env::exists(std::string_view id) const
{
  return find(id) != m_env.front().cend();
}

lox::var::Var_t& env::get(
    std::string_view id,
    lox::ErrorHandler&  errHdl,
    lox::var::Token_itr loc)
{
  if (auto itr = find(id); itr != m_env.back().end()) {
    return itr->second;
  }
  return errUnDeclVar(errHdl, loc, id);
}

lox::var::Var_t const& env::get(
    std::string_view id,
    lox::ErrorHandler&  errHdl,
    lox::var::Token_itr loc) const
{
  if (auto itr = find(id); itr != m_env.back().cend()) {
    return itr->second;
  }
  return errUnDeclVar(errHdl, loc, id);
}

bool env::push(
    std::string_view id,
    bool val,
    lox::ErrorHandler&  errHdl,
    var::Token_itr loc)
{
  if (!push_impl(id, val)) {
    detail::errReDeclVar(errHdl, id, loc);
    return false;
  }
  return true;
}

bool env::push(
    std::string_view id,
    Lint_t val,
    lox::ErrorHandler& errHdl,
    var::Token_itr     loc)
{
  if (!push_impl(id, val)) {
    detail::errReDeclVar(errHdl, id, loc);
    return false;
  }
  return true;
}

bool env::push(
    std::string_view id,
    double val,
    lox::ErrorHandler& errHdl,
    var::Token_itr     loc)
{
  if (!push_impl(id, val)) {
    detail::errReDeclVar(errHdl, id, loc);
    return false;
  }
  return true;
}

bool env::push(
    std::string_view id,
    std::string_view val,
    lox::ErrorHandler& errHdl,
    var::Token_itr     loc)
{
  if (!push_impl(id, val)) {
    detail::errReDeclVar(errHdl, id, loc);
    return false;
  }
  return true;
}

bool env::push(
    std::string_view id,
    std::string &&val,
    lox::ErrorHandler& errHdl,
    var::Token_itr     loc)
{
  if (!push_impl(id, val)) {
    detail::errReDeclVar(errHdl, id, loc);
    return false;
  }
  return true;
}

bool env::push(
    std::string_view id,
    var::NilState  nil __attribute__((unused)),
    ErrorHandler&  errHdl,
    var::Token_itr loc)
{
  return push(id, errHdl, loc);
}

bool env::push(
    std::string_view id,
    ErrorHandler&  errHdl,
    var::Token_itr loc)
{
  if (!push_impl(id, var::NilState{})) {
    detail::errReDeclVar(errHdl, id, loc);
    return false;
  }
  return true;
}

lox::var::Var_t& lox::VarEnv::errUnDeclVar(
    lox::ErrorHandler&  errHdl,
    var::Token_itr      loc,
    std::string_view    arg)
{
  errHdl.recordErrMsg(
      [&arg](auto& msg) {
        msg += "variable ";
        msg += arg;
        msg += " was not declared in this scope";
      },
      loc
  );
  return m_err;
}

lox::var::Var_t const& lox::VarEnv::errUnDeclVar(
    lox::ErrorHandler&  errHdl,
    var::Token_itr      loc,
    std::string_view    arg) const
{
  errHdl.recordErrMsg(
      [&arg](auto& msg) {
        msg += "variable ";
        msg += arg;
        msg += " was not declared in this scope";
      },
      loc
  );
  return m_err;
}
