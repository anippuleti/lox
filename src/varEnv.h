#ifndef LOX_VARENV_H
#define LOX_VARENV_H

#include <vector>
#include <string>
#include <variant>
#include "token.h"
#include "astNode.h"
#include "errorHandler.h"

namespace lox {
namespace var {
///lox::ErrorReported state is only for error detection. The elements
///stored in below 'm_env' data structure will never hold this state.
///Only the class member variable 'm_err' will hold this state.
using Var_t  = std::variant<
    lox::ErrorReported, ///ErrorReported state is only for error detection.
    lox::NilValue,      ///NilState is to denote an uninitialized variable
    bool,
    long long,
    double,
    std::string
>;
} ///end of namespace var

class VarEnv {
 private:
  using Lint_t = long long;
  struct Node {
   private:
    ///Better data structure for these operations is std::unordered_map
    ///Due to lack of string_view lookup support for hashes, we picked vector
    ///Need to analyze performance penalty because all operations are linear
    ///with vector instead of mLog(n) lookup
    std::vector<std::pair<std::string, var::Var_t>> m_var;

   public:
    using iterator       = decltype(m_var)::iterator;
    using const_iterator = decltype(m_var)::const_iterator;
    [[nodiscard]] bool put(std::string_view sv, bool val);
    [[nodiscard]] bool put(std::string_view sv, Lint_t val);
    [[nodiscard]] bool put(std::string_view sv, double val);
    [[nodiscard]] bool put(std::string_view sv, std::string_view val);
    [[nodiscard]] bool put(std::string_view sv, std::string&& val);
    [[nodiscard]] bool put(std::string_view sv, NilValue&& val);

    [[nodiscard]] iterator find(std::string_view sv);
    [[nodiscard]] const_iterator find(std::string_view sv) const;
    [[nodiscard]] iterator end();
    [[nodiscard]] const_iterator cend() const;
  };

  std::vector<Node> m_env;
  ///Varying 'm_err' state will break the program
  var::Var_t m_err{ErrorReported{}};

 public:
  //There must exist only one instance of this class.
  //Hence, explicitly deleting copy and move
  //constructors and assignment methods
  VarEnv();
  VarEnv(VarEnv const& )                = delete;
  VarEnv& operator=(VarEnv const& )     = delete;
  VarEnv(VarEnv&& ) noexcept            = delete;
  VarEnv& operator=(VarEnv&& ) noexcept = delete;
  ~VarEnv() noexcept;

  ///Allocate and deallocate nodes on start and end of scopes { ... }
  void allocBlk();
  void deallocBlk();

  //Returns true if a variable with same name 'id' exists
  //Iterates over all the scopes, returns false only if no
  //variable with same name is found
  [[nodiscard]] bool exists(std::string_view id) const;

  [[nodiscard]] bool push(
      std::string_view id,
      bool val,
      ErrorWrapper err_hdl);
  [[nodiscard]] bool push(
      std::string_view id,
      Lint_t val,
      ErrorWrapper err_hdl);
  [[nodiscard]] bool push(
      std::string_view id,
      double val,
      ErrorWrapper err_hdl);
  [[nodiscard]] bool push(
      std::string_view id,
      std::string_view val,
      ErrorWrapper err_hdl);
  [[nodiscard]] bool push(
      std::string_view id,
      std::string &&val,
      ErrorWrapper err_hdl);
  [[nodiscard]] bool push(
      std::string_view id,
      lox::NilValue    nil,
      ErrorWrapper     err_hdl);
  [[nodiscard]] bool push(
      std::string_view id,
      ErrorWrapper  err_hdl);

  [[nodiscard]] var::Var_t& get(
      std::string_view   id,
      ErrorWrapper       err_hdl);
  [[nodiscard]] var::Var_t const& get(
      std::string_view   id,
      ErrorWrapper       err_hdl) const;

 private:
  ///We start for lowest scope go on to highest scope until we find an hit.
  ///Another way of looking at this API is we iterate in reverse direction
  ///starting from last node in m_env. Within a node we iterate in forward
  ///direction until variable is found or reach end. If no variable is
  ///found in all scopes we return the first node (global node) end().
  ///Worst case, is an O(n * m) algorithm with 'n' being number of scopes and
  ///'m' being max number of variables within a node.
  [[nodiscard]] Node::iterator       find(std::string_view id);
  [[nodiscard]] Node::const_iterator find(std::string_view id) const;
  [[nodiscard]] var::Var_t& errUnDeclVar(
      lox::ErrorWrapper   err_hdl,
      std::string_view    arg);
  [[nodiscard]] var::Var_t const& errUnDeclVar(
      ErrorWrapper        err_hdl,
      std::string_view    arg) const;
  template<typename T>
  [[nodiscard]] bool push_impl(
      std::string_view id,
      T&& val)
  {
    return m_env.back().put(id, std::forward<T>(val));
  }
};

}//end of namespace

#endif //LOX_VARENV_H
