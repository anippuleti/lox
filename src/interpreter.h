#ifndef LOX_INTERPRETER_H
#define LOX_INTERPRETER_H

#include "vecWrapper.h"
#include "errorHandler.h"

namespace lox {

class Stmt;

class Interpreter {
 ErrorHandler& m_err_hdl;

 public:
  explicit Interpreter(ErrorHandler& err_hdl);
  ~Interpreter();

  Interpreter(Interpreter const &) = delete;
  Interpreter &operator=(Interpreter const &) = delete;
  Interpreter(Interpreter &&) noexcept = delete;
  Interpreter &operator=(Interpreter &&) noexcept = delete;

  [[nodiscard]] bool evaluate(
      VecWrapper<std::unique_ptr<Stmt>, int> eval_ast) const;
};

} ///end of namespace lox

#endif //LOX_INTERPRETER_H
