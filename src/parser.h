#ifndef LOX_PARSER_H
#define LOX_PARSER_H

#include <vector>
#include <memory>
#include "vecWrapper.h"
#include "token.h"

namespace lox {

class Stmt;
class Expr;
class ErrorHandler;

namespace psr {
using Expr_ptr = std::unique_ptr<Expr>;
using Stmt_ptr = std::unique_ptr<Stmt>;
using Node_t = std::variant<
    std::monostate, ///Error state. If error observed then holds this state
    Expr_ptr,       ///Holds nodes of base type lox::Expr
    Stmt_ptr        ///Holds nodes of base type lox::Stmt
>;

enum class Node_e {
  error_st, ///get_var_state() returns error_st if Node_t holds std::monostate
  expr_st,  ///get_var_state() returns expr_st if Node_t holds expr_ptr
  stmt_st   ///get_var_state() returns stmt_st if Node_t holds stmt_ptr
};


class Allocator {
 public:
  template<typename T, typename... Args>
  [[nodiscard]] std::unique_ptr<T> alloc(Args&&... args) const
  {
    return std::make_unique<T>(std::forward<Args>(args)...);
  }
};

}///end of namespace psr

class Parser {
  std::vector<std::unique_ptr<Stmt>> m_ast;
  ErrorHandler&                      m_err_hdl;
  psr::Allocator                     m_allocator;

 public:
  explicit Parser(ErrorHandler& err_hdl);
  ~Parser() noexcept;

  Parser(Parser const& )                = delete;
  Parser& operator=(Parser const& )     = delete;
  Parser(Parser&& ) noexcept            = delete;
  Parser& operator=(Parser&& ) noexcept = delete;

  [[nodiscard]] VecWrapper<std::unique_ptr<Stmt>, int> get_ast();
  [[nodiscard]] bool gen_ast(VecWrapper<Token, const int> tokens);

 private:
  ///Invoked on error. Increment until delimiter is observed or EOF occurred
  void synchronize(TokenRange& token_r, Token_e delimiter) const;

  ///Methods that walk over tokens and build an AST graph
  [[nodiscard]] psr::Node_t declaration(TokenRange& token_r);
  [[nodiscard]] psr::Node_t varDeclaration(TokenRange& token_r);
  [[nodiscard]] psr::Node_t statement(TokenRange& token_r);
  [[nodiscard]] psr::Node_t ifStatement(TokenRange& token_r);
  [[nodiscard]] psr::Node_t elseStatement(TokenRange& token_r);
  [[nodiscard]] psr::Node_t ifElseIfStatement(TokenRange& token_r);
  [[nodiscard]] psr::Node_t printStatement(TokenRange& token_r);
  [[nodiscard]] psr::Node_t blockDeclaration(TokenRange& token_r);
  [[nodiscard]] psr::Node_t expressionStatement(TokenRange& token_r) const;
  [[nodiscard]] psr::Node_t expression(TokenRange& token_r)          const;
  [[nodiscard]] psr::Node_t assignment(TokenRange&   token_r)        const;
  [[nodiscard]] psr::Node_t logicalOr(TokenRange& token_r)           const;
  [[nodiscard]] psr::Node_t logicalAnd(TokenRange& token_r)          const;
  [[nodiscard]] psr::Node_t bitwiseOr(TokenRange&   token_r)         const;
  [[nodiscard]] psr::Node_t bitwiseXor(TokenRange& token_r)          const;
  [[nodiscard]] psr::Node_t bitwiseAnd(TokenRange& token_r)          const;
  [[nodiscard]] psr::Node_t equality(TokenRange& token_r)            const;
  [[nodiscard]] psr::Node_t comparison(TokenRange& token_r)          const;
  [[nodiscard]] psr::Node_t bitwiseShft(TokenRange& token_r)         const;
  [[nodiscard]] psr::Node_t term(TokenRange& token_r)                const;
  [[nodiscard]] psr::Node_t factor(TokenRange& token_r)              const;
  [[nodiscard]] psr::Node_t unary(TokenRange& token_r)               const;
  [[nodiscard]] psr::Node_t prefix(TokenRange& token_r)              const;
  [[nodiscard]] psr::Node_t suffix(TokenRange& token_r)              const;
  [[nodiscard]] psr::Node_t primary(TokenRange& token_r)             const;
  [[nodiscard]] psr::Node_t grouping(TokenRange& token_r)            const;
};

} //end of namespace lox

#endif //LOX_PARSER_H
