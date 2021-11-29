#include "parser.h"
#include "stmtNodes.h"
#include "exprNodes.h"
#include "errorHandler.h"

//////////////////////////////////////////////////////////////////////////////
namespace psrnms {
//////////////////////////////////////////////////////////////////////////////
namespace psr = lox::psr;

template<psr::Node_e ST>
using isMonoSt = lox::traits::isValid<ST == psr::Node_e::error_st>;

template<psr::Node_e ST>
using isExprSt = lox::traits::isValid<ST == psr::Node_e::expr_st>;

///Template specializations for dispatching operations
template<psr::Node_e ST>
using Node_ret = std::conditional<
    isMonoSt<ST>::value,
    std::monostate,
    std::conditional_t<
        isExprSt<ST>::value,
        lox::psr::Expr_ptr,
        lox::psr::Stmt_ptr
    >
>;

template<psr::Node_e ST>
using Node_ret_t = typename Node_ret<ST>::type;

///Increments token iterator if match found;
[[nodiscard]] bool match(
    std::initializer_list<lox::Token_e> tokens,
    lox::TokenRange&                   token_r)
{
  if (token_r.isEnd())
    return false;
  auto res = std::any_of(
      tokens.begin(),
      tokens.end(),
      [&token_r] (auto elem) { return (*token_r)() == elem; }
  );
  if (res) {
    ++token_r;
    return true;
  }
  return false;
}

[[maybe_unused]][[nodiscard]] bool notMatch(
    std::initializer_list<lox::Token_e> tokens,
    lox::TokenRange&                   token_r) {
  if (token_r.isEnd())
    return false;
  auto res = std::none_of(
      tokens.begin(),
      tokens.end(),
      [&token_r](auto elem) { return (*token_r)() == elem; }
  );
  if (res) {
    ++token_r;
    return true;
  }
  return false;
}

psr::Node_e getVarState(psr::Node_t const& state)
{
  return std::visit(
      [] (auto&& arg) -> psr::Node_e {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, std::monostate>)
          return lox::psr::Node_e::error_st;
        else if constexpr (std::is_same_v<T, psr::Expr_ptr>)
          return lox::psr::Node_e::expr_st;
        else if constexpr (std::is_same_v<T, psr::Stmt_ptr>)
          return lox::psr::Node_e::stmt_st;
        else
            static_assert(lox::traits::always_false_v<T>, "Not exhaustive");
      },
      state
  );
}

[[nodiscard]] bool isExprState(psr::Node_t const& state)
{
  return getVarState(state) == psr::Node_e::expr_st;
}

[[nodiscard]] bool isStmtState(psr::Node_t const& state)
{
  return getVarState(state) == psr::Node_e::stmt_st;
}

[[nodiscard]] bool isErrorState(psr::Node_t const& state)
{
  return getVarState(state) == psr::Node_e::error_st;
}


template<lox::psr::Node_e ST>
Node_ret_t<ST> getVarType(psr::Node_t&& state)
{
  if constexpr (ST == psr::Node_e::error_st)
    return std::monostate{};
  else if constexpr (ST == psr::Node_e::expr_st)
    return std::move(std::get<psr::Expr_ptr>(state));
  else
    return std::move(std::get<psr::Stmt_ptr>(state));
}

bool pushNode(
    std::vector<psr::Stmt_ptr>& vec,
    psr::Node_t var_node)
{
  using namespace psr;
  auto res = getVarState(var_node);
  if (res == Node_e::error_st || res == Node_e::expr_st)
    return true;
  vec.emplace_back(
      getVarType<Node_e::stmt_st>(std::move(var_node))
  );
  return false;
}

[[nodiscard]] bool isVarIdentifier(psr::Node_t const& lhs)
{
  if (std::holds_alternative<psr::Expr_ptr>(lhs)) {
    auto tinfo = (*std::get<psr::Expr_ptr>(lhs)->getTokenRange().cur_loc())();
    return tinfo == lox::Token_e::identifier;
  }
  return false;
}

using TokenList = std::initializer_list<lox::Token_e>;
template<typename Function, typename Object, typename Arg1>
psr::Node_t invokeBinOpLoops(
    Function&&            func,
    Object&&              obj,
    Arg1&                 token_r,
    TokenList&&           token_list,
    psr::Allocator const& m_allocator)
{
  auto stitr = token_r.cur_loc();
  auto lhs   = std::invoke(
      std::forward<Function>(func),
      std::forward<Object>(obj),
      token_r
  );

  if (!psrnms::isExprState(lhs))
    return psr::Node_t{};

  while (psrnms::match(std::forward<TokenList>(token_list), token_r)) {
    auto tkn = token_r.prev()->m_type;
    auto rhs = std::invoke(
        std::forward<Function>(func),
        std::forward<Object>(obj),
        token_r
    );
    if (!psrnms::isExprState(rhs))
      return psr::Node_t{};

    lhs = psr::Node_t{
        m_allocator.alloc<lox::Binary>(
            psrnms::getVarType<psr::Node_e::expr_st>(std::move(lhs)),
            tkn,
            psrnms::getVarType<psr::Node_e::expr_st>(std::move(rhs)),
            lox::TokenRange(stitr, token_r.cur_loc())
        )
    };
    stitr = token_r.cur_loc();
  }
  return lhs;
}


template<typename Function, typename Object, typename Condition>
[[nodiscard]] lox::smt::Status_e fillup_elseif_clauses(
    Function&& func,
    Object&&   obj,
    std::vector<lox::psr::Stmt_ptr>& if_clauses,
    Condition&& predicate,
    lox::TokenRange& token_r)
{
  do {
    ++(++token_r);
    auto eif_clause = std::invoke(func, obj, token_r);
    if (!psrnms::isStmtState(eif_clause))
      return lox::smt::Status_e::Failure;

    if_clauses.push_back(
        getVarType<lox::psr::Node_e::stmt_st>(std::move(eif_clause))
    );
  } while (predicate(token_r));
  return lox::smt::Status_e::Success;
}

void errOnVarDecl(
    lox::ErrorHandler&  err,
    lox::Token_itr loc)
{
  err.recordErrMsg(
      [] (auto& msg) {
        msg += "expected variable name before ";
      },
      loc
  );
}

void errUnexpectedToken(
    lox::ErrorHandler& err,
    std::string_view   exp_res,
    lox::Token_itr     loc)
{
  err.recordErrMsg(
      [exp_res, loc] (auto& msg) {
        msg += "expected ";
        msg += exp_res;
        lox::toStr(msg, loc->m_type);
      },
      loc
  );
}

void errMissingAfterToken(
    lox::ErrorHandler& err,
    char token,
    lox::Token_itr     loc)
{
  err.recordErrMsg(
      [loc, token] (auto& msg) {
        msg += "missing ";
        msg += token;
        msg += " after ";
        lox::toStr(msg, (*loc)());
      },
      loc
  );
}

void errMissingAfterToken(
    lox::ErrorHandler& err,
    std::pair<char, char> tokens,
    lox::Token_itr     loc)
{
  err.recordErrMsg(
      [loc, tokens] (auto& msg) {
        msg += "either missing ";
        msg += tokens.first;
        msg += " or ";
        msg += tokens.second;
        msg += " after ";
        lox::toStr(msg, (*loc)());
      },
      loc
  );
}

//////////////////////////////////////////////////////////////////////////////
}///end of namespace psrnms
//////////////////////////////////////////////////////////////////////////////

lox::Parser::Parser(lox::ErrorHandler& err_hdl):
  m_err_hdl{err_hdl}
{

}

lox::Parser::~Parser() noexcept = default;

lox::VecWrapper<std::unique_ptr<lox::Stmt>, int> lox::Parser::get_ast()
{
  return VecWrapper<std::unique_ptr<Stmt>, int>{m_ast};
}

bool lox::Parser::gen_ast(
    VecWrapper<Token, const int> tokens)
{
  bool err_obsrvd{false};
  TokenRange token_r(tokens.begin(), tokens.end());
  while (!token_r.isEnd()) {
    err_obsrvd |= psrnms::pushNode(m_ast, declaration(token_r));
    if (err_obsrvd)
      synchronize(token_r, Token_e::semicolan);
  }
  return err_obsrvd;
}

void lox::Parser::synchronize(
    TokenRange& token_r,
    Token_e     delimiter) const
{
  while (!token_r.isEnd()) {
    if (token_r.cur_loc()->m_type == delimiter) {
      ++token_r;
      return;
    }
    ++token_r;
  }
}

lox::psr::Node_t lox::Parser::declaration(
    TokenRange& token_r)
{
  if (psrnms::match({Token_e::varK}, token_r))
    return varDeclaration(token_r);

  return statement(token_r);
}

lox::psr::Node_t lox::Parser::varDeclaration(
    TokenRange& token_r)
{
  auto stitr = token_r.prev();
  auto var_name = primary(token_r);
  if (psrnms::isErrorState(var_name))
    return var_name;

  if (psrnms::match({lox::Token_e::eq}, token_r)) {
    auto init = expression(token_r);
    if (!psrnms::match({Token_e::semicolan}, token_r)) {
      psrnms::errMissingAfterToken(m_err_hdl, ';', token_r.prev());
      return psr::Node_t{};
    }
    if (psrnms::isExprState(init))
      return psr::Node_t{
          m_allocator.alloc<lox::VarDecl>(
              psrnms::getVarType<psr::Node_e::expr_st>(std::move(var_name)),
              psrnms::getVarType<psr::Node_e::expr_st>(std::move(init)),
              TokenRange(stitr, token_r.cur_loc())
          )
      };

  } else if (psrnms::match({lox::Token_e::semicolan}, token_r)) {
    return psr::Node_t{
        m_allocator.alloc<lox::VarDecl>(
            psrnms::getVarType<psr::Node_e::expr_st>(std::move(var_name)),
            TokenRange(stitr, token_r.cur_loc())
        )
    };
  }
  psrnms::errMissingAfterToken(m_err_hdl, {'=', ';'}, token_r.prev());
  return psr::Node_t{};
}

lox::psr::Node_t lox::Parser::statement(
    TokenRange& token_r)
{
  if (psrnms::match( {Token_e::ifK}, token_r))
    return ifElseIfStatement(token_r);
  if (psrnms::match({Token_e::printK}, token_r))
    return printStatement(token_r);
  else if (psrnms::match({Token_e::leftBrace}, token_r))
    return blockDeclaration(token_r);
  return expressionStatement(token_r);
}

lox::psr::Node_t lox::Parser::ifStatement(TokenRange& token_r)
{
  auto stitr = token_r.prev();
  if (psrnms::match({Token_e::leftParan}, token_r)) {
    auto if_cond = grouping(token_r);
    if (psrnms::isExprState(if_cond)) {
      if (psrnms::match({Token_e::leftBrace}, token_r)) {
        auto blk_smt = blockDeclaration(token_r);
        if (psrnms::isStmtState(blk_smt))
          return psr::Node_t{
              m_allocator.alloc<IfStmt>(
                  psrnms::getVarType<psr::Node_e::expr_st>(std::move(if_cond)),
                  psrnms::getVarType<psr::Node_e::stmt_st>(std::move(blk_smt)),
                  TokenRange(stitr, token_r.cur_loc())
              )
          };
        else
          return psr::Node_t{};
      } else {
        psrnms::errMissingAfterToken(m_err_hdl, '{', token_r.prev());
        return psr::Node_t{};
      }
    } else {
      return psr::Node_t{};
    }
  }
  psrnms::errMissingAfterToken(m_err_hdl, '(', token_r.prev());
  return psr::Node_t{};
}

lox::psr::Node_t lox::Parser::elseStatement(TokenRange& token_r) {
  auto stitr = token_r.prev();
  if (psrnms::match({Token_e::leftBrace}, token_r)) {
    auto blk_smt = blockDeclaration(token_r);
    if (psrnms::isStmtState(blk_smt))
      return psr::Node_t{
          m_allocator.alloc<ElseStmt>(
              psrnms::getVarType<psr::Node_e::stmt_st>(std::move(blk_smt)),
              TokenRange(stitr, token_r.cur_loc())
              )
      };
    else
      return psr::Node_t{};
  }
  psrnms::errMissingAfterToken(m_err_hdl, '{', token_r.prev());
  return psr::Node_t{};
}

lox::psr::Node_t lox::Parser::ifElseIfStatement(TokenRange& token_r)
{
  auto stitr = token_r.prev();
  auto if_clause = ifStatement(token_r);
  if (!psrnms::isStmtState(if_clause))
    return psr::Node_t{};

  ///Intentionally passing Token range by value to avoid consuming 2 tokens
  ///at once. If this predicated returns true, then fillup_elseif_clauses()
  ///will explicitly consume bo the the tokens.
  auto isElseIfClause = [] (auto token_loc) {
    return psrnms::match({Token_e::elseK}, token_loc) &&
           psrnms::match({Token_e::ifK}, token_loc);
  };
  auto isElseClause = [] (auto& token_loc) {
    return psrnms::match({Token_e::elseK}, token_loc);
  };

  if (isElseIfClause(token_r)) {
    std::vector<psr::Stmt_ptr> if_clauses;
    if_clauses.push_back(
        psrnms::getVarType<psr::Node_e::stmt_st>(std::move(if_clause))
    );
    auto elif_res = psrnms::fillup_elseif_clauses(
        &Parser::ifStatement,
        this,
        if_clauses,
        isElseIfClause,
        token_r);
    if (elif_res == smt::Status_e::Failure)
      return psr::Node_t{};

    if (isElseClause(token_r)) {
      auto else_clause = elseStatement(token_r);
      if (!psrnms::isStmtState(else_clause))
        return psr::Node_t{};
      return psr::Node_t{
          m_allocator.alloc<IfElseIfStmt>(
                std::move(if_clauses),
                psrnms::getVarType<psr::Node_e::stmt_st>(std::move(else_clause)),
                TokenRange(stitr, token_r.cur_loc())
            )
      };
    } else {
      return psr::Node_t{
          m_allocator.alloc<IfElseIfStmt>(
              std::move(if_clauses),
              TokenRange(stitr, token_r.cur_loc())
          )
      };
    }
  } else if (isElseClause(token_r)) {
    auto else_clause = elseStatement(token_r);
    if (!psrnms::isStmtState(else_clause))
      return psr::Node_t{};
    return psr::Node_t{
        m_allocator.alloc<IfElseStmt>(
            psrnms::getVarType<psr::Node_e::stmt_st>(std::move(if_clause)),
            psrnms::getVarType<psr::Node_e::stmt_st>(std::move(else_clause)),
            TokenRange(stitr, token_r.cur_loc())
        )
    };
  }
  return if_clause;
}

lox::psr::Node_t lox::Parser::printStatement(
    TokenRange& token_r)
{
  std::vector<psr::Expr_ptr> expr_list;
  auto stitr = token_r.prev();
  do {
    auto res = expression(token_r);
    if (psrnms::isExprState(res)) {
      expr_list.push_back(
          psrnms::getVarType<psr::Node_e::expr_st>(std::move(res))
      );
    } else {
      psrnms::errUnexpectedToken(
          m_err_hdl,
          "an expression but observed ",
          token_r.cur_loc()
      );
      return psr::Node_t{};
    }
  } while (psrnms::match({Token_e::comma}, token_r));

  if (psrnms::match({Token_e::semicolan}, token_r)) {
    return psr::Node_t{
      m_allocator.alloc<PrintDecl>(
              std::move(expr_list), TokenRange(stitr, token_r.cur_loc())
        )
    };
  }
  psrnms::errMissingAfterToken(m_err_hdl, ';', token_r.prev());
  return psr::Node_t{};
}

lox::psr::Node_t lox::Parser::blockDeclaration(
    TokenRange& token_r)
{
  std::vector<psr::Stmt_ptr> stmt_list;
  auto sitr = token_r.prev();
  do {
    auto res = declaration(token_r);
    if (psrnms::isStmtState(res)) {
      stmt_list.push_back(
          psrnms::getVarType<psr::Node_e::stmt_st>(std::move(res))
      );
    } else {
      psrnms::errUnexpectedToken(
          m_err_hdl,
          "a statement or variable declaration but observed ",
          token_r.cur_loc());
      ///excluding rest of the statements in the block
      synchronize(token_r, Token_e::rightBrace);
      return psr::Node_t{};
    }
  } while (!psrnms::match({Token_e::rightBrace}, token_r));

  if (token_r.isEnd() && token_r.prev()->m_type != Token_e::rightBrace) {
    psrnms::errMissingAfterToken(m_err_hdl, '}', token_r.prev());
    return psr::Node_t{};
  }

  return psr::Node_t{
    m_allocator.alloc<BlockDecl>(
        std::move(stmt_list),
        TokenRange(sitr, token_r.cur_loc())
        )
  };
}

lox::psr::Node_t lox::Parser::expressionStatement(
    TokenRange&   token_r) const
{
  auto sitr = token_r.cur_loc();
  auto res = expression(token_r);
  if (psrnms::isExprState(res)) {
    if (!psrnms::match({Token_e::semicolan}, token_r)) {
      psrnms::errMissingAfterToken(m_err_hdl, ';', token_r.prev());
      return psr::Node_t{};
    }
    return psr::Node_t{
        m_allocator.alloc<ExprStmt>(
            psrnms::getVarType<psr::Node_e::expr_st>(std::move(res)),
            TokenRange(sitr, token_r.cur_loc())
        )
    };
  }
  psrnms::errUnexpectedToken(
      m_err_hdl,
      "an expression but observed ",
      token_r.cur_loc());
  return psr::Node_t{};
}

lox::psr::Node_t lox::Parser::expression(
    TokenRange& token_r) const
{
  return assignment(token_r);
}

lox::psr::Node_t lox::Parser::assignment(
    TokenRange& token_r) const
{
  auto stitr = token_r.cur_loc();
  auto lhs   = logicalOr(token_r);
  auto tknq  = std::initializer_list<Token_e>{
      Token_e::eq, Token_e::plusEq, Token_e::minusEq, Token_e::mulEq,
      Token_e::divEq, Token_e::modEq, Token_e::andEq, Token_e::orEq,
      Token_e::xorEq, Token_e::shLftEq, Token_e::shRhtEq
  };

  if (psrnms::isExprState(lhs) && psrnms::match(tknq, token_r))
  {
    auto tkn = (*token_r.prev())();
    auto rhs = assignment(token_r);
    if (psrnms::isExprState(rhs)) {
      if (!psrnms::isVarIdentifier(lhs)) {
        psrnms::errOnVarDecl(m_err_hdl, stitr);
        return psr::Node_t{};
      }
      return psr::Node_t{
          m_allocator.alloc<Assignment>(
              psrnms::getVarType<psr::Node_e::expr_st>(std::move(lhs)),
              tkn,
              psrnms::getVarType<psr::Node_e::expr_st>(std::move(rhs)),
              TokenRange(stitr, token_r.next())
         )
      };
    }
  }
  return lhs;
}

lox::psr::Node_t lox::Parser::logicalOr(
    TokenRange& token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::logicalAnd,
      this,
      token_r,
      {Token_e::lgOr},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::logicalAnd(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::bitwiseOr,
      this,
      token_r,
      {Token_e::lgAnd},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::bitwiseOr(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::bitwiseXor,
      this,
      token_r,
      {Token_e::bwOr},
      m_allocator
  );
}
lox::psr::Node_t lox::Parser::bitwiseXor(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::bitwiseAnd,
      this,
      token_r,
      {Token_e::bwXor},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::bitwiseAnd(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::equality,
      this,
      token_r,
      {Token_e::bwAnd},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::equality(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::comparison,
      this,
      token_r,
      {Token_e::eqEq, Token_e::nEq},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::comparison(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::bitwiseShft,
      this,
      token_r,
      {Token_e::lessThan, Token_e::lessThanEq,
           Token_e::greatThan, Token_e::greatThanEq},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::bitwiseShft(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::term,
      this,
      token_r,
      {Token_e::bwShLft, Token_e::bwtShRht},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::term(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::factor,
      this,
      token_r,
      {Token_e::plus, Token_e::minus},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::factor(
    TokenRange &token_r) const
{
  return psrnms::invokeBinOpLoops(
      &Parser::unary,
      this,
      token_r,
      {Token_e::mul, Token_e::div, Token_e::mod},
      m_allocator
  );
}

lox::psr::Node_t lox::Parser::unary(
    TokenRange& token_r) const
{
  auto tknq =  {
      Token_e::plus, Token_e::minus, Token_e::lgNot, Token_e::bwNot
  };
  auto stitr = token_r.cur_loc();
  if (psrnms::match(tknq, token_r)) {
    auto tkn = token_r.prev()->m_type;
    auto rhs = unary(token_r);
    if (psrnms::isExprState(rhs)) {
      return psr::Node_t{
        m_allocator.alloc<Unary>(
              tkn,
              psrnms::getVarType<psr::Node_e::expr_st>(std::move(rhs)),
              TokenRange(stitr, token_r.cur_loc())
           )
      };
    }
  }
  return prefix(token_r);
}

lox::psr::Node_t lox::Parser::prefix(
    TokenRange& token_r) const
{
  auto tknq  = {Token_e::incr, Token_e::decr};
  auto stitr = token_r.cur_loc();
  if (psrnms::match(tknq, token_r)) {
    auto tkn = token_r.prev()->m_type;
    auto rhs = primary(token_r);
    if (psrnms::isExprState(rhs))
      return psr::Node_t{
          m_allocator.alloc<Prefix>(
              tkn,
              psrnms::getVarType<psr::Node_e::expr_st>(std::move(rhs)),
              TokenRange(stitr, token_r.cur_loc())
            )
      };
  }
  return suffix(token_r);
}

lox::psr::Node_t lox::Parser::suffix(
    TokenRange& token_r) const
{
  auto lhs   = primary(token_r);
  auto stitr = token_r.cur_loc();
  auto tknq  = {Token_e::incr, Token_e::decr};
  if (psrnms::match(tknq, token_r) &&
      psrnms::isExprState(lhs))
  {
    return psr::Node_t{
        m_allocator.alloc<Suffix>(
            psrnms::getVarType<psr::Node_e::expr_st>(std::move(lhs)),
            token_r.prev()->m_type,
            TokenRange(stitr, token_r.cur_loc())
        )
    };
  }
  return lhs;
}

lox::psr::Node_t lox::Parser::primary(
   TokenRange& token_r) const
{
  auto tknq = {
      Token_e::trueK, Token_e::falseK, Token_e::integer,
      Token_e::fractional, Token_e::string, Token_e::identifier
  };
  if (psrnms::match(tknq, token_r))
    return psr::Node_t{
        m_allocator.alloc<LRExpr>(
            TokenRange(token_r.prev(), token_r.cur_loc())
        )
    };
  else if (psrnms::match({Token_e::leftParan}, token_r))
    return grouping(token_r);

  return psr::Node_t{};
}

lox::psr::Node_t lox::Parser::grouping(TokenRange& token_r) const
{
  auto stitr = token_r.prev();
  auto expr  = expression(token_r);
  if (!psrnms::match({Token_e::rightParan}, token_r)) {
    psrnms::errMissingAfterToken(m_err_hdl, ')', token_r.cur_loc());
    return psr::Node_t{};
  }

  if (psrnms::isExprState(expr))
    return psr::Node_t{
        m_allocator.alloc<Grouping>(
            psrnms::getVarType<psr::Node_e::expr_st>(std::move(expr)),
            TokenRange(stitr, token_r.cur_loc())
        )
    };
  return psr::Node_t{};
}
