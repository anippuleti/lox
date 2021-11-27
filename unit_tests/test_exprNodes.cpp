#include <iostream>
#include "token.h"
#include "varEnv.h"
#include "vecWrapper.h"
#include "errorHandler.h"
#include "exprNodes.h"

#include "catch2/catch_test_macros.hpp"

TEST_CASE("Testing 3 + 5", "T1")
{
  using llint = long long;
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
    lox::Token{lox::Token_e::integer, static_cast<llint>(3), 0, 0},
    lox::Token{lox::Token_e::plus, 0, 2},
    lox::Token{lox::Token_e::integer, static_cast<llint>(5), 0, 4}
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tokenWrapper{m_tokens};
  auto m_token_r = lox::TokenRange(
      m_tokenWrapper.begin(), m_tokenWrapper.end()
  );

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tokenWrapper};

  ///Construction Binary Expression LHS and RHS in place
  lox::Binary m_plus{
    std::make_unique<lox::LRExpr>(m_token_r++),
    m_token_r->m_type,
    std::make_unique<lox::LRExpr>(
        lox::TokenRange(m_token_r.next(), m_tokenWrapper.end())
    ),
    lox::TokenRange(m_tokenWrapper.begin(), m_tokenWrapper.end())
  };
  ++m_token_r;
  ++m_token_r;

  REQUIRE(m_token_r.isEnd());
  auto m_value = m_plus.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<llint>(m_value));
  REQUIRE(std::get<llint>(m_value) == 8);
  m_env.deallocBlk();
}

TEST_CASE("Testing a + b", "a + b")
{
  ///Constructing tokens
  double a = 9.5;
  double b = 6.5;
  std::string m1 = "Helo World!!";
  std::string m2 = " From lox unit test env";
  std::vector<lox::Token> m_tokens{
      lox::Token{lox::Token_e::identifier, std::string{'a'}, 0, 0},
      lox::Token{lox::Token_e::plus, 0, 2},
      lox::Token{lox::Token_e::identifier, std::string{'b'}, 0, 6},
      lox::Token{lox::Token_e::leftBrace, 1, 0},
      lox::Token{lox::Token_e::string, m1,  2, 0},
      lox::Token{lox::Token_e::plus, 2, 2},
      lox::Token{lox::Token_e::string, m2,  2, 4},
      lox::Token{lox::Token_e::rightBrace, 3, 0},
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tokenWrapper{m_tokens};
  auto m_token_r = lox::TokenRange(
      m_tokenWrapper.begin(), m_tokenWrapper.end()
  );

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tokenWrapper};

  ///Ignore value of m_tokenItr, passing just to satisfy API
  REQUIRE(m_env.push("a", a, m_err, m_token_r.cur_loc()));
  REQUIRE(m_env.push("b", b, m_err, m_token_r.cur_loc()));
  ///Expected to return fail since we are re-declaring same variable
  ///within same scope
  REQUIRE(!m_env.push("a", a, m_err, m_token_r.cur_loc()));

  ///Construction Binary Expressions with variables
  lox::Binary m_plus1{
      std::make_unique<lox::LRExpr>(m_token_r++),
      m_token_r->m_type,
      std::make_unique<lox::LRExpr>(
          lox::TokenRange(m_token_r.next(), m_tokenWrapper.end())
      ),
      lox::TokenRange(m_tokenWrapper.begin(), m_tokenWrapper.end())
  };
  ++m_token_r;
  ++m_token_r;
  auto m_value = m_plus1.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<double>(m_value));
  REQUIRE(std::get<double>(m_value) == 16);

  ///Start of new scope
  REQUIRE(m_token_r->m_type == lox::Token_e::leftBrace);

  //Allocating new scope block in varenv and assigning them to same variables
  m_env.allocBlk();
  REQUIRE(m_env.push("a", m1, m_err, m_token_r.cur_loc()));
  REQUIRE(m_env.push("b", m2, m_err, m_token_r.cur_loc()));
  ///Expected to return fail since we are re-declaring same variable
  ///within same scope
  REQUIRE(!m_env.push("b", a, m_err, m_token_r.cur_loc()));
  ///Binary Expression with strings. increment tokenItr to point to m1
  ++m_token_r;
  REQUIRE(m_token_r->m_type == lox::Token_e::string);
  lox::Binary m_plus2{
      std::make_unique<lox::LRExpr>(m_token_r++),
      m_token_r->m_type,
      std::make_unique<lox::LRExpr>(
          lox::TokenRange(m_token_r.next(), m_tokenWrapper.end())
      ),
      lox::TokenRange(m_tokenWrapper.begin(), m_tokenWrapper.end())
  };
  ++m_token_r;
  ++m_token_r;
  REQUIRE(m_token_r->m_type == lox::Token_e::rightBrace);
  ++m_token_r;
  REQUIRE(m_token_r.isEnd());
  auto m_value2 = m_plus2.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<std::string>(m_value2));
  REQUIRE(std::get<std::string>(m_value2) == m1 + m2);

  {
    std::string s;
    m_plus2.toStr(s);
    std::cout << s << std::endl;
  }
}

TEST_CASE("Testing V1 / (V2 + 5)", "T3")
{
  using llint = long long;
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
      lox::Token{lox::Token_e::identifier, std::string{"v1"}, 0, 0},
      lox::Token{lox::Token_e::div, 0, 2},
      lox::Token{lox::Token_e::leftParan, 0, 4},
      lox::Token{lox::Token_e::identifier, std::string{"v2"}, 0, 6},
      lox::Token{lox::Token_e::plus, 0, 8},
      lox::Token{lox::Token_e::integer, static_cast<llint>(5), 0, 10},
      lox::Token{lox::Token_e::rightParan, 0, 12},
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tokenWrapper{m_tokens};
  auto m_tokenItr = m_tokenWrapper.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tokenWrapper};
  llint v1 = 100;
  llint v2 = 5;
  REQUIRE(m_env.push("v1", v1, m_err, m_tokenItr));
  REQUIRE(m_env.push("v2", v2, m_err, m_tokenItr));
  ///Construction Binary Expression LHS and RHS in place
  lox::Binary m_bin(
      std::make_unique<lox::LRExpr>(m_tokenItr),
      std::next(m_tokenItr)->m_type,
      std::make_unique<lox::Grouping>(
          std::make_unique<lox::Binary>(
              std::make_unique<lox::LRExpr>(std::next(m_tokenItr, 3)),
              std::next(m_tokenItr, 4)->m_type,
              std::make_unique<lox::LRExpr>(std::next(m_tokenItr, 5)),
              std::next(m_tokenItr, 3),
              std::next(m_tokenItr, 6)
          ),
          std::next(m_tokenItr, 2),
          std::next(m_tokenItr, 7)
      ),
      m_tokenItr,
      std::next(m_tokenItr, 7)
  );

  auto m_value = m_bin.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<double>(m_value));
  REQUIRE(std::get<double>(m_value) == 10.00);
}


///Changing constant to -5 in above expression V1 / (V2 - 5)
///div(100, 0) expected to fail safely
TEST_CASE("Testing V1 / (V2 - 5)", "T4")
{
  using llint = long long;
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
      lox::Token{lox::Token_e::identifier, std::string{"v1"}, 0, 0},
      lox::Token{lox::Token_e::div, 0, 2},
      lox::Token{lox::Token_e::leftParan, 0, 4},
      lox::Token{lox::Token_e::identifier, std::string{"v2"}, 0, 6},
      lox::Token{lox::Token_e::minus, 0, 8},
      lox::Token{lox::Token_e::integer, static_cast<llint>(5), 0, 10},
      lox::Token{lox::Token_e::rightParan, 0, 12},
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tokenWrapper{m_tokens};
  auto m_tokenItr = m_tokenWrapper.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tokenWrapper};
  llint v1 = 100;
  llint v2 = 5;
  REQUIRE(m_env.push("v1", v1, m_err, m_tokenItr));
  REQUIRE(m_env.push("v2", v2, m_err, m_tokenItr));
  ///Construction Binary Expression LHS and RHS in place
  lox::Binary m_bin(
      std::make_unique<lox::LRExpr>(m_tokenItr),
      std::next(m_tokenItr)->m_type,
      std::make_unique<lox::Grouping>(
          std::make_unique<lox::Binary>(
              std::make_unique<lox::LRExpr>(std::next(m_tokenItr, 3)),
              std::next(m_tokenItr, 4)->m_type,
              std::make_unique<lox::LRExpr>(std::next(m_tokenItr, 5)),
              std::next(m_tokenItr, 3),
              std::next(m_tokenItr, 6)
          ),
          std::next(m_tokenItr, 2),
          std::next(m_tokenItr, 7)
      ),
      m_tokenItr,
      std::next(m_tokenItr, 7)
  );

  auto m_value = m_bin.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<lox::exp::ErrorReported>(m_value));
  {
    std::string s;
    m_bin.toStr(s);
    std::cout << s << std::endl;
  }
}

TEST_CASE("Testing ++a, a--", "T5")
{
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
      lox::Token{lox::Token_e::incr, 0, 0},
      lox::Token{lox::Token_e::identifier, std::string{"incr"}, 0, 2}
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tokenWrapper{m_tokens};
  auto m_tokenItr = m_tokenWrapper.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();
  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tokenWrapper};

  double const incr = 9.9;
  REQUIRE(m_env.push("incr", incr, m_err, m_tokenItr));
  lox::Prefix m_pfix(
      m_tokenItr->m_type,
      std::make_unique<lox::LRExpr>(std::next(m_tokenItr)),
      m_tokenWrapper.begin(),
      m_tokenWrapper.end()
  );
  auto m_value = m_pfix.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<double>(m_value));
  REQUIRE(std::get<double>(m_value) == incr + 1);
  REQUIRE(std::get<double>(m_env.get("incr", m_err, m_tokenItr)) == incr + 1);

  {
    std::string s;
    m_pfix.toStr(s);
    std::cout << s << std::endl;
  }
   //Suffix operation
  {
    lox::Suffix m_sfix(
        std::make_unique<lox::LRExpr>(std::next(m_tokenItr)),
        lox::Token_e::decr,
        m_tokenWrapper.begin(),
        m_tokenWrapper.end()
    );
    auto m_value2 = m_sfix.evaluate(m_env, m_err);
    REQUIRE(std::holds_alternative<double>(m_value2));
    REQUIRE(std::get<double>(m_value2) == incr + 1);
    REQUIRE(std::get<double>(m_env.get("incr", m_err, m_tokenItr)) == incr);
    {
      std::string s;
      m_sfix.toStr(s);
      std::cout << s << std::endl;
    }
  }
}

//var v = 42;
//{
//  var ngv = -v;
//}
TEST_CASE("Testing a = -b", "T6")
{
  using llint = long long;
  llint input = 42;
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
      lox::Token{lox::Token_e::identifier, std::string{"ngOfVar"}, 0, 0},
      lox::Token{lox::Token_e::eq, 0 , 2},
      lox::Token{lox::Token_e::minus, 0, 4},
      lox::Token{lox::Token_e::identifier, std::string{"var"}, 0, 5}
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tokenWrapper{m_tokens};
  auto m_tokenItr = m_tokenWrapper.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();
  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tokenWrapper};
  REQUIRE(m_env.push("var", input, m_err, m_tokenItr));
  m_env.allocBlk();
  REQUIRE(m_env.push("ngOfVar", static_cast<llint>(0), m_err, m_tokenItr));

  lox::Assignment m_assign(
      std::make_unique<lox::LRExpr>(m_tokenItr),
      std::next(m_tokenItr)->m_type,
      std::make_unique<lox::Unary>(
          std::next(m_tokenItr, 2)->m_type,
          std::make_unique<lox::LRExpr>(std::next(m_tokenItr, 3)),
          std::next(m_tokenItr, 2),
          m_tokenWrapper.end()
      ),
      m_tokenWrapper.begin(),
      m_tokenWrapper.end()
  );

  auto m_value = m_assign.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<lox::exp::Assignment>(m_value));
  REQUIRE(std::get<llint>(m_env.get("ngOfVar", m_err, m_tokenItr)) == -input);
  REQUIRE(std::get<llint>(m_env.get("var", m_err, m_tokenItr)) == input);
}
