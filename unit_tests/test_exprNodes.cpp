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
  using T1    = lox::LRExpr;
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
    lox::Token{lox::Token_e::integer, static_cast<llint>(3), 0, 0},
    lox::Token{lox::Token_e::plus, 0, 2},
    lox::Token{lox::Token_e::integer, static_cast<llint>(5), 0, 4}
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tknWrp{m_tokens};

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tknWrp};

  ///Construction Binary Expression LHS and RHS in place
  lox::Binary m_plus{
    std::make_unique<T1>(T1(lox::TokenRange(m_tknWrp.begin(), std::next(m_tknWrp.begin())))),
    std::next(m_tknWrp.begin())->m_type,
    std::make_unique<T1>(T1(lox::TokenRange(std::next(m_tknWrp.begin(), 2), m_tknWrp.end()))),
    lox::TokenRange(m_tknWrp.begin(), m_tknWrp.end())
  };

  auto m_value = m_plus.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<llint>(m_value));
  REQUIRE(std::get<llint>(m_value) == 8);
  REQUIRE(m_plus.getTokenRange().cur_loc() == m_tknWrp.begin());
  m_env.deallocBlk();
}

TEST_CASE("Testing a + b", "a + b")
{
  using T1    = lox::LRExpr;
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
  lox::VecWrapper<lox::Token, const int> m_tknWrp{m_tokens};

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tknWrp};
  lox::ErrorWrapper m_err_wrap{m_err, m_tknWrp.begin()};

  ///Ignore value of m_tokenItr, passing just to satisfy API
  REQUIRE(m_env.push("a", a, m_err_wrap));
  REQUIRE(m_env.push("b", b, m_err_wrap));
  ///Expected to return fail since we are re-declaring same variable
  ///within same scope
  REQUIRE(!m_env.push("a", a, m_err_wrap));

  ///Construction Binary Expressions with variables
  lox::Binary m_plus1{
      std::make_unique<T1>(T1(lox::TokenRange(m_tknWrp.begin(), std::next(m_tknWrp.begin())))),
      std::next(m_tknWrp.begin())->m_type,
      std::make_unique<T1>(T1(lox::TokenRange(std::next(m_tknWrp.begin(), 2), std::next(m_tknWrp.begin(), 3)))),
      lox::TokenRange(m_tknWrp.begin(), m_tknWrp.end())
  };
  auto m_value = m_plus1.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<double>(m_value));
  REQUIRE(std::get<double>(m_value) == 16);

  //Allocating new scope block in varenv and assigning them to same variables
  m_env.allocBlk();
  REQUIRE(m_env.push("a", m1, m_err_wrap));
  REQUIRE(m_env.push("b", m2, m_err_wrap));
  ///Expected to return fail since we are re-declaring same variable
  ///within same scope
  REQUIRE(!m_env.push("b", a, m_err_wrap));
  lox::Binary m_plus2{
      std::make_unique<T1>(T1(lox::TokenRange(std::next(m_tknWrp.begin(), 4), std::next(m_tknWrp.begin(), 5)))),
      std::next(m_tknWrp.begin(), 5)->m_type,
      std::make_unique<T1>(T1(lox::TokenRange(std::next(m_tknWrp.begin(), 6), std::next(m_tknWrp.begin(), 7)))),
      lox::TokenRange(m_tknWrp.begin(), m_tknWrp.end())
  };
  auto m_value2 = m_plus2.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<std::string>(m_value2));
  REQUIRE(std::get<std::string>(m_value2) == m1 + m2);
}

TEST_CASE("Testing V1 / (V2 + 5)", "T3")
{
  using llint = long long;
  using T1    = lox::LRExpr;
  using T2    = lox::Grouping;
  using T3    = lox::Binary;
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
  lox::VecWrapper<lox::Token, const int> m_tknWrp{m_tokens};
  auto m_tokenItr = m_tknWrp.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tknWrp};
  lox::ErrorWrapper m_err_wrap{m_err, m_tokenItr};
  llint v1 = 100;
  llint v2 = 5;
  REQUIRE(m_env.push("v1", v1, m_err_wrap));
  REQUIRE(m_env.push("v2", v2, m_err_wrap));
  ///Construction Binary Expression LHS and RHS in place
  lox::Binary m_bin(
      std::make_unique<T1>(T1(lox::TokenRange(m_tokenItr, std::next(m_tokenItr)))),
      std::next(m_tokenItr)->m_type,
      std::make_unique<T2>(
          T2(std::make_unique<T3>(T3(
              std::make_unique<T1>(T1(lox::TokenRange(std::next(m_tokenItr, 3), std::next(m_tokenItr, 4)))),
              std::next(m_tokenItr, 4)->m_type,
              std::make_unique<T1>(T1(lox::TokenRange(std::next(m_tokenItr, 5), std::next(m_tokenItr, 6)))),
              lox::TokenRange(std::next(m_tokenItr, 3), std::next(m_tokenItr, 6))
          )),
          lox::TokenRange(std::next(m_tokenItr, 2), std::next(m_tokenItr, 7))
      )),
      lox::TokenRange(m_tokenItr, std::next(m_tokenItr, 7))
  );

  {
    ///All the variables are integer and result  (100/ 10) is an integer. Hence integer is expected
    auto m_value = m_bin.evaluate(m_env, m_err);
    REQUIRE(std::holds_alternative<lox::llint>(m_value));
    REQUIRE(std::get<lox::llint>(m_value) == 10);
  }
  {
    ///Changing last but one (constant value) token to 11.
    ///Though all variables are integers, result of 100 /16 is not integer (6.25). Hence double is expected
    *std::next(m_tokens.rbegin()) = lox::Token{lox::Token_e::integer, static_cast<llint>(11), 0, 10};
    auto m_value = m_bin.evaluate(m_env, m_err);
    REQUIRE(std::holds_alternative<double>(m_value));
    REQUIRE(std::get<double>(m_value) == 6.25);
  }
  {
    ///Changing v1 to 5, v2 to 2.5 and last but one token to 0
    ///v1 is integer but v2 is double. Though the result 5 / 2.5 is integer (2) but must remain double.
    *std::next(m_tokens.rbegin()) = lox::Token{lox::Token_e::integer, static_cast<llint>(0), 0, 10};
    m_env.get("v1", m_err_wrap) = 5;
    m_env.get("v2", m_err_wrap) = 2.5;
    auto m_value = m_bin.evaluate(m_env, m_err);
    REQUIRE(std::holds_alternative<double>(m_value));
    REQUIRE(std::get<double>(m_value) == 2.00);
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
  lox::VecWrapper<lox::Token, const int> m_tknWrp{m_tokens};
  auto m_tokenItr = m_tknWrp.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();
  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tknWrp};
  lox::ErrorWrapper m_err_wrap{m_err, m_tokenItr};

  double const incr = 9.9;
  REQUIRE(m_env.push("incr", incr, m_err_wrap));
  lox::Prefix m_pfix(
      m_tokenItr->m_type,
      std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_tokenItr), std::next(m_tokenItr, 2)}),
      lox::TokenRange{m_tknWrp.begin(), m_tknWrp.end()}
  );
  auto m_value = m_pfix.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<double>(m_value));
  REQUIRE(std::get<double>(m_value) == incr + 1);
  REQUIRE(std::get<double>(m_env.get("incr", m_err_wrap)) == incr + 1);

   //Suffix operation
  {
    lox::Suffix m_sfix(
        std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_tokenItr), std::next(m_tokenItr, 2)}),
        lox::Token_e::decr,
        lox::TokenRange{m_tknWrp.begin(), m_tknWrp.end()}
    );
    auto m_value2 = m_sfix.evaluate(m_env, m_err);
    REQUIRE(std::holds_alternative<double>(m_value2));
    REQUIRE(std::get<double>(m_value2) == incr + 1);
    REQUIRE(std::get<double>(m_env.get("incr", m_err_wrap)) == incr);
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
  lox::VecWrapper<lox::Token, const int> m_tknWrp{m_tokens};
  auto m_tokenItr = m_tknWrp.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();
  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tknWrp};
  lox::ErrorWrapper m_err_wrap{m_err, m_tokenItr};

  REQUIRE(m_env.push("var", input, m_err_wrap));
  m_env.allocBlk();
  REQUIRE(m_env.push("ngOfVar", static_cast<llint>(0), m_err_wrap));

  lox::AssignExpr m_assign(
      std::make_unique<lox::LRExpr>(lox::TokenRange{m_tokenItr, std::next(m_tokenItr)}),
      std::next(m_tokenItr)->m_type,
      std::make_unique<lox::Unary>(
          std::next(m_tokenItr, 2)->m_type,
          std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_tokenItr, 3), std::next(m_tokenItr, 4)}),
          lox::TokenRange{std::next(m_tokenItr, 2), m_tknWrp.end()}
      ),
      lox::TokenRange{m_tknWrp.begin(), m_tknWrp.end()}
  );

  auto m_value = m_assign.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<lox::Assignment>(m_value));
  REQUIRE(std::get<llint>(m_env.get("ngOfVar", m_err_wrap)) == -input);
  REQUIRE(std::get<llint>(m_env.get("var", m_err_wrap)) == input);
}
///Changing constant to -5 in above expression V1 / (V2 - 5)
///Also adding assignment
///div(100, 0) expected to fail safely
TEST_CASE("Testing var result = V1 / (V2 - 5)", "T4")
{
  using llint = long long;
  ///Constructing tokens
  std::vector<lox::Token> m_tokens{
      lox::Token{lox::Token_e::varK, 0, 0},
      lox::Token{lox::Token_e::identifier, std::string{"result"}, 0, 4},
      lox::Token{lox::Token_e::eq, 0, 11},
      lox::Token{lox::Token_e::identifier, std::string{"v1"}, 0, 13},
      lox::Token{lox::Token_e::div, 0, 16},
      lox::Token{lox::Token_e::leftParan, 0, 18},
      lox::Token{lox::Token_e::identifier, std::string{"v2"}, 0, 19},
      lox::Token{lox::Token_e::plus, 0, 22},
      lox::Token{lox::Token_e::integer, static_cast<llint>(5), 0, 24},
      lox::Token{lox::Token_e::rightParan, 0, 25},
  };
  ///Wrapping them in Constant Token Wrapper for external interface
  lox::VecWrapper<lox::Token, const int> m_tknWrp{m_tokens};
  auto m_itr = m_tknWrp.begin();

  ///Constructing an Environment to store variables
  lox::VarEnv m_env;
  m_env.allocBlk();

  ///Constructing Error Handler
  lox::ErrorHandler m_err{m_tknWrp};
  lox::ErrorWrapper m_err_wrap{m_err, m_itr};
  llint v1 = 100;
  llint v2 = 5;
  llint result = 0;
  REQUIRE(m_env.push("v1", v1, m_err_wrap));
  REQUIRE(m_env.push("v2", v2, m_err_wrap));
  REQUIRE(m_env.push("result", result, m_err_wrap));
  ///Construction Binary Expression LHS and RHS in place
  lox::AssignExpr m_assign(
      std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_itr), std::next(m_itr, 2)}),
      std::next(m_itr, 2)->m_type,
      std::make_unique<lox::Binary>(
          lox::Binary(
              std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_itr, 3), std::next(m_itr, 4)}),
              std::next(m_itr, 4)->m_type,
              std::make_unique<lox::Grouping>(
                  std::make_unique<lox::Binary>(
                      std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_itr, 6), std::next(m_itr, 7)}),
                      std::next(m_itr, 7)->m_type,
                      std::make_unique<lox::LRExpr>(lox::TokenRange{std::next(m_itr, 8), std::next(m_itr, 9)}),
                      lox::TokenRange{std::next(m_itr, 6), std::next(m_itr, 9)}
                  ),
                  lox::TokenRange{std::next(m_itr, 5), std::next(m_itr, 10)}
              ),
              lox::TokenRange{std::next(m_itr, 3), std::next(m_itr, 10)}
          )
      ),
      lox::TokenRange{m_tknWrp.begin(), m_tknWrp.end()}
  );

  auto m_value = m_assign.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<lox::Assignment>(m_value));
  REQUIRE(std::get<llint>(m_env.get("result", m_err_wrap)) == 10);
  ///Changing v2 from 5 to -5. Second push will result in false Also generates error message
  REQUIRE_FALSE(m_env.push("v2", -v2, m_err_wrap));
  m_env.get("v2", m_err_wrap) = -v2;
  m_value = m_assign.evaluate(m_env, m_err);
  REQUIRE(std::holds_alternative<lox::ErrorReported>(m_value));
  m_err.printErrMsgs();
  ///Due to an div by 0 Error result holds previous value. Not corrupted
  REQUIRE(std::get<llint>(m_env.get("result", m_err_wrap)) == 10);
}
