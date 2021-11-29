#ifndef LOX_TOKEN_H
#define LOX_TOKEN_H

#include <variant>
#include <string>
#include <vector>

namespace lox {

enum class Token_e {
///Unexpected token
  error,
///Char indentfiers
  leftParan, rightParan, leftBrace, rightBrace, comma, dot, semicolan,
///Arithmetic operators
  plus, minus, mul, div, mod, bwNot, bwAnd, bwOr, bwXor, bwShLft, bwtShRht,
///Assignment operators
  eq, plusEq, minusEq, mulEq, divEq, modEq, andEq, orEq,  xorEq,
  shLftEq, shRhtEq,
///Incr and Decr operators
   incr, decr,
///Logical and Comparison operators
  lgNot, lgAnd, lgOr, eqEq, nEq, lessThan, greatThan, lessThanEq, greatThanEq,
///Literals
  identifier, string, integer, fractional,
///Reserved keywords
    ifK, elseK, varK, classK, superK, thisK, funK, returnK,
    falseK, trueK, nilK, printK, forK, whileK
};

///This enum maps the Token_e value to the applicable type
///within the variant Token::m_value
enum class Token_val_t {
  None, Bool, Lint, Double, String
};

struct Token {
  std::variant<bool, long long, double, std::string> m_value;
  std::pair<std::size_t, std::size_t>    m_loc;
  Token_e                                m_type;

  inline Token(
      Token_e type, std::size_t ln, std::size_t ps
  );

  inline Token(
      Token_e type, bool value, std::size_t ln, std::size_t ps
  );
  inline Token(
      Token_e type, long long value, std::size_t ln, std::size_t ps
  );
  inline Token(
      Token_e type, double value, std::size_t ln, std::size_t ps
  );
  inline Token(
      Token_e type, std::string_view value, std::size_t ln, std::size_t ps
  );

  using Token_itr = std::vector<lox::Token>::const_iterator;
  inline Token_e operator()() const;
};

///Non-member helper functions. Implemented in scanner.cpp
void toStr(std::string& strm, Token_e token);
Token_val_t toTokenValType(Token::Token_itr loc);
void TokenValToStr(std::string& strm, Token::Token_itr loc);

Token::Token(Token_e type, std::size_t ln, std::size_t ps):
    m_value(),
    m_loc{ln, ps},
    m_type{type}
{

}

Token::Token(Token_e type, bool value, std::size_t ln, std::size_t ps):
    m_value{std::in_place_type<bool>, value},
    m_loc{ln, ps},
    m_type{type}
{

}

Token::Token(Token_e type, long long value, std::size_t ln, std::size_t ps):
    m_value{std::in_place_type<long long>, value},
    m_loc{ln, ps},
    m_type{type}
{

}

Token::Token(Token_e type, double value, std::size_t ln, std::size_t ps):
    m_value{std::in_place_type<double>, value},
    m_loc{ln, ps},
    m_type{type}
{

}

Token::Token(Token_e type, std::string_view value, std::size_t ln, std::size_t ps):
    m_value{std::in_place_type<std::string>, value},
    m_loc{ln, ps},
    m_type{type}
{

}

Token_e Token::operator()() const { return m_type; }

//Implemented in scanner.cpp
void toStr(std::string& strm, Token_e token);

} //end of namespace lox

#endif //LOX_TOKEN_H
