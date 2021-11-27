#include <cassert>
#include "scanner.h"

using Rsvd_identifier = const std::unordered_map<std::string, lox::Token_e>;

Rsvd_identifier lox::Scanner::m_reservedk = {
    {"if", lox::Token_e::ifK},
    {"else", lox::Token_e::elseK},
    {"elseif", lox::Token_e::elseIfK},
    {"var", lox::Token_e::varK},
    {"class", lox::Token_e::classK},
    {"super", lox::Token_e::superK},
    {"this", lox::Token_e::thisK},
    {"fun", lox::Token_e::funK},
    {"return", lox::Token_e::returnK},
    {"false", lox::Token_e::falseK},
    {"true", lox::Token_e::trueK},
    {"nil", lox::Token_e::nilK},
    {"print", lox::Token_e::printK},
    {"for", lox::Token_e::forK},
    {"while", lox::Token_e::whileK}
};

lox::Scanner::Scanner(fs::path const& rdfile):
  m_tokens(),
  m_srcrd{rdfile},
  m_errObsrvd{false}
{

}

bool lox::Scanner::scanSrcFile()
{
  while (!m_srcrd.isEOF()) {
    char ch = m_srcrd.getChar();
    switch (ch) {
      case '(':
        adTk(Token_e::leftParan, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case ')':
        adTk(Token_e::rightParan, m_srcrd.getRow(),  m_srcrd.getCol());
        break;
      case '{':
        adTk(Token_e::leftBrace, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case '}':
        adTk(Token_e::rightBrace, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case ',':
        adTk(Token_e::comma, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case '.':
        adTk(Token_e::dot, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case ';':
        adTk(Token_e::semicolan, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case '+':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::plusEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else if (m_srcrd.isNextChar('+')) {
          adTk(Token_e::incr, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::plus, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '-':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::minusEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else if (m_srcrd.isNextChar('-')) {
          adTk(Token_e::decr, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::minus, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '*':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::mulEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::mul, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '/':
        if (m_srcrd.isNextChar('/')) {
          //Entire line is a comment, skip the line
          m_srcrd.skipCurLn();
        } else if (m_srcrd.isNextChar('*')) {
          //C like multi-line comments. skip until pattern */ is observed
          skipMultiLn();
        } else if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::divEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::div, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '=':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::eqEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::eq, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '!':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::nEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::lgNot, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '&':
        if (m_srcrd.isNextChar('&')) {
          adTk(Token_e::lgAnd, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::andEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::bwAnd, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '|':
        if (m_srcrd.isNextChar('|')) {
          adTk(Token_e::lgOr, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::orEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::bwOr, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '^':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::xorEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::bwXor, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '~':
        adTk(Token_e::bwNot, m_srcrd.getRow(), m_srcrd.getCol());
        break;
      case '%':
        if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::modEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::mod, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '<':
        if (m_srcrd.isNextChar('<')) {
          m_srcrd.advance();
          if (m_srcrd.isNextChar('=')) {
            adTk(Token_e::shLftEq, m_srcrd.getRow(), m_srcrd.getCol());
            m_srcrd.advance();
          } else {
            adTk(Token_e::bwShLft, m_srcrd.getRow(), m_srcrd.getCol());
          }
        } else if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::lessThanEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::lessThan, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case '>':
        if (m_srcrd.isNextChar('>')) {
          m_srcrd.advance();
          if (m_srcrd.isNextChar('=')) {
            adTk(Token_e::shRhtEq, m_srcrd.getRow(), m_srcrd.getCol());
            m_srcrd.advance();
          } else {
            adTk(Token_e::bwtShRht, m_srcrd.getRow(), m_srcrd.getCol());
          }
        } else if (m_srcrd.isNextChar('=')) {
          adTk(Token_e::greatThanEq, m_srcrd.getRow(), m_srcrd.getCol());
          m_srcrd.advance();
        } else {
          adTk(Token_e::greatThan, m_srcrd.getRow(), m_srcrd.getCol());
        }
        break;
      case ' ':
      case '\r':
      case '\t':
        //ignore white-space
        break;
      case '"':
      {
        std::string s;
        auto row = m_srcrd.getRow();
        auto col = m_srcrd.getCol();
        while (!m_srcrd.isEOF() && !m_srcrd.isNextChar('"'))
          s.push_back(m_srcrd.getChar());
        if (m_srcrd.isEOF()) {
          m_errObsrvd = true;
          adTk(
              Token_e::error,
              "end terminal for string '\"' not found",
              row,
              col
          );
        } else {
          adTk(Token_e::string, s, row, col);
          m_srcrd.advance();
        }
        break;
      }
      default:
        if (isDigit(ch))
          procNumber(ch);
        else
          procIdentifier(ch);
        break;
    }
  }
  return m_errObsrvd;
}

void lox::Scanner::adTk(
    Token_e token,
    std::size_t row,
    std::size_t col)
{
  m_tokens.emplace_back(token, row, col);
}

void lox::Scanner::adTk(
    Token_e token,
    std::string_view data,
    std::size_t row,
    std::size_t col)
{
  m_tokens.emplace_back(token, data, row, col);
}

void lox::Scanner::adTk(
    double value,
    std::size_t row,
    std::size_t col)
{
  m_tokens.emplace_back(Token_e::fractional, value, row, col);
}

void lox::Scanner::adTk(
    long long value,
    std::size_t row,
    std::size_t col)
{
  m_tokens.emplace_back(Token_e::integer, value, row, col);
}

void lox::Scanner::adTk(
    Token_e token,
    bool v,
    std::size_t row,
    std::size_t col)
{
  m_tokens.emplace_back(token, v, row, col);
}

void lox::Scanner::skipMultiLn()
{
  while (true) {
    m_srcrd.advance();
    m_srcrd.skipUntil('*');
    if (m_srcrd.isEOF() || m_srcrd.isNextChar('/')) {
      m_srcrd.advance();
      break;
    }
  }
}

void lox::Scanner::procNumber(char ch)
{
  std::string s{ch};
  auto row = m_srcrd.getRow();
  auto col = m_srcrd.getCol();
  bool isDot{false};
  auto pred = [this](char ch) { return this->isDigit(ch) ||
                                       this->isDecimal(ch);
  };
  while (!m_srcrd.isEOF() && m_srcrd.isNextChar(pred)) {
    auto c = m_srcrd.getChar();
    if ( isDecimal(c))
      isDot = true;
    s.push_back(c);
  }

  if (m_srcrd.isEOF()) {
    m_errObsrvd = true;
    adTk(Token_e::error, "end of statement ';' not found", row, col);
  } else if (isDot) {
    adTk(std::stod(s), row, col);
  } else {
    adTk(std::stoll(s), row, col);
  }
}

void lox::Scanner::procIdentifier(char ch)
{
  std::string s{ch};
  auto row = m_srcrd.getRow();
  auto col = m_srcrd.getCol();
  auto pred = [this](char ch) { return this->isDigit(ch) ||
                                       this->isAlpha(ch);
  };
  while (!m_srcrd.isEOF() && m_srcrd.isNextChar(pred)) {
    s.push_back(m_srcrd.getChar());
  }
  if (auto itr = m_reservedk.find(s); itr != m_reservedk.cend()) {
    if (itr->second == Token_e::trueK)
      adTk(Token_e::trueK, true, row, col);
    else if (itr->second == Token_e::falseK)
      adTk(Token_e::falseK, false, row, col);
    else
      adTk(itr->second, row, col);
  } else {
    adTk(Token_e::identifier, s, row, col);
  }
}

bool lox::Scanner::isDigit(char ch) const { return ch >= '0' && ch <= '9'; }
bool lox::Scanner::isDecimal(char ch) const { return ch == '.'; }
bool lox::Scanner::isAlpha(char ch) const
{
  return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
}
lox::VecWrapper<lox::Token, const int> lox::Scanner::get_tokens() const
{
  return VecWrapper<Token, const int>(m_tokens);
}
void lox::toStr(std::string& strm, Token_e token)
{
  switch (token) {
    case Token_e::leftParan:  strm += "( ";          break;
    case Token_e::rightParan: strm += ") ";          break;
    case Token_e::leftBrace:  strm += "{ ";          break;
    case Token_e::rightBrace: strm += "} ";          break;
    case Token_e::comma:      strm += ", ";          break;
    case Token_e::dot:        strm += ". ";          break;
    case Token_e::semicolan:  strm += "; ";          break;
    case Token_e::plus:       strm += "+ ";          break;
    case Token_e::minus:      strm += "- ";          break;
    case Token_e::mul:        strm += "* ";          break;
    case Token_e::div:        strm += "/ ";          break;
    case Token_e::mod:        strm += "% ";          break;
    case Token_e::bwNot:      strm += "~ ";          break;
    case Token_e::bwAnd:      strm += "& ";          break;
    case Token_e::bwOr:       strm += "| ";          break;
    case Token_e::bwXor:      strm += "^ ";          break;
    case Token_e::bwShLft:    strm += "+= ";         break;
    case Token_e::bwtShRht:   strm += ">> ";         break;
    case Token_e::eq:         strm += "= ";          break;
    case Token_e::plusEq:     strm += "+= ";         break;
    case Token_e::minusEq:    strm += "-= ";         break;
    case Token_e::mulEq:      strm += "*= ";         break;
    case Token_e::divEq:      strm += "/= ";         break;
    case Token_e::modEq:      strm += "%= ";         break;
    case Token_e::andEq:      strm += "&= ";         break;
    case Token_e::orEq:       strm += "|= ";         break;
    case Token_e::xorEq:      strm += "^= ";         break;
    case Token_e::shLftEq:    strm += "+== ";        break;
    case Token_e::shRhtEq:    strm += ">>= ";        break;
    case Token_e::incr:       strm += "++ ";         break;
    case Token_e::decr:       strm += "-- ";         break;
    case Token_e::lgNot:      strm += "! ";          break;
    case Token_e::lgAnd:      strm += "&& ";         break;
    case Token_e::lgOr:       strm += "|| ";         break;
    case Token_e::eqEq:       strm += "== ";         break;
    case Token_e::nEq:        strm += "!= ";         break;
    case Token_e::lessThan:   strm += "< ";          break;
    case Token_e::greatThan:  strm += "> ";          break;
    case Token_e::lessThanEq: strm += "<= ";         break;
    case Token_e::greatThanEq:strm += ">= ";         break;
    case Token_e::identifier: strm += "identifier "; break;
    case Token_e::string:     strm += "string ";     break;
    case Token_e::integer:    strm += "integer ";    break;
    case Token_e::fractional: strm += "real ";       break;
    case Token_e::ifK:        strm += "if ";         break;
    case Token_e::elseK:      strm += "else ";       break;
    case Token_e::elseIfK:    strm += "elif ";       break;
    case Token_e::varK:       strm += "var ";        break;
    case Token_e::funK:       strm += "fun ";        break;
    case Token_e::classK:     strm += "class ";      break;
    case Token_e::superK:     strm += "this ";       break;
    case Token_e::thisK:      strm += "fun ";        break;
    case Token_e::returnK:    strm += "return ";     break;
    case Token_e::trueK:      strm += "true ";       break;
    case Token_e::falseK:     strm += "flase ";      break;
    case Token_e::nilK:       strm += "nil ";        break;
    case Token_e::printK:     strm += "print ";      break;
    case Token_e::forK:       strm += "for ";        break;
    case Token_e::whileK:     strm += "while ";      break;
    default:                  strm += "error ";      break;
  }
}

lox::Token_val_t lox::toTokenValType(Token_itr loc)
{
  auto tkn = loc->m_type;
  if (tkn == Token_e::error || tkn == Token_e::string ||
      tkn == Token_e::identifier)
    return Token_val_t::String;
  else if (tkn == Token_e::falseK || tkn == Token_e::trueK)
    return Token_val_t::Bool;
  else if (tkn == Token_e::integer)
    return Token_val_t::Lint;
  else if (tkn == Token_e::fractional)
    return Token_val_t::Double;
  return Token_val_t::None;
}

void lox::TokenValToStr(std::string& strm, Token_itr loc)
{
  auto tkn_t = toTokenValType(loc);
  if (tkn_t == Token_val_t::String)
    strm += std::get<std::string>(loc->m_value);
  else if (tkn_t == Token_val_t::Double)
    strm += std::to_string(std::get<double>(loc->m_value));
  else if (tkn_t == Token_val_t::Lint)
    strm += std::to_string(std::get<long long>(loc->m_value));
  else if (tkn_t == Token_val_t::Bool)
    strm += std::to_string(std::get<bool>(loc->m_value));
  else
    assert(0); //Must not enter this loop
}
