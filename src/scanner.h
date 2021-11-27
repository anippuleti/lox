#ifndef LOX_SCANNER_H
#define LOX_SCANNER_H

#include <vector>
#include <unordered_map>
#include "token.h"
#include "srcReader.h"
#include "vecWrapper.h"

namespace lox {
namespace fs = std::filesystem;

class Scanner {
 private:
  static const std::unordered_map<std::string, Token_e> m_reservedk;
  std::vector<Token> m_tokens;
  SrcReader          m_srcrd;
  bool               m_errObsrvd;

 public:
  explicit Scanner(fs::path const& rdfile);
  [[nodiscard]] bool scanSrcFile();
  [[nodiscard]] VecWrapper<Token, const int> get_tokens() const;

 private:
  void adTk(
      Token_e token,
      std::size_t row,
      std::size_t col);
  void adTk(
      Token_e token,
      std::string_view data,
      std::size_t row,
      std::size_t col);
  void adTk(
      Token_e token,
      bool v,
      std::size_t row,
      std::size_t col);
  void adTk(
      double value,
      std::size_t row,
      std::size_t col);
  void adTk(
      long long value,
      std::size_t row,
      std::size_t col);
  void skipMultiLn();
  void procNumber(char ch);
  void procIdentifier(char ch);

  bool isDigit(char ch) const;
  bool isDecimal(char ch) const;
  bool isAlpha(char ch) const;
};

} //end of namespace lox

#endif //LOX_SCANNER_H
