#ifndef LOX_SRCREADER_H
#define LOX_SRCREADER_H

#include <string>
#include <filesystem>
#include <fstream>

namespace lox {
namespace fs = std::filesystem;

template<typename T>
using isClassType = std::enable_if_t<
    std::is_class_v<typename std::remove_reference_t<T>>, T>;

class SrcReader {
 private:
  std::ifstream m_rdStrm;
  std::string m_rdline;
  std::string::const_iterator m_pos;
  std::pair<std::size_t, std::size_t> m_line_pos;
  std::size_t m_lnGapCnt; //Temporary var, stores cur new line
  bool        m_isEOF;
 public:
  explicit SrcReader(fs::path const &rfile);

  [[nodiscard]] bool isEOF() const;
  [[nodiscard]] std::size_t getRow() const;
  [[nodiscard]] std::size_t getCol() const;
  [[nodiscard]] bool isNextChar(char ch) const;
  template<typename Predicate, typename = isClassType<Predicate>>
  [[nodiscard]] bool isNextChar(Predicate&& pred)
  {
    return !isEOF() && pred(*m_pos);
  }

  char getChar();
  void advance();
  void skipCurLn();
  void skipUntil(char ch);

 private:
  void readLine();
  void incrLnCnt();

};

} //end of namespace

#endif //LOX_SRCREADER_H
