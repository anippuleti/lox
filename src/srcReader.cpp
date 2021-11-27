#include "srcReader.h"

lox::SrcReader::SrcReader(fs::path const& rfile):
    m_rdStrm(rfile),
    m_rdline(),
    m_pos(m_rdline.cend()),
    m_line_pos{0, 0},
    m_lnGapCnt{0},
    m_isEOF{false}
{
  if (fs::is_empty(rfile)) {
    m_isEOF = true;
  } else {
    readLine();
  }
}

void lox::SrcReader::readLine()
{
  m_lnGapCnt = 0;
  do {
    if (!std::getline(m_rdStrm, m_rdline)) {
      m_isEOF = true;
      return;
    }
    ++m_lnGapCnt;
  } while (m_rdline.empty());
  m_pos = m_rdline.cbegin();
}

char lox::SrcReader::getChar()
{
  if (m_pos == m_rdline.cbegin())
    incrLnCnt();
  char ch = *m_pos++;
  ++m_line_pos.second;
  if (m_pos == m_rdline.cend())
    readLine();
  return ch;
}

void lox::SrcReader::advance()
{
  ++m_pos;
  if (m_pos == m_rdline.cend())
    readLine();
}

void lox::SrcReader::skipUntil(char ch)
{
  while (!isEOF() && (getChar() != ch)) { /* do nothing */  }
}

void lox::SrcReader::incrLnCnt()
{
  m_line_pos.first += m_lnGapCnt;
  m_line_pos.second = 0;
}

void lox::SrcReader::skipCurLn() { readLine(); }
bool lox::SrcReader::isEOF() const          { return m_isEOF;           }
std::size_t lox::SrcReader::getRow() const { return m_line_pos.first;  }
std::size_t lox::SrcReader::getCol() const  { return m_line_pos.second; }
bool lox::SrcReader::isNextChar(char ch) const
{
  return (!isEOF()) && (*m_pos == ch);
}
