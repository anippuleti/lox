#include <iostream>
#include "scanner.h"
#include "errorHandler.h"
#include "parser.h"
#include "interpreter.h"

int main(int argc, char** argv)
{
  if (argc != 2) {
    std::cout << "ERROR: Lox source file nor provided. Example > lox sample.lox"
              << std::endl;
    return 1;
  }

  lox::Scanner      m_scanner(argv[1]);
  lox::ErrorHandler m_errorhdl(m_scanner.get_tokens());
  auto tknStgErrExists = m_scanner.scanSrcFile();
  if (tknStgErrExists) {
    m_errorhdl.printTokenStageErrMsgs();
    std::cout << "<source>: error: failed to tokenize"
                 " source code due to above errors" << std::endl;
    return 0;
  }
/*
  lox::Parser m_parser{m_errorhdl};
  auto psrStgErrExists = m_parser.gen_ast(m_scanner.get_tokens());
  if (psrStgErrExists) {
    m_errorhdl.printErrMsgs();
    std::cout << "<source>: error: failed to parse"
                 " source code due to above errors" << std::endl;
    return 0;
  }

  lox::Interpreter m_interpreter(m_errorhdl);
  auto iprStgErrExists = m_interpreter.evaluate(m_parser.get_ast());
  if (iprStgErrExists) {
    m_errorhdl.printErrMsgs();
    std::cout << "<source>: error: failed to execute"
                 " source code due to above runtime errors" << std::endl;
    return 0;
  }
*/
  return 0;
}
