#include "errorHandler.h"
#include <iostream>

lox::ErrorHandler::ErrorHandler(
    VecWrapper<Token, const int> const& tokens): m_tokens(tokens)
{

}

lox::ErrorHandler::~ErrorHandler() noexcept = default;


void lox::ErrorHandler::printErrMsgs()
{
  for (auto const& msg : m_errMsgs)
    std::cout << msg << std::endl;
}

void lox::ErrorHandler::printTokenStageErrMsgs()
{
  for (auto const& tokens : m_tokens) {
    if (tokens() == Token_e::error)
      std::cout << "File: " << tokens.m_loc.first << ':' << tokens.m_loc.second
                 << " error: " << std::get<std::string>(tokens.m_value) << std::endl;
  }
}

