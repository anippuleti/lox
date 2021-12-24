#ifndef LOX_ERRORHANDLER_H
#define LOX_ERRORHANDLER_H

#include <string_view>
#include <string>
#include "token.h"
#include "vecWrapper.h"

namespace lox {

class ErrorHandler {
  VecWrapper<Token, const int> m_tokens;
  std::vector<std::string> m_errMsgs;

 public:
  using Titr = decltype(m_tokens)::CItr;

  //There must exist only one instance of this class.
  //Hence, explicitly deleting copy and move
  //constructors and assignment methods
  explicit ErrorHandler(VecWrapper<Token, const int> const& tokens);
  ErrorHandler(ErrorHandler const& )                = delete;
  ErrorHandler& operator=(ErrorHandler const& )     = delete;
  ErrorHandler(ErrorHandler&& ) noexcept            = delete;
  ErrorHandler& operator=(ErrorHandler&& ) noexcept = delete;
  ~ErrorHandler() noexcept;

  ///First recordErrMsg() overload must be invoked only from Scanner
  ///Since tokens are not yet available
  template<class InvokeCB>
  void recordErrMsg(InvokeCB&& cb);

  template<class InvokeCB>
  void recordErrMsg(InvokeCB&& cb, Titr tokenLoc);

  void printErrMsgs();
  void printTokenStageErrMsgs();
};

template<class InvokeCB>
void ErrorHandler::recordErrMsg(InvokeCB&& cb)
{
  auto& loc = m_errMsgs.emplace_back();
  cb(loc);
}

template<class InvokeCB>
void ErrorHandler::recordErrMsg(InvokeCB&& cb, Titr tokenLoc)
{
  auto& loc = m_errMsgs.emplace_back();
  loc  = "<source>:";
  loc += std::to_string(tokenLoc->m_loc.first) + ':';
  loc += std::to_string(tokenLoc->m_loc.second);
  loc += " error: ";
  cb(loc);
}

struct ErrorWrapper {
  std::reference_wrapper<ErrorHandler> m_err;
  lox::Token_itr m_loc;

  inline explicit ErrorWrapper(ErrorHandler& err, Token_itr loc);
  template<typename InvokeCB>
  void recordErrMsg(InvokeCB&& cb);
};

ErrorWrapper::ErrorWrapper(ErrorHandler& err, Token_itr loc): m_err{err}, m_loc{loc} { }
template<typename InvokeCB>
void ErrorWrapper::recordErrMsg(InvokeCB&& cb)
{
  m_err.get().recordErrMsg(std::forward<InvokeCB>(cb));
}

} //end of namespace

#endif //LOX_ERRORHANDLER_H
