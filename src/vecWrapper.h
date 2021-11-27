#ifndef LOX_VECWRAPPER_H
#define LOX_VECWRAPPER_H

#include <vector>
#include <functional>
#include "usrTraits.h"
#include "token.h"

namespace lox {
using Token_itr = std::vector<lox::Token>::const_iterator;

///Class Template Specialization is used to determine if
///reference_wrapper stores a reference to const Vector
///container of T  or non-const Vector container of T
template<typename T>
using isConstInvoked = std::is_const<
    std::remove_pointer_t<std::remove_reference_t<T>>>;

template<typename T, typename objType,
         bool isConst = isConstInvoked<objType>::value
        >
class VecWrapper {
 private:
  std::reference_wrapper<std::vector<T> const> m_ref;

 public:
  using type = T;
  using CItr = typename std::vector<type>::const_iterator;

  explicit VecWrapper(std::vector<T> const& container): m_ref(container) { }
  CItr begin() const noexcept   { return m_ref.get().cbegin();  }
  CItr end() const noexcept     { return m_ref.get().cend();    }
};

template<typename T, typename objType>
class VecWrapper<T, objType, false> {
 private:
  std::reference_wrapper<std::vector<T>> m_ref;

 public:
  using type = T;
  using Itr = typename std::vector<type>::iterator;

  explicit VecWrapper(std::vector<T>& container): m_ref(container) { }
  Itr begin() noexcept { return m_ref.get().begin();  }
  Itr end()   noexcept { return m_ref.get().end();    }
};

class TokenRange {
  Token_itr cur_ptr;
  Token_itr end_ptr;
 public:
  inline TokenRange(
      Token_itr st,
      Token_itr ed):
      cur_ptr{st},
      end_ptr{ed}
  {

  }

  inline TokenRange& operator++()
  {
    ++cur_ptr;
    return *this;
  }

  inline TokenRange  operator++(int )
  {
    TokenRange prev(cur_ptr, end_ptr);
    cur_ptr++;
    return prev;
  }

  inline Token const& operator*()  const { return *cur_ptr; }
  inline Token const* operator->() const { return &(*cur_ptr); }
  [[nodiscard]] inline bool isEnd() const noexcept {return cur_ptr == end_ptr;}
  [[nodiscard]] inline Token_itr cur_loc() const noexcept { return cur_ptr; }
  [[nodiscard]] inline Token_itr end() const noexcept { return end_ptr; }
  [[nodiscard]] inline Token_itr next() const noexcept
  {
    return std::next(cur_ptr);
  }

  [[nodiscard]] inline Token_itr prev() const noexcept
  {
    return std::prev(cur_ptr);
  }
};

} //end of namespace lox

#endif //LOX_VECWRAPPER_H
