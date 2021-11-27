#ifndef LOX_USRTRAITS_H
#define LOX_USRTRAITS_H

#include <type_traits>

namespace lox::traits {

template<typename T>
struct remove_cvref {
  using type = typename std::remove_cv_t<std::remove_reference_t<T>>;
};

template<typename T>
using remove_cvref_t = typename remove_cvref<T>::type;

template<bool Condition>
struct isValid {
  inline static constexpr auto value = Condition;
};

template<>
struct isValid<false> {
  inline static constexpr auto value = false;
};

template<class> inline constexpr bool always_false_v = false;

} //end of namespace lox::traits

#endif //LOX_USRTRAITS_H
