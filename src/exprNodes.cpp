#include <stdexcept>
#include <cmath>
#include <cassert>
#include <type_traits>
#include "exprNodes.h"
#include "varEnv.h"
#include "errorHandler.h"
#include "usrTraits.h"

//////////////////////////////////////////////////////////////////////////////
namespace expr {
//////////////////////////////////////////////////////////////////////////////

///Template specializations for dispatching operations
template<typename T>
using remove_cvref_t = lox::traits::remove_cvref_t<T>;

template<typename T>
using isDouble = std::is_floating_point<remove_cvref_t<T>>;

template<typename T>
using isInteger = std::is_same<remove_cvref_t<T>, lox::llint>;

template<typename T>
using isBool = std::is_same<remove_cvref_t<T>, bool>;

template<typename T>
using isString = std::is_same<remove_cvref_t<T>, std::string>;

template<typename T>
using isStringView = std::is_same<remove_cvref_t<T>, lox::StrLiteral>;

template<typename T>
using isConvToStr = lox::traits::isValid<
    isString<T>::value || isStringView<T>::value
>;

template<typename T>
using isLoxVarNil = lox::traits::isValid<std::is_same_v<remove_cvref_t<T>, lox::NilValue>>;

template<typename T>
using isLoxErr = lox::traits::isValid<std::is_same_v<remove_cvref_t<T>, lox::ErrorReported>>;

template<typename T>
using isLoxNonStrLtrl = lox::traits::isValid<
    isInteger<T>::value || isDouble<T>::value || isBool<T>::value || isLoxVarNil<T>::value
>;

template<typename T>
using isNumber = lox::traits::isValid<isDouble<T>::value || isInteger<T>::value>;

template<typename T>
using isLoxLiteral [[maybe_unused]] = lox::traits::isValid<
    isBool<T>::value   || isInteger<T>::value || isDouble<T>::value ||
    isString<T>::value || isStringView<T>::value || isLoxVarNil<T>::value
>;

template<typename T, typename U>
using isFloatingOp = std::enable_if<
     (isDouble<T>::value  && isDouble<U>::value) ||
     (isInteger<T>::value && isDouble<U>::value) ||
     (isDouble<T>::value  && isInteger<U>::value),
     bool
>;

template<typename T, typename U>
using isLoxVarNilOp = std::enable_if<
   (isLoxVarNil<T>::value && !isLoxErr<U>::value) || (!isLoxErr<T>::value && isLoxVarNil<U>::value),
    bool
>;

template<typename T, typename U>
using isIllegalBoolOp = lox::traits::isValid<
    ((isBool<T>::value && (isInteger<U>::value || isDouble<U>::value)) ||
     (isBool<U>::value && (isInteger<T>::value || isDouble<T>::value)))
>;

template<typename T, typename U>
using isFloatingOp_t = typename isFloatingOp<T, U>::type;

template<typename T, typename U>
using isLoxVarNilOp_t = typename isLoxVarNilOp<T, U>::type;

template<typename T, typename U>
using isBothInteger = std::enable_if<
    isInteger<T>::value && isInteger<U>::value,
    bool
>;

template<typename T, typename U>
using isBothInteger_t [[maybe_unused]] = typename isBothInteger<T, U>::type;

template<typename T, typename U>
using isBothBool = std::enable_if<
    isBool<T>::value && isBool<U>::value,
    bool
>;

template<typename T, typename U>
using isBothBool_t [[maybe_unused]] = typename isBothBool<T, U>::type;

template<typename T, typename U>
using isStringOp = std::enable_if<
    (((isConvToStr<T>::value     && isLoxNonStrLtrl<U>::value) ||
      (isLoxNonStrLtrl<T>::value && isConvToStr<U>::value)     ||
      (isConvToStr<T>::value     && isConvToStr<U>::value)
     ) && (
      !(isLoxVarNil<T>::value    || isLoxVarNil<U>::value ||
        isLoxErr<T>::value       || isLoxErr<U>::value)
     )),
    bool
>;

template<typename T, typename U>
using isStringOp_t = typename isStringOp<T, U>::type;

template<typename T, typename U>
using isEitherErrOrNil = std::enable_if<
    (isLoxErr<T>::value    || isLoxErr<U>::value     ||
     isLoxVarNil<T>::value || isLoxVarNil<U>::value),
     bool
>;

template<typename T, typename U>
using isEitherErrOrNil_t [[maybe_unused]] = typename isEitherErrOrNil<T, U>::type;

///Yields type bool if condition is true
template<typename T, typename U>
using isBothNumber [[maybe_unused]] = lox::traits::isValid<
    isNumber<T>::value  && isNumber<U>::value
>;

template<typename T, typename U>
using isIllegalBinOp [[maybe_unused]] = lox::traits::isValid<
    isIllegalBoolOp<T, U>::value            ||
    std::is_same_v<T, lox::Assignment> ||
    std::is_same_v<U, lox::Assignment>
>;

template<typename T, typename U>
using isLoxVarOp = lox::traits::isValid<
    std::is_same_v<T, lox::VarIdentifier> &&
    std::is_same_v<U, lox::VarIdentifier>
>;

template<typename T, typename U>
using isLoxErrRptd [[maybe_unused]] = lox::traits::isValid<
    std::is_same_v<T, lox::ErrorReported> ||
    std::is_same_v<U, lox::ErrorReported>
>;

template<typename T, typename U>
using isLoxVarNotFound [[maybe_unused]] = lox::traits::isValid<
    std::is_same_v<T, lox::ErrorReported> ||
    std::is_same_v<U, lox::ErrorReported>
>;

template<typename T, typename U>
using isStringType = lox::traits::isValid<
    isString<T>::value || isString<U>::value ||
    isStringView<T>::value || isStringView<U>::value
>;

template<typename T, typename U>
using isDouble_t = std::conditional_t<
    isDouble<T>::value || isDouble<U>::value,
    double,
    lox::llint
>;

template<typename T, typename U>
using isPlusEqRet = std::conditional<
    isStringType<T, U>::value,
    std::string,
    isDouble_t<T, U>
>;

template<typename T, typename U>
using isPlusEqRet_t [[maybe_unused]] = typename isPlusEqRet<T, U>::type;

//////////////////////////////////////////////////////////////////////////////
///non-member helper functions
//////////////////////////////////////////////////////////////////////////////
template<typename T, typename U>
[[nodiscard]] auto add(T&& a, U&& b) -> decltype(a + b) { return a + b; }

template<typename T, typename U>
[[nodiscard]] auto sub(T&& a, U&& b) -> decltype(a - b) { return a - b; }

template<typename T, typename U>
[[nodiscard]] auto mul(T&& a, U&& b) -> decltype(a * b) { return a * b; }

template<typename T, typename U>
[[nodiscard]] auto mod(T&& a, U&& b) -> decltype(a % b) { return a % b; }

template<typename T, typename U>
[[nodiscard]] auto bwAnd(T&& a, U&& b) -> decltype(a & b) { return a & b; }

template<typename T, typename U>
[[nodiscard]] auto bwOr(T&& a, U&& b) -> decltype(a | b) { return a | b; }

template<typename T, typename U>
[[nodiscard]] auto bwXor(T&& a, U&& b) -> decltype(a ^ b) { return a ^ b; }

template<typename T, typename U>
[[nodiscard]] auto bwShLft(T&& a, U&& b) ->decltype(a << b) { return a << b; }

template<typename T, typename U>
[[nodiscard]] auto bwShRht(T&& a, U&& b) ->decltype(a >> b) { return a >> b; }

template<typename T, typename U>
[[nodiscard]] auto lgAnd(T&& a, U&& b) -> decltype(a && b) { return a && b; }

template<typename T, typename U>
[[nodiscard]] auto lgOr(T&& a, U&& b) -> decltype(a || b) { return a || b; }

template<typename T, typename U>
[[nodiscard]] auto eqEq(T&& a, U&& b) -> decltype(a == b) { return a == b; }

template<typename T, typename U>
[[nodiscard]] auto nEq(T&& a, U&& b) -> decltype(a != b)
{
  return !eqEq(std::forward<T>(a), std::forward<U>(b));
}

template<typename T, typename U>
[[nodiscard]] auto lessThan(T&& a, U&& b) -> decltype(a < b) { return a < b; }

template<typename T, typename U>
[[nodiscard]] auto lessThanEq(T&& a, U&& b) -> decltype(a <= b)
{
  return lessThan(std::forward<T>(a), std::forward<U>(b)) ||
         eqEq(std::forward<T>(a), std::forward<U>(b));
}

template<typename T, typename U>
[[nodiscard]] auto greatThan(T&& a, U&& b) -> decltype(a > b)
{
  return !lessThan(std::forward<T>(a), std::forward<U>(b));
}

template<typename T, typename U>
[[nodiscard]] auto greatThanEq(T&& a, U&& b) -> decltype(a > b)
{
  return !lessThan(std::forward<T>(a), std::forward<U>(b)) ||
         eqEq(std::forward<T>(a), std::forward<U>(b));
}

[[nodiscard]] auto errAccNilValue(
    lox::ErrorWrapper err_env,
    lox::Token_e      token) -> lox::ErrorReported
{
  err_env.recordErrMsg(
      [token] (auto& msg) {
        msg += "accessing uninitialized variable for operator ";
        lox::toStr(msg, token);
      }
  );
  return lox::ErrorReported{};
}

template<typename T, typename U>
[[nodiscard]] auto errIllegalOp(
    lox::ErrorWrapper   err_hdl,
    lox::Token_e        token) -> lox::ErrorReported
{
  using Type1 = std::decay_t<T>;
  using Type2 = std::decay_t<U>;
  constexpr auto lhs_op = []() {
    if constexpr (std::is_same_v<Type1, bool>)
      return "operand types are bool and ";
    else if constexpr (std::is_same_v<Type1, lox::llint>)
      return "operand types are int and ";
    else if constexpr (std::is_same_v<Type1, double>)
      return "operand types are double and ";
    else if constexpr (std::is_same_v<Type1, std::string> ||
                       std::is_same_v<Type1, lox::StrLiteral>)
      return "operand types are string and ";
    else if constexpr (std::is_same_v<Type1, lox::Assignment>)
      return "illegal binary operation on assignment and ";
    else if constexpr (std::is_same_v<Type1, lox::NilValue>)
      return "illegal binary operation on uninitialized value and ";
    else if constexpr (std::is_same_v<Type1, lox::VarIdentifier>)
      return "illegal binary operation on variable declaration and ";
    else {
      static_assert("ErrorReported must be supported by caller");
      return "";
    }
  };

  constexpr auto rhs_op = []() {
    if constexpr (std::is_same_v<Type2, bool>)
      return "bool ";
    else if constexpr (std::is_same_v<Type2, lox::llint>)
      return "int ";
    else if constexpr (std::is_same_v<Type2, double>)
      return "double ";
    else if constexpr (std::is_same_v<Type2, std::string> ||
        std::is_same_v<Type2, lox::StrLiteral>)
      return "string ";
    else if constexpr (std::is_same_v<Type2, lox::Assignment>)
      return "assignment";
    else if constexpr (std::is_same_v<Type1, lox::NilValue>)
      return "uninitialized value";
    else if constexpr (std::is_same_v<Type2, lox::VarIdentifier>)
      return "variable declaration";
    else {
      static_assert("ErrorReported must be supported by caller");
      return "";
    }
  };

  err_hdl.recordErrMsg(
      [token, lhs_op, rhs_op] (auto& msg) {
        msg += "no match for operator ";
        lox::toStr(msg, token);
        msg += lhs_op();
        msg += rhs_op();
      }
  );
  return lox::ErrorReported{};
}

template<typename T>
[[nodiscard]] auto errIllegalOp(
    lox::ErrorWrapper   err_hdl,
    lox::Token_e        token) -> lox::ErrorReported
{
  using Type = std::decay_t<T>;
  constexpr auto op = []() {
    if constexpr (std::is_same_v<Type, bool>)
      return "use of an operand of type bool with operator ";
    else if constexpr (std::is_same_v<Type, lox::llint>)
      return "use of an operand of type int with operator ";
    else if constexpr (std::is_same_v<Type, double>)
      return "use of an operand of type double with operator ";
    else if constexpr (std::is_same_v<Type, std::string> ||
        std::is_same_v<Type, lox::StrLiteral>)
      return "use of an operand of type string with operator ";
    else if constexpr (std::is_same_v<Type, lox::Assignment>)
      return "use of an assignment expression within this context ";
    else if constexpr (std::is_same_v<Type, lox::VarIdentifier>)
      return "use of an variable declaration within this context ";
    static_assert("ErrorReported must be supported by caller");
    return "";
  };

  err_hdl.recordErrMsg(
      [token, op] (auto& msg) {
        msg += op();
        lox::toStr(msg, token);
        msg += "is forbidden";
      }
  );
  return lox::ErrorReported{};
}

[[nodiscard]] auto errIllegalAssignOp(
    lox::ErrorWrapper   err_hdl,
    lox::Token_e        token) -> lox::ErrorReported
{
  err_hdl.recordErrMsg(
      [token] (auto& msg) {
        msg += "lhs of assignment operator ";
        lox::toStr(msg, token);
        msg += "must be a pre-defined variable";
      }
  );
  return lox::ErrorReported{};
}

template<typename T, typename U>
[[nodiscard]] auto div(
    T&& a,
    U&& b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  if (b == 0) {
    err_hdl.recordErrMsg(
        [] (auto& msg) { msg += "trying to divide by 0"; }
    );
    return lox::ErrorReported{};
  }

  auto res = lox::exp::DivOpRes(a, b);
  if (res.isFloatingOp) {
    return res.decimal;
  } else {
    if (res.fractional == 0)
      return static_cast<lox::llint>(res.decimal);
    else
      return res.decimal + res.fractional;
  }
}


template<typename T, typename U,
    isStringOp_t<T, U> = true>
[[nodiscard]] auto operator_dispatch(
    T&& a,
    lox::Token_e oper,
    U&& b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  using namespace std;
  using namespace lox::traits;
  if (oper == Token_e::plus) {
    if constexpr (isString<T>::value && isString<U>::value)
      return a + b;

    if constexpr (isString<T>::value && isStringView<U>::value)
      return a + std::string{b.sview.begin(), b.sview.end()};

    if constexpr (isString<U>::value && isStringView<T>::value)
      return std::string{a.sview.begin(), a.sview.end()} + b;

    if constexpr (isStringView<T>::value && isStringView<U>::value)
      return std::string{a.sview.begin(), a.sview.end()} +
             std::string{b.sview.begin(), b.sview.end()};

    if constexpr (isString<T>::value && isLoxNonStrLtrl<U>::value)
      return a + std::to_string(b);

    if constexpr (isString<U>::value && isLoxNonStrLtrl<T>::value)
      return std::to_string(a) + b;

    if constexpr (isStringView<T>::value && isLoxNonStrLtrl<U>::value)
      return std::string{a.sview.begin(), a.sview.end()} + std::to_string(b);

    if constexpr (isStringView<U>::value && isLoxNonStrLtrl<T>::value)
      return std::to_string(a) + std::string{b.sview.begin(), b.sview.end()};

  } else if (oper == Token_e::eq) {
    if constexpr (isStringView<U>::value) {
      return std::string{b.sview.begin(), b.sview.end()};
    } else {
      return b;
    }
  }
  return errIllegalOp<T, U>(err_hdl, oper);
}

[[nodiscard]] auto operator_dispatch(
    bool a,
    lox::Token_e oper,
    bool b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::lgAnd:
      return lgAnd(a, b);
    case Token_e::lgOr:
      return lgOr(a, b);
    case Token_e::eqEq:
      return eqEq(a, b);
    case Token_e::nEq:
      return nEq(a, b);
    case Token_e::bwAnd:
      return bwAnd(a, b);
    case Token_e::bwOr:
      return bwOr(a, b);
    case Token_e::bwXor:
      return bwXor(a, b);
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<bool, bool>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    bool a,
    lox::Token_e oper,
    lox::llint   b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::lgAnd:
      return lgAnd(a, b);
    case Token_e::lgOr:
      return lgOr(a, b);
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<bool, lox::llint>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::llint a,
    lox::Token_e oper,
    bool   b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::lgAnd:
      return lgAnd(a, b);
    case Token_e::lgOr:
      return lgOr(a, b);
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<lox::llint, bool>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    bool a,
    lox::Token_e oper,
    double b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::lgAnd:
      return lgAnd(a, b);
    case Token_e::lgOr:
      return lgOr(a, b);
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<bool, double>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    double a,
    lox::Token_e oper,
    bool   b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::lgAnd:
      return lgAnd(a, b);
    case Token_e::lgOr:
      return lgOr(a, b);
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<double, bool>(err_hdl, oper);
  }
}

template<typename T, typename U,
    isFloatingOp_t<T, U> = true>
[[nodiscard]] auto operator_dispatch(
    T&& a,
    lox::Token_e opr,
    U&& b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
using namespace lox;
  switch (opr) {
    case Token_e::plus:
      return add(std::forward<T>(a), std::forward<U>(b));
    case Token_e::minus:
      return sub(std::forward<T>(a), std::forward<U>(b));
    case Token_e::mul:
      return mul(std::forward<T>(a), std::forward<U>(b));
    case Token_e::lgAnd:
      return lgAnd(std::forward<T>(a), std::forward<U>(b));
    case Token_e::lgOr:
      return lgOr(std::forward<T>(a), std::forward<U>(b));
    case Token_e::eqEq:
      return eqEq(std::forward<T>(a), std::forward<U>(b));
    case Token_e::nEq:
      return nEq(std::forward<T>(a), std::forward<U>(b));
    case Token_e::lessThan:
      return lessThan(std::forward<T>(a), std::forward<U>(b));
    case Token_e::lessThanEq:
      return lessThanEq(std::forward<T>(a), std::forward<U>(b));
    case Token_e::greatThan:
      return greatThan(std::forward<T>(a), std::forward<U>(b));
    case Token_e::greatThanEq:
      return greatThanEq(std::forward<T>(a), std::forward<U>(b));
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<T, U>(err_hdl, opr);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::llint a,
    lox::Token_e oper,
    lox::llint b,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::plus:
      return add(a, b);
    case Token_e::minus:
      return sub(a, b);
    case Token_e::mul:
      return mul(a, b);
    case Token_e::mod:
      return mod(a, b);
    case Token_e::bwAnd:
      return bwAnd(a, b);
    case Token_e::bwOr:
      return bwOr(a, b);
    case Token_e::bwXor:
      return bwXor(a, b);
    case Token_e::bwShLft:
      return bwShLft(a, b);
    case Token_e::bwtShRht:
      return bwShRht(a, b);
    case Token_e::lgAnd:
      return lgAnd(a, b);
    case Token_e::lgOr:
      return lgOr(a, b);
    case Token_e::eqEq:
      return eqEq(a, b);
    case Token_e::nEq:
      return nEq(a, b);
    case Token_e::lessThan:
      return lessThan(a, b);
    case Token_e::lessThanEq:
      return lessThanEq(a, b);
    case Token_e::greatThan:
      return greatThan(a, b);
    case Token_e::greatThanEq:
      return greatThanEq(a, b);
    case Token_e::eq:
      return b;
    default:
      return errIllegalOp<llint, llint>(err_hdl, oper);
  }
}

template<typename T, typename U,
    isLoxVarNilOp_t<T, U> = true>
[[nodiscard]] auto operator_dispatch(
    T&&,
    lox::Token_e oper,
    U&& rhs,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  if (oper == lox::Token_e::eq) {
    if constexpr (isStringView<U>::value)
      return std::string{rhs.sview.cbegin(), rhs.sview.cend()};
    else
      return rhs;
  }
  return errAccNilValue(err_hdl, oper);
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    bool a,
    lox::ErrorWrapper env_hdl) -> lox::ExprRet_t
{
   if (oper == lox::Token_e::lgNot)
     return !a;
  return errIllegalOp<bool>(env_hdl, oper);
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    double& a,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::incr: return ++a;
    case Token_e::decr: return --a;
    default: return errIllegalOp<double>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    lox::llint& a,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::incr:  return ++a;
    case Token_e::decr:  return --a;
    default: return errIllegalOp<llint>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    double const& a,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::plus: return a;
    case Token_e::minus: return -a;
    case Token_e::lgNot: return !static_cast<bool>(a);
    default: return errIllegalOp<double>(err_hdl, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    lox::llint const& a,
    lox::ErrorWrapper err_hdl) -> lox::ExprRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::plus:  return  a;
    case Token_e::minus: return -a;
    case Token_e::bwNot: return ~a;
    case Token_e::lgNot: return !static_cast<bool>(a);
    default: return errIllegalOp<llint>(err_hdl, oper);
  }
}

template<typename Variant>
struct type_exists {
  template<typename Alternative, std::size_t... I>
  static constexpr bool valid_impl(
      std::index_sequence<I...> )
  {
    return (std::is_same_v<std::variant_alternative_t<I, Variant>, Alternative> || ...);
  }
  template<typename Alternative,
      std::size_t N = std::variant_size_v<Variant>,
      typename Indices = std::make_index_sequence<N>
  >
  static constexpr bool isValid()
  {
    return valid_impl<std::decay_t<Alternative>>(Indices{});
  }
};

template<typename VarEnvironment>
struct Eval_Var {
  std::reference_wrapper<VarEnvironment> m_env;
  lox::ErrorWrapper                      m_err;
  Eval_Var(VarEnvironment& env, lox::ErrorWrapper err): m_env{env}, m_err{err} { }

  [[nodiscard]] lox::ExprRet_t getResult(
      lox::VarIdentifier& variable,
      lox::Token_e        oper) const
  {
    auto& var_res = m_env.get().get(variable.sview, m_err);

    auto invoke_opdispatch = [this, oper] (auto&& arg) -> lox::ExprRet_t {
      using T = std::decay_t<decltype(arg)>;
      if constexpr (type_exists<lox::ExprRet_t>::template isValid<T>()) {
        return operator_dispatch(oper, std::forward<decltype(arg)>(arg), m_err);
      } else {
        return errIllegalOp<T>(m_err, oper);
      }
    };

    return std::visit(
        [this, oper, invoke_opdispatch] (auto&& arg) -> lox::ExprRet_t {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (isLoxErr<T>::value) {
            return lox::ErrorReported{};
          } else if constexpr (isLoxVarNil<T>::value) {
              return lox::NilValue{};
          } else if constexpr (isBool<T>::value || isInteger<T>::value || isDouble<T>::value) {
            return invoke_opdispatch(std::forward<decltype(arg)>(arg));
          } else if constexpr (isString<T>::value) {
            return errIllegalOp<T>(m_err, oper);
          } else {
            static_assert(lox::traits::always_false_v<T>, "Not exhaustive");
          }
        },
        var_res
    );
  }

  [[nodiscard]] lox::ExprRet_t getResult(
    lox::VarIdentifier lhs,
    lox::Token_e       oper,
    lox::VarIdentifier rhs) const
  {
    auto& var_lhs = m_env.get().get(lhs.sview, m_err);
    auto& var_rhs = m_env.get().get(rhs.sview, m_err);

    auto invoke_opdispatch = [this, oper] (auto&& arg1, auto&& arg2) -> lox::ExprRet_t {
        return operator_dispatch(
            std::forward<decltype(arg1)>(arg1),
            oper,
            std::forward<decltype(arg2)>(arg2),
            m_err
        );
    };

    return std::visit(
        [this, oper, invoke_opdispatch] (auto&& arg1, auto&& arg2) -> lox::ExprRet_t {
          using T = std::decay_t<decltype(arg1)>;
          using U = std::decay_t<decltype(arg2)>;

          if constexpr (isLoxErr<T>::value || isLoxErr<U>::value) {
            return lox::ErrorReported{};
          } else if constexpr (isLoxVarNil<T>::value || isLoxVarNil<U>::value) {
            return errAccNilValue(m_err, oper);
          } else {
            return invoke_opdispatch(arg1, arg2);
          }
        },
        var_lhs,
        var_rhs
    );
  }
};

struct Eval_LRExpr {
  lox::Token_e m_token;
  explicit Eval_LRExpr(lox::Token_e token): m_token{token} { }

  [[nodiscard]] lox::ExprRet_t operator()(std::string_view sv) const
  {
    if (m_token == lox::Token_e::identifier)
      return lox::VarIdentifier{sv};
    else
      return lox::StrLiteral{sv};
  }

  [[nodiscard]] lox::ExprRet_t operator()(bool val) const       { return val; }
  [[nodiscard]] lox::ExprRet_t operator()(lox::llint val) const { return val; }
  [[nodiscard]] lox::ExprRet_t operator()(double val) const     { return val; }
};

template<typename VarEnvironment>
struct Eval_SigOpExpr {
  Eval_Var<VarEnvironment> m_var;
  lox::Token_e m_token;

  Eval_SigOpExpr(
      VarEnvironment&   env,
      lox::ErrorWrapper err,
      lox::Token_e      token):
    m_var{env, err},
    m_token{token}
  {

  }

  [[nodiscard]] lox::ExprRet_t operator()(bool val) const
  {
    return operator_dispatch(m_token, val, m_var.m_err);
  }

  [[nodiscard]] lox::ExprRet_t operator()(lox::llint& val) const
  {
    return operator_dispatch(m_token, val, m_var.m_err);
  }

  [[nodiscard]] lox::ExprRet_t operator()(double& val) const
  {
    return operator_dispatch(m_token, val, m_var.m_err);
  }

  [[nodiscard]] lox::ExprRet_t operator()(lox::StrLiteral) const
  {
    return errIllegalOp<lox::StrLiteral>(m_var.m_err, m_token);
  }

  [[nodiscard]] lox::ExprRet_t operator()(lox::VarIdentifier var) const
  {
    return m_var.getResult(var, m_token);
  }

  [[nodiscard]] lox::ExprRet_t operator()(std::string const& ) const
  {
    return errIllegalOp<std::string>(m_var.m_err, m_token);
  }

  [[nodiscard]] lox::ExprRet_t operator()(lox::Assignment ) const
  {
    return errIllegalAssignOp(m_var.m_err, m_token);
  }
  [[nodiscard]] lox::ExprRet_t operator()(lox::NilValue ) const
  {
    return errAccNilValue(m_var.m_err, m_token);
  }

  [[nodiscard]] lox::ExprRet_t operator()(lox::ErrorReported ) const { return lox::ErrorReported{}; }
};

struct Eval_BinOpExpr {
  std::reference_wrapper<lox::VarEnv const> m_env;
  lox::ErrorWrapper                         m_err;
  lox::Token_e                              m_token;

  Eval_BinOpExpr(
      lox::VarEnv const& env,
      lox::ErrorWrapper  err,
      lox::Token_e       token):
    m_env{env},
    m_err{err},
    m_token{token}
  {

  }

  [[nodiscard]] auto operator()(bool lhs, bool rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(bool lhs, lox::llint rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::llint lhs, bool rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(bool lhs, double rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(double lhs, bool rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(bool lhs, lox::StrLiteral rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::StrLiteral lhs, bool rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(bool lhs, std::string const& rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(std::string const& lhs, bool rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::Assignment, bool ) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(bool, lox::Assignment) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::NilValue, bool ) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(bool, lox::NilValue) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, bool ) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(bool, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  template<typename T,
  typename std::enable_if_t<!std::is_same_v<T, lox::VarIdentifier>, bool> = true>
      [[nodiscard]] auto operator()(T lhs, lox::VarIdentifier rhs) const -> lox::ExprRet_t
  {
    auto& var_res = m_env.get().get(rhs.sview, m_err);
    static constexpr bool BinOp = isBool<T>::value || isInteger<T>::value || isDouble<T>::value || isString<T>::value;

    if constexpr (BinOp) {
      if (std::holds_alternative<bool>(var_res)) {
        return operator_dispatch(lhs, m_token, std::get<bool>(var_res), m_err);
      } else if (std::holds_alternative<lox::llint>(var_res)) {
        return operator_dispatch(lhs, m_token, std::get<lox::llint>(var_res), m_err);
      } else if (std::holds_alternative<double>(var_res)) {
        return operator_dispatch(lhs, m_token, std::get<double>(var_res), m_err);
      } else  if (std::holds_alternative<std::string>(var_res)) {
        return operator_dispatch(lhs, m_token, std::get<std::string>(var_res), m_err);
      } else {
        assert(0); //"Must be exhaustive"
      }
    } else {
      if (std::holds_alternative<lox::NilValue>(var_res)) {
        return errAccNilValue(m_err, m_token);
      }
      return lox::ErrorReported{};
    }
  }

  template<typename T,
      typename std::enable_if_t<!std::is_same_v<T, lox::VarIdentifier>, bool> = true>
  [[nodiscard]] auto operator()(lox::VarIdentifier lhs, T rhs) const -> lox::ExprRet_t
  {
    auto& var_res = m_env.get().get(lhs.sview, m_err);
    static constexpr bool BinOp = isBool<T>::value || isInteger<T>::value || isDouble<T>::value || isString<T>::value;

    if constexpr (BinOp) {
      if (std::holds_alternative<bool>(var_res)) {
        return operator_dispatch(std::get<bool>(var_res), m_token, rhs, m_err);
      } else if (std::holds_alternative<lox::llint>(var_res)) {
        return operator_dispatch(std::get<lox::llint>(var_res), m_token, rhs, m_err);
      } else if (std::holds_alternative<double>(var_res)) {
        return operator_dispatch(std::get<double>(var_res), m_token, rhs, m_err);
      } else if (std::holds_alternative<std::string>(var_res)) {
        return operator_dispatch(std::get<std::string>(var_res), m_token, rhs, m_err);
      } else {
        assert(0); //"Must be exhaustive"
      }
    } else {
      if (std::holds_alternative<lox::NilValue>(var_res)) {
        return errAccNilValue(m_err, m_token);
      }
      return lox::ErrorReported{};
    }
  }

  [[nodiscard]] auto operator()(lox::llint lhs, lox::llint rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::llint lhs, double rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(double lhs, lox::llint rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::llint lhs, lox::StrLiteral rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::StrLiteral lhs, lox::llint rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::llint lhs, std::string const& rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(std::string const& lhs, lox::llint rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::Assignment, lox::llint ) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::llint, lox::Assignment) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::NilValue, lox::llint ) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::llint, lox::NilValue) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, lox::llint ) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::llint, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(double lhs, double rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(double lhs, lox::StrLiteral rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::StrLiteral lhs, double rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(double lhs, std::string const& rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(std::string const& lhs, double rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::Assignment, double ) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(double, lox::Assignment) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::NilValue, double ) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(double, lox::NilValue) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, double ) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(double, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(std::string const& lhs, std::string const& rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(std::string const& lhs, lox::StrLiteral rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::StrLiteral lhs, std::string const& rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::Assignment, std::string const& ) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(std::string const&, lox::Assignment) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::NilValue, std::string const& ) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(std::string const&, lox::NilValue) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, std::string const& ) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(std::string const&, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::StrLiteral lhs, lox::StrLiteral rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::Assignment, lox::StrLiteral ) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::StrLiteral, lox::Assignment) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::NilValue, lox::StrLiteral ) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::StrLiteral, lox::NilValue) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, lox::StrLiteral ) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::StrLiteral, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::VarIdentifier lhs, lox::VarIdentifier rhs) const -> lox::ExprRet_t
  {
    return Eval_Var<lox::VarEnv const>{m_env, m_err}.getResult(lhs, m_token, rhs);
  }

  [[nodiscard]] auto operator()(lox::NilValue lhs, lox::NilValue rhs) const -> lox::ExprRet_t
  {
    return operator_dispatch(lhs, m_token, rhs, m_err);
  }

  [[nodiscard]] auto operator()(lox::NilValue, lox::Assignment) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::Assignment, lox::NilValue) const -> lox::ExprRet_t
  {
    return errAccNilValue(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::NilValue, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, lox::NilValue) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::Assignment, lox::Assignment) const -> lox::ExprRet_t
  {
    return errIllegalAssignOp(m_err, m_token);
  }

  [[nodiscard]] auto operator()(lox::Assignment, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, lox::Assignment) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }

  [[nodiscard]] auto operator()(lox::ErrorReported, lox::ErrorReported) const -> lox::ExprRet_t
  {
    return lox::ErrorReported{};
  }
};

struct DivImpl {
  std::reference_wrapper<lox::VarEnv const>    m_env;
  lox::ErrorWrapper                            m_err;

  DivImpl(
    lox::VarEnv const& env,
    lox::ErrorWrapper  err):
  m_env{env},
  m_err{err}
  {

  }

  template<typename Type1, typename Type2>
  [[nodiscard]] auto operator()(Type1&& arg1, Type2&& arg2) const -> lox::ExprRet_t
  {
    using T = std::decay_t<Type1>;
    using U = std::decay_t<Type2>;
    if constexpr (isBothNumber<T, U>::value) {
      return div(arg1, arg2, m_err);
    } else if constexpr (isLoxVarOp<T, U>::value) {
      return binVar(std::forward<Type1>(arg1), std::forward<Type2>(arg2));
    } else if constexpr ((isLoxVarOp<T, T>::value && isNumber<U>::value) ||
                         (isLoxVarOp<U, U>::value && isNumber<T>::value)) {
      return sigVar(std::forward<Type1>(arg1), std::forward<Type2>(arg2));
    } else {
      return errIllegalOp<T, U>(m_err, lox::Token_e::div);
    }
  }

  template<typename T>
  [[nodiscard]] auto sigVar(lox::VarIdentifier arg1, T&& arg2) const -> lox::ExprRet_t
  {
    auto& res = m_env.get().get(arg1.sview, m_err);
    if (std::holds_alternative<lox::llint>(res)) {
      return div(std::get<lox::llint>(res), arg2,  m_err);
    } else if (std::holds_alternative<double>(res)) {
      return div(std::get<double>(res), arg2,  m_err);
    } else {
      return errIllegalOp<bool, T>(m_err, lox::Token_e::div);
    }
  }

  template<typename T>
  [[nodiscard]] auto sigVar(T&& arg1, lox::VarIdentifier arg2) const -> lox::ExprRet_t
  {
    auto& res = m_env.get().get(arg2.sview, m_err);
    if (std::holds_alternative<lox::llint>(res)) {
      return div(arg1, std::get<lox::llint>(res),  m_err);
    } else if (std::holds_alternative<double>(res)) {
      return div(arg1, std::get<double>(res),  m_err);
    } else {
      return errIllegalOp<bool, T>(m_err, lox::Token_e::div);
    }
  }

  [[nodiscard]] auto binVar(lox::VarIdentifier arg1, lox::VarIdentifier arg2) const -> lox::ExprRet_t
  {
    auto& res_lhs = m_env.get().get(arg1.sview, m_err);
    auto& res_rhs = m_env.get().get(arg2.sview, m_err);
    return std::visit(
        [this] (auto&& lhs, auto&& rhs) -> lox::ExprRet_t {
          using T = std::decay_t<decltype(lhs)>;
          using U = std::decay_t<decltype(rhs)>;

          if constexpr (isLoxErr<T>::value || isLoxErr<U>::value) {
            return lox::ErrorReported{};
          } else if constexpr (isNumber<T>::value && isNumber<U>::value) {
            return div(lhs, rhs, m_err);
          } else {
            return errIllegalOp<T, U>(m_err, lox::Token_e::div);
          }
        },
        res_lhs,
        res_rhs
    );
  }
};

struct Eval_AssignExpr {
  std::reference_wrapper<lox::VarEnv>     m_env;
  std::reference_wrapper<lox::var::Var_t> m_lhs;
  lox::ErrorWrapper                       m_err;
  lox::Token_e                            m_token;

  Eval_AssignExpr(
      lox::VarEnv& env,
      lox::var::Var_t& lhs,
      lox::ErrorWrapper err,
      lox::Token_e token):
    m_env{env},
    m_lhs{lhs},
    m_err{err},
    m_token{token}
  {

  }

  static lox::Token_e get_token(lox::Token_e token)
  {
    switch (token) {
      case lox::Token_e::eq:      return lox::Token_e::eq;
      case lox::Token_e::plusEq:  return lox::Token_e::plus;
      case lox::Token_e::minusEq: return lox::Token_e::minus;
      case lox::Token_e::mulEq:   return lox::Token_e::mul;
      case lox::Token_e::divEq:   return lox::Token_e::div;
      case lox::Token_e::modEq:   return lox::Token_e::mod;
      case lox::Token_e::andEq:   return lox::Token_e::bwAnd;
      case lox::Token_e::orEq:    return lox::Token_e::bwOr;
      case lox::Token_e::xorEq:   return lox::Token_e::bwXor;
      case lox::Token_e::shLftEq: return lox::Token_e::bwShLft;
      case lox::Token_e::shRhtEq: return lox::Token_e::bwtShRht;
      default: exit(1);
    }
  }

  template<typename T, typename U>
  [[nodiscard]] auto getResult(T&& lhs_arg, U&& rhs_arg) -> lox::ExprRet_t {
    using Type2 = std::decay_t<decltype(rhs_arg)>;
    ///Division special case
    if (get_token(m_token) == lox::Token_e::div) {
      static constexpr bool divOp2 = isInteger<Type2>::value || isDouble<Type2>::value;
      if constexpr (divOp2) {
        return std::visit(
            [this](auto &&arg) -> lox::ExprRet_t {
              using Arg_t = std::decay_t<decltype(arg)>;
              if constexpr (isInteger<Arg_t>::value || isDouble<Arg_t>::value) {
                m_lhs.get() = arg;
                return lox::Assignment{};
              } else {
                return lox::ErrorReported{};
              }
            },
            expr::DivImpl(m_env.get(), m_err)(lhs_arg, rhs_arg)
        );
      } else {
        return errIllegalOp<T, U>(m_err, m_token);
      }
    }
    return std::visit(
        [this](auto &&arg) -> lox::ExprRet_t {
          using Arg_t = std::decay_t<decltype(arg)>;
          if constexpr (isStringView<Arg_t>::value) {
            m_lhs.get() = std::string{arg.sview.cbegin(), arg.sview.cend()};
            return lox::Assignment{};
          } else if constexpr (std::is_same_v<Arg_t, lox::VarIdentifier> || isLoxErr<Arg_t>::value ||
              std::is_same_v<Arg_t, lox::Assignment>) {
            return lox::ErrorReported{};
          } else {
            m_lhs.get() = arg;
            return lox::Assignment{};
          }
        },
        expr::Eval_BinOpExpr(m_env.get(), m_err, get_token(m_token))(lhs_arg, rhs_arg)
    );
  }
};

//////////////////////////////////////////////////////////////////////////////
}///end of namespace expr
//////////////////////////////////////////////////////////////////////////////

lox::LRExpr::LRExpr(lox::TokenRange&& range):
  m_range{range}
{
  assert(std::distance(m_range.cur_loc(), m_range.end()) == 1);
}

lox::ExprRet_t lox::LRExpr::evaluate(VarEnv& env, ErrorHandler& errHdl)
{
  (void)env; (void) errHdl; //Statements only to silence unused variable error
  return std::visit(expr::Eval_LRExpr(m_range.cur_loc()->m_type), m_range.cur_loc()->m_value);
}

lox::TokenRange lox::LRExpr::getTokenRange() const noexcept
{
  return m_range;
}

lox::Prefix::Prefix(
    Token_e      token,
    Expr_t       rhs,
    TokenRange&& range):
  m_rhs{std::move(rhs)},
  m_range{range},
  m_token{token}
{

}

lox::ExprRet_t lox::Prefix::evaluate(VarEnv& env, ErrorHandler& errHdl)
{
  return std::visit(
      expr::Eval_SigOpExpr<VarEnv>(env, ErrorWrapper{errHdl, m_range.cur_loc()}, m_token),
      m_rhs->evaluate(env, errHdl)
  );
}

lox::TokenRange lox::Prefix::getTokenRange() const noexcept
{
  return m_range;
}

lox::Suffix::Suffix(
    Expr_t       lhs,
    Token_e      token,
    TokenRange&& range):
  m_lhs{std::move(lhs)},
  m_range{range},
  m_token{token}
{

}

lox::ExprRet_t lox::Suffix::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto suffixOp = [oper = m_token] (auto val) -> ExprRet_t {
    if (std::holds_alternative<llint>(val)) {
      if (oper == lox::Token_e::incr) {
        return std::get<llint>(val) - 1;
      } else if (oper == lox::Token_e::decr) {
        return std::get<llint>(val) + 1;
      } else {
        assert(0); //Illegal path
        return lox::ErrorReported{};
      }
    } else if (std::holds_alternative<double>(val)) {
      if (oper == lox::Token_e::incr) {
        return std::get<double>(val) - 1;
      } else if (oper == lox::Token_e::decr) {
        return std::get<double>(val) + 1;
      } else {
        assert(0); //Illegal path
        return lox::ErrorReported{};
      }
    } else {
      return lox::ErrorReported{};
    }
  };

  return suffixOp(std::visit(
      expr::Eval_SigOpExpr<VarEnv>(env, ErrorWrapper{errHdl, m_range.cur_loc()}, m_token),
      m_lhs->evaluate(env, errHdl)
  ));
}

lox::TokenRange lox::Suffix::getTokenRange() const noexcept
{
  return m_range;
}

lox::Unary::Unary(
    Token_e      token,
    Expr_t       rhs,
    TokenRange&& range):
  m_rhs{std::move(rhs)},
  m_range{range},
  m_token{token}
{

}

lox::ExprRet_t lox::Unary::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  return std::visit(
      expr::Eval_SigOpExpr<VarEnv const>(env, ErrorWrapper{errHdl, m_range.cur_loc()}, m_token),
      m_rhs->evaluate(env, errHdl)
  );
}

lox::TokenRange lox::Unary::getTokenRange() const noexcept
{
  return m_range;
}

lox::Binary::Binary(
    Expr_t       lhs,
    Token_e      token,
    Expr_t       rhs,
    TokenRange&& range):
  m_lhs{std::move(lhs)},
  m_rhs{std::move(rhs)},
  m_range{range},
  m_token{token}
{

}

auto lox::Binary::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl) -> ExprRet_t
{
  if (m_token == Token_e::div) {
    return std::visit(
        expr::DivImpl(env, ErrorWrapper{errHdl, m_range.cur_loc()}),
        m_lhs->evaluate(env, errHdl),
        m_rhs->evaluate(env, errHdl)
    );
  }

  return std::visit(
      expr::Eval_BinOpExpr(env, ErrorWrapper{errHdl, m_range.cur_loc()}, m_token),
      m_lhs->evaluate(env, errHdl),
      m_rhs->evaluate(env, errHdl)
  );
}

lox::TokenRange lox::Binary::getTokenRange() const noexcept
{
  return m_range;
}

lox::Grouping::Grouping(
    Expr_t       expr,
    TokenRange&& range):
  m_expr{std::move(expr)},
  m_range{range}
{

}
auto lox::Grouping::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl) -> ExprRet_t
{
  return m_expr->evaluate(env, errHdl);
}

lox::TokenRange lox::Grouping::getTokenRange() const noexcept
{
  return m_range;
}

lox::AssignExpr::AssignExpr(
    Expr_t       lhs,
    Token_e      token,
    Expr_t       rhs,
    TokenRange&& range):
 m_lhs{std::move(lhs)},
 m_rhs{std::move(rhs)},
 m_range{range},
 m_token{token}
{

}

auto lox::AssignExpr::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl) -> ExprRet_t
{
  auto err_rpt = ErrorWrapper{errHdl, m_range.cur_loc()};
  auto lhs_var = m_lhs->evaluate(env, errHdl);
  if (!std::holds_alternative<VarIdentifier>(lhs_var)) {
    return expr::errIllegalAssignOp(err_rpt, m_token);
  }

  auto& lhs_var_res = env.get(std::get<VarIdentifier>(lhs_var).sview, err_rpt);
  return std::visit(
      [this, &lhs_var_res, &env, err_rpt] (auto&& arg1, auto&& arg2) -> ExprRet_t {
        return expr::Eval_AssignExpr{env, lhs_var_res, err_rpt, m_token}.getResult(
            std::forward<decltype(arg1)>(arg1),
            std::forward<decltype(arg2)>(arg2));
      },
      lhs_var_res,
      m_rhs->evaluate(env, errHdl)
  );
}

lox::TokenRange lox::AssignExpr::getTokenRange() const noexcept
{
  return m_range;
}
