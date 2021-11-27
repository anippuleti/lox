#include <stdexcept>
#include <cmath>
#include "exprNodes.h"
#include "varEnv.h"
#include "errorHandler.h"
#include "usrTraits.h"

///D.S used in VarEnv class. Passed to nodes on Variable lookups
using llint = lox::exp::LongInt_t;
using evalRet_t = lox::exp::evalRet_t;
using envVar_t  = lox::var::Var_t;

//////////////////////////////////////////////////////////////////////////////
namespace expnms {
//////////////////////////////////////////////////////////////////////////////

///Template specializations for dispatching operations
template<typename T>
using remove_cvref_t = lox::traits::remove_cvref_t<T>;

template<typename T>
using isDouble = std::is_floating_point<remove_cvref_t<T>>;

template<typename T>
using isInteger = std::is_same<remove_cvref_t<T>, llint>;

template<typename T>
using isBool = std::is_same<remove_cvref_t<T>, bool>;

template<typename T>
using isString = std::is_same<remove_cvref_t<T>, std::string>;

template<typename T>
using isStringView = std::is_same<remove_cvref_t<T>, lox::exp::StrLiteral>;

template<typename T>
using isConvToStr = lox::traits::isValid<
    isString<T>::value || isStringView<T>::value
>;

template<typename T>
using isLoxVarNil = lox::traits::isValid<
    std::is_same_v<T, lox::var::NilState>
>;

template<typename T>
using isLoxNonStrLtrl = lox::traits::isValid<
    isInteger<T>::value || isDouble<T>::value || isBool<T>::value || isLoxVarNil<T>::value
>;

template<typename T>
using isLoxLiteral = lox::traits::isValid<
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
using isIllegalBoolOp = lox::traits::isValid<
    ((isBool<T>::value && (isInteger<U>::value || isDouble<U>::value)) ||
     (isBool<U>::value && (isInteger<T>::value || isDouble<T>::value)))
>;

template<typename T, typename U>
using isFloatingOp_t = typename isFloatingOp<T, U>::type;

template<typename T, typename U>
using isBothInteger = std::enable_if<
    isInteger<T>::value && isInteger<U>::value,
    bool
>;

template<typename T, typename U>
using isBothInteger_t = typename isBothInteger<T, U>::type;

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
      !(isLoxVarNil<T>::value    || isLoxVarNil<U>::value)
     )),
    bool
>;

template<typename T, typename U>
using isStringOp_t = typename isStringOp<T, U>::type;

///Yields type bool if condition is true
template<typename T, typename U>
using isBothNumber = lox::traits::isValid<
    ((isDouble<T>::value  && isDouble<U>::value)  ||
     (isInteger<T>::value && isDouble<U>::value)  ||
     (isDouble<T>::value  && isInteger<U>::value) ||
     (isInteger<T>::value && isInteger<U>::value))
>;

template<typename T, typename U>
using isIllegalBinOp = lox::traits::isValid<
    isIllegalBoolOp<T, U>::value            ||
    std::is_same_v<T, lox::exp::Assignment> ||
    std::is_same_v<U, lox::exp::Assignment>
>;

template<typename T, typename U>
using isLoxVarOp = lox::traits::isValid<
    std::is_same_v<T, lox::exp::VarIdentifier> &&
    std::is_same_v<U, lox::exp::VarIdentifier>
>;

template<typename T, typename U>
using isLoxErrRptd = lox::traits::isValid<
    std::is_same_v<T, lox::exp::ErrorReported> ||
    std::is_same_v<U, lox::exp::ErrorReported>
>;

template<typename T, typename U>
using isLoxVarNotFound = lox::traits::isValid<
    std::is_same_v<T, std::monostate> ||
    std::is_same_v<U, std::monostate>
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
    llint
>;

template<typename T, typename U>
using isPlusEqRet = std::conditional<
    isStringType<T, U>::value,
    std::string,
    isDouble_t<T, U>
>;

template<typename T, typename U>
using isPlusEqRet_t = typename isPlusEqRet<T, U>::type;

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

template<typename T, typename U>
isPlusEqRet_t<T, U> plusEq(T a, U b)
{
  using A1 = std::decay_t<T>;
  using A2 = std::decay_t<U>;

  if constexpr ((isString<A1>::value && isString<A2>::value) ||
                 isBothNumber<A1, A2>::value)
    return a + b;
  else if constexpr (isString<A1>::value && isStringView<A2>::value)
    return a + std::string{b.sview.begin(), b.sview.end()};
  else if constexpr (isStringView<A1>::value && isString<A2>::value)
    return std::string{a.sview.begin(), a.sview.end()} + b;
  else if constexpr (isStringView<A1>::value && isStringView<A2>::value)
    return std::string{a.sview.begin(), a.sview.end()} +
           std::string{b.sview.begin(), b.sview.end()};
  else if constexpr (isString<A1>::value && isLoxLiteral<A2>::value)
    return a + std::to_string(b);
  else if constexpr (isLoxLiteral<A1>::value && isString<A2>::value)
    return std::to_string(a) + b;
  else if constexpr (isStringView<A1>::value && isLoxLiteral<A2>::value)
    return std::string{a.sview.begin(), a.sview.end()} + std::to_string(b);
  else if constexpr (isLoxLiteral<A1>::value && isStringView<A2>::value)
    return std::to_string(a) + std::string{b.sview.begin(), b.sview.end()};
  else
    static_assert(lox::traits::always_false_v<A1>, "Must be exhaustive");
}

template<typename T, typename U>
 auto minusEq(T a, U b) -> decltype(a - b) { return a - b; }

template<typename T, typename U>
auto mulEq(T a, U b) -> decltype(a * b) { return a * b; }

template<typename T, typename U>
auto divEq(T a, U b) -> lox::exp::DivOpRes
{
  return lox::exp::DivOpRes(a, b);
}

template<typename T, typename U>
auto modEq(T a, U b) -> decltype(a % b) { return a % b; }

template<typename T, typename U>
auto andEq(T a, U b) -> std::decay_t<T> { return a & b; }

template<typename T, typename U>
auto orEq(T a, U b) -> std::decay_t<T> { return a | b; }

template<typename T, typename U>
auto xorEq(T a, U b) -> std::decay_t<T> { return a ^ b; }

template<typename T, typename U>
auto shLftEq(T a, U b) -> std::decay_t<T> { return a << b; }

template<typename T, typename U>
auto shRhtEq(T a, U b) -> std::decay_t<T> { return a >> b; }

template<typename T>
void errAccNilValue(
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc,
    lox::Token_e        token)
{
  errHdl.recordErrMsg(
      [token] (auto& msg) {
        msg += "accessing uninitialized variable for operator ";
        lox::toStr(msg, token);
      },
      loc
  );
}

template<typename T, typename U>
[[nodiscard]] evalRet_t errIllegalOp(
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc,
    lox::Token_e        token)
{
  using Type1 = std::decay_t<T>;
  using Type2 = std::decay_t<U>;
  constexpr auto lhs_op = []() {
    if constexpr (std::is_same_v<Type1, bool>)
      return "operand types are bool and ";
    else if constexpr (std::is_same_v<Type1, llint>)
      return "operand types are int and ";
    else if constexpr (std::is_same_v<Type1, double>)
      return "operand types are double and ";
    else if constexpr (std::is_same_v<Type1, std::string> ||
                       std::is_same_v<Type1, lox::exp::StrLiteral>)
      return "operand types are string and ";
    else if constexpr (std::is_same_v<Type1, lox::exp::Assignment>)
      return "illegal binary operation on assignment and ";
    else if constexpr (std::is_same_v<Type1, lox::exp::VarIdentifier>)
      return "illegal binary operation on variable declaration and ";
    static_assert("ErrorReported must be supported by caller");
    return "";
  };

  constexpr auto rhs_op = []() {
    if constexpr (std::is_same_v<Type2, bool>)
      return "bool ";
    else if constexpr (std::is_same_v<Type2, llint>)
      return "int ";
    else if constexpr (std::is_same_v<Type2, double>)
      return "double ";
    else if constexpr (std::is_same_v<Type2, std::string> ||
        std::is_same_v<Type2, lox::exp::StrLiteral>)
      return "string ";
    else if constexpr (std::is_same_v<Type2, lox::exp::Assignment>)
      return "assignment";
    else if constexpr (std::is_same_v<Type2, lox::exp::VarIdentifier>)
      return "variable declaration";
    static_assert("ErrorReported must be supported by caller");
    return "";
  };

  errHdl.recordErrMsg(
      [token, lhs_op, rhs_op] (auto& msg) {
        msg += "no match for operator ";
        lox::toStr(msg, token);
        msg += lhs_op();
        msg += rhs_op();
      },
      loc
  );
  return lox::exp::ErrorReported{};
}

template<typename T>
evalRet_t errIllegalOp(
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc,
    lox::Token_e        token)
{
  using Type = std::decay_t<T>;
  constexpr auto op = []() {
    if constexpr (std::is_same_v<Type, bool>)
      return "use of an operand of type bool with operator ";
    else if constexpr (std::is_same_v<Type, llint>)
      return "use of an operand of type int with operator ";
    else if constexpr (std::is_same_v<Type, double>)
      return "use of an operand of type double with operator ";
    else if constexpr (std::is_same_v<Type, std::string> ||
        std::is_same_v<Type, lox::exp::StrLiteral>)
      return "use of an operand of type string with operator ";
    else if constexpr (std::is_same_v<Type, lox::exp::Assignment>)
      return "use of an assignment expression within this context ";
    else if constexpr (std::is_same_v<Type, lox::exp::VarIdentifier>)
      return "use of an variable declaration within this context ";
    static_assert("ErrorReported must be supported by caller");
    return "";
  };

  errHdl.recordErrMsg(
      [token, op] (auto& msg) {
        msg += op();
        lox::toStr(msg, token);
        msg += "is forbidden";
      },
      loc
  );
  return lox::exp::ErrorReported{};
}

template<typename T, typename U>
[[nodiscard]] evalRet_t div(
    T&& a,
    U&& b,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc)
{
  if (b == 0) {
    errHdl.recordErrMsg(
        [] (auto& msg) { msg += "trying to divide by 0"; },
        loc
    );
    return lox::exp::ErrorReported{};
  }

  auto res = lox::exp::DivOpRes(a, b);
  if (res.isFloatingOp) {
    return res.decimal;
  } else {
    if (res.fractional == 0)
      return static_cast<llint>(res.decimal);
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
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc) -> evalRet_t
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
  }
  return errIllegalOp<T, U>(errHdl, loc, oper);
}

[[nodiscard]] auto operator_dispatch(
    bool a,
    lox::Token_e oper,
    bool b,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc) -> evalRet_t
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
    default:
      return errIllegalOp<bool, bool>(errHdl, loc, oper);
  }
}

template<typename T, typename U,
    isFloatingOp_t<T, U> = true>
[[nodiscard]] auto operator_dispatch(
    T&& a,
    lox::Token_e opr,
    U&& b,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc) -> evalRet_t
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
    default:
      return errIllegalOp<T, U>(errHdl, loc, opr);
  }
}

[[nodiscard]] auto operator_dispatch(
    llint a,
    lox::Token_e oper,
    llint b,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc) -> evalRet_t
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
    default:
      return errIllegalOp<llint, llint>(errHdl, loc, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    bool a,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc) -> evalRet_t
{
   if (oper == lox::Token_e::lgNot)
     return !a;
  return errIllegalOp<bool>(errHdl, loc, oper);
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    double& a,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc) -> evalRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::incr: return ++a;
    case Token_e::decr: return --a;
    default: return errIllegalOp<double>(errHdl, loc, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    llint& a,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc) -> evalRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::incr:  return ++a;
    case Token_e::decr:  return --a;
    default: return errIllegalOp<llint>(errHdl, loc, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    double const& a,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc) -> evalRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::plus: return a;
    case Token_e::minus: return -a;
    case Token_e::lgNot: return !static_cast<bool>(a);
    default: return errIllegalOp<double>(errHdl, loc, oper);
  }
}

[[nodiscard]] auto operator_dispatch(
    lox::Token_e oper,
    llint const& a,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc) -> evalRet_t
{
  using namespace lox;
  switch (oper) {
    case Token_e::plus:  return  a;
    case Token_e::minus: return -a;
    case Token_e::bwNot: return ~a;
    case Token_e::lgNot: return !static_cast<bool>(a);
    default: return errIllegalOp<llint>(errHdl, loc, oper);
  }
}

template<typename VAR1, typename VAR2>
[[nodiscard]] evalRet_t div_impl(
    VAR1&& lhs,
    VAR2&& rhs,
    lox::VarEnv const& env,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc)
{
  auto innrVisit = [&errHdl, loc] (auto&& var1, auto&& var2) -> evalRet_t {
    return std::visit(
        [&errHdl, loc] (auto&& arg1, auto&& arg2) -> evalRet_t {
          using N1 = std::decay_t<decltype(arg1)>;
          using N2 = std::decay_t<decltype(arg2)>;
          if constexpr(isBothNumber<N1, N2>::value) {
            return div(
                std::forward<decltype(arg1)>(arg1),
                std::forward<decltype(arg2)>(arg2),
                errHdl,
                loc
            );
          } else if constexpr (isLoxVarNotFound<N1, N2>::value ||
                               isLoxErrRptd<N1, N2>::value) {
            return lox::exp::ErrorReported{};
          } else {
            return errIllegalOp<N1, N2>(errHdl, loc, lox::Token_e::div);
          }
        },
        std::forward<decltype(var1)>(var1),
        std::forward<decltype(var2)>(var2)
    );
  };

  return std::visit(
      [&env, &errHdl, loc, innrVisit] (auto&& arg1, auto&& arg2) -> evalRet_t {
        using namespace lox::exp;
        using T = std::decay_t<decltype(arg1)>;
        using U = std::decay_t<decltype(arg2)>;
        using numVar = std::variant<llint, double>;

        if constexpr (std::is_same_v<T, VarIdentifier> &&
                      std::is_same_v<U, VarIdentifier>) {
          auto const& var1 = env.get(arg1.sview, errHdl, loc);
          auto const& var2 = env.get(arg2.sview, errHdl, loc);
          return innrVisit(var1, var2);

        } else if constexpr (std::is_same_v<T, VarIdentifier> &&
                            (isInteger<U>::value || isDouble<U>::value)) {
          auto const& var1 = env.get(arg1.sview, errHdl, loc);
          return innrVisit(var1,  numVar{arg2});

        } else if constexpr ((isInteger<T>::value || isDouble<T>::value) &&
                              std::is_same_v<U, VarIdentifier>) {
          auto const &var2 = env.get(arg2.sview, errHdl, loc);
          return innrVisit(numVar{arg1}, var2);

        } else if constexpr (isBothNumber<T, U>::value) {
          return div(
              std::forward<decltype(arg1)>(arg1),
              std::forward<decltype(arg2)>(arg2),
              errHdl,
              loc
          );
        } else {
          return errIllegalOp<T, U>(errHdl, loc, lox::Token_e::div);
        }
      },
      std::forward<VAR1>(lhs),
      std::forward<VAR2>(rhs)
  );
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    bool         var_value,
    lox::Token_e oper,
    bool         rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc
    )
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::andEq:
      variable = andEq(var_value, rhs_value);
      return true;
    case Token_e::orEq:
      variable = orEq(var_value, rhs_value);
      return true;
    case Token_e::xorEq:
      variable = xorEq(var_value, rhs_value);
      return true;
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
          )
      );
      return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    bool         var_value,
    lox::Token_e oper,
    llint        rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc
)
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::andEq:
      variable = andEq(var_value, rhs_value);
      return true;
    case Token_e::orEq:
      variable = orEq(var_value, rhs_value);
      return true;
    case Token_e::xorEq:
      variable = xorEq(var_value, rhs_value);
      return true;
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
          )
      );
      return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    bool         var_value,
    lox::Token_e oper,
    double       rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc
)
{
  if (oper == lox::Token_e::eq) {
    variable = rhs_value;
    return true;
  }
  static_cast<void>(
      errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
      )
 );
  return false;
}

template<typename U,
    typename std::enable_if_t<!std::is_same_v<U, lox::var::NilState>, bool> = true>
[[nodiscard]] bool operator_dispatch(
    envVar_t&           variable,
    lox::var::NilState  var_value,
    lox::Token_e        oper,
    U                   rhs_value,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc)
{
  if constexpr (isStringView<U>::value) {
    if (oper == lox::Token_e::eq) {
      variable  = std::string{rhs_value.sview};
      return true;
    }
  } else {
    if (oper == lox::Token_e::eq) {
      variable  = rhs_value;
      return true;
    }
  }
  errAccNilValue<decltype(var_value)>(errHdl, loc, oper);
  return false;
}

template<typename T>
[[nodiscard]] bool operator_dispatch(
    envVar_t&           variable,
    T                   var_value,
    lox::Token_e        oper,
    lox::var::NilState  rhs_value,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc)
{
  if (oper == lox::Token_e::eq) {
      variable  = rhs_value;
      return true;
  }
  //constexpr loop used just to avoid unused var_value compiler error
  if constexpr (isLoxVarNil<T>::value)
    errAccNilValue<decltype(var_value)>(errHdl, loc, oper);
  else
    errAccNilValue<decltype(rhs_value)>(errHdl, loc, oper);
  return false;
}

  template<typename T, typename U,
    isStringOp_t<T, U> = true>
[[nodiscard]] bool operator_dispatch(
    envVar_t&           variable,
    T                   var_value,
    lox::Token_e        oper,
    U                   rhs_value,
    lox::ErrorHandler&  errHdl,
    lox::Token_itr loc)
{
  if constexpr (isString<T>::value && isStringView<U>::value) {
    if (oper == lox::Token_e::eq) {
      variable = std::string{rhs_value.sview.begin(), rhs_value.sview.end()};
      return true;
    } else if (oper == lox::Token_e::plusEq) {
      variable = plusEq(var_value, rhs_value);
      return true;
    }
    static_cast<void>(
        errIllegalOp<decltype(var_value), decltype(rhs_value)>(
            errHdl, loc, oper
        )
    );
    return false;

  } else if constexpr (isString<T>::value && isLoxNonStrLtrl<U>::value) {
    if (oper == lox::Token_e::eq) {
      variable = std::to_string(rhs_value);
      return true;
    } else if (oper == lox::Token_e::plusEq) {
      variable = plusEq(var_value, rhs_value);
      return true;
    }
    static_cast<void>(
        errIllegalOp<decltype(var_value), decltype(rhs_value)>(
            errHdl, loc, oper
        )
    );
    return false;

  } else if constexpr (isLoxNonStrLtrl<T>::value && isStringView<U>::value) {
      if (oper == lox::Token_e::eq) {
        variable = std::string{rhs_value.sview};
        return true;
      } else if (oper == lox::Token_e::plusEq) {
        variable = plusEq(var_value, rhs_value);
        return true;
      }
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
              errHdl, loc, oper
          )
      );
      return false;

    } else if constexpr (isLoxNonStrLtrl<T>::value && isString<U>::value) {
      if (oper == lox::Token_e::eq) {
        variable = rhs_value;
        return true;
      } else if (oper == lox::Token_e::plusEq) {
        variable = plusEq(var_value, rhs_value);
        return true;
      }
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
              errHdl, loc, oper
          )
      );
      return false;

  } else if constexpr (isString<T>::value && isString<U>::value){
    if (oper == lox::Token_e::eq) {
      variable = rhs_value;
      return true;
    } else if (oper == lox::Token_e::plusEq) {
      variable = plusEq(var_value, rhs_value);
      return true;
    }
    static_cast<void>(
        errIllegalOp<decltype(var_value), decltype(rhs_value)>(
            errHdl, loc, oper
        )
    );
    return false;
  } else {
    static_assert(lox::traits::always_false_v<T>, "Must be exhaustive");
    return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    llint         var_value,
    lox::Token_e  oper,
    bool          rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc
)
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::andEq:
      variable = andEq(var_value, rhs_value);
      return true;
    case Token_e::orEq:
      variable = orEq(var_value, rhs_value);
      return true;
    case Token_e::xorEq:
      variable = xorEq(var_value, rhs_value);
      return true;
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
          )
      );
      return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    llint         var_value,
    lox::Token_e oper,
    llint        rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc)
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::plusEq:
      variable = plusEq(var_value, rhs_value);
      return true;
    case Token_e::minusEq:
      variable = minusEq(var_value, rhs_value);
      return true;
    case Token_e::mulEq:
      variable = mulEq(var_value, rhs_value);
      return true;
    case Token_e::divEq: {
      if (rhs_value == 0) {
        errHdl.recordErrMsg(
            [](auto &msg) { msg += "trying to divide by 0"; },
            loc
        );
        return false;
      } else {
        auto res = divEq(var_value, rhs_value);
        if (res.isFloatingOp) {
          variable = res.decimal;
        } else {
          if (res.fractional == 0)
            variable = static_cast<llint>(res.decimal);
          else
            variable = res.decimal + res.fractional;
        }
        return true;
      }
    }
    case Token_e::modEq:
      variable = modEq(var_value, rhs_value);
      return true;
    case Token_e::andEq:
      variable = andEq(var_value, rhs_value);
      return true;
    case Token_e::orEq:
      variable = orEq(var_value, rhs_value);
      return true;
    case Token_e::xorEq:
      variable = xorEq(var_value, rhs_value);
      return true;
    case Token_e::shLftEq:
      variable = shLftEq(var_value, rhs_value);
      return true;
    case Token_e::shRhtEq:
      variable = shRhtEq(var_value, rhs_value);
      return true;
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
           errHdl, loc, oper
          )
      );
      return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    llint         var_value,
    lox::Token_e oper,
    double        rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc)
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::plusEq:
      variable = plusEq(var_value, rhs_value);
      return true;
    case Token_e::minusEq:
      variable = minusEq(var_value, rhs_value);
      return true;
    case Token_e::mulEq:
      variable = mulEq(var_value, rhs_value);
      return true;
    case Token_e::divEq: {
      if (rhs_value == 0) {
        errHdl.recordErrMsg(
            [](auto &msg) { msg += "trying to divide by 0"; },
            loc
        );
        return false;
      } else {
        auto res = divEq(var_value, rhs_value);
        if (res.isFloatingOp) {
          variable = res.decimal;
        } else {
          if (res.fractional == 0)
            variable = static_cast<llint>(res.decimal);
          else
            variable = res.decimal + res.fractional;
        }
        return true;
      }
    }
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
          )
      );
      return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    double        var_value,
    lox::Token_e oper,
    bool          rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc
)
{
  if (oper == lox::Token_e::eq) {
    variable = rhs_value;
    return true;
  }
  static_cast<void>(
      errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
      )
  );
  return false;
}


[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    double        var_value,
    lox::Token_e oper,
    llint        rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc)
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::plusEq:
      variable = plusEq(var_value, rhs_value);
      return true;
    case Token_e::minusEq:
      variable = minusEq(var_value, rhs_value);
      return true;
    case Token_e::mulEq:
      variable = mulEq(var_value, rhs_value);
      return true;
    case Token_e::divEq: {
      if (rhs_value == 0) {
        errHdl.recordErrMsg(
            [](auto &msg) { msg += "trying to divide by 0"; },
            loc
        );
        return false;
      } else {
        auto res = divEq(var_value, rhs_value);
        if (res.isFloatingOp) {
          variable = res.decimal;
        } else {
          if (res.fractional == 0)
            variable = static_cast<llint>(res.decimal);
          else
            variable = res.decimal + res.fractional;
        }
        return true;
      }
    }
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
          )
      );
      return false;
  }
}

[[nodiscard]] bool operator_dispatch(
    envVar_t&     variable,
    double        var_value,
    lox::Token_e oper,
    double        rhs_value,
    lox::ErrorHandler& errHdl,
    lox::Token_itr loc)
{
  using namespace lox;
  switch (oper) {
    case Token_e::eq:
      variable = rhs_value;
      return true;
    case Token_e::plusEq:
      variable = plusEq(var_value, rhs_value);
      return true;
    case Token_e::minusEq:
      variable = minusEq(var_value, rhs_value);
      return true;
    case Token_e::mulEq:
      variable = mulEq(var_value, rhs_value);
      return true;
    case Token_e::divEq: {
      if (rhs_value == 0) {
        errHdl.recordErrMsg(
            [](auto &msg) { msg += "trying to divide by 0"; },
            loc
        );
        return false;
      } else {
        auto res = divEq(var_value, rhs_value);
        if (res.isFloatingOp) {
          variable = res.decimal;
        } else {
          if (res.fractional == 0)
            variable = static_cast<llint>(res.decimal);
          else
            variable = res.decimal + res.fractional;
        }
        return true;
      }
    }
    default:
      static_cast<void>(
          errIllegalOp<decltype(var_value), decltype(rhs_value)>(
          errHdl, loc, oper
          )
      );
      return false;
  }
}

//////////////////////////////////////////////////////////////////////////////
}///end of namespace expnms
//////////////////////////////////////////////////////////////////////////////

lox::Expr::Expr()           = default;
lox::Expr::~Expr() noexcept = default;

lox::Binary::Binary(
    std::unique_ptr<Expr> lhs,
    Token_e token,
    std::unique_ptr<Expr> rhs,
    TokenRange&&          range):
  Expr(),
  m_lhs{std::move(lhs)},
  m_rhs{std::move(rhs)},
  m_range{range},
  m_token{token}
{

}

evalRet_t lox::Binary::evaluate(
    VarEnv& env, ErrorHandler& errHdl)
{
  auto const& cenv = std::as_const(env);
  if (m_token == lox::Token_e::div) {
    return expnms::div_impl(
        m_lhs->evaluate(env, errHdl),
        m_rhs->evaluate(env, errHdl),
        cenv,
        errHdl,
        m_range.cur_loc());
 }

  auto innrVisit = [&errHdl, this] (auto&& var1, auto var2) -> evalRet_t {
      return std::visit(
          [&errHdl, this] (auto&& arg1, auto&& arg2) -> evalRet_t {
            using N1 = std::decay_t<decltype(arg1)>;
            using N2 = std::decay_t<decltype(arg2)>;

            if constexpr (expnms::isLoxErrRptd<N1, N2>::value ||
                          expnms::isLoxVarNotFound<N1, N2>::value) {
              return lox::exp::ErrorReported{};

            } else if constexpr (expnms::isIllegalBinOp<N1, N2>::value ||
                                 expnms::isLoxVarOp<N1, N1>::value     ||
                                 expnms::isLoxVarOp<N2, N2>::value) {
              return expnms::errIllegalOp<N1, N2>(errHdl, m_range.cur_loc(), m_token);

            } else if constexpr (expnms::isLoxVarNil<N1>::value ||
                                 expnms::isLoxVarNil<N2>::value) {
              expnms::errAccNilValue<var::NilState>(errHdl, m_range.cur_loc(), m_token);
              return lox::exp::ErrorReported{};
            } else {
              return expnms::operator_dispatch(
                  std::forward<decltype(arg1)>(arg1),
                  m_token,
                  std::forward<decltype(arg2)>(arg2),
                  errHdl,
                  m_range.cur_loc()
              );
            }
          },
          std::forward<decltype(var1)>(var1),
          std::forward<decltype(var2)>(var2)
      );
  };

  return std::visit(
      [&cenv, &errHdl, this, innrVisit] (auto&& arg1, auto&& arg2) -> evalRet_t {
        using namespace lox::exp;
        using T = std::decay_t<decltype(arg1)>;
        using U = std::decay_t<decltype(arg2)>;

        if constexpr (expnms::isLoxVarOp<T, U>::value) {
          auto const& var1 = cenv.get(arg1.sview, errHdl, m_range.cur_loc());
          auto const& var2 = cenv.get(arg2.sview, errHdl, m_range.cur_loc());
          return innrVisit(var1, var2);

        } else if constexpr (std::is_same_v<T, VarIdentifier> &&
                             expnms::isLoxLiteral<U>::value) {
          auto const& var1 = cenv.get(arg1.sview, errHdl, m_range.cur_loc());
          return innrVisit(var1,  evalRet_t{arg2});

        } else if constexpr (expnms::isLoxLiteral<T>::value &&
                             std::is_same_v<U, VarIdentifier>) {
          auto const &var2 = cenv.get(arg2.sview, errHdl, m_range.cur_loc());
          return innrVisit(evalRet_t{arg1}, var2);

        } else if constexpr (expnms::isIllegalBinOp<T, U>::value) {
          return expnms::errIllegalOp<T, U>(errHdl, m_range.cur_loc(), m_token);

        } else if constexpr (expnms::isLoxErrRptd<T, U>::value) {
          return ErrorReported{};

        } else {
          return expnms::operator_dispatch(
              std::forward<decltype(arg1)>(arg1),
              m_token,
              std::forward<decltype(arg2)>(arg2),
              errHdl,
              m_range.cur_loc()
          );
        }
      },
      m_lhs->evaluate(env, errHdl),
      m_rhs->evaluate(env, errHdl)
  );
}

lox::AstType lox::Binary::getType() const { return AstType::binary; }
lox::TokenRange lox::Binary::getTokenRange() const { return m_range; }

lox::Unary::Unary(
    Token_e token,
    std::unique_ptr<Expr> rhs,
    TokenRange&&          range):
  Expr(),
  m_rhs{std::move(rhs)},
  m_range{range},
  m_token{token}
{

}

evalRet_t lox::Unary::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  auto const& cenv = std::as_const(env);
  return std::visit(
      [this, &cenv, &errHdl] (auto&& arg) -> evalRet_t {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, exp::VarIdentifier>) {
          auto const& rhs_val = cenv.get(arg.sview, errHdl, m_range.cur_loc());
          return std::visit(
              [this, &errHdl] (auto const& innArg) -> evalRet_t {
                using U = std::decay_t<decltype(innArg)>;
                if constexpr (expnms::isLoxVarNil<U>::value) {
                  expnms::errAccNilValue<var::NilState>(errHdl, m_range.cur_loc(), m_token);
                  return exp::ErrorReported{};
                } else if constexpr (expnms::isLoxNonStrLtrl<U>::value) {
                  return expnms::operator_dispatch(
                      m_token,
                      std::forward<decltype(innArg)>(innArg),
                      errHdl,
                      m_range.cur_loc()
                  );
                } else if constexpr (std::is_same_v<U, std::monostate>) {
                  return exp::ErrorReported{};
                } else if constexpr (expnms::isString<U>::value) {
                  return expnms::errIllegalOp<U>(errHdl, m_range.cur_loc(), m_token);
                } else {
                  using namespace lox::traits;
                  static_assert(always_false_v<U>, "Must be a exhaustive list");
                  return exp::ErrorReported{};
                }
              },
              rhs_val
          );
        } else if constexpr (expnms::isLoxNonStrLtrl<T>::value) {
          return expnms::operator_dispatch(
              m_token,
              std::forward<decltype(arg)>(arg),
              errHdl,
              m_range.cur_loc()
          );
        } else if constexpr (std::is_same_v<T, exp::ErrorReported>) {
          return exp::ErrorReported{};
        } else if constexpr (std::is_same_v<T, exp::Assignment> ||
                             expnms::isLoxLiteral<T>::value) {
          return expnms::errIllegalOp<T>(errHdl, m_range.cur_loc(), m_token);
        } else {
          using namespace lox::traits;
          static_assert(always_false_v<T>, "Must be a exhaustive list");
          return exp::ErrorReported{};
        }
      },
      m_rhs->evaluate(env, errHdl)
  );
}

lox::AstType lox::Unary::getType() const { return AstType::unary; }
lox::TokenRange lox::Unary::getTokenRange() const { return m_range; }

lox::Prefix::Prefix(
    Token_e token,
    std::unique_ptr<Expr> rhs,
    TokenRange&& range):
  Expr(),
  m_rhs{std::move(rhs)},
  m_range{range},
  m_token{token}
{

}

evalRet_t lox::Prefix::evaluate(
    VarEnv& env, ErrorHandler& errHdl)
{
  return std::visit(
      [this, &env, &errHdl] (auto&& arg) -> evalRet_t {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, exp::VarIdentifier>) {
          using namespace expnms;
          auto& rhs_val = env.get(arg.sview, errHdl, m_range.cur_loc());
          return std::visit(
              [this, &errHdl] (auto& innArg) -> evalRet_t {
                using U = std::decay_t<decltype(innArg)>;
                if constexpr (std::is_same_v<U, llint> ||
                              std::is_same_v<U, double>) {
                  return expnms::operator_dispatch(
                      m_token,
                      std::forward<decltype(innArg)>(innArg),
                      errHdl,
                      m_range.cur_loc()
                  );
                } else if constexpr (std::is_same_v<U, std::monostate>) {
                  return exp::ErrorReported{};
                } else if constexpr (isString<U>::value || isBool<U>::value) {
                  return errIllegalOp<U>(errHdl, m_range.cur_loc(), m_token);
                } else if constexpr (isLoxVarNil<U>::value) {
                  errAccNilValue<var::NilState>(errHdl, m_range.cur_loc(), m_token);
                  return exp::ErrorReported{};
                } else {
                  using namespace lox::traits;
                  static_assert(always_false_v<U>, "Must be a exhaustive list");
                  return exp::ErrorReported{};
                }
              },
              rhs_val
          );
        } else if constexpr (std::is_same_v<T, exp::ErrorReported>) {
          return exp::ErrorReported{};
        } else if constexpr (std::is_same_v<T, exp::Assignment> ||
                             expnms::isLoxLiteral<T>::value) {
          return expnms::errIllegalOp<T>(errHdl, m_range.cur_loc(), m_token);
        } else {
          using namespace lox::traits;
          static_assert(always_false_v<T>, "Must be a exhaustive list");
          return exp::ErrorReported{};
        }
      },
      m_rhs->evaluate(env, errHdl)
  );
}

lox::AstType lox::Prefix::getType() const { return AstType::prefix; }
lox::TokenRange lox::Prefix::getTokenRange() const { return m_range; }

lox::Suffix::Suffix(
    std::unique_ptr<Expr> lhs,
    Token_e token,
    TokenRange&&          range):
  Expr(),
  m_lhs{std::move(lhs)},
  m_range{range},
  m_token{token}
{

}

evalRet_t lox::Suffix::evaluate(
    VarEnv& env, ErrorHandler& errHdl)
{
  return std::visit(
      [this, &env, &errHdl] (auto&& arg) -> evalRet_t {
        using T = std::decay_t<decltype(arg)>;
        using namespace expnms;
        if constexpr (std::is_same_v<T, exp::VarIdentifier>) {
          auto& lhs_val = env.get(arg.sview, errHdl, m_range.cur_loc());
          return std::visit(
              [this, &errHdl] (auto& innArg) -> evalRet_t {
                using U = std::decay_t<decltype(innArg)>;
                if constexpr (std::is_same_v<U, llint> ||
                              std::is_same_v<U, double>) {
                  auto oldVal = innArg;
                  static_cast<void>(
                      expnms::operator_dispatch(
                          m_token,
                          std::forward<decltype(innArg)>(innArg),
                          errHdl,
                          m_range.cur_loc()
                      )
                  );
                  return oldVal;
                } else if constexpr (std::is_same_v<U, std::monostate>) {
                  return exp::ErrorReported{};
                } else if constexpr (isBool<U>::value || isString<U>::value) {
                  return expnms::errIllegalOp<U>(errHdl, m_range.cur_loc(), m_token);
                } else if constexpr (isLoxVarNil<U>::value) {
                  errAccNilValue<var::NilState>(errHdl, m_range.cur_loc(), m_token);
                  return exp::ErrorReported{};
                } else {
                  using namespace lox::traits;
                  static_assert(always_false_v<U>, "Must be a exhaustive list");
                  return exp::ErrorReported{};
                }
              },
              lhs_val
          );
        } else if constexpr (std::is_same_v<T, exp::ErrorReported>) {
          return exp::ErrorReported{};
        } else if constexpr (std::is_same_v<T, exp::Assignment> ||
                             expnms::isLoxLiteral<T>::value) {
          return expnms::errIllegalOp<T>(errHdl, m_range.cur_loc(), m_token);
        } else {
          using namespace lox::traits;
          static_assert(always_false_v<T>, "Must be a exhaustive list");
          return exp::ErrorReported{};
        }
      },
      m_lhs->evaluate(env, errHdl)
  );
}

lox::AstType lox::Suffix::getType() const { return AstType::suffix; }
lox::TokenRange lox::Suffix::getTokenRange() const { return m_range; }

lox::LRExpr::LRExpr(lox::TokenRange&& range):
  Expr(),
  m_range{range}
{

}

evalRet_t lox::LRExpr::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  (void)env; (void) errHdl; //Statements only to silence unused variable error
  return std::visit(
      [this] (auto&& arg) -> evalRet_t {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, std::string>) {
          if ((*m_range.cur_loc())() == Token_e::identifier)
            return exp::VarIdentifier{arg};
          else
            return exp::StrLiteral{arg};
        } else {
          return arg;
        }
      },
      m_range.cur_loc()->m_value
  );
}

lox::AstType lox::LRExpr::getType() const
{
  return std::visit(
      [this] (auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, std::string>) {
          if ((*m_range.cur_loc())() == Token_e::identifier)
            return AstType::identifier;
          else
            return AstType::string;
        } else if constexpr (std::is_same_v<T, bool>) {
          return AstType::boolean;
        } else if constexpr (std::is_same_v<T, llint>) {
          return AstType::integer;
        } else if constexpr (std::is_same_v<T, double>) {
          return AstType::fractional;
        } else {
          static_assert(lox::traits::always_false_v<T>, "Must be exhaustive");
        }
      },
      m_range.cur_loc()->m_value
  );
}

lox::TokenRange lox::LRExpr::getTokenRange() const { return m_range; }

lox::Grouping::Grouping(
    std::unique_ptr<Expr> expr,
    TokenRange&&          range):
  Expr(),
  m_expr{std::move(expr)},
  m_range{range}
{

}

evalRet_t lox::Grouping::evaluate(
    VarEnv& env,
    ErrorHandler& errHdl)
{
  return m_expr->evaluate(env, errHdl);
}

lox::AstType lox::Grouping::getType() const { return AstType::grouping; }
lox::TokenRange lox::Grouping::getTokenRange() const { return m_range; }

lox::Assignment::Assignment(
    std::unique_ptr<Expr> lhs,
    Token_e               token,
    std::unique_ptr<Expr> rhs,
    TokenRange&&          range):
 Expr(),
 m_lhs{std::move(lhs)} ,
 m_rhs{std::move(rhs)},
 m_range{range},
 m_token{token}
{

}

evalRet_t lox::Assignment::evaluate(
    VarEnv& env, ErrorHandler& errHdl)
{
  using namespace expnms;
  auto isRhsLtrl = [&env, &errHdl, this] (auto& lhs, auto&& rhs) -> bool {
    auto& var_ref = env.get(lhs.sview, errHdl, m_range.cur_loc());
    return std::visit(
        [&var_ref, &rhs, &errHdl, this] (auto arg) -> bool {
          using T = decltype(arg);
          using U = std::decay_t<decltype(rhs)>;
          if constexpr (std::is_same_v<T, std::monostate>) {
            return false;
          } else if constexpr (isLoxLiteral<U>::value) {
            return operator_dispatch(
                var_ref, arg, m_token, rhs, errHdl, m_range.cur_loc());
          } else {
            using namespace lox::traits;
            static_assert(always_false_v<T>, "Must be exhaustive");
            return false;
          }
        },
        var_ref
    );
  };

  auto isRhsVar = [&env,&errHdl, this] (auto& lhs, auto const& rhs) -> bool {
    auto& lhs_var = env.get(lhs.sview, errHdl, m_range.cur_loc());
    auto const& cenv = std::as_const(env);
    auto const& rhs_var = cenv.get(rhs.sview, errHdl, m_range.cur_loc());
    return std::visit(
        [&lhs_var, &errHdl, this] (auto arg1, auto const& arg2) -> bool {
          using T = decltype(arg1);
          using U = std::decay_t<decltype(arg2)>;
          if constexpr (std::is_same_v<T, std::monostate> ||
                        std::is_same_v<U, std::monostate>)
          {
            return false;
          } else if constexpr (isLoxLiteral<T>::value || isLoxLiteral<U>::value) {
            return operator_dispatch(
                lhs_var, arg1, m_token, arg2, errHdl, m_range.cur_loc());
          } else {
            using namespace lox::traits;
            static_assert(always_false_v<T>, "Must be exhaustive");
            return false;
          }
        },
        lhs_var, rhs_var
    );
  };

  return std::visit(
      [&errHdl, this, isRhsLtrl, isRhsVar] (auto&& arg1, auto&& arg2) -> evalRet_t {
        using A1 = std::decay_t<decltype(arg1)>;
        using A2 = std::decay_t<decltype(arg2)>;
        if constexpr (std::is_same_v<A1, exp::VarIdentifier> &&
                      (isLoxLiteral<A2>::value))
        {
          if (isRhsLtrl(arg1, std::forward<decltype(arg2)>(arg2)))
            return exp::Assignment{};
          else
            return exp::ErrorReported{};
        } else if constexpr (std::is_same_v<A1, exp::VarIdentifier> &&
                             std::is_same_v<A2, exp::VarIdentifier>) {
          if (isRhsVar(arg1, arg2))
            return exp::Assignment{};
          else
            return exp::ErrorReported{};
        } else if constexpr (std::is_same_v<A1, exp::ErrorReported> ||
                             std::is_same_v<A2, exp::ErrorReported>) {
          return exp::ErrorReported{};
        } else if constexpr (std::is_same_v<A1, exp::Assignment> ||
                             std::is_same_v<A2, exp::Assignment> ||
                             isLoxLiteral<A1>::value)
        {
          return errIllegalOp<A1, A2>(errHdl, m_range.cur_loc(), m_token);
        } else {
          using namespace lox::traits;
          static_assert(always_false_v<A1>, "Must be Exhaustive");
          return exp::ErrorReported{};
        }
      },
      m_lhs->evaluate(env, errHdl),
      m_rhs->evaluate(env, errHdl)
  );
}

lox::AstType lox::Assignment::getType() const { return AstType::assignment; }
lox::TokenRange lox::Assignment::getTokenRange() const { return m_range; }
