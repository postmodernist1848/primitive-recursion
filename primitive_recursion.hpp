#pragma once

#include <concepts>
#include <cstddef>
#include <utility>

typedef std::size_t Nat;

namespace primitive_recursion_detail {

template <typename T>
concept Nats = std::same_as<Nat, T>;

template <Nat K, Nats... Args>
constexpr Nat kth_arg(Nat arg, Args... args) {
    if constexpr (K == 1) {
        return arg;
    } else {
        return kth_arg<K - 1>(args...);
    }
}

template <Nat> using alwaysNat = Nat;

template <Nat K, typename Seq>
struct U;

template <Nat K, Nat... Is>
struct U<K, std::index_sequence<Is...>> {

    constexpr U(alwaysNat<Is>... args) : n(this->operator()(args...)) {}

    constexpr operator Nat() { return n; }

    static constexpr Nat operator()(alwaysNat<Is>... args) {
        return kth_arg<K>(args...);
    }
private:
    Nat n;
};

template <typename G, typename Signature, typename... Fs> struct S;
template <typename G, typename... Args, typename... Fs> 
struct S<G, Nat(Args...), Fs...> {

    constexpr S(Args... args) : n(this->operator()(args...)) {}
    constexpr operator Nat() { return n; }

    static constexpr Nat operator()(Args... args) {
        return G::operator()(Fs::operator()(args...)...);
    }

private:
    Nat n;
};

template <typename Signature, typename F, typename G> struct R;
template <typename... Args, typename F, typename G> 
struct R<Nat (Args...), F, G> {
    constexpr R(Args... x, Nat y) : n(this->operator()(x..., y)) {}
    constexpr operator Nat() { return n; }

    static constexpr Nat operator()(Args... x, Nat y) {
        if (y == 0) {
            return F::operator()(x...);
        } else {
            return G::operator()(x..., y - 1, R<decltype(F::operator()), F, G>::operator()(x..., y - 1));
        }
    }

private:
    Nat n;
};

} // namespace primitive_recursion_detail

struct Z {
    constexpr Z(Nat) {}
    constexpr operator Nat() { return 0; };

    static constexpr Nat operator()(Nat) {
        return 0;
    }
};

struct N {
    constexpr N(Nat n) : n(this->operator()(n)) {}
    constexpr operator Nat() { return n; };

    static constexpr Nat operator()(Nat n) {
        return n + 1;
    }

private:
    Nat n;
};

template <Nat K, Nat N>
using U = primitive_recursion_detail::U<K, std::make_index_sequence<N>>;

template <typename G, typename F, typename... Fs>
using S = primitive_recursion_detail::S<G, decltype(F::operator()), F, Fs...>;

template <typename F, typename G>
using R = primitive_recursion_detail::R<decltype(F::operator()), F, G>;

static_assert(Z(0) == 0);
static_assert(Z(1) == 0);
static_assert(Z(54) == 0);
static_assert(N(41) == 42);
static_assert(U<1, 3>(1, 2, 3) == 1);
static_assert(U<3, 6>(1, 2, 3, 4, 5, 6) == 3);
static_assert(S<N, Z>(69) == 1);                         // const 1
static_assert(S<N, S<N, S<N, N>>>(52) == 56);            //      +4

using Sum = R<U<1, 1>, S<N, U<3, 3>>>;
static_assert(Sum(34, 35) == 69);

using Sum13 = S<Sum, U<1, 3>, U<3, 3>>;
using Mul = R<Z, Sum13>;
static_assert(Mul(7, 11) == 77);
static_assert(Mul(12, 14) == 168);


using Sub1 = S<R<Z, U<2, 3>>, U<1, 1>, U<1, 1>>; // using y - 1 here
static_assert(Sub1(0) == 0);
static_assert(Sub1(1) == 0);
static_assert(Sub1(4) == 3);

using Sub = R<U<1, 1>, S<Sub1, U<3, 3>>>;
static_assert(Sub(0, 2) == 0);
static_assert(Sub(3, 3) == 0);
static_assert(Sub(7, 3) == 4);

// if c != 0 then x else y
using If = S<R<U<2, 2>, U<1, 4>>, U<2, 3> /*x*/, U<3, 3> /*y*/, U<1, 3>/*c*/>;
static_assert(If(0, 1, 2) == 2);
static_assert(If(3, 1, 2) == 1);
static_assert(If(1, 1, 2) == 1);

using ToBool = S<If, U<1, 1>, S<N, Z>, Z>;
static_assert(ToBool(0) == 0);
static_assert(ToBool(1) == 1);
static_assert(ToBool(2) == 1);

using LT = S<If, S<Sub, U<2,2>, U<1, 2>>, S<S<N, Z>, U<1, 2>>, S<Z, U<1, 2>>>;
static_assert(LT(1, 2) == 1);
static_assert(LT(1, 1) == 0);
static_assert(LT(2, 1) == 0);
static_assert(LT(4, 1) == 0);

using Rem_g = S<If, S<LT, S<N, U<3, 3>>, U<1, 3>>, S<N, U<3, 3>>, S<Z, U<1, 3>>>; // rem(a, m + 1) = if (rem(a, m) + 1 < a) rem(a, m) + 1 else 0.
using Rem = R<Z, Rem_g>;

// Rem(a, b) = b % a
static_assert(Rem(1, 0) == 0);
static_assert(Rem(1, 3) == 0);
static_assert(Rem(3, 0) == 0);
static_assert(Rem(3, 6) == 0);
static_assert(Rem(3, 5) == 2);
static_assert(Rem(3, 17) == 2);
static_assert(Rem(5, 17) == 2);
static_assert(Rem(5, 14) == 4);
static_assert(Rem(17, 29) == 12);

using Div_g = S<If, S<Rem, U<1, 3>, S<N, U<2, 3>>>, U<3, 3>, S<N, U<3, 3>>>; // div(a, m + 1) = if (0 < rem(a, m + 1)) div(a, m) else div(a, m) + 1.
using Div = R<Z, Div_g>;

// Div(a, b) = b % a
static_assert(Div(2, 0) == 0);
static_assert(Div(1, 3) == 3);
static_assert(Div(2, 3) == 1);
static_assert(Div(7, 14) == 2);
static_assert(Div(7, 17) == 2);
