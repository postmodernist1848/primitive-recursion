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
template <Nats... Args, typename F, typename G> 
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

using LE = S<If, S<Sub, U<1, 2>, U<2,2>>, S<Z, U<1, 2>>, S<S<N, Z>, U<1, 2>>>;
static_assert(LE(1, 2) == 1);
static_assert(LE(1, 1) == 1);
static_assert(LE(2, 1) == 0);
static_assert(LE(4, 1) == 0);

using Eq = S<Mul, LE, S<LE, U<2, 2>, U<1, 2>>>;
static_assert(Eq(1, 2) == 0);
static_assert(Eq(3, 2) == 0);
static_assert(Eq(0, 2) == 0);
static_assert(Eq(2, 2) == 1);
static_assert(Eq(0, 0) == 1);

using GT = S<LT, U<2, 2>, U<1, 2>>;
using GE = S<LE, U<2, 2>, U<1, 2>>;

// min<P>(y, x) = min z <= x such that P(z, y) (x if none)
// g(y, x - 1, Min(y, x - 1));

template <typename P>
using Min = R<Z, 
      S<Sum, 
        U<3, 3>, 
        S<Mul, S<Eq, U<3, 3>, U<2, 3>> /*... = x*/, S<Eq, S<P, U<2, 3>, U<1, 3>> /*P(x, ~y)*/, S<Z, U<1, 3>> > >
        // minz≤x+1(P (z, ~y) = 0) = minz≤x(P (z, ~y)) + [minz≤x(P (z, ~y)) = x ∧ P (x, ~y) = 0];
>>;
static_assert(Min<Eq>(3, 3) == 3);
static_assert(Min<Eq>(2, 3) == 2);
static_assert(Min<Eq>(10, 2) == 2);
static_assert(Min<GT>(2, 5) == 3);
static_assert(Min<GE>(3, 5) == 3);
static_assert(Min<GE>(10, 20) == 10);

using Conj = S<If, U<1, 2>, S<N, S<Z, U<1, 2>>>, U<2, 2>>;
static_assert(Conj(1, 1) == 1);
static_assert(Conj(1, 0) == 1);
static_assert(Conj(0, 1) == 1);
static_assert(Conj(0, 0) == 0);

// Forall<P>(y, x) - forall z < x P(z, y)

template <typename P>
using Forall = R< 
    S<N, Z>, 
    S<Mul, S<P, U<2, 3>, U<1, 3>>, U<3, 3>>
>;

static_assert(Forall<LT>(4, 4) == 1);
static_assert(Forall<GT>(1, 4) == 0);
static_assert(Forall<GE>(0, 4) == 1);

// Prime_cond(z, x) := z > 0 && (z = 1 ∨ ¬(z|x))
using Prime_cond = S<Conj, 
      S<LE, U<1, 2>, S<N, S<Z, U<1, 2>>>> /*z <= 1*/, 
      S<GT, Rem, S<Z, U<1, 2>>> /*z % x > 0*/ 
>;

using Two = S<N, S<N, Z>>;
using Mul13 = S<Mul, U<1, 3>, U<3, 3>>;
using Pow = R<S<N, Z>, Mul13>;
static_assert(Pow(2, 4) == 16);
using Pow2 = S<Pow, Two, U<1, 1>>;

using Prime = S<Mul, S<LT, S<N, Z>, U<1, 1>> /*1 < x*/, S< Forall<Prime_cond>, U<1, 1>, U<1, 1>> /*forall z < x ...*/ >;
static_assert(Prime(1) == 0);
static_assert(Prime(3) == 1);
static_assert(Prime(4) == 0);
static_assert(Prime(5) == 1);
static_assert(Prime(9) == 0);
static_assert(Prime(11) == 1);
static_assert(Prime(12) == 0);
static_assert(Prime(13) == 1);

using P = S<Mul, S<Prime, U<1, 2>>, S<LT, U<2, 2>, U<1, 2>>>; /* Prime(z) ∧ x < z */
using PrimeAfter = S<Min<P>, U<1, 1>, S<N, Pow2>>;

static_assert(Min<P>(3, 10) == 5);
static_assert(PrimeAfter(1) == 2);
static_assert(PrimeAfter(2) == 3);
static_assert(PrimeAfter(4) == 5);
//static_assert(PrimeAfter(5) == 7);
//static_assert(PrimeAfter(6) == 7);
//static_assert(PrimeAfter(7) == 11);

using NthPrime_g = S<PrimeAfter, U<3, 3>>;
using NthPrime = S<R<Two, NthPrime_g>, Z, U<1, 1>>;

static_assert(NthPrime(0) == 2);
static_assert(NthPrime(1) == 3);
static_assert(NthPrime(2) == 5);
//static_assert(NthPrime(3) == 7);

// lh(x) = minz≤x¬(p(z)|x)
using NotDivisible = S<GT, Rem, S<Z, U<1, 2>>>; /*z % x > 0*/
using LenP = S<NotDivisible, S<NthPrime, U<1, 2>>, U<2, 2>>;
using Len = S<Min<LenP>, U<1, 1>, U<1, 1>>;

