#pragma once

#include <algorithm>
#include <array>
#include <concepts>
#include <cstddef>
#include <utility>

typedef std::size_t Int;

template <typename T>
concept Ints = std::same_as<Int, T>;

template <Int K, Ints... Args>
constexpr Int kth_arg(Int arg, Args... args) {
    if constexpr (K == 1) {
        return arg;
    } else {
        return kth_arg<K - 1>(args...);
    }
}

template <typename F>
struct Nargs;

template <Ints... Args>
struct Nargs<Int (Args...)> {
    inline static constexpr Int value = sizeof...(Args);
};

template <Int> using alwaysInt = Int;

template <Int K, typename Seq>
struct U_impl;

template <Int K, Int... Is>
struct U_impl<K, std::index_sequence<Is...>> {

    constexpr U_impl(alwaysInt<Is>... args) : n(this->operator()(args...)) {}

    constexpr operator Int() { return n; }

    static constexpr Int operator()(alwaysInt<Is>... args) {
        return kth_arg<K>(args...);
    }

    Int n;
};

template <typename G, typename Ff, typename... Fs> struct S_impl;

template <typename G, typename... Args, typename... Fs> 
struct S_impl<G, Int(Args...), Fs...> {

    constexpr S_impl(Args... args) : n(this->operator()(args...)) {}
    constexpr operator Int() { return n; }

    static constexpr Int operator()(Args... args) {
        return G::operator()(Fs::operator()(args...)...);
    }

    Int n;
};

struct Z {
    constexpr Z(Int) {}
    constexpr operator Int() { return 0; };

    static constexpr Int operator()(Int) {
        return 0;
    }
};

struct N {
    constexpr N(Int n) : n(this->operator()(n)) {}
    constexpr operator Int() { return n; };

    static constexpr Int operator()(Int n) {
        return n + 1;
    }

private:
    Int n;
};

template <Int K, Int N>
using U = U_impl<K, std::make_index_sequence<N>>;


template <typename G, typename F, typename... Fs>
using S = S_impl<G, decltype(F::operator()), F, Fs...>;



template <typename Signature, typename F, typename G> struct R_impl;


template <typename... Args, typename F, typename G> 
struct R_impl<Int (Args...), F, G> {
    constexpr R_impl(Args... x, Int y) : n(this->operator()(x..., y)) {}
    constexpr operator Int() { return n; }

    static constexpr Int operator()(Args... x, Int y) {
        if (y == 0) {
            return F::operator()(x...);
        } else {
            return G::operator()(x..., y - 1, R_impl<decltype(F::operator()), F, G>::operator()(x..., y - 1));
        }
    }

    Int n;
};

template <typename F, typename G>
using R = R_impl<decltype(F::operator()), F, G>;

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
static_assert(Mul(21, 23) == 483);
