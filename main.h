#include <algorithm>
#include <cstddef>
#include <array>

inline constexpr auto z = [](std::array<int, 1> arg) static {
    return 0;
};

using Z = decltype(z);

inline constexpr auto n = [](std::array<int, 1> arg) static {
    return arg[0] + 1;
};

using N = decltype(n);


template <std::size_t K, std::size_t N>
inline constexpr auto u = [](std::array<int, N> args) static {
    static_assert(1 <= K && K <= N);
    return args[K - 1];
};

template <std::size_t K, std::size_t N>
using U = decltype(u<K, N>);

template <typename G, typename... Fs>
struct S {

    template <std::size_t N> 
    static constexpr int operator()(std::array<int, N> args) {
        return G::operator()({Fs::operator()(args)...});
    }
};

template <typename G, typename... Fs>
inline constexpr auto s = S<G, Fs...>();

template <typename F, typename G>
struct R {
    template <std::size_t N1> 
    static constexpr int operator()(std::array<int, N1> args) {
        constexpr std::size_t N = N1 - 1;

        if (args[N] == 0) {
            std::array<int, N> f_args; // x_

            auto end = args.end();
            --end;

            std::copy(args.begin(), end, f_args.begin());
            return F::operator()(f_args);
        } else {
            std::array<int, N + 2> g_args;
            std::copy(std::begin(args), std::end(args), std::begin(g_args)); // x_, y
            g_args[N] -= 1; // y - 1

            std::array<int, N + 1> r_args;

            auto end = g_args.end();
            --end;

            std::copy(std::begin(g_args), end, std::begin(r_args));

            g_args[N + 1] = R<F, G>()(r_args);

            return G::operator()(g_args);
        }
    }
};

template <typename G, typename F>
inline constexpr auto r = R<G, F>();

static_assert(s<N, N>(std::array{7}) == 9);
static_assert(r<U<1, 1>, S<N, U<3, 3>>>(std::array{34, 35}) == 69);
