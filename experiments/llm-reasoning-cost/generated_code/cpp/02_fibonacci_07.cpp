#include <cstdint>

int32_t fib(int32_t n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}