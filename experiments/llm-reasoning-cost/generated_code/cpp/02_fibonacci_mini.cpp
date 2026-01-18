```cpp
#include <iostream>

int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    std::cout << fib(0) << std::endl;  // Output: 0
    std::cout << fib(1) << std::endl;  // Output: 1
    std::cout << fib(5) << std::endl;  // Output: 5
    std::cout << fib(10) << std::endl; // Output: 55
    return 0;
}
```