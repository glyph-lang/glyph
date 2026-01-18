```c
#include <stdio.h>

int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    printf("%d\n", fib(0));  // Output: 0
    printf("%d\n", fib(1));  // Output: 1
    printf("%d\n", fib(5));  // Output: 5
    printf("%d\n", fib(10)); // Output: 55
    return 0;
}
```