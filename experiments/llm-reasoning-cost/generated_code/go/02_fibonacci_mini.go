```go
package main

import "fmt"

func fib(n int) int {
    if n < 2 {
        return n
    }
    return fib(n-1) + fib(n-2)
}

func main() {
    fmt.Println(fib(0))  // Output: 0
    fmt.Println(fib(1))  // Output: 1
    fmt.Println(fib(5))  // Output: 5
    fmt.Println(fib(10)) // Output: 55
}
```