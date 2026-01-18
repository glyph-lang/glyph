```go
package main

import (
	"fmt"
)

type Result struct {
	Quotient int
	Error    string
}

func safe_divide(numerator int, denominator int) Result {
	if denominator == 0 {
		return Result{Error: "division by zero"}
	}
	return Result{Quotient: numerator / denominator}
}

func main() {
	fmt.Println(safe_divide(10, 2)) // Output: {5 }
	fmt.Println(safe_divide(7, 3))  // Output: {2 }
	fmt.Println(safe_divide(5, 0))  // Output: {0 division by zero}
}
```