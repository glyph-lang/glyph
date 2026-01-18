package main

import "fmt"

func fib(n int32) int32 {
	if n < 2 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	fmt.Println(fib(0))
	fmt.Println(fib(1))
	fmt.Println(fib(5))
	fmt.Println(fib(10))
}