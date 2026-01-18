```go
package main

import "fmt"

func sum_positive(nums []int32) int32 {
    var positiveNums []int32
    for _, num := range nums {
        if num > 0 {
            positiveNums = append(positiveNums, num)
        }
    }
    
    var sum int32
    for _, num := range positiveNums {
        sum += num
    }
    
    return sum
}

func main() {
    input := []int32{1, -2, 3, -4, 5}
    result := sum_positive(input)
    fmt.Println(result) // Output: 9
}
```