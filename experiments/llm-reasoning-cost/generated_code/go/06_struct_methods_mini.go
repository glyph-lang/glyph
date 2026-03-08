package main

import (
	"fmt"
)

type Point struct {
	x int32
	y int32
}

func (p Point) squared_mag() int32 {
	return p.x*p.x + p.y*p.y
}

func filter_and_sum(points []Point, max_mag int32) int32 {
	var total int32 = 0
	for _, point := range points {
		if point.squared_mag() <= max_mag {
			total += point.squared_mag()
		}
	}
	return total
}

func main() {
	points := []Point{{1, 2}, {3, 4}, {0, 1}}
	max_mag := int32(10)
	result := filter_and_sum(points, max_mag)
	fmt.Println(result) // Output: 6
}
