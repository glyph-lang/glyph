package main

type Point struct {
	x int32
	y int32
}

func (p Point) squared_mag() int32 {
	return p.x*p.x + p.y*p.y
}

func filter_and_sum(points []Point, max_mag int32) int32 {
	var total int32
	for _, p := range points {
		mag := p.squared_mag()
		if mag <= max_mag {
			total += mag
		}
	}
	return total
}