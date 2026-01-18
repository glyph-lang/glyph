package main

func sum_positive(nums []int32) int32 {
	positives := []int32{}
	for _, v := range nums {
		if v > 0 {
			positives = append(positives, v)
		}
	}
	var sum int32
	for _, v := range positives {
		sum += v
	}
	return sum
}
