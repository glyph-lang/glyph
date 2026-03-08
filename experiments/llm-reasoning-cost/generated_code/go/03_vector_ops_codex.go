package main

func sum_positive(nums []int32) int32 {
	positives := make([]int32, 0)

	for _, n := range nums {
		if n > 0 {
			positives = append(positives, n)
		}
	}

	var sum int32 = 0
	for _, n := range positives {
		sum += n
	}

	return sum
}