package main

import "errors"

func safe_divide(numerator, denominator int32) (int32, error) {
	if denominator == 0 {
		return 0, errors.New("division by zero")
	}
	return numerator / denominator, nil
}