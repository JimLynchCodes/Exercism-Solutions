package hamming

import (
	"errors"
)

// Distance calculates the hamming distance of two DNA strands.
func Distance(a, b string) (int, error) {

	if len(a) != len(b) {
		return -1, errors.New("strings not equal length")
	}

	distance := 0

	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			distance++
		}
	}

	return distance, nil
}
