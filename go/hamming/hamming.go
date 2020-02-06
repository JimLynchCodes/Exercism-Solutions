package hamming

import (
	"errors"
)

// Distance calculates the hamming distance of two DNA strands.
func Distance(a, b string) (int, error) {

	aRunes, bRunes := []rune(a), []rune(b)

	if len(aRunes) != len(bRunes) {
		return 0, errors.New("strings not equal length")
	}

	var distance int

	for i := 0; i < len(aRunes); i++ {
		if aRunes[i] != bRunes[i] {
			distance++
		}
	}

	return distance, nil
}
