package raindrops

import "strconv"

// Convert takes a raindrop number returns a string of the sounds.
func Convert(n int) string {

	var sounds string

	if n%3 == 0 {
		sounds += "Pling"
	}

	if n%5 == 0 {
		sounds += "Plang"
	}

	if n%7 == 0 {
		sounds += "Plong"
	}

	if sounds == "" {
		sounds = strconv.Itoa(n)
	}

	return sounds

}
