package twofer

import "fmt"

// ShareWith should return a string containing "you" or the name passed in.
func ShareWith(name string) string {

	if name == "" {
		name = "you"
	}

	return fmt.Sprintf("One for %s, one for me.", name)
}
