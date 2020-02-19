package leap

// IsLeapYear should return true if the year passed in is a leap year, otherwise false.
func IsLeapYear(year int) bool {
	return year%400 == 0 || year%4 == 0 && year%100 != 0
}
