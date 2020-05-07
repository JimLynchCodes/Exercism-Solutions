/**
* Your implmentation of the Leap exercise
*/
component {
	
	/**
	* @year The input year to consider
	*
	* @returns A boolean for whether the inputted year is true or false
	*/
	 function leapYear( year ) {
		 
		return year % 4 == 0 && year % 100 != 00 || year % 400 == 0
	}
	
}