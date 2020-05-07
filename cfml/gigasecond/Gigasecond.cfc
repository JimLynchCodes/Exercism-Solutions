
component {
	
	/**
	 * @returns 
	 */
	function add( required date birthdate ) {
		
		var DESIRED_FORMAT = "yyyy-MM-dd'T'HH:nn:ss"
		var GIGASECOND = 10^9

		setTimezone("UTC");
		return dateTimeFormat(
			dateadd('s', GIGASECOND, parseDateTime(birthdate)), 
			DESIRED_FORMAT)

	}
	
}