module Leap

let leapYear (year: int): bool = 
    year % 400 = 0 || year % 4 = 0 && year % 100 <> 0