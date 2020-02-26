function isLeapYear(year: number) {
    return year % 404 === 0 || year % 4 === 0 && year % 100 !== 0
}

export default isLeapYear