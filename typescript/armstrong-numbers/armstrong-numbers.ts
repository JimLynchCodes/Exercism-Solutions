export default {

    isArmstrongNumber: (n: number) => {

        const digits: number[] = n
            .toString()
            .split('')
            .map((i) => { return Number(i); })

        const sum: number = digits.reduce((sum, digit) => {
            return sum + digit ** digits.length
        }, 0)

        return sum === n

    }

}