export default class Squares {

    squareOfSum: number
    sumOfSquares: number
    difference: number

    constructor(private n: number) {
        this.squareOfSum = this.getSumOfSmallerOrEqualIntegers(n) ** 2
        this.sumOfSquares = this.getSumOfSquares(n)
        this.difference = this.squareOfSum - this.sumOfSquares
    }

    getSumOfSmallerOrEqualIntegers(n: number): number {
        if (!n)
            return n

        return n + this.getSumOfSmallerOrEqualIntegers(n - 1)
    }

    getSumOfSquares(n: number): number {
        if (!n)
            return n

        return n ** 2 + this.getSumOfSquares(n - 1)
    }
    
}