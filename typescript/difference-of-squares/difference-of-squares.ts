 export default class Squares {
     
    squareOfSum: number = 0

    constructor(private n: number) {

        console.log('getting num: ', n)
        this.getSquareOfSum(n)
        console.log('new num: ', n)
        

    }

    getSquareOfSum(n: number): number {
        console.log('now: ', n)
        if (n === 1) {

            console.log('here', n)
            return n**2
        }
        
        console.log('now2: ', n)

        this.
        return (n + this.getSquareOfSum((n-1)))
    }

    sumOfSquares() {

    }

    difference() {

    }

 }