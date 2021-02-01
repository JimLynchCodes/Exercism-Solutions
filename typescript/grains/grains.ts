const TOTAL_SQUARES_ON_CHESSBOARD = 64
const MULTIPLIER_PER_SQUARE = 2

export default class Grains {

    static square(num: number) {
        if (num < 1 || num > TOTAL_SQUARES_ON_CHESSBOARD)
            throw 'derp!'

        return MULTIPLIER_PER_SQUARE ** (num - 1);
    }

    static total() {
        return [...Array(TOTAL_SQUARES_ON_CHESSBOARD).keys()]
            .map(num => num + 1)
            .reduce((acc, curr) => {
                return acc + this.square(curr);
            }, 0)
    }

}
