
const ROWS_IN_CHESSBOARD = 8
const COLUMNS_IN_CHESSBOARD = 8

enum DiagonalDirection {
    UpAndToTheLeft,
    UpAndToTheRight,
    DownAndToTheLeft,
    DownAndToTheRight,
}

export default class QueenAttack {

    white: number[]
    black: number[]

    board: string

    canAttackEachOther: boolean

    constructor(positions: { white: number[], black: number[] }) {

        if (this.occupySameSquare(positions.white, positions.black))
            throw new Error('Queens cannot share the same space')

        this.white = positions.white
        this.black = positions.black

        this.board = this.createBoard(this.white, this.black)

        this.canAttackEachOther = this.checkIfQueensCanAttackEachOther(this.white, this.black)
    }

    toString(): string {
        return this.board
    }

    canAttack(): boolean {
        return this.canAttackEachOther
    }

    private createBoard(white: number[], black: number[]): string {
        return [...Array(ROWS_IN_CHESSBOARD).keys()].map((_row, _rowIndex) => {

            return [...Array(COLUMNS_IN_CHESSBOARD).keys()].map((_column, _columnIndex) => {

                if (white[0] === _rowIndex && white[1] === _columnIndex)
                    return 'W'

                if (black[0] === _rowIndex && black[1] === _columnIndex)
                    return 'B'

                return '_'

            }).join(' ')

        }).join('\n') + '\n'
    }

    private checkIfQueensCanAttackEachOther(_white: number[], _black: number[]): boolean {

        // Queens in the same row or column can attack each other.
        if (_white[0] === _black[0] || _white[1] === _black[1])
            return true

        return this.canQueensAttackEachOtherDiagonally(_white, _black)

    }

    private canQueensAttackEachOtherDiagonally(_white: number[], _black: number[]): boolean {

        return this.canQueensAttackInDirection(_white, _black, DiagonalDirection.UpAndToTheLeft) ||
            this.canQueensAttackInDirection(_white, _black, DiagonalDirection.UpAndToTheRight) ||
            this.canQueensAttackInDirection(_white, _black, DiagonalDirection.DownAndToTheLeft) ||
            this.canQueensAttackInDirection(_white, _black, DiagonalDirection.DownAndToTheRight)

    }

    private canQueensAttackInDirection(attackingQueen: number[], targetQueen: number[], direction: DiagonalDirection): boolean {

        if (this.occupySameSquare(attackingQueen, targetQueen))
            return true

        if (direction === DiagonalDirection.UpAndToTheLeft &&
            attackingQueen[0] > 0 &&
            attackingQueen[1] > 0)
            return this.canQueensAttackInDirection([attackingQueen[0] - 1, attackingQueen[1] - 1], targetQueen, direction)

        if (direction === DiagonalDirection.UpAndToTheRight &&
            attackingQueen[0] > 0 &&
            attackingQueen[1] < (COLUMNS_IN_CHESSBOARD - 1))
            return this.canQueensAttackInDirection([attackingQueen[0] - 1, attackingQueen[1] + 1], targetQueen, direction)


        if (direction === DiagonalDirection.DownAndToTheLeft &&
            attackingQueen[0] < (ROWS_IN_CHESSBOARD - 1) &&
            attackingQueen[1] > 0)
            return this.canQueensAttackInDirection([attackingQueen[0] + 1, attackingQueen[1] - 1], targetQueen, direction)

        if (direction === DiagonalDirection.DownAndToTheRight &&
            attackingQueen[0] < (ROWS_IN_CHESSBOARD - 1) &&
            attackingQueen[1] < (COLUMNS_IN_CHESSBOARD - 1))
            return this.canQueensAttackInDirection([attackingQueen[0] + 1, attackingQueen[1] + 1], targetQueen, direction)

        return false

    }

    private occupySameSquare(queenA: number[], queenB: number[]): boolean {
        return queenA[0] === queenB[0] && queenA[1] === queenB[1]
    }

}