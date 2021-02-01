
const BOMB = '*'

export default class Minesweeper {

    annotate(boardOfBombs: string[]): string[] {

        const annotatedBoard: string[] = boardOfBombs.map(((row: string, rowIndex: number) => {

            return row.split('').map((cell: string, columnIndex: number) => {

                if (cell === BOMB)
                    return cell

                const bombsAroundCell = checkForBombsAroundCell(boardOfBombs, rowIndex, columnIndex)

                return bombsAroundCell === 0 ? ' ' : JSON.stringify(bombsAroundCell)

            }).join('')

        }))

        return annotatedBoard

    }

}

/**
 *  Counts the number of bombs around a given row index and column (and only checks valid indices for cells on the edge of the board)
 */
const checkForBombsAroundCell = (boardOfBombs: string[], rowIndex: number, columnIndex: number): number => {

    const columnLength = boardOfBombs.length
    const rowLength = boardOfBombs[0].length

    let numberOfBombsAroundCell = 0

    // upper-left cell
    if (rowIndex > 0 &&
        columnIndex > 0 &&
        boardOfBombs[rowIndex - 1][columnIndex - 1] === BOMB)
        ++numberOfBombsAroundCell

    // cell above
    if (rowIndex > 0 &&
        boardOfBombs[rowIndex - 1][columnIndex] === BOMB)
        ++numberOfBombsAroundCell

    // upper-right cell
    if (rowIndex > 0 &&
        columnIndex < (rowLength - 1) &&
        boardOfBombs[rowIndex - 1][columnIndex + 1] === BOMB)
        ++numberOfBombsAroundCell

    // cell to the left
    if (columnIndex > 0 &&
        boardOfBombs[rowIndex][columnIndex - 1] === BOMB)
        ++numberOfBombsAroundCell

    // cell to the right
    if (columnIndex < (rowLength - 1) &&
        boardOfBombs[rowIndex][columnIndex + 1] === BOMB)
        ++numberOfBombsAroundCell

    // bottom-left cell
    if (rowIndex < (columnLength - 1) &&
        columnIndex > 0 &&
        boardOfBombs[rowIndex + 1][columnIndex - 1] === BOMB)
        ++numberOfBombsAroundCell

    // cell below
    if (rowIndex < (columnLength - 1) &&
        boardOfBombs[rowIndex + 1][columnIndex] === BOMB)
        ++numberOfBombsAroundCell

    // bottom-right cell
    if (rowIndex < (columnLength - 1) &&
        columnIndex < (columnLength - 1) &&
        boardOfBombs[rowIndex + 1][columnIndex + 1] === BOMB)
        ++numberOfBombsAroundCell

    return numberOfBombsAroundCell

}
