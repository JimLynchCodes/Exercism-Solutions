
let rows
let columns

export class Matrix {

  constructor(inputString) {

    rows = inputString.split("\n").map(row => {

      const cells = row.split(' ')

      return cells.map(cell => {
        return parseInt(cell)
      })

    })

    columns = []

    rows.forEach((row) => {

      row.forEach((cell, i) => {

        if (columns[i] === undefined)
          columns[i] = []

        columns[i].push(cell)

      })

    })

  }

  get rows() {
    return rows
  }

  get columns() {
    return columns
  }

}
