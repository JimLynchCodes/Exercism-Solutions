
export class Matrix {

  constructor(inputString) {

    this._rows = inputString.split("\n").map(row => {

      const cells = row.split(' ')

      return cells.map(cell => {
        return parseInt(cell)
      })

    })

    this._columns = []

    this._rows.forEach((row) => {

      row.forEach((cell, i) => {

        if (this._columns[i] === undefined)
          this._columns[i] = []

        this._columns[i].push(cell)

      })

    })

  }

  get rows() {
    return this._rows
  }

  get columns() {
    return this._columns
  }

}
