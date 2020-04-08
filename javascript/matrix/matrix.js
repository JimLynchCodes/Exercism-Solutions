//
// This is only a SKELETON file for the 'Matrix' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

let rows

export class Matrix {

  constructor(inputString) {

    rows = inputString.split("\n").map(row => {

      const match = row.match(/\S/g);

      return match.map(char => {
        return parseInt(char)
      });

    });

  }

  get rows() {

    return rows
  }

  get columns() {
    return 'foo'
  }
}
