export class Triangle {

  constructor(a, b, c) {
    this.validInput = this.validateInput(a, b, c)
    this.uniqueSides = new Set([a, b, c])
  }

  isEquilateral() {
    return this.uniqueSides.size === 1 && this.validInput
  }

  isIsosceles() {
    return this.uniqueSides.size <= 2 && this.validInput
  }

  isScalene() {
    return this.uniqueSides.size === 3 && this.validInput
  }

  validateInput(a, b, c) {

    if (a === 0 && b === 0 && c == 0)
      return false

    const sorted = [a, b, c].sort()

    if (sorted[0] + sorted[1] < sorted[2])
      return false

    return true 

  }

}
