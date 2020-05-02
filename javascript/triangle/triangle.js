const SIDES_ON_A_TRIANGLE = 3

export class Triangle {

  constructor(sideA, sideB, sideC) {
    this.validInput = this.validateInput(sideA, sideB, sideC)

    this.matchingSides = 0

    if (sideA === sideB && sideA === sideC)
      this.matchingSides = SIDES_ON_A_TRIANGLE

    else if (sideA === sideB || sideA === sideC || sideB === sideC)
      this.matchingSides = SIDES_ON_A_TRIANGLE - 1

  }

  isEquilateral() {
    return this.matchingSides === 3 && this.validInput
  }

  isIsosceles() {
    return this.matchingSides >= 2 && this.validInput
  }

  isScalene() {
    return this.matchingSides === 0 && this.validInput
  }

  isAPoint(sideA, sideB, sideC) {
    return sideA === 0 && sideB === 0 && sideC === 0
  }

  isALine(sideA, sideB, sideC) {
    const sorted = [sideA, sideB, sideC].sort()
    return sorted[0] + sorted[1] < sorted[2]
  }

  validateInput(sideA, sideB, sideC) {
    return !this.isAPoint(sideA, sideB, sideC) && !this.isALine(sideA, sideB, sideC)
  }

}
