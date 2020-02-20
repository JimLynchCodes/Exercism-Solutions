export default class Triangle {

    sides: number[]

    constructor(...sides: number[]) {
        this.sides = sides
    }

    private validateLegalInput(sides: number[]) {

        if (sides.some( (s) => s < 0))
            throw new Error('negative measurements are not allowed.')
    
        if (sides.every( (s) => s === 0))
            throw new Error('all sides cannot be 0.')
    
        const sorted = sides.sort((a, b) => (a - b))

        if (sorted[0] + sorted[1] < sorted[2])
            throw new Error('triangle violates triangle inequality rule.')
    }

    kind() {

        this.validateLegalInput(this.sides)

        const uniqueItems = Array.from(new Set(this.sides))

        if (uniqueItems.length === 1)
            return "equilateral"

        if (uniqueItems.length === 2) {
            return "isosceles"
        }

        return "scalene"
    }
}