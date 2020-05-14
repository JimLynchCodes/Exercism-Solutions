export default {
    steps(num: number, currentSteps = 0): number {

        if (num <= 0)
            throw new Error('Only positive numbers are allowed')

        if (num === 1)
            return currentSteps

        if (num % 2 === 0)
            return this.steps(num / 2, ++currentSteps)

        return this.steps(num * 3 + 1, ++currentSteps)

    }

}
