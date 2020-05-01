
export const steps = (num, stepCount) => {

  if (num <= 0)
    throw new Error('Only positive numbers are allowed')

  if (stepCount === undefined)
    stepCount = 0

  if (num === 1) {
    return stepCount
  }

  const isOdd = num % 2 === 1

  if (isOdd)
    return steps(3 * num + 1, ++stepCount)

  if (!isOdd)
    return steps(num / 2, ++stepCount)

};
