
export const steps = (n, stepsTaken) => {

  if (n <= 0)
    throw new Error('Only positive numbers are allowed')

  if (!stepsTaken)
    stepsTaken = 0

  if (n === 1)
    return stepsTaken

  const isEven = n % 2 === 0

  if (isEven)
    return steps(n / 2, ++stepsTaken)

  else
    return steps(n * 3 + 1, ++stepsTaken)

}

