
export const keep = <T>(numbers: T[], predicate: (e: T) => Boolean): T[] => {

    return numbers.filter(num => {
        return predicate(num)
    })

}

export const discard = <T>(numbers: T[], predicate: (e: T) => Boolean): T[] => {

    return numbers.filter(num => {
        return !predicate(num)
    })

}