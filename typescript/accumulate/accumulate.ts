
type T = number | string

const accumulate = (collection: Array<T>, accumulatorFn: (e: T) => T): Array<T> => {

    let newCollection: Array<T> = []

    for (let i = 0; i < collection.length; i++) {

        newCollection.push(accumulatorFn(collection[i]))

    }

    return newCollection

}

export default accumulate