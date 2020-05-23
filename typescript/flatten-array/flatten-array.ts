import { isArray } from "util"

type T = number | undefined | Array<number | undefined | Array<any>>

const flatten = (input: Array<T>): T => {

    return input.map((item: T) => {

        if (isArray(item))
            return flatten(item.flat())

        return item

    }).flat()
        .filter(item => item !== undefined)

}

export default {
    flatten
}
