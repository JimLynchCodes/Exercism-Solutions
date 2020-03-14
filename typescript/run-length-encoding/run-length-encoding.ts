export default {
    encode: (s: string): string => {

        let current
        let count = 1
        let previous = s[0]
        let encoded = ''

        for (let i = 1; i <= s.length; i++) {
            current = s[i]
            if (current === previous)
                count++
            else {
                encoded += (count == 1 ? '' : count) + previous
                count = 1
                previous = current
            }
        }

        return encoded
    },
    decode: (s: string): string => {

        const numbers = s.match(/[0-9]+/g)

        numbers?.forEach((n: string) => {
            const indexOfNum = s.indexOf(n)
            s = s.replace(n, s[indexOfNum + n.length].repeat(parseInt(n) - 1))
        })

        return s
    }
}
