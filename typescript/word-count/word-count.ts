export default class Words {

    count(input: string): object {

        const entries = new Map()

        input
            .toLowerCase()
            .trim()
            .split(/[ \t\n]+/)
            .forEach((word: string) => {
                entries.set(word, entries.get(word) + 1 || 1)
            })

        return entries
    }

}