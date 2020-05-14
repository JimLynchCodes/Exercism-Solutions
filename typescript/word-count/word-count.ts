export default class Words {

    count(input: string): object {

        const entries = new Map()

        input
            .toLowerCase()
            .trim()
            .split(/[ \t\n]+/)
            .map((word: string) => {
                const lowercaseWord = word
                entries.set(lowercaseWord, entries.get(lowercaseWord) + 1 || 1)
            })

        return entries
    }

}