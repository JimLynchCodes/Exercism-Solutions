export default class Words {

    count(input: String): object {

        const entries = new Map()

        input.trim().split(/[ \t\n]+/).map((word: string) => {
            const lowercaseWord = word.toLowerCase()
            entries.set(lowercaseWord, entries.get(lowercaseWord) + 1 || 1)
        })

        return entries
    }

}