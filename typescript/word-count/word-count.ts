export default class Words {

    count(input: string): object {

        return input
            .toLowerCase()
            .split(/[ \t\n]+/)
            .reduce((acc: Map<string, number>, word: string) => {
                return acc.set(word, (acc.get(word) || 0) + 1)
            }, new Map())

    }

}