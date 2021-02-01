return input
            .toLowerCase()
            .trim()
            .split(/[ \t\n]+/)
            .reduce((acc: Map<string, number>, word: string) => {
                return acc.set(word, entries.get(word) + 1 || 1)
            }, new Map())