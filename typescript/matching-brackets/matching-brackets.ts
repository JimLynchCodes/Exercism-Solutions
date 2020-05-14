const OPENING_BRACKETS = [
    '[', '{', '('
]

const CLOSING_BRACKETS = [
    ']', '}', ')'
]

export default class MatchingBrackets {

    paired = true

    constructor(private str: string) {

        const openedScopes: any = []

        this.str.split('').map((char: string) => {

            if (OPENING_BRACKETS.includes(char))

                openedScopes.push(char)


            if (CLOSING_BRACKETS.includes(char)) {

                const indexOfLastOpener = OPENING_BRACKETS.indexOf(openedScopes[openedScopes.length - 1])

                const indexOfCloser = CLOSING_BRACKETS.indexOf(char)

                const matchingCloser = indexOfLastOpener === indexOfCloser

                if (matchingCloser)

                    openedScopes.pop()

                else
                
                    this.paired = false;

            }

        })

        this.paired = this.paired && openedScopes.length === 0

    }

    isPaired() {
        return this.paired
    }

}