import {Memoize} from 'typescript-memoize';

const OPENING_BRACKETS = ['[', '{', '(']

const CLOSING_BRACKETS = [']', '}', ')']

export default class MatchingBrackets {

    paired: boolean

    constructor(private str: string) {

        this.paired = this.calculatePaired()
        
        this.paired = this.calculatePaired()

    }

    isPaired() {
        return this.paired
    }
    
    @Memoize()
    calculatePaired() {

        console.log('running...')

        const openedScopes: string[] = []

        this.str.split('').forEach((char: string) => {

            if (OPENING_BRACKETS.includes(char))

                openedScopes.push(char)


            else if (CLOSING_BRACKETS.includes(char)) {

                const indexOfLastOpener = OPENING_BRACKETS.indexOf(openedScopes[openedScopes.length - 1])

                const indexOfCloser = CLOSING_BRACKETS.indexOf(char)

                const matchingCloser = indexOfLastOpener === indexOfCloser

                if (matchingCloser)

                    openedScopes.pop()

                else

                    return false;

            }

        })

        return this.paired && openedScopes.length === 0

    }

}