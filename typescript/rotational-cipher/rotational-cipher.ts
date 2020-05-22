
const LETTERS_OF_ALPHABET = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
]

const UPPERCASE_LETTERS_OF_ALPHABET = LETTERS_OF_ALPHABET.map(letter => letter.toUpperCase())

export default {
    rotate: (originalCipher: string, rotationAmount: number) => {

        return originalCipher.split('').map(char => {

            const lowercaseIndex = LETTERS_OF_ALPHABET.indexOf(char)

            const uppercaseIndex = UPPERCASE_LETTERS_OF_ALPHABET.indexOf(char)

            const originalIndex = lowercaseIndex > uppercaseIndex ? lowercaseIndex : uppercaseIndex

            if (originalIndex === -1)
                return char

            const newIndex = (originalIndex + rotationAmount) % LETTERS_OF_ALPHABET.length

            const newChar = LETTERS_OF_ALPHABET[newIndex]

            if (uppercaseIndex >= 0)
                return newChar.toUpperCase()

            return newChar

        }).join('')

    }

}