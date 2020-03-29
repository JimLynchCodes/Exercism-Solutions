
const ALPHABET_LENGTH = 26

export const isPangram = (str) => {

  const lowercase = str.toLowerCase()
  const lettersOnly = lowercase.match(/[a-zA-Z]+/g) || []
  const uniques = new Set(lettersOnly.join(''))

  return uniques.size === ALPHABET_LENGTH
}
