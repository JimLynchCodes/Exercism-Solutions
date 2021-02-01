
const ALPHABET_LENGTH = 26

export const isPangram = (str) => {

  const lowercase = str.toLowerCase()
  const lettersOnly = lowercase.match(/[a-z]/g)
  const uniques = new Set(lettersOnly)

  return uniques.size === ALPHABET_LENGTH
}
