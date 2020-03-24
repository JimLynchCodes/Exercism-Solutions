
export const isPangram = (s) => {

  const lowercase = s.toLowerCase()
  const lettersOnly = lowercase.match(/[a-z]+/g) || []
  const uniques = new Set(lettersOnly.join(''))

  return uniques.size === 26
};
