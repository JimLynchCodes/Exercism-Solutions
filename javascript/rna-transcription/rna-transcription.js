const DNAtoRNA = {
  'C': 'G',
  'G': 'C',
  'T': 'A',
  'A': 'U'
}

export const toRna = (dna) => {
  return dna.split('').map(char => {
    return DNAtoRNA[char]
  }).join('')
};
