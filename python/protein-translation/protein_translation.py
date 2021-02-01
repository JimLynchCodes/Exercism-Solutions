import re

# Protein constants
METHIONINE = 'Methionine'
PHENYLALANINE = 'Phenylalanine'
LEUCINE = 'Leucine'
SERINE = 'Serine'
TYROSINE = 'Tyrosine'
CYSTEINE = 'Cysteine'
TRYPTOPHAN = 'Tryptophan'
STOP = 'STOP'

codon_to_protein = {
    'AUG': METHIONINE,
    'UUU': PHENYLALANINE,
    'UUC': PHENYLALANINE,
    'UUA': LEUCINE,
    'UUG': LEUCINE,
    'UCU': SERINE,
    'UCC': SERINE,
    'UCA': SERINE,
    'UCG': SERINE,
    'UAU': TYROSINE,
    'UAC': TYROSINE,
    'UGC': CYSTEINE,
    'UGU': CYSTEINE,
    'UGG': TRYPTOPHAN,
    'UAA': STOP,
    'UAG': STOP,
    'UGA': STOP,
}


def proteins(strand):

    # Split strand into 3 character chunks called "codons".
    codons = re.findall('.{1,3}', strand)

    result = []

    for codon in codons:

        if (codon_to_protein[codon] == STOP):
            return result

        result.append(codon_to_protein[codon])

    return result
