

class Transcriptor {

    private DNA_TO_RNA: { [_: string]: string } = {
        'G': 'C',
        'C': 'G',
        'T': 'A',
        'A': 'U'
    };

    toRna(dna: string): any {

        return dna.split('').map(neucleotide => {

            if (!this.DNA_TO_RNA[neucleotide])
                throw new Error('Invalid input DNA.')

            return this.DNA_TO_RNA[neucleotide]

        }).join('')
    }

}

export default Transcriptor