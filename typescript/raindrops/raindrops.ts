export default class Raindrops {

    convert(drops: number): string {

        let sound = ''

        if (drops % 3 === 0) {
            sound += 'Pling'
        }

        if (drops % 5 === 0) {
            sound += 'Plang'
        }
        
        if (drops % 7 === 0) {
            sound += 'Plong'
        }

        return sound || drops.toString()

    }

}