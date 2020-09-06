export default class Raindrops {

    convert(_drops: number): string {

        let sound = ''

        if (_drops % 3 === 0) {
            sound += 'Pling'
        }

        if (_drops % 5 === 0) {
            sound += 'Plang'
        }
        
        if (_drops % 7 === 0) {
            sound += 'Plong'
        }

        if (sound === '') {
            return _drops.toString();
        } 
        
        return sound

    }

}