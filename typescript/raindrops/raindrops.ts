
const soundMap = {
    3: 'Pling',
    5: 'Plang',
    7: 'Plong',
}

export default class Raindrops {

    convert(drops: number): string {

        let sound = ''

        Object.entries(soundMap).forEach(([key, val]) => {
            if (drops % +key === 0)
                sound += val
        })

        return sound || drops.toString()

    }

}