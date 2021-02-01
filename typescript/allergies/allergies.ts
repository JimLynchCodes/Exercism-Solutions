const IGNORED = 'ignored'

enum Allergens {
    ignored = 256,
    cats = 128,
    pollen = 64,
    chocolate = 32,
    tomatoes = 16,
    strawberries = 8,
    shellfish = 4,
    peanuts = 2,
    eggs = 1
}

export default class Allergies {

    constructor(private allergyScore: number, private allergies: string[] = []) {
        this.list()
    }

    allergicTo(food: string): boolean {
        return this.allergies.indexOf(food) >= 0
    }

    list(): string[] {

        for (let allergyName in Allergens) {

            if (this.allergyScore >= +Allergens[allergyName] &&
                this.allergies.indexOf(allergyName) < 0) {

                this.allergies.unshift(allergyName)
                this.allergyScore -= +Allergens[allergyName]

                this.list()
            }

        }

        return this.allergies.filter(allergy => allergy !== IGNORED)
    }

}