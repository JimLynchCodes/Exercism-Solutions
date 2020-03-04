
class Gigasecond {

    private readonly GIGASECOND: number = 1e12

    constructor(private inputDate: Date) { }

    date(): Date {
        return new Date(this.inputDate.getTime() + this.GIGASECOND)
    }

}

export default Gigasecond
