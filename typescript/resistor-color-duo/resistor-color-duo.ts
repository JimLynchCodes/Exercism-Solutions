export class ResistorColor {
  private Colors: string[] = ["black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"]

  constructor(private colors: string[]) {
    if (colors.length < 2) throw "At least two colors need to be present"
  }
  value = (): number => {
    const color1: number = this.Colors.indexOf(this.colors[0])
    const color2: number = this.Colors.indexOf(this.colors[1])
    return color1 * 10 + color2
  }

}