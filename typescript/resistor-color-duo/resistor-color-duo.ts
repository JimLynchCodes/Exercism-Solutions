export class ResistorColor {
  readonly Colors: string[] = ["black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"]
  readonly MINIMUM_ACCEPTABLE_COLORS = 2;

  constructor(private colors: string[]) {
    if (colors.length < this.MINIMUM_ACCEPTABLE_COLORS) throw "At least two colors need to be present"
    this.colors = colors.slice(0, 2);
  }

  value = (): number => {

    const result = this.colors.reduce(
      (accumulator: string, current: string) =>
        `${accumulator}${this.Colors.indexOf(current)}`,
      ""
    );

    return parseInt(result);

  }

}