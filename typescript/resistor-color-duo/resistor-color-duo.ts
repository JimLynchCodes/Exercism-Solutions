export class ResistorColor {
  
  private readonly Colors: string[] = ["black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"]
  
  private readonly MINIMUM_ACCEPTABLE_COLORS = 2;

  constructor(private colors: string[]) {
    
    if (colors.length < this.MINIMUM_ACCEPTABLE_COLORS)
      throw "At least two colors need to be present";

  }

  value = (): number => {
    
    const result = this.colors
      .slice(0, this.MINIMUM_ACCEPTABLE_COLORS)
      .reduce(
        (accumulator: string, current: string) =>
          `${accumulator}${this.Colors.indexOf(current)}`,
          ''
      );

    return parseInt(result);

  }

}