
enum Color {
  black = 0,
  brown = 1,
  red = 2,
  orange = 3,
  yellow = 4,
  green = 5,
  blue = 6,
  violet = 7,
  grey = 8,
  white = 9
}

export class ResistorColor {

  private readonly MINIMUM_ACCEPTABLE_COLORS = 2;

  constructor(private colors: string[]) {

    if (colors.length < this.MINIMUM_ACCEPTABLE_COLORS)
      throw "At least two colors need to be present"

  }

  value = (): number => {

    const result = (this.colors as unknown as Color[])
      .slice(0, this.MINIMUM_ACCEPTABLE_COLORS)
      .reduce(
        (accumulator: string, color: Color) =>
          `${accumulator}${Color[color]}`,
        ''
      );

    return parseInt(result)

  }

}