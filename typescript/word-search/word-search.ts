
type IWordPositionsMap = { [key: string]: string | undefined }

export default class WordSearch {

  constructor(private readonly grid: string[]) {

  }

  public find(words: string[]) {
    console.log(words)

    return words.reduce((acc: IWordPositionsMap, row: string) => {

      console.log('row: ', row)
      acc[row] = undefined

      

      return acc

    }, {})

  }

}
