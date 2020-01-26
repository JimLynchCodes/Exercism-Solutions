export const decodedValue = ([...args]) => {

  return parseInt(args.slice(0, 2).map(arg => {

    return COLORS.indexOf(arg).toString();

  }).join(''))

};

export const COLORS = ['black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet', 'grey', 'white'];
