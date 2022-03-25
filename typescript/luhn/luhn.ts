export function valid(digits: string): boolean {

  digits = digits.replace(/\s+/g, '');

  if (digits.length <= 1)
    return false;

  const doubledSecondDigits = doubleSecondDigitsFunctionally(digits);

  const sumOfDigits = doubledSecondDigits.reduce((prev, acc) => acc + prev);

  return sumOfDigits % 10 === 0;
}

function doubleSecondDigits(digits: string): number[] {

  const newDigits = [];

  for (let i = digits.length - 1; i >= 0; i--) {

    let newDigit = +digits[i];

    if (i % 2 !== 0) {
      const doubledDigit = newDigit * 2;
      newDigit = doubledDigit < 10 ? doubledDigit : doubledDigit - 9;
    }

    newDigits.unshift(newDigit);
  }

  return newDigits;
}

function doubleSecondDigitsFunctionally(digits: string): number[] {

  return digits.split('').reverse().map((digit, i) => {

    if (i % 2 !== 0) {
      const doubledDigit = +digit * 2;
      return doubledDigit < 10 ? doubledDigit : doubledDigit - 9;
    }

    return +digit;
  })

}
