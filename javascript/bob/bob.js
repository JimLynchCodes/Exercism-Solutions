
export function hey(message) {

  const trimmedMessage = message.trim()
  const isEmpty = trimmedMessage === ''
  const hasLetter = /[A-Za-z]/.test(message)
  const isAllUppercase = message.toUpperCase() === message
  const isYelled = hasLetter & isAllUppercase
  const isQuestion = trimmedMessage.substr(trimmedMessage.length - 1) === '?'

  if (isEmpty)
    return 'Fine. Be that way!'

  if (isYelled && isQuestion)
    return 'Calm down, I know what I\'m doing!'

  if (isYelled)
    return 'Whoa, chill out!'

  if (isQuestion)
    return 'Sure.'

  return 'Whatever.'
}
