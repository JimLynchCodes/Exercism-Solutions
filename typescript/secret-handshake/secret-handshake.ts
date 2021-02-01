const BINARY_ONE = '1'

const COMMANDS = ['wink', 'double blink', 'close your eyes', 'jump']

export default class Handshake {

    constructor(private readonly input: number) { }

    commands() {

        const commands: string[] = []

        const binaryVersionOfInput = this.input.toString(2)

        binaryVersionOfInput
            .split('')
            .reverse()
            .forEach( (num, numIndex) => {

                if (num === BINARY_ONE) {

                    const commandForIndex = COMMANDS[numIndex]

                    if (commandForIndex)
                        commands.push(commandForIndex)
                    
                    else
                        commands.reverse()
                }

            })
            
            return commands
            
        }
        
    }