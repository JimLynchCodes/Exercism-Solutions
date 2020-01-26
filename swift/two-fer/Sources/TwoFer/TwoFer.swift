
// Works because the filename is TwoFer?
func twoFer(name: String = "you") -> String {
    "One for \(name), one for me."
} 

// Both of these work as well if I change the name to "TwoFer".
// Wondering if there is any real difference under the hood?
struct TwoFer2 {
    static func twoFer(name: String = "you") -> String {
            "One for \(name), one for me."
        } 
}

class TwoFer3 {
    static func twoFer(name: String = "you") -> String {
            return "One for \(name), one for me."
        } 
}