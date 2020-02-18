//Solution goes in Sources

struct Raindrops {
    var sounds: String = ""
    init(_ n: Int) {

        if n % 3 == 0 {
            sounds += "Pling"
        }

        if n % 5 == 0 {
            sounds += "Plang"
        }

        if n % 7 == 0 {
            sounds += "Plong"
        }

        if (sounds == "") {
            sounds = String(n)
        }
    }
}