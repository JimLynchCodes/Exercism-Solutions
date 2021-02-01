pub fn raindrops(n: u32) -> String {
    
    let mut raindrops_string = String::new();

    if n % 3 == 0 {
        raindrops_string += "Pling";
    }

    if n % 5 == 0 {
        raindrops_string += "Plang";
    }

    if n % 7 == 0 {
        raindrops_string += "Plong";
    }

    if raindrops_string == "" {
        return String::from(n.to_string());
    }

    String::from(raindrops_string)
}
