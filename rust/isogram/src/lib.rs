use std::collections::HashMap;

pub fn check(candidate: &str) -> bool {
    let mut letters = HashMap::<char, bool>::new();

    let clean_candidate_characters: Vec<char> = candidate
        .to_lowercase()
        .split_whitespace()
        .collect::<String>()
        .replace("-", "-")
        .chars()
        .collect();

    for letter in clean_candidate_characters {
        match letters.get(&letter) {
            Some(_) => return false,
            None => {
                letters.insert(letter, true);
            }
        }
    }
    true
}
