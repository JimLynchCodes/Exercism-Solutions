use std::collections::HashMap;

pub fn check(candidate: &str) -> bool {
    let mut letters = HashMap::<char, bool>::new();

    let lowercase_candidate: String = candidate.to_lowercase();
    let candidate_no_spaces: String = lowercase_candidate.split_whitespace().collect();
    let candidate_no_dashes: String = candidate_no_spaces.replace("-", "");
    let vec_of_candidate_chars: Vec<char> = candidate_no_dashes.chars().collect();

    for letter in vec_of_candidate_chars {
        
        match letters.get(&letter) {
            Some(_) => { return false },
            None => { letters.insert(letter, true); }
        }
    }
    true
}
