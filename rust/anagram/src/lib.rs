use std::collections::HashMap;
use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &'a [&str]) -> HashSet<&'a str> {
    let mut found = HashSet::new();

    let target_character_hashes: HashMap<String, i32> = get_character_counts(&word.to_lowercase());

    for input in possible_anagrams {
        let character_counts = get_character_counts(&input.to_lowercase());

        if character_counts == target_character_hashes
            && input.to_lowercase() != word.to_lowercase()
        {
            found.insert(*input);
        }
    }

    found
}

fn get_character_counts(word: &str) -> HashMap<String, i32> {
    let mut map: HashMap<String, i32> = HashMap::new();

    for char in word.chars() {
        match &map.get(&char.to_string()) {
            Some(_) => map.insert(char.to_string(), (&map.get(&char.to_string())).unwrap() + 1),
            None => map.insert(char.to_string(), 1),
        };
    }

    map
}
