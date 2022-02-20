use std::collections::HashSet;
pub fn check(input: &str) -> bool {
    let mut s = HashSet::new();
    input
        .to_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic())
        .all(|c| s.insert(c))
}