pub fn build_proverb(list: &[&str]) -> String {
    let mut lyrics = String::new();

    let first = match list.get(0) {
        Some(first) => first,
        None => return lyrics,
    };

    let last_idx = list.len() - 1;

    for (idx, current) in list.iter().enumerate() {
        if idx < last_idx {
            // PANIC: This won't panic because the line above checks we aren't at or past
            // the last index, so we're guaranteed not to index out of bounds.
            let next = list[idx + 1];

            lyrics += &format!("For want of a {} the {} was lost.\n", current, next);
        } else {
            lyrics += &format!("And all for the want of a {}.", first);
        }
    }

    lyrics
}
