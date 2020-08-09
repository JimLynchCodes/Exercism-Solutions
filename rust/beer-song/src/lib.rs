pub fn verse(num: u32) -> String {
    match num {
        0 => "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n".to_string(),
        1 => "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n".to_string(),
        2 => format!("{num} bottles of beer on the wall, {num} bottles of beer.\nTake one down and pass it around, {numMinusOne} bottle of beer on the wall.\n", num= num, numMinusOne= num-1),
        _ => format!("{num} bottles of beer on the wall, {num} bottles of beer.\nTake one down and pass it around, {numMinusOne} bottles of beer on the wall.\n", num= num, numMinusOne= num-1),
    }
}

pub fn sing(start: u32, end: u32) -> String {
    let mut song = "".to_string();

    for index in (end..=start).rev() {
        song.push_str(&verse(index));

        if index != end {
            song.push_str("\n");
        }
    }

    song
}
