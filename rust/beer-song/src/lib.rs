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

pub fn verse(num: u32) -> String {
    format!(
        "{}\n{}\n",
        beers_on_wall_sentence(num),
        take_down_sentence(num)
    )
}

fn beers_on_wall_sentence(num: u32) -> String {
    match num {
        0 => "No more bottles of beer on the wall, no more bottles of beer.".to_string(),
        1 => "1 bottle of beer on the wall, 1 bottle of beer.".to_string(),
        _ => format!(
            "{} bottles of beer on the wall, {} bottles of beer.",
            num, num
        ),
    }
}

fn take_down_sentence(num: u32) -> String {
    match num {
        0 => "Go to the store and buy some more, 99 bottles of beer on the wall.".to_string(),
        1 => "Take it down and pass it around, no more bottles of beer on the wall.".to_string(),
        _ => format!(
            "Take one down and pass it around, {} bottle{} of beer on the wall.",
            num - 1,
            if num == 2 { "" } else { "s" }
        ),
    }
}
