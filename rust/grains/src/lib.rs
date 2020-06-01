const SQUARES_IN_CHESSBOARD: u32 = 64;

pub fn square(s: u32) -> u64 {
    // unimplemented!("grains of rice on square {}", s);

    if s < 1 || s > SQUARES_IN_CHESSBOARD {
        panic!("Square must be between 1 and 64")
    }

    2u64.pow(s - 1)
}

pub fn total() -> u64 {
    let total_squares = 0..SQUARES_IN_CHESSBOARD;

    return total_squares.fold(0, |mut sum, val| {
        let grains_of_sand_for_square = 2u64.pow(val);

        sum += grains_of_sand_for_square;

        sum
    });

}
