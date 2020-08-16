const MULTIPLIER_PER_SQUARE: u64 = 2;
const TOTAL_NUMBER_OF_SQUARES: u32 = 64;

pub fn square(s: u32) -> u64 {
    if s < 1 || s > 64 {
        panic!("Square must be between 1 and 64");
    }

    let base: u64 = MULTIPLIER_PER_SQUARE;
    base.pow(s - 1)
}

pub fn total() -> u64 {
    (1..=TOTAL_NUMBER_OF_SQUARES).map(square).sum()
}
