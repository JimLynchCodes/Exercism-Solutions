/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    const RADIX: u32 = 10;
    let mut luhn_sum = 0;
    let mut chars_in_code = 0;

    for (i, x) in code
        .split_whitespace()
        .collect::<String>()
        .chars()
        .rev()
        .enumerate()
    {
        chars_in_code = i + 1;

        let x_int = match x.to_digit(RADIX) {
            Some(x) => x,
            None => return false,
        };

        if (i + 1) % 2 != 0 {
            luhn_sum += x_int;
        } else {
            let x_doubled = x_int * 2;
            let val_to_add = if x_doubled > 9 {
                x_doubled - 9
            } else {
                x_doubled
            };
            luhn_sum += val_to_add;
        }
    }

    luhn_sum % 10 == 0 && (luhn_sum > 0 || chars_in_code > 1)
}
