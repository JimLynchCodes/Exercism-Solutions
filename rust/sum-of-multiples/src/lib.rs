pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    let mut sum = 0;

    for num in 1..limit {
        for factor in factors {
            
            // Dereferencing to work with u32 directly
            let factor = *factor;

            if factor <= limit && evenly_divides(num, factor) {
                sum += num;
                break;
            }
        }
    }

    sum
}

#[inline]
fn evenly_divides(num: u32, factor: u32) -> bool {
    if factor == 0 {
        return false;
    }

    num % factor == 0
}
