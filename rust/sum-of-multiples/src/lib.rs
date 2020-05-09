pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    
    println!(
        "Sum the multiples of all of {:?} which are less than {}",
        factors, limit
    );

    let mut sum: u32 = 0;

    for i in 1..limit {
        println!("i is {}", i);
        // println!("facs is {}", factors.);

        for f in factors.iter() {
            println!("operator: {} and {}", f, i);
            println!("the mod: {}", i % f);

            // Limit must be higher than factor to be considered a "multiple".
            if &limit >= f && 
            // divides evenly means it's a multiple
            i % f == 0 &&
            // allow zero as a factor but basically ignore it.
            f != 0
            {
                sum += i;
                println!("added to sum, new num: {}", sum);
                break;
            }

            println!("new sum: {}", sum);
        }
    }

    sum
}
