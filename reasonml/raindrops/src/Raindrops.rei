let raindrops: (n: int): string => {
    let output = (n mod 3 == 0 ? "Pling" : "")
        ++ (n mod 5 == 0 ? "Plang" : "")
        ++ (n mod 7 == 0 ? "Plong" : "");

    output == "" ? string_od_int(n) : output
};
