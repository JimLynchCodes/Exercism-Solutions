<?php

function distance(string $strandA, string $strandB): int
{
    if (strlen($strandA) !== strlen($strandB)) {
        throw new InvalidArgumentException("DNA strands must be of equal length.");
    }

    return count(array_diff_assoc(str_split($strandA), str_split($strandB)));

}
