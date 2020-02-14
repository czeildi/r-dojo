library("stringr")

lastDigit <- function(input) {
    if (length(input) == 1) {
        input %% 10
    } else {
        reduced_input <- reduceByLastExponentLevel(input)
        lastDigit(reduced_input)
    }
}

reduceByLastExponentLevel <- function(input) {
    n <- length(input)
    base <- input[n - 1] %% 10
    exponent <- input[n]
    
    simplified_exponent <- simplifyExponent(base, exponent)
    last_exponent <- base ^ simplified_exponent
    
    c(
        input[seq_len(n - 2)],
        last_exponent
    )
}

simplifyExponent <- function(base_digit, exponent) {
    cycle <- getCycleOfExponents(base_digit)
    # remainder, but divisor instead of 0
    (exponent %% length(cycle)) + length(cycle)
}

getCycleOfExponents <- function(digit) {
    list(
        c(0),
        c(1),
        c(2, 4, 8, 6),
        c(3, 9, 7, 1),
        c(4, 6),
        c(5),
        c(6),
        c(7, 9, 3, 1),
        c(8, 4, 2, 6),
        c(9, 1)
    )[[digit + 1]]
}
