pancake_sort <- function(input) {
    if (length(input) < 2) {
        return(input)
    }
    
    if (isFirstTheSmallest(input)) {
        c(input[1], pancake_sort(input[2:length(input)]) )
    } else {
        pancake_sort(shift_right(input))
    }
}

isFirstTheSmallest <- function(input) {
    input[1] == min(input)
}

shift_right <- function(input) {
    pancake_flip(input, length(input)) %>% 
        pancake_flip(., length(input) - 1) 
}

pancake_flip <- function(input, n) {
    len <- length(input)
    if (n == len) 
        rev(input)
    else
        c(input[1:(len - n)], rev(input[(len - n + 1):len]))
}
