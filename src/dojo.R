library("stringr")
library("data.table")

roman_digits <- data.table(
    arabic = c(1, 4, 5),
    roman = c("I", "IV", "V")
)

to_roman <- function(number) {
    if (number == 0) {
        return("")
    }
    largest_digit <- largest_usable_digit(number)
    paste0(
        roman_digits[arabic == largest_digit, roman],
        to_roman(number - largest_digit)
    )
}

largest_usable_digit <- function(number) {
    max(roman_digits[arabic <= number, arabic])
}
