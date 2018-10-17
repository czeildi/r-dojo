library(purrr)
library(stringr)

reformat <- function(lines) {
    length_of_standard_lhs <- calculate_length_of_standard_lhs(lines)
    
    modify_vec_if(
        lines, 
        contains_operator,
        ~reformat_line(.x, length_of_standard_lhs) 
    )
}

calculate_length_of_standard_lhs <- function(lines) {
    lhs_lengths <- map_int(lines, ~str_length(lhs(.x)))
    max(lhs_lengths)
}

contains_operator <- function(line) {
    !is.na(rhs(line))
}

reformat_line <- function(line, length_of_standard_lhs) {
    spaces <- additional_lhs_spaces(line, length_of_standard_lhs)
    
    paste0(
        lhs(line), spaces,
        "=",
        rhs(line)
    )
}

additional_lhs_spaces <- function(line, length_of_standard_lhs) {
    num_needed_spaces <- length_of_standard_lhs - str_length(lhs(line))
    rep(" ", num_needed_spaces) %>% 
        paste(collapse = "")
}

lhs <- function(line) {
    str_split(line, "=")[[1]][1]
}

rhs <- function(line) {
    str_split(line, "=")[[1]][2]
}

modify_vec_if <- function(.x, .p, .f, ...) {
    modify_if(.x, .p, .f, ...) %>% unlist()
}
