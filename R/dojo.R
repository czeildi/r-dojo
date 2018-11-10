library("stringr")

reformat <- function(text) {
    if (length(text) < 2) {
        return(text)
    }
    longest_lhs <- max(lengthOfLhs(text), na.rm = TRUE)
    map_chr(text, ~reformatLine(.x, longest_lhs))
}

reformatLine <- function(line, longest_lhs) {
    if (!str_detect(line, "=")) {
        return(line)
    }
    current_lhs <- lengthOfLhs(line)
    spaces_to_insert <- createSpacesToInsert(longest_lhs, current_lhs)
    insertSpacesBeforeOperator(line, spaces_to_insert)
}


insertSpacesBeforeOperator <- function(line, spaces_to_insert) {
    str_replace(line, "=", paste0(spaces_to_insert, "="))
}

createSpacesToInsert <- function(longest_lhs, current_lhs) {
    num_spaces_needed <- longest_lhs - current_lhs
    paste(rep(" ", num_spaces_needed), collapse = "")
}

lengthOfLhs <- function(lines) {
    str_locate(lines, "=")[, 1] - 1
}
