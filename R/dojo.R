library("stringr")

findLongestPart <- function(input) {
    if (!isInputValid(input)) {
        return("no valid substring")
    }
    part_indices <- getSubPartIndex(input)
    printResult(part_indices)
}

getSubPartIndex <- function(input) {
    list(
        "start" = str_locate(input, "\\(")[1, "start"],
        "end" = str_locate(input, "\\)")[1, "start"]
    )
}

isInputValid <- function(input) {
    part_indices <- getSubPartIndex(input)
    isTRUE(part_indices$start < part_indices$end)
}

printResult <- function(part_indices) {
    glue(
        "longest valid substring is between characters ",
        "{part_indices$start}, {part_indices$end}"
    )
}
