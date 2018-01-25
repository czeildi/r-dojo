library("stringr")

wrap <- function(text, columnLength) {
    if (str_length(text) <= columnLength) {
        return(text)
    }
    firstLine <- getFirstLine(text, columnLength)
    remainingText <- getRemainingText(text, firstLine)

    str_c(firstLine, "\n", wrap(remainingText, columnLength))
}

getFirstLine <- function(text, columnLength) {
    firstLineLength <- calculateFirstLineLength(text, columnLength)
    str_sub(text, 1, firstLineLength)
}

getRemainingText <- function(text, firstLine) {
    str_trim(str_sub(text, str_length(firstLine) + 1))
}

calculateFirstLineLength <- function(text, columnLength) {
    firstChunkWOptionalSpace <- str_sub(text, 1, columnLength + 1)
    spaceIndexesInFirstLine <- str_locate_all(firstChunkWOptionalSpace, " ")[[1]][, 1]
    lastSpaceIndex <- tail(spaceIndexesInFirstLine, 1)
    min(columnLength, lastSpaceIndex - 1)
}
