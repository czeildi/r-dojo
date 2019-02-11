areaOfOverlap <- function(rectangleA, rectangleB) {
    if (isPoint(rectangleA) | isPoint(rectangleB)) {
        0
    } else if (isTRUE(all.equal(rectangleA, rectangleB))){
        areaOfRectangle(rectangleA)
    } else {
        xBl <- max(rectangleA[["bl"]]["x"], rectangleB[["bl"]]["x"])
        xTr <- min(rectangleA[["tr"]]["x"], rectangleB[["tr"]]["x"])
        areaOfRectangle(
            list(
                "bl" = c(x = xBl, y = rectangleA[["bl"]]["y"]), 
                "tr" = c(x = xTr, y = rectangleB[["tr"]]["y"])
            )
        )
    }
}

isPoint <- function(rectangle) {
    isTRUE(all.equal(rectangle[["bl"]], rectangle[["tr"]]))
}

areaOfRectangle <- function(rectangle) {
    lengthOfSide(rectangle, "x") * lengthOfSide(rectangle, "y")
}

lengthOfSide <- function(rectangle, axis) {
    as.numeric(rectangle[["tr"]][axis] - rectangle[["bl"]][axis])
}
