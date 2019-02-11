context("dojo")

test_that("if one rectangle is a point, overlap area is 0", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 0, y = 0)),
            list("bl" = c(x = 3, y = 0), "tr" = c(x = 7, y = 6))
        ),
        0
    )
})

test_that("if all of the rectangles is a point, the area is zero", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 0, y = 0)),
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 0, y = 0))
        ),
        0
    )
})

test_that("if the two rectange is the same unit square, the overlap area is the area of one of the rectangles ", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 1, y = 1)),
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 1, y = 1))
        ),
        1
    )
})


test_that("if any of the rectangles is a point, the area is zero", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 1, y = 1)),
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 0, y = 0))
        ),
        0
    )
})

test_that("if the two rectangle is the same origo based square, the overlap area is the area of one of the rectangles", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 2, y = 2)),
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 2, y = 2))
        ),
        4
    )
})

test_that("if the two rectangle is the same square, the overlap area is the area of one of the rectangles", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 1, y = 1), "tr" = c(x = 3, y = 3)),
            list("bl" = c(x = 1, y = 1), "tr" = c(x = 3, y = 3))
        ),
        4
    )
})

test_that("if the two rectangle is the same, the overlap area is the area of one of the rectangles", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 1, y = 1), "tr" = c(x = 4, y = 3)),
            list("bl" = c(x = 1, y = 1), "tr" = c(x = 4, y = 3))
        ),
        6
    )
})

test_that("overlap is calculated id x side has overlap, y side is the same", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 3, y = 1)),
            list("bl" = c(x = 2, y = 0), "tr" = c(x = 4, y = 1))
        ),
        1
    )
})

test_that("overlap is calculated id x side has overlap, y side is the same and rectangles are in reverse order", {
    expect_equal(
        areaOfOverlap(
            list("bl" = c(x = 2, y = 0), "tr" = c(x = 4, y = 1)),
            list("bl" = c(x = 0, y = 0), "tr" = c(x = 3, y = 1))
        ),
        1
    )
})
