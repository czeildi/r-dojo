context("dojo")

test_that("returns number if input is single digit number", {
    expect_equal(
        lastDigit(6),
        6
    )
})

test_that("returns last digit of number if input is multiple digit number", {
    expect_equal(
        lastDigit(32),
        2
    )
})

test_that("returns last digit of power if input is 2 storey power", {
    expect_equal(
        lastDigit(c(9, 2)),
        1
    )
})

test_that("returns last digit of power if input is 3 storey power", {
    expect_equal(
        lastDigit(c(2, 2, 2)),
        6
    )
})

test_that("returns last digit of power if input is 2 storey power with large number in base", {
    expect_equal(
        lastDigit(c(1123456789, 2)),
        1
    )
})

test_that("returns last digit when 6 is raised to the power of a huge number", {
    expect_equal(
        lastDigit(c(6, 123456789)),
        6
    )
})
