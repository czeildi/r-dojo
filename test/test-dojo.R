source("../src/dojo.R")

context("last digit of power")

test_that("anything on the zeroth returns one", {
    expect_equal(lastDigitOfPower(base = 5, exponent = 0), 1)
})

test_that("any integer less than 10 on the first returns the last digit of the base", {
    expect_equal(lastDigitOfPower(base = 2, exponent = 1), 2)
})

test_that("anything on the first returns the last digit of the base", {
    expect_equal(lastDigitOfPower(base = 23, exponent = 1), 3)
})

test_that("returns last digit of power if small number is squared", {
    expect_equal(lastDigitOfPower(base = 23, exponent = 2), 9)
})

test_that("returns last digit of power if big number is squared", {
    expect_equal(lastDigitOfPower(base = 233333333, exponent = 2), 9)
})

test_that("returns last digit of power if power of base's last digit is >10", {
    expect_equal(lastDigitOfPower(base = 3, exponent = 4), 1)
})

test_that("returns last digit of power if exponent is a big number", {
    expect_equal(lastDigitOfPower(base = 5, exponent = 100), 5)
})

test_that("returns last digit of power if exponent is a big number and digit has longer period", {
    expect_equal(lastDigitOfPower(base = 9, exponent = 100), 1)
})
