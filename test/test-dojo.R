source("../src/dojo.R")

context("dojo")

test_that("one step of one digit of 1", {
    expect_equal(
        look_and_say('1', 1),
        '11'
    )
})

test_that("one step of one digit of any type", {
    expect_equal(
        look_and_say('3', n = 1),
        '13'
    )
})

test_that("one step of two digit of 1", {
    expect_equal(
        look_and_say('11', n = 1),
        '21'
    )
})

test_that("one step of two different digits", {
    expect_equal(
        look_and_say('12', n = 1),
        '1112'
    )
})

test_that("one step of two different alternating digits", {
    expect_equal(
        look_and_say('121', n = 1),
        '111211'
    )
})
