source("../src/dojo.R")

context("pancake sort")


test_that("sorted empty vector is same empty vector", {
    expect_equal(
        pancake_sort(numeric(0)),
        numeric(0)
    )
})

test_that("sorted one element vector is the same vector", {
    expect_equal(
        pancake_sort(c(12)),
        c(12)
    )
})

test_that("sorts two element vector in reversed order", {
    expect_equal(
        pancake_sort(c(2, 1)),
        c(1, 2)
    )
})

test_that("two elements in order: no change", {
    expect_equal(
        pancake_sort(c(1, 2)),
        c(1, 2)
    )
})

test_that("three elements, last two in reversed order", {
    expect_equal(
        pancake_sort(c(1, 3, 2)),
        1:3
    )
})

test_that("three elements in order: no change", {
    expect_equal(
        pancake_sort(1:3),
        1:3
    )
})

test_that("three elements in reverse order: correctly sorted", {
    expect_equal(
        pancake_sort(c(3, 2, 1)),
        1:3
    )
})

test_that("three elements in mixed order: correctly sorted", {
    expect_equal(
        pancake_sort(c(3, 1, 2)),
        1:3
    )
})

test_that("very complicated case: works on not small input", {
    expect_equal(
        pancake_sort(c(3, 8, 4, 1, 7, 5, 2, 6)),
        1:8
    )
})
