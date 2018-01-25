source("../src/dojo.R")

context("wrap")

test_that("wrapped empty string with >0 num col is empty string", {
    expect_equal(
        wrap("", 3),
        ""
    )
})

test_that("when the text is shorter than max line length it should return the input", {
    expect_equal(
        wrap("alma", 30),
        "alma"
    )
})

test_that("when text is one word that fits on two lines, word is broken at line length", {
    expect_equal(
        wrap("aaaaa", 3),
        "aaa\naa"
    )
})

test_that("when text is one word that fits only on several lines, word is broken at line length multiples", {
    expect_equal(
        wrap("abcdefghijk", 3),
        "abc\ndef\nghi\njk"
    )
})

test_that("when text is exactly one line long, return the input", {
    expect_equal(
        wrap("abc", 3),
        "abc"
    )
})

test_that("wrap on space if possible", {
    expect_equal(
        wrap("aaa aaa", 5),
        "aaa\naaa"
    )
})

test_that("wrap on latest possible space", {
    expect_equal(
        wrap("ab def gh", 6),
        "ab def\ngh"
    )
})

test_that("wrap on last space before line break", {
    expect_equal(
        wrap("abc def ghi jkl", 9),
        "abc def\nghi jkl"
    )
})

test_that("wraps compicated text correctly", {
    expect_equal(
        wrap("quick Helicopter jumped over the red fox", 9),
        "quick\nHelicopte\nr jumped\nover the\nred fox"
    )
})

test_that("it works for unicode", {
    expect_equal(
        wrap("árvíztűrőtükörfúrógép", 4),
        "árví\nztűr\nőtük\nörfú\nrógé\np"
    )
})

test_that("wraps compicated text correctly", {
    expect_equal(
        wrap(paste(rep("a", 300), collapse = ""), 1),
        paste(rep("a", 300), collapse = "\n")
    )
})
