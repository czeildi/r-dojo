source("../src/dojo.R")

context("dojo")

test_that("reformatted empty string is empty string", {
    expect_equal(
        reformat(""),
        ""
    )
})

test_that("one liners are returned as is", {
    expect_equal(
        reformat("one  = liner"),
        "one  = liner"
    )
})

test_that("if 2nd line starts with 1-char shorter LHS, 1 space is inserted", {
    expect_equal(
        reformat(c(
            "aa = bb",
            "a = bb"
        )),
        c(
            "aa = bb",
            "a  = bb"
        )
    )
})

test_that("if 2nd line does not contain operator, it is returned as is", {
    expect_equal(
        reformat(c(
            "aaaa = bb",
            "a b"
        )),
        c(
            "aaaa = bb",
            "a b"
        )   
    )
})

test_that("if 2nd line starts with n-char shorter LHS, n spaces are inserted", {
    expect_equal(
        reformat(c(
            "aaaa = bb",
            "a = bb"
        )),
        c(
            "aaaa = bb",
            "a    = bb"
        )   
    )
})

test_that("reformats all lines such that lhs is not shorter than first line", {
    expect_equal(
        reformat(c(
            "aaaa = bb",
            "aaa = bb",
            "aa = bb"
        )),
        c(
            "aaaa = bb",
            "aaa  = bb",
            "aa   = bb"
        )
    )
})

test_that("reformats all lines to align with longest LHS", {
    expect_equal(
        reformat(c(
            "aa = bb",
            "aaaa = bb",
            "aaa = bb"
        )),
        c(
            "aa   = bb",
            "aaaa = bb",
            "aaa  = bb"
        )
    )
})
